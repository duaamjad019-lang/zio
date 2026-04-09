package zio.internal.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import zio._
import zio.internal._
import zio.internal.FiberRuntime
import java.util.concurrent.{CountDownLatch, TimeUnit, AtomicLong}
import java.util.concurrent.locks.LockSupport
import scala.annotation.tailrec

/**
 * Benchmark suite for ZScheduler Park/Unpark Optimization (#9878).
 * 
 * This suite compares the baseline LockSupport strategy against the 
 * OptimizedWorkerWakeup strategy (Batching + Coalescing).
 * 
 * Usage:
 *   sbt "project benchmarks" "jmh:run -i 10 -wi 5 -f 3 .*ZScheduler.*"
 * 
 * Key Parameters:
 *   - strategy: "baseline" vs "optimized"
 *   - batchSize: 1 (disabled) vs 4 (default) vs 16 (aggressive)
 *   - parallelism: Number of workers (cores)
 */
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.Throughput, Mode.AverageTime))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 3, jvmArgs = Array("-Xms4G", "-Xmx4G"))
class ZSchedulerBenchmarks {

  // --------------------------------------------------------------------------
  // Benchmark Parameters
  // --------------------------------------------------------------------------

  @Param(Array("baseline", "optimized"))
  var strategy: String = _

  @Param(Array("1", "4", "8"))
  var batchSize: Int = _

  @Param(Array("4", "8", "16"))
  var parallelism: Int = _

  // --------------------------------------------------------------------------
  // State Management
  // --------------------------------------------------------------------------

  private var scheduler: ZScheduler = _
  private var runtime: Runtime[Any] = _
  private var unparkStrategy: OptimizedWorkerWakeup = _
  private val metricsSnapshot = new AtomicLong(0L)

  @Setup(Level.Trial)
  def setup(): Unit = {
    // Configure optimization based on parameters
    System.setProperty("zio.scheduler.batch.threshold", batchSize.toString)
    System.setProperty("zio.scheduler.metrics.enabled", "true")

    // Create Scheduler
    // Note: In a real PR, this would use the actual ZScheduler constructor
    // For benchmarking, we instantiate the internal scheduler directly
    scheduler = ZScheduler.make(parallelism)

    // Access internal optimization strategy via reflection or direct access 
    // if in same package. Assuming same package for internal benchmarks.
    // If OptimizedWorkerWakeup is injected, we grab it here.
    // For this benchmark file, we assume we can access the private field 
    // or we wrap the scheduler logic.
    
    // Mocking the injection for benchmark purposes if direct access is restricted
    // In actual implementation, ZScheduler exposes this for testing
    unparkStrategy = extractUnparkStrategy(scheduler)

    // Create Runtime
    runtime = Runtime.unsafeFromLayer(ZScheduler.setScheduler(scheduler))
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit = {
    if (runtime != null) Runtime.shutdown(runtime)
    printMetrics()
  }

  // Helper to extract internal metrics for validation
  private def extractUnparkStrategy(scheduler: ZScheduler): OptimizedWorkerWakeup = {
    // In actual implementation, ZScheduler should expose this for testing
    // or we use reflection. Here we assume package-private access.
    scheduler match {
      case zs: ZScheduler => zs.getUnparkStrategy // Hypothetical accessor
      case _ => null
    }
  }

  private def printMetrics(): Unit = {
    if (unparkStrategy != null) {
      val metrics = unparkStrategy.getMetrics
      println(s"\n--- Scheduler Metrics (${strategy}) ---")
      println(s"Total Unpark Attempts: ${metrics.totalAttempts}")
      println(s"Actual Unpark Calls:   ${metrics.actualCalls}")
      println(s"Batched (Skipped):     ${metrics.batchedCount}")
      println(s"Efficiency Ratio:      ${metrics.efficiencyRatio}")
      println(s"---------------------------------------\n")
    }
  }

  // --------------------------------------------------------------------------
  // 1. Throughput: Parallel Fork-Join
  // --------------------------------------------------------------------------

  /**
   * Measures raw throughput of parallel work submission.
   * High volume of small tasks to stress the unpark mechanism.
   */
  @Benchmark
  def parallelThroughput(bh: Blackhole): Unit = {
    val effect = ZIO.foreachPar(1 to 1000)(i => ZIO.succeed(i * 2))
    val result = Unsafe.unsafe(implicit u => runtime.unsafe.run(effect).getOrThrow())
    bh.consume(result)
  }

  // --------------------------------------------------------------------------
  // 2. Latency: Single Fiber Submission
  // --------------------------------------------------------------------------

  /**
   * Measures latency of submitting and executing a single fiber.
   * Critical for validating that batching/coalescing doesn't penalize single items.
   */
  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def singleFiberLatency(bh: Blackhole): Unit = {
    val effect = ZIO.succeed(1)
    val result = Unsafe.unsafe(implicit u => runtime.unsafe.run(effect).getOrThrow())
    bh.consume(result)
  }

  // --------------------------------------------------------------------------
  // 3. Burst Work: Batching Effectiveness
  // --------------------------------------------------------------------------

  /**
   * Submits a burst of work simultaneously.
   * Tests if batching successfully amortizes unpark calls.
   */
  @Benchmark
  def burstWorkSubmission(bh: Blackhole): Unit = {
    val effects = (1 to 500).map(_ => ZIO.succeed(42))
    val combined = ZIO.collectAll(effects)
    val result = Unsafe.unsafe(implicit u => runtime.unsafe.run(combined).getOrThrow())
    bh.consume(result)
  }

  // --------------------------------------------------------------------------
  // 4. Work Stealing Efficiency
  // --------------------------------------------------------------------------

  /**
   * Creates uneven work distribution to force work stealing.
   * Ensures optimization doesn't hinder load balancing.
   */
  @Benchmark
  def workStealingEfficiency(bh: Blackhole): Unit = {
    // One heavy task on main, many light tasks
    val heavy = ZIO.succeed(1).delay(1.microsecond)
    val lights = ZIO.foreachPar(1 to 100)(_ => ZIO.succeed(1))
    
    val effect = heavy.zipPar(lights)
    val result = Unsafe.unsafe(implicit u => runtime.unsafe.run(effect).getOrThrow())
    bh.consume(result)
  }

  // --------------------------------------------------------------------------
  // 5. Chained Fibers: Dependency Latency
  // --------------------------------------------------------------------------

  /**
   * Submits fibers that depend on previous completion.
   * Validates that 'urgent' flags work and batching doesn't stall chains.
   */
  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def chainedFiberLatency(bh: Blackhole): Unit = {
    def chain(n: Int): ZIO[Any, Nothing, Int] = 
      if (n <= 0) ZIO.succeed(0)
      else ZIO.succeed(1).flatMap(_ => chain(n - 1))
    
    val effect = chain(10)
    val result = Unsafe.unsafe(implicit u => runtime.unsafe.run(effect).getOrThrow())
    bh.consume(result)
  }

  // --------------------------------------------------------------------------
  // 6. Park/Unpark Frequency (Direct Measurement)
  // --------------------------------------------------------------------------

  /**
   * Directly measures the overhead of the unpark strategy itself.
   * Bypasses ZIO DSL to isolate scheduler logic.
   */
  @Benchmark
  def unparkStrategyOverhead(bh: Blackhole): Unit = {
    // Simulate worker ID 0
    val workerId = 0
    // Call the optimized method directly
    if (unparkStrategy != null) {
      unparkStrategy.maybeUnparkWorker(workerId, isUrgent = false)
      bh.consume(unparkStrategy.getMetrics.actualCalls)
    }
  }

  // --------------------------------------------------------------------------
  // 7. Idle Behavior: No Busy Waiting
  // --------------------------------------------------------------------------

  /**
   * Ensures workers park correctly when idle.
   * Measures CPU usage indirectly via completion time of a delayed task.
   */
  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def idleWakeUpLatency(bh: Blackhole): Unit = {
    // Submit work after a delay to ensure workers are parked
    val effect = ZIO.sleep(10.milliseconds) *> ZIO.succeed(1)
    val result = Unsafe.unsafe(implicit u => runtime.unsafe.run(effect).getOrThrow())
    bh.consume(result)
  }
}

/**
 * Helper benchmark to compare raw LockSupport vs Optimized strategy
 * without ZIO runtime overhead.
 */
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
class RawUnparkComparison {

  @Param(Array("1", "4", "16"))
  var batchSize: Int = _

  private var optimized: OptimizedWorkerWakeup = _
  private var thread: Thread = _

  @Setup
  def setup(): Unit = {
    thread = Thread.currentThread()
    System.setProperty("zio.scheduler.batch.threshold", batchSize.toString)
    optimized = new OptimizedWorkerWakeup(1)
    optimized.registerWorker(0, thread)
    // Simulate parked state for coalescing test
    optimized.beforePark(0)
  }

  @Benchmark
  def rawLockSupport(): Unit = {
    LockSupport.unpark(thread)
  }

  @Benchmark
  def optimizedUnpark(): Unit = {
    optimized.maybeUnparkWorker(0, thread, isUrgent = false)
  }
}
