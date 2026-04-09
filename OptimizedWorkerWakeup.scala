package zio.internal

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.atomic.AtomicReferenceArray

private[zio] final class OptimizedWorkerWakeup(maxWorkers: Int) {

  private val batchThreshold: Int =
    System.getProperty("zio.scheduler.batch.threshold", "4").toInt
  private val metricsEnabled: Boolean =
    System.getProperty("zio.scheduler.metrics.enabled", "false").toBoolean

  private val parkedState = Array.fill(maxWorkers)(new AtomicBoolean(false))
  private val pendingUnparks = Array.fill(maxWorkers)(new AtomicLong(0L))

  // store worker Thread references so we can unpark them
  private val workerThreads = new AtomicReferenceArray[Thread](maxWorkers)

  private val totalUnparkAttempts = new AtomicLong(0L)
  private val actualUnparkCalls = new AtomicLong(0L)
  private val batchedUnparks = new AtomicLong(0L)

  def registerWorker(workerId: Int, thread: Thread): Unit = {
    if (workerId < 0 || workerId >= maxWorkers) {
      throw new IllegalArgumentException(s"Invalid workerId: $workerId")
    }
    if (thread != null) workerThreads.set(workerId, thread)
  }

  def beforePark(workerId: Int): Unit =
    parkedState(workerId).set(true)

  def afterUnpark(workerId: Int): Unit = {
    parkedState(workerId).set(false)
    pendingUnparks(workerId).set(0L)
  }

  /**
   * Optimized unpark invocation.
   *
   * Accepts a Thread to avoid an extra lookup when available.
   */
  def maybeUnparkWorker(workerId: Int, thread: Thread, isUrgent: Boolean = false): Unit = {
    if (metricsEnabled) totalUnparkAttempts.incrementAndGet()

    if (workerId < 0 || workerId >= maxWorkers) return

    val isParked = parkedState(workerId).get()

    // Coalescing: if not parked, skip unpark
    if (!isParked) return

    // Batching (unless urgent)
    if (!isUrgent) {
      val pending = pendingUnparks(workerId).incrementAndGet()
      if (pending < batchThreshold) {
        if (metricsEnabled) batchedUnparks.incrementAndGet()
        return
      }
      pendingUnparks(workerId).set(0L)
    }

    // perform unpark using provided thread or stored thread
    val t = if (thread != null) thread else workerThreads.get(workerId)
    executeUnpark(workerId, t)
  }

  private def executeUnpark(workerId: Int, thread: Thread): Unit = {
    if (thread == null) return
    if (metricsEnabled) actualUnparkCalls.incrementAndGet()
    LockSupport.unpark(thread)
    parkedState(workerId).set(false)
    pendingUnparks(workerId).set(0L)
  }

  def forceUnparkWorker(workerId: Int, thread: Thread): Unit = {
    if (metricsEnabled) {
      totalUnparkAttempts.incrementAndGet()
      actualUnparkCalls.incrementAndGet()
    }
    if (thread != null) LockSupport.unpark(thread)
    parkedState(workerId).set(false)
    pendingUnparks(workerId).set(0L)
  }

  def getMetrics: UnparkMetrics = {
    val total = totalUnparkAttempts.get()
    val actual = actualUnparkCalls.get()
    val ratio = if (total == 0) 0.0 else actual.toDouble / total.toDouble
    UnparkMetrics(
      totalAttempts = total,
      actualCalls = actual,
      batchedCount = batchedUnparks.get(),
      efficiencyRatio = ratio
    )
  }

  def resetMetrics(): Unit = {
    totalUnparkAttempts.set(0L)
    actualUnparkCalls.set(0L)
    batchedUnparks.set(0L)
  }
}

private[zio] final case class UnparkMetrics(
  totalAttempts: Long,
  actualCalls: Long,
  batchedCount: Long,
  efficiencyRatio: Double
)
