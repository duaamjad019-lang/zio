package zio.internal

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.util.concurrent.locks.LockSupport
import scala.annotation.tailrec

/**
 * Optimized strategy for unparking ZScheduler workers.
 * 
 * Optimization Strategy:
 * 1. Park State Coalescing: Only unpark if the worker is actually parked.
 * 2. Batching: Amortize unpark costs over multiple work items (configurable).
 * 3. Metrics: Track unpark frequency for monitoring.
 * 
 * @param maxWorkers Total number of workers in the scheduler
 */
private[zio] final class OptimizedWorkerWakeup(maxWorkers: Int) {

  // Configuration
  private val batchThreshold = 
    System.getProperty("zio.scheduler.batch.threshold", "4").toInt
  private val metricsEnabled = 
    System.getProperty("zio.scheduler.metrics.enabled", "false").toBoolean

  // State per worker
  // Tracks if the worker is currently parked (waiting for work)
  private val parkedState = Array.fill(maxWorkers)(new AtomicBoolean(false))
  
  // Tracks pending unpark requests for batching
  private val pendingUnparks = Array.fill(maxWorkers)(new AtomicLong(0L))

  // Metrics
  private val totalUnparkAttempts = new AtomicLong(0L)
  private val actualUnparkCalls = new AtomicLong(0L)
  private val batchedUnparks = new AtomicLong(0L)

  /**
   * Register a worker thread with the strategy.
   * Must be called during worker initialization.
   */
  def registerWorker(workerId: Int, thread: Thread): Unit = {
    // Validation to prevent ID out of bounds
    if (workerId < 0 || workerId >= maxWorkers) {
      throw new IllegalArgumentException(s"Invalid workerId: $workerId")
    }
    // Thread reference is managed by ZScheduler Worker class, 
    // we rely on workerId for lookup here to avoid storing Thread refs twice.
  }

  /**
   * Called by the worker just before it parks.
   * Sets the state to 'parked' so offers know to wake it.
   */
  def beforePark(workerId: Int): Unit = {
    parkedState(workerId).set(true)
  }

  /**
   * Called by the worker immediately after waking up or deciding not to park.
   * Resets the state to 'active'.
   */
  def afterUnpark(workerId: Int): Unit = {
    parkedState(workerId).set(false)
    // Reset pending count on wake to avoid stale batches
    pendingUnparks(workerId).set(0L)
  }

  /**
   * Optimized unpark invocation.
   * 
   * Logic:
   * 1. Increment pending count.
   * 2. If worker is NOT parked, skip unpark (coalescing).
   * 3. If worker IS parked, check batch threshold.
   * 4. If threshold met OR urgent, perform unpark.
   */
  def maybeUnparkWorker(workerId: Int, isUrgent: Boolean = false): Unit = {
    if (metricsEnabled) totalUnparkAttempts.incrementAndGet()

    if (workerId < 0 || workerId >= maxWorkers) return

    val isParked = parkedState(workerId).get()
    
    // OPTIMIZATION 1: Coalescing
    // If the worker is already active (not parked), do not unpark.
    // The worker will pick up the work from the queue in its next loop.
    if (!isParked) {
      // Still track pending for potential future parking cycles if needed, 
      // but generally we can skip counting if not parked.
      return
    }

    // OPTIMIZATION 2: Batching
    if (!isUrgent) {
      val pending = pendingUnparks(workerId).incrementAndGet()
      if (pending < batchThreshold) {
        if (metricsEnabled) batchedUnparks.incrementAndGet()
        return // Batch not full, delay unpark
      }
      // Reset counter as we are about to unpark
      pendingUnparks(workerId).set(0L)
    }

    // Perform the actual unpark
    // Note: We need the Thread reference. 
    // In ZScheduler, Worker holds the Thread. 
    // This method assumes access to Worker thread map or passed thread.
    // For this implementation plan, we assume ZScheduler passes the Thread.
    // See Integration Step 2 for adjustment.
    executeUnpark(workerId)
  }

  // Helper to handle the actual LockSupport call 
  // (Requires Thread reference, handled in integration)
  private def executeUnpark(workerId: Int): Unit = {
    // This method is a placeholder. 
    // In actual integration, ZScheduler passes the Thread object.
    // See Integration Step 2 below for the correct signature.
    if (metricsEnabled) actualUnparkCalls.incrementAndGet()
  }

  /**
   * Direct unpark bypassing batching (used for urgent work or shutdown).
   */
  def forceUnparkWorker(workerId: Int, thread: Thread): Unit = {
    if (metricsEnabled) {
      totalUnparkAttempts.incrementAndGet()
      actualUnparkCalls.incrementAndGet()
    }
    LockSupport.unpark(thread)
    parkedState(workerId).set(false)
    pendingUnparks(workerId).set(0L)
  }

  /**
   * Retrieve current metrics.
   */
  def getMetrics: UnparkMetrics = {
    UnparkMetrics(
      totalAttempts = totalUnparkAttempts.get(),
      actualCalls = actualUnparkCalls.get(),
      batchedCount = batchedUnparks.get(),
      efficiencyRatio = if (totalUnparkAttempts.get() == 0) 0.0 
                        else actualUnparks.get().toDouble / totalUnparkAttempts.get()
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
