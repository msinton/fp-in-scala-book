package parallel

import java.util.concurrent.{Future, TimeUnit}


private case class UnitFuture[A](get: A) extends Future[A] {

  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

  override def isCancelled: Boolean = false

  override def isDone: Boolean = true

  override def get(timeout: Long, unit: TimeUnit): A = get
}
