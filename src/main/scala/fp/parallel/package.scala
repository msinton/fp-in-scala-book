package fp

import java.util.concurrent.{ExecutorService, Future}

package object parallel {

  type Par[A] = ExecutorService => Future[A]
}
