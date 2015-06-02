package lambdajam

import scala.concurrent.Future

import java.io.File

import scalaz._, Scalaz._

object Extensions {

  def tMap[M[_], A, B](ma: M[A])(f: Result[A] => Result[B])(implicit M: MonadResult[M]): M[B] = ma.flatMap { a =>
    f(Result.ok(a)).fold(_.point[M], M.raiseError)
  }

  def relMonad[R[_], M[_]](implicit M: Monad[M], RM: RelMonad[M, R]): Monad[R] = new Monad[R] {
    def point[A](v: => A): R[A] = RM.rPoint(M.point(v))
    def bind[A, B](ma: R[A])(f: A => R[B]): R[B] = RM.rBind(ma)(M.lift(f))
  }

  val fsRelReader: RelMonad[({ type λ[α] = Reader[File, α] })#λ, FS] = new RelMonad[({ type λ[α] = Reader[File, α] })#λ, FS] {
    override def rPoint[A](v: => Reader[File, A]): FS[A] = FS(v.run andThen Result.ok)
    override def rBind[A, B](ma: FS[A])(f: (Reader[File, A]) => Reader[File, FS[B]]): FS[B] = FS {
      file => ma.runFS(file).fold(a => f(Reader(_ => a)).run(file).runFS(file), error => Result.error(error))
    }
  }

  val futureRelResult: RelMonad[Result, Future] = new RelMonad[Result, Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.util.{ Failure, Success }
    override def rPoint[A](v: => Result[A]): Future[A] =
      Future.fromTry(v.fold(Success.apply, error => Failure(new Exception(error)) ))
    override def rBind[A, B](ma: Future[A])(f: (Result[A]) => Result[Future[B]]): Future[B] =
      ma.flatMap(a => rPoint(f(Result.ok(a)))).flatMap(identity)
  }
}

object RelReader {
  @inline def apply[R[_], X](implicit RM: RelMonad[({ type λ[α] = Reader[X, α] })#λ, R]) = RM

  /*
  def rAsk[R[_], X, A](implicit RM: RelMonad[({ type λ[α] = Reader[X, α] })#λ, R]): R[A] = {
    type Z[α] = Reader[X, α]
    val monad: Monad[R] = Extensions.relMonad[R, Z](implicitly[Monad[Z]], RM)
    monad.point()
    RM.rPoint()
  }*/

  def rAsk[R[_], X](implicit RM: RelMonad[({ type λ[α] = Reader[X, α] })#λ, R]): R[X] =
    RM.rPoint(Reader(identity))

}

final class RelReaderOps[R[_], X, A](val self: R[A])(implicit RM: RelMonad[({ type λ[α] = Reader[X, α] })#λ, R]) {
  def rLocal(f: X => X): R[A] =
    RM.rBind(RM.rPoint(Reader(f))) { mx =>
      Reader{ _: X => self }.contramap(mx.run)
    }
}

trait ToRelReaderOps {
  implicit def ToRelReaderOps[R[_], X, A](v: R[A])(implicit M0: RelMonad[({ type λ[α] = Reader[X, α] })#λ, R]): RelReaderOps[R, X, A] =
    new RelReaderOps[R, X, A](v)
}

object RelReaderSyntax extends ToRelReaderOps
