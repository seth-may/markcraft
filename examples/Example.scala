import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success, Failure}

// --- Type-safe Builder with Phantom Types ---
sealed trait HasHost; sealed trait HasPort
case class ServerConfig[H, P](host: String = "", port: Int = 0, ssl: Boolean = false)

object ServerConfig {
  def builder: ServerConfig[Nothing, Nothing] = ServerConfig()
  implicit class Ops[H, P](val c: ServerConfig[H, P]) extends AnyVal {
    def withHost(h: String): ServerConfig[HasHost, P] = ServerConfig(h, c.port, c.ssl)
    def withPort(p: Int): ServerConfig[H, HasPort] = ServerConfig(c.host, p, c.ssl)
    def withSSL: ServerConfig[H, P] = c.copy(ssl = true)
  }
  def build(c: ServerConfig[HasHost, HasPort]): ServerConfig[HasHost, HasPort] = c
}

// --- Effect System ---
sealed trait IO[+A] { def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f) }
case class Pure[A](value: A) extends IO[A]
case class Suspend[A](thunk: () => A) extends IO[A]
case class FlatMap[A, B](io: IO[A], f: A => IO[B]) extends IO[B]

object IO {
  def pure[A](a: A): IO[A] = Pure(a)
  def suspend[A](a: => A): IO[A] = Suspend(() => a)
  def run[A](io: IO[A]): A = io match {
    case Pure(a) => a
    case Suspend(t) => t()
    case FlatMap(io2, f) => run(f(run(io2)))
  }
}

// --- Tagless Final ---
trait Console[F[_]] {
  def readLine: F[String]
  def printLine(s: String): F[Unit]
}

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

def program[F[_]: Monad: Console]: F[Unit] = {
  val M = implicitly[Monad[F]]; val C = implicitly[Console[F]]
  import M._; import C._
  flatMap(printLine("What is your name?"))(_ =>
    flatMap(readLine)(name => printLine(s"Hello, $$name!")))
}
