package raytracer.yaml

import scala.util.control.NonFatal
import ParseResult._

sealed abstract class ParseResult[+A] {
  def map[B](f: A => B): ParseResult[B]
  def flatMap[B](f: A => ParseResult[B]): ParseResult[B]
  def isSuccess: Boolean
  def withMessage(msg: String): ParseResult[A] = fold(_.copy(message = msg), identity)
  def withDetail(detail: YamlMap): ParseResult[A] = fold(_.copy(detail = Some(detail)), identity)
  @inline final def orElse[B >: A](alternative: => ParseResult[B]): ParseResult[B] =
    if (isSuccess) this else alternative

  def fold[B](failedF: Failed[A] => B, successF: Success[A] => B) = {
    this match {
      case f: Failed[A] => failedF(f)
      case s: Success[A] => successF(s)
    }
  }

  final def get: A = {
    this match {
      case Success(value) => value
      case Failed(message, detail, cause) =>
        throw new ParseException(
          s"""
            |$message
            | detail: $detail
            | cause: $cause
          """.stripMargin
        )
    }
  }

}

object ParseResult {

  case class Success[+A](value: A) extends ParseResult[A] {
    override def map[B](f: A => B): ParseResult[B] = Success(f(value))
    override def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = f(value)
    override def isSuccess: Boolean = true
  }

  case class Failed[+A](
    message: String,
    detail: Option[YamlMap],
    cause: Option[Throwable]) extends ParseResult[A] {

    override def map[B](f: A => B): ParseResult[B] = this.asInstanceOf[ParseResult[B]]
    override def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = this.asInstanceOf[ParseResult[B]]
    override def isSuccess: Boolean = false

  }

  case class ParseException(msg: String) extends RuntimeException(msg)

  def wrap[A](code: => A): ParseResult[A] = {
    flatWrap(Success(code))
  }

  def flatWrap[A](code: => ParseResult[A]): ParseResult[A] = {
    try code
    catch {
      case NonFatal(ex) => fail[A](ex)
    }
  }

  def fromOption[A](opt: Option[A], ifEmpty: ParseResult[A]): ParseResult[A] =
    opt.fold(ifEmpty)(success)
  def fromOption[A](opt: Option[A], errorMsg: String): ParseResult[A] =
    fromOption(opt, fail(errorMsg))

  def fail[A](msg: String, detail: YamlMap): ParseResult[A] = Failed(msg, Some(detail), None)
  def fail[A](msg: String): ParseResult[A] = Failed(msg, None, None)
  def fail[A](msg: String, cause: Exception): ParseResult[A] = Failed(msg, None, Some(cause))
  def fail[A](cause: Throwable): ParseResult[A] = Failed(cause.getMessage, None, Some(cause))

  def success[A](a: A): ParseResult[A] = Success[A](a)

  def sequence[A, B](as: Seq[A], f: A => ParseResult[B]): ParseResult[Seq[B]] = {
    val initial = success(Seq.empty[B])
    as.foldLeft(initial) { case (result, a) =>
      result.flatMap(bs => f(a).map(b => bs :+ b))
    }
  }

  def sequencePf[A, B](as: Seq[A])(pf: PartialFunction[A, ParseResult[B]]): ParseResult[Seq[B]] = {
    val initial = success(Seq.empty[B])
    as.foldLeft(initial) { case (result, a) =>
      result.flatMap { bs =>
        if (pf.isDefinedAt(a)) pf(a).map(b => bs :+ b)
        else fail(s"failed to match value '$a'")
      }
    }
  }
}
