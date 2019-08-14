package raytracer.resource.waveform

object SharedParsers {
  import fastparse._
  import NoWhitespace._

  def spaces[_: P]: P[Unit] = P(CharsWhileIn(" \t", 0))
  def spaceOrNewlines[_: P]: P[Unit] = P(CharsWhileIn(" \r\n", 0))
  def newLine[_: P]: P[Unit] = P(" ".? ~ CharsWhileIn("\n\r", 1))
  def digits[_: P]: P[Unit] = P( CharsWhileIn("0-9") )
  def exponent[_: P]: P[Unit] = P( CharIn("eE") ~ CharIn("+\\-").? ~ digits )
  def fractional[_: P]: P[Unit] = P( "." ~ digits )
  def integral[_: P]: P[Unit] = P( ("0" | CharIn("1-9")) ~ digits.? )
  def decimal[_: P]: P[Double] = P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(_.toDouble)
  def index[_: P]: P[Int] = integral.!.map(_.toInt)
  def two[T, _: P](p: => P[T], sep: String = " "): P[(T, T)] = P(p ~ sep ~ p)
  def triple[T, _: P](p: => P[T], sep: String = " "): P[(T, T, T)] = P(p ~ sep ~ p ~ sep ~ p)

  def name[_: P]: P[String] = P(CharsWhileIn("_()0-9a-zA-Z\\-.", 1).!)
  def onOrOff[_:P]: P[Boolean] = P(("off" | "1").!).map(_ == "1")
  def comment[_: P]: P[Unit] = P(spaces.? ~ "#" ~/ CharsWhile(_ != '\n').? )

}
