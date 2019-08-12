package raytracer.resource.ppm

import raytracer.{Canvas, Color}
import fastparse._
import NoWhitespace._

import scala.util.control.NonFatal

object PPMParser {

  def newLine[_: P]: P[Unit] = P(CharsWhileIn("\n\r", 1))
  def digits[_: P]: P[Unit] = P(CharsWhileIn("0-9"))
  def number[_: P]: P[Int] = P(digits.rep(1).!.map(_.toInt))
  def widthHeight[_:P]: P[(Int, Int)] = P(number ~ " " ~ number ~ newLine)
  // magic, Width, height, scala
  def comment[_: P]: P[Unit] = P(("#" ~/ CharsWhile(_ != '\n' )).?)
  def headerComment[_: P] = P((comment ~ newLine).?)
  def header[_: P]: P[(Int, Int, Int)] = P(
    "P3" ~/ newLine ~ //headerComment  ~
      widthHeight ~ //headerComment ~
      number  ~/ newLine //~ headerComment
  ).log

  def byte[_:P]: P[Int] = P(digits.rep(1, max=3).!).map(_.toInt)
  def color[_:P]: P[(Int, Int, Int)] = P(byte ~ " " ~ byte ~ " " ~ byte)
  def colorLine[_:P]: P[Seq[(Int, Int, Int)]] = P(color.rep(1, " ")  ~ " ".? ~ &(newLine | End))
//  def colorLine2[_:P]: P[Seq[(Int, Int, Int)]] = P(color.rep(1, " " ~ " ".?) ~ &(newLine | End))
  def colorOnLines[_: P]: P[Seq[(Int, Int, Int)]] = P(byte ~ newLine ~ byte ~ newLine ~ byte  ~ &(newLine | End)).map(Seq(_))

  def line[_:P]: P[Seq[(Int, Int, Int)]] = P(colorLine | colorOnLines | comment.map(_ => Nil))
  def lines[_: P]: P[Seq[(Int, Int, Int)]] = line.rep(0, newLine).map(_.flatten)
  def file[_: P]: P[(Int, Int, Int, Seq[(Int, Int, Int)])] = header ~/ lines ~ End

  def parse(content: String): Option[Canvas] = {
    try {
      val parsed = fastparse.parse(content, file(_), startIndex = 0)
      val Parsed.Success((width, height, scale, xs), _) = parsed
      val canvas = Canvas(width, height)
      val f = cap(scale.toDouble) _
      var x = 0
      var y = 0
      xs.foreach { case (r, g, b) =>
        val c = Color(f(r), f(g), f(b))
        canvas.writePixel(x, y, c)
        x += 1
        if (x >= width) {
          x = 0
          y += 1
        }
      }
      Some(canvas)
    } catch {
      case NonFatal(ex) =>
        ex.printStackTrace()
        None
    }
  }
  def cap(scale: Double)(x: Int) = {
    val d = x.toDouble / scale
    if (d > 1.0) 1.0
    else if (d < 0.0) 0.0
    else d
  }

}
