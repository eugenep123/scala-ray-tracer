package raytracer.patterns

import raytracer.{Canvas, Color}

import scala.math.{floor, round}

trait UVPattern {
  def colorAt(u: Double, v: Double): Color
}

object UVPattern {

  case class Checkers(width: Int, height: Int, a: Color, b: Color) extends UVPattern {
    def colorAt(u: Double, v: Double): Color = {
      val u2 = floor(u * width).toInt
      val v2 = floor(v * height).toInt
      if ((u2 + v2) % 2 == 0) a else b
    }
  }

  case class AlignCheck(main: Color, ul: Color, ur: Color, bl: Color, br: Color) extends UVPattern {
    override def colorAt(u: Double, v: Double): Color = {
      //# remember: v=0 at the bottom, v=1 at the top
      if (v > 0.8) {
        if (u < 0.2) ul
        else if (u > 0.8) ur
        else main
      } else if (v < 0.2) {
        if (u < 0.2) bl
        else if (u > 0.8) br
        else main
      } else main
    }
  }

  case class Image(canvas: Canvas) extends UVPattern {
    override def colorAt(u: Double, v: Double): Color = {
      // flip v over so it matches the image layout, with y at the top
      val flip = 1 - v
      val x = u * (canvas.width - 1)
      val y = flip * (canvas.height - 1)
      // be sure and round x and y to the nearest whole number
      canvas(round(x).toInt, round(y).toInt)
    }
  }
}