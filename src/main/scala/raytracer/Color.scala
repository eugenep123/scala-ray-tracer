package raytracer

import java.lang.Math.ceil
import math._

case class Color(red: Double, green: Double, blue: Double) {

  def +(c: Color): Color = Color(red + c.red, green + c.green, blue + c.blue)
  def -(c: Color): Color = Color(red - c.red, green - c.green, blue - c.blue)
  def *(c: Color): Color = Color(red * c.red, green * c.green, blue * c.blue)

  def *(s: Double): Color = Color (red * s, green * s, blue * s)
  def /(s: Double): Color = Color (red / s, green / s, blue / s)

  // Options
  def +(opt: Option[Color]): Color = opt.fold(this)(this + _)

  override def equals(other: Any): Boolean = other match {
    case c: Color => (red ~= c.red) && (green ~= c.green) && (blue ~= c.blue)
    case _ => false
  }

  override def toString: String = mkString("color", red, green, blue)

  def rgbString: String = {
    import Color._
    val rV = normalize(red)
    val gV = normalize(green)
    val bV = normalize(blue)
    s"$rV $gV $bV"
  }

  def toRGBA(alpha: Double): Int = {
    import Color._
    val a = normalize(alpha)
    val r = normalize(red)
    val g = normalize(green)
    val b = normalize(blue)
    ((a & 0xFF) << 24) |
      ((r & 0xFF) << 16) |
      ((g & 0xFF) << 8)  |
      ((b & 0xFF) << 0)
  }
}


object Color {

  val Black = Color(0, 0, 0)
  val White = Color(1, 1, 1)
  val Red   = Color(1, 0, 0)
  val Green = Color(0, 1, 0)
  val Blue  = Color(0, 0, 1)

  def normalize(v: Double): Int = {
    ceil(if (v > 1) 255 else if (v < 0) 0 else v * 255).toInt
  }
}


