package raytracer

import java.io.File

import raytracer.resource.ppm.PPMWriter

class Canvas private (
  val width: Int,
  val height: Int,
  private val array: Array[Color]) {

  def apply(x: Int, y: Int): Color = array(width * y + x)

  def writePixel(x: Int, y: Int, color: Color): Unit = {
    array(width * y + x) = color
  }

  def foreach(f: (Int, Int, Color) => Unit): Unit = {
    for {
      x <- 0 until width
      y <- 0 until height
    } f(x, y, this(x, y))
  }

  def fill(color: Color): Unit = {
    (0 until array.length).foreach { index =>
      array(index) = color
    }
  }

  def save(folder: String): File = PPMWriter.save(this, folder)
}

object Canvas {
  def apply(width: Int, height: Int) = {
    val array = Array.fill[Color](width * height)(Color.Black)
    new Canvas(width, height, array)
  }

  def apply(width: Int, height: Int, array: Array[Color]): Canvas = {
    require(array.length == width * height)
    new Canvas(width, height, array)
  }
}