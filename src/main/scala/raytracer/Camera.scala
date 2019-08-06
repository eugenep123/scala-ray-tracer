package raytracer

import math._

import scala.math.tan
import scala.collection.parallel.ParSeq

case class Camera(
  width: Int,
  height: Int,
  fieldOfView: Double,
  transform: Matrix) {

  val halfView = tan(fieldOfView / 2)
  val aspect = width.toDouble / height.toDouble
  val (halfWidth, halfHeight) =
    if (aspect >= 1) (halfView, halfView / aspect)
    else (halfView * aspect, halfView)
  val pixelSize =  (halfWidth * 2) / width.toDouble

  val inverseTransform = transform.inverse
  val origin = inverseTransform * Point3D.origin

  def rayForPixel(px: Int, py: Int): Ray = {
    // The offset from the edge of the canvas to the pixel's center
    val xOffset = (px + 0.5) * pixelSize
    val yOffset = (py + 0.5) * pixelSize

    // The untransformed coordinates of the pixel in world space
    // camera looks toward -z, so +x is to the *left*
    val worldX = halfWidth - xOffset
    val worldY = halfHeight - yOffset

    val pixel = inverseTransform * Point3D(worldX, worldY, -1)
    val direction = (pixel - origin).normalize
    Ray(origin, direction)
  }

  def pixels: Seq[(Int, Int)] = {
    for {
      y <- (0 until height)
      x <- (0 until width)
    } yield (x, y)
  }

  def rays: Seq[(Int, Int, Ray)] = {
    pixels.map { case (x, y) => (x, y, rayForPixel(x, y)) }
  }

  def renderPar(world: World): ParSeq[(Int, Int, Color)] = {
    rays.par.map { case (x,y, ray) => (x, y, world.colorAt(ray)) }
  }

  def render(world: World)(f: (Int, Int, Color) => Unit): Unit = {
    renderPar(world)
      .foreach { case (x, y, color) => f(x, y, color) }
  }

  def renderToCanvas(world: World): Canvas = {
    val canvas = Canvas(width, height)
    render(world)(canvas.writePixel)
    canvas
  }
}

object Camera {
  def apply(
    width: Int,
    height: Int,
    fieldOfView: Radians,
    transform: Matrix = Matrix.identity): Camera =
    new Camera(width, height, fieldOfView, transform)
}


