package chapters

import raytracer._
import shapes._
import math._

object Chapter6 {

  def folder: String = {
    val homeDir = System.getProperty("user.home")
    s"$homeDir/dev/other/output"
  }

  def main(args: Array[String]): Unit = {
    val width = 400
    val height = 400
    val canvas = Canvas(width, height)
    render(canvas)
    canvas.save(folder)
  }



  def render(canvas: Canvas, flush: () => Unit = () => ()): Unit = {
//    val canvasPixels = 400
    val canvasPixels = Math.min(canvas.width, canvas.height)
    // Lighting
    val lightPosition = Point3D(-10, 10, -10)
    val lightColor = Color(1, 1, 1)
    val light = PointLight(lightPosition, lightColor)

    val sphere = Shapes.sphere(material = Material(color = Color(1, 0.2, 1)))

    val rayOrigin = Point3D(0, 0, -5)
    val wallZ = 10d
    val wallSize = 7d

    val pixelSize = wallSize / canvasPixels
    val half = wallSize / 2

    (0 until canvasPixels).foreach { y =>
      (0 until canvasPixels).foreach { x =>
        // compute the world x coordinate (left = -half, right = half)
        val worldX = -half + pixelSize * x
        // compute the world y coordinate (top = +half, bottom = -half)
        val worldY = half - pixelSize * y
        // describe the Point3D on the wall that the ray will target
        val position = Point3D(worldX, worldY, wallZ)
        val subtractedPosition = position - rayOrigin
        val normalizedPosition = subtractedPosition.normalize
        val ray = Ray(rayOrigin, normalizedPosition)

        val xs = Intersection.sort(sphere.intersect(ray))
        Intersection.hit(xs)
          .foreach { hit =>
            val Point3D = ray.position(hit.t)
            val normal = hit.obj.normalAt(Point3D, hit)
            val eye = -ray.direction
            val color = hit.obj.material.lighting(light, Point3D, eye, normal, false)
            canvas.writePixel(x, y, color)
          }
      }
      flush()
    }
  }
}
