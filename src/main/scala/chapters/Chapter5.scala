package chapters

import raytracer._
import shapes._
import math._
import Operations._
import raytracer.resource.ppm.PPMWriter

object Chapter5 extends App {


  // This is verrrry slow. (~1 min)
  (0 to 4).foreach { opt =>
    val sphere = mkFunSphere(opt)
    val rayOrigin = Point3D(0, 0, -5)
    val wallZ = 10d
    val wallSize = 7d
    val canvasPixels = 400
    val pixelSize = wallSize / canvasPixels
    val half = wallSize / 2

    val canvas = Canvas(canvasPixels, canvasPixels)
    val red = Color(1, 0, 0)

    (0 until canvasPixels).foreach { y =>
      (0 until canvasPixels).foreach { x =>
        // compute the world x coordinate (left = -half, right = half)
        val worldX = -half + pixelSize * x
        // compute the world y coordinate (top = +half, bottom = -half)
        val worldY = half - pixelSize * y
        // describe the Point3D on the wall that the ray will target
        val position = Point3D(worldX, worldY, wallZ)
        val substractedPosition = position - rayOrigin
        val normalizedPosition = substractedPosition.normalize
        val ray = Ray(rayOrigin, normalizedPosition)

        val xs = Intersection.sort(sphere.intersect(ray))
        Intersection.hit(xs)
          .foreach { _ =>
            canvas.writePixel(x, y, red)
          }

      }
    }
    def folder: String = {
      val homeDir = System.getProperty("user.home")
      s"$homeDir/dev/other/output"
    }
    PPMWriter.save(canvas, folder)
  }


  private def mkFunSphere(opt: Int): Shape = {
    opt match {
      // borin sphere
      case 0 => Shapes.sphere()
      // shrink it along the y axis
      case 1 => Shapes.sphere(scaling(1, 0.5, 1))
      // shrink it along the x-axis
      case 2 => Shapes.sphere(scaling(0.5, 1, 1))
      // shrink it, and rotate it!
      case 3 => Shapes.sphere(rotationZ(PI / 4) * scaling(0.5, 1, 1))
      // shrink it, and skew it!
      case 4 => Shapes.sphere(shearing(1, 0, 0, 0, 0, 0) * scaling(0.5, 1, 1))
    }
  }
}