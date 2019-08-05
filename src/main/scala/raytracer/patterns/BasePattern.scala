package raytracer.patterns

import raytracer._
import math._
import shapes._

abstract class BasePattern extends Pattern {

  final def colorAt(worldPoint: Point3D, obj: Shape): Color = {
    val objectPoint = obj.transform.inverse * worldPoint
    val patternPoint = transform.inverse * objectPoint
    colorAt(patternPoint)
  }
}
