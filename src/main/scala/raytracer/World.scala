package raytracer

import raytracer.Defaults.MaxRecursion

import scala.math.sqrt

final case class World(
  objects: Seq[Shape],
  lights: Seq[PointLight]) {

  def colorAt(ray: Ray, remaining: Int = MaxRecursion, allShapes: Boolean): Option[Color] = {
    val xs = intersect(ray)
    for {
      intersection <- Intersection.hit(xs, allShapes)
      hit = intersection.prepare(ray, xs)
      color <- shadeHit(hit, remaining)
    } yield color
  }

  def colorAtOrBlack(ray: Ray, remaining: Int = MaxRecursion): Color =
    colorAt(ray, remaining, true).getOrElse(Color.Black)

  def intersect(r: Ray): Seq[Intersection] =
    Intersection.sort(objects.flatMap(_.intersect(r)))

  def shadeHit(hit: RayIntersection, remaining: Int = MaxRecursion): Option[Color] = {
    import hit.{eye, normal, obj, overPoint}
    // Can have multiple lights, calculate for each and add together (Page 96)
    val colors = lights.map { light =>
      val inShadow = hit.renderAllRays && isShadowed(overPoint, light)
      obj.material.lighting(light, overPoint, eye, normal, inShadow)
    }
    val surfaceOpt = colors.reduceOption(_ + _)
    // If no light, this will be none, if which case we return none
    surfaceOpt map { surface =>
      val reflected = reflectedColor(hit, remaining)
      val refracted = refractedColor(hit, remaining)

      val material = hit.obj.material
      if (material.reflective > 0.0 && material.transparency > 0.0) {
        val reflectance = hit.reflectance
        surface + (reflected.map(_ * reflectance)) +
                   refracted.map(_ * (1.0 - reflectance))
      } else {
        surface + reflected + refracted
      }
    }
  }

  def reflectedColor(hit: RayIntersection, remaining: Int = MaxRecursion): Option[Color] = {
    if (remaining <= 0 || hit.obj.material.reflective == 0.0) None
    else {
      val color = colorAt(hit.reflectiveRay, remaining - 1, false)
      color.map(_ * hit.obj.material.reflective)
    }
  }

  def refractedColor(hit: RayIntersection, remaining: Int = MaxRecursion): Option[Color] = {
    if (remaining <= 0 || hit.obj.material.transparency == 0.0) None
    else {
      // Find the ration of first index of refraction to the second
      // (yup, this is inverted from the definition of snell's Law.)
      val nRatio = hit.n1 / hit.n2

      // cos(theta-i) is the same as the dot product of the two vectors
      val cosI = hit.eye.dot(hit.normal)

      // Find sin(theta_t2)^2 via trigonometric identity
      val sin2T = (nRatio * nRatio) * (1 - (cosI * cosI))

      // check for total internal refraction
      if (sin2T > 1) None
      else {
        // find con(theta_t) via trigonometric identity
        val cosT = sqrt(1.0 - sin2T)

        // Compute the direction of the refracted ray
        val direction = hit.normal * (nRatio * cosI - cosT) -
          (hit.eye * nRatio)

        // Create the directed ray
        val refractRay = Ray(hit.underPoint, direction)

        // Find the color of the refracted ray, making sure to multiply
        // by the transparency value to account for any opacity
        val colorOpt = colorAt(refractRay, remaining - 1, false)
        colorOpt.map(_ * hit.obj.material.transparency)
      }
    }
  }

  def isShadowed(point: Point3D): Boolean = {
    lights.exists(isShadowed(point, _))
  }

  private def isShadowed(point: Point3D, light: PointLight): Boolean = {
    val v = light.position - point
    val distance = v.magnitude
    val direction = v.normalize
    val r = Ray(point, direction)
    val hit = Intersection.hit(intersect(r), false)
    hit.exists(_.t < distance)
  }

  def setLight(light: PointLight): World = this.copy(lights = Seq(light))
  def add(light: PointLight): World = this.copy(lights = lights :+ light)
  def add(shape: Shape): World = this.copy(objects = objects :+ shape)
  def add(shapes: Shape*): World = this.copy(objects = objects ++ shapes)
  def updateObject(index: Int)(f: Shape => Shape): World = {
    val os = objects.zipWithIndex
      .map { case (o, i) => if (index == i) f(o) else o }
    this.copy(objects = os)
  }
  def setObjectAt(index: Int, s: Shape): World = updateObject(index)(_ => s)

  def first: Shape = objects(0)
  def second: Shape = objects(1)
}

object World {
  val empty: World = new World(Nil, Nil)

  def apply(objects: Seq[Shape] = Nil, lights: Seq[PointLight] = Nil): World =
    new World(objects, lights)

}