package raytracer

case class Ray(origin: Point3D, direction: Vector3D) {

  def position(t: Double): Point3D = origin + direction * t
  def transform(m: Matrix): Ray = Ray(m * origin, m * direction)

  override def toString: String = s"ray($origin, $direction)"
}



