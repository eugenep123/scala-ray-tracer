package raytracer

import Math.pow

import math.{Point3D, Vector3D}
import patterns.Pattern

case class Material(
  color: Color,
  ambient: Double,
  diffuse: Double,
  specular: Double,
  shininess: Double,
  reflective: Double,
  transparency: Double,
  refractiveIndex: Double,
  pattern: Option[Pattern]) {

  @inline final def lighting(
    light: PointLight,
    point: Point3D,
    eye: Vector3D,
    normal: Vector3D,
    inShadow: Boolean): Color = {
    // Combine the surface color with the light's color/intensity.

    //    val effectiveColor = color * light.intensity
    val c = pattern.fold(color)(_.colorAt(point))
    val effectiveColor = c * light.intensity

    // Compute the ambient contribution.
    val ambientComputed = effectiveColor * ambient

    if (inShadow) ambientComputed //light source is obstructed (in shadow: p110)
    else {
      // Find the direction to the light source.
      val lightVec = (light.position - point).normalize

      // lightDotNormal represents the cosine of the angle between the light
      // vector and the normal vector. A negative number means the light is on the
      // other side of the surface.
      val lightDotNormal = lightVec.dot(normal)
      if (lightDotNormal < 0) ambientComputed // diffuseComputed  = specularComputed = black
      else {
        // Compute the diffuse contribution.
        val diffuseComputed = effectiveColor * diffuse * lightDotNormal

        // reflectDotEye represents the cosine of the angle between the reflection
        // vector and the eye vector. A negative number means the light reflects
        // away from the eye.
        val reflectV = Vector3D.reflect(-lightVec, normal)
        val reflectDotEye = reflectV.dot(eye)
        if (reflectDotEye <= 0) ambientComputed + diffuseComputed // (specularComputed == black)
        else {
          // Compute the specular contribution.
          val factor = pow(reflectDotEye, shininess)
          val specularComputed = light.intensity * specular * factor
          // Add the three contributions together to get the final shading
          ambientComputed + diffuseComputed + specularComputed
        }
      }
    }
  }

  def setPattern(p: Pattern): Material = this.copy(pattern = Some(p))
  def setReflective(r: Double): Material = this.copy(reflective = r)
  def setRefractiveIndex(i: Double): Material = this.copy(refractiveIndex = i)
  def setTransparency(t: Double): Material = this.copy(transparency = t)
  def setAmbient(a: Double): Material = this.copy(ambient = a)
  def setDiffuse(a: Double): Material = this.copy(diffuse = a)
  def setSpecular(a: Double): Material = this.copy(specular = a)
  def setColor(c: Color): Material = this.copy(color = c)
  def setColor(r: Double, g: Double, b: Double): Material = setColor(Color(r, g, b))

}

object Material {
  val Default = new Material(
    Color.White,
    ambient = 0.1,
    diffuse = 0.9,
    specular = 0.9,
    shininess = 200.0,
    reflective = 0.0,
    transparency = 0.0,
    refractiveIndex = 1.0,
    pattern = None
  )

  def apply(): Material = Default
  def apply(
    color: Color = Color.White,
    ambient: Double = Default.ambient,
    diffuse: Double = Default.diffuse,
    specular: Double = Default.specular,
    shininess: Double = Default.shininess,
    reflective: Double = Default.reflective,
    transparency: Double = Default.transparency,
    refractiveIndex: Double = Default.refractiveIndex,
    pattern: Option[Pattern] = None): Material = {
    require(ambient >= 0, "Material:ambient cannot be a negative values.")
    require(diffuse >= 0, "Material:diffuse cannot be a negative values.")
    require(specular >= 0, "Material:specular cannot be a negative values.")
    require(shininess >= 0, "Material:shininess cannot be a negative values.")
    require(reflective >= 0, "Material:reflective cannot be a negative values.")
    require(transparency >= 0, "Material:transparency cannot be a negative values.")
    require(refractiveIndex >= 0, "Material:refractiveIndex cannot be a negative values.")
    new Material(
      color, ambient, diffuse, specular, shininess,
      reflective, transparency, refractiveIndex,
      pattern)
  }

  implicit def materialToOptionMatrix(m: Material): Option[Material] = Some(m)

  val Glass = Material(transparency = 1.0, refractiveIndex = 1.5)
  //ambient to 0, and diffuse to (1 - transparency)
  ///(0.5, 0.5, 0) for the Color and Transparency = 1. Also Ambient is 0 or 0.1
  val TransparentBox = Material(
    Color(0.5, 0.5, 0),
    ambient = 0.1, //0.1, // or 0
    diffuse = (1.0 - 0.8), //0.0,
    specular = 0.0,
    shininess = 0,
    reflective = 0.0,
    transparency = 1.0,
    refractiveIndex = 1
  )
}