package raytracer

import math._
import raytracer.patterns.Pattern

import scala.math.sqrt

class MaterialSpec extends BaseSpec {

  def lighting(material: Material,
               light: PointLight,
               point: Point3D,
               eye: Vector3D,
               normal: Vector3D,
               inShadow: Boolean = false): Color =
    material.lighting(testShape(), light, point, eye, normal, inShadow)

  feature("Materials") {

    Given("m ← material()")
    And("position ← point(0, 0, 0)")
    val m = Material()
    val position = point(0, 0, 0)

    scenario("The default material") {
      Then("m.color = color(1, 1, 1)")
      And(" m.ambient = 0.1")
      And(" m.diffuse = 0.9")
      And("m.specular = 0.9")
      And("m.shininess = 200.0")
      assert(m.color == Color(1, 1, 1))
      assert(m.ambient == 0.1)
      assert(m.diffuse == 0.9)
      assert(m.specular == 0.9)
      assert(m.shininess == 200.0)
    }

    scenario("Lighting with the eye between the light and the surface") {
      Given("eyev ← vector(0, 0, -1)")
      And("normalv ← vector(0, 0, -1)")
      And("light ← point_light(point(0, 0, -10), color(1, 1, 1))")
      When("result ← lighting(m, light, position, eyev, normalv)")
      Then("result = color(1.9, 1.9, 1.9)")
      val eyev = vector(0, 0, -1)
      val normalv = vector(0, 0, -1)
      val light = PointLight(point(0, 0, -10), Color(1, 1, 1))
      val result = lighting(m, light, position, eyev, normalv)
      assert(result == Color(1.9, 1.9, 1.9))
    }

    scenario("Lighting with the eye between light and surface, eye offset 45°") {
      Given("eyev ← vector(0, √2/2, -√2/2)")
      And("normalv ← vector(0, 0, -1)")
      And("light ← point_light(point(0, 0, -10), color(1, 1, 1))")
      When("result ← lighting(m, light, position, eyev, normalv)")
      Then("result = color(1.0, 1.0, 1.0)")
      val eyev = vector(0, sqrt(2) / 2, -sqrt(2) / 2)
      val normalv = vector(0, 0, -1)
      val light = PointLight(point(0, 0, -10), Color(1, 1, 1))
      val result = lighting(m, light, position, eyev, normalv)
      assert(result == Color(1.0, 1.0, 1.0))
    }

    scenario("Lighting with eye opposite surface, light offset 45°") {
      Given("eyev ← vector(0, 0, -1)")
      And("normalv ← vector(0, 0, -1)")
      And("light ← point_light(point(0, 10, -10), color(1, 1, 1))")
      When("result ← lighting(m, light, position, eyev, normalv)")
      Then("result = color(0.7364, 0.7364, 0.7364)")
      val eyev = vector(0, 0, -1)
      val normalv = vector(0, 0, -1)
      val light = PointLight(point(0, 10, -10), Color(1, 1, 1))
      val result = lighting(m, light, position, eyev, normalv)
      assert(result == Color(0.7364, 0.7364, 0.7364))
    }

    scenario("Lighting with eye in the path of the reflection vector") {
      Given("eyev ← vector(0, -√2/2, -√2/2)")
      And("normalv ← vector(0, 0, -1)")
      And("light ← point_light(point(0, 10, -10), color(1, 1, 1))")
      When("result ← lighting(m, light, position, eyev, normalv)")
      Then("result = color(1.6364, 1.6364, 1.6364)")
      val eyev = vector(0, -sqrt(2) / 2, -sqrt(2) / 2)
      val normalv = vector(0, 0, -1)
      val light = PointLight(point(0, 10, -10), Color(1, 1, 1))
      val result = lighting(m, light, position, eyev, normalv)
      assert(result == Color(1.6364, 1.6364, 1.6364))
    }

    scenario("Lighting with the light behind the surface") {
      Given("eyev ← vector(0, 0, -1)")
      And("normalv ← vector(0, 0, -1)")
      And("light ← point_light(point(0, 0, 10), color(1, 1, 1))")
      When("result ← lighting(m, light, position, eyev, normalv)")
      Then("result = color(0.1, 0.1, 0.1)")
      val eyev = vector(0, 0, -1)
      val normalv = vector(0, 0, -1)
      val light = PointLight(point(0, 0, 10), Color(1, 1, 1))
      val result = lighting(m, light, position, eyev, normalv)
      assert(result == Color(0.1, 0.1, 0.1))
    }

    scenario("Lighting with the surface in shadow") {
      Given("eyev ← vector(0, 0, -1)")
      And("normalv ← vector(0, 0, -1)")
      And("light ← point_light(point(0, 0, -10), color(1, 1, 1))")
      And("in_shadow ← true")
      When("result ← lighting(m, light, position, eyev, normalv, in_shadow)")
      Then("result = color(0.1, 0.1, 0.1)")
      val eye = vector(0, 0, -1)
      val normal = vector(0, 0, -1)
      val light = PointLight(point(0, 0, -10), Color(1, 1, 1))
      val inShadow = true
      val result = lighting(m, light, position, eye, normal, inShadow)
      assert(result == Color(0.1, 0.1, 0.1))
    }

    scenario("Lighting with a pattern applied") {
      Given("m.pattern ← stripe_pattern(color(1, 1, 1), color(0, 0, 0))")
      And("m.ambient ← 1")
      And("m.diffuse ← 0")
      And("m.specular ← 0")
      And("eyev ← vector(0, 0, -1)")
      And("normalv ← vector(0, 0, -1)")
      And("light ← point_light(point(0, 0, -10), color(1, 1, 1))")
      When("c1 ← lighting(m, light, point(0.9, 0, 0), eyev, normalv, false)")
      And("c2 ← lighting(m, light, point(1.1, 0, 0), eyev, normalv, false)")
      Then("c1 = color(1, 1, 1)")
      And("c2 = color(0, 0, 0)")
      val m = Material(
        //color = Color.White,
        ambient = 1,
        diffuse = 0,
        specular = 0,
        pattern = Some(Pattern.stripe(Color(1, 1, 1), Color(0, 0, 0)))
      )
      val eyev = vector(0, 0, -1)
      val normalv = vector(0, 0, -1)
      val light = PointLight(point(0, 0, -10), Color(1, 1, 1))
      val c1 = lighting(m, light, point(0.9, 0, 0), eyev, normalv, false)
      val c2 = lighting(m, light, point(1.1, 0, 0), eyev, normalv, false)
      assert(c1 == Color(1, 1, 1))
      assert(c2 == Color(0, 0, 0))
    }

    scenario("Reflectivity for the default material") {
      Given(" m ← material()")
      Then("m.reflective = 0.0")
      val m = Material()
      assert(m.reflective ~= 0.0)
    }

    scenario("Transparency and Refractive Index for the default material") {
      Given("m ← material()")
      Then("m.transparency = 0.0")
      And(" m.refractive_index = 1.0")
      val m = Material()
      assert(m.transparency == 0.0)
      assert(m.refractiveIndex == 1.0)
    }
  }

}
