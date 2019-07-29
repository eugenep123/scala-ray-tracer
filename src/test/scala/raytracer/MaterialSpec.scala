package raytracer

import Math.sqrt

class MaterialSpec extends BaseSpec {

  feature("Materials") {

    Given("m ← material()")
    And("position ← point(0, 0, 0)")
    val m = material()
    val position = point(0, 0, 0)

    scenario("The default material") {

      Then("m.color = color(1, 1, 1)")
      assert(m.color == color(1, 1, 1))
      And(" m.ambient = 0.1")
      assert(m.ambient == 0.1)
      And(" m.diffuse = 0.9")
      assert(m.diffuse == 0.9)
      And("m.specular = 0.9")
      assert(m.specular == 0.9)
      And("m.shininess = 200.0")
      assert(m.shininess == 200.0)
    }

    scenario("Lighting with the eye between the light and the surface") {
      Given("eyev ← vector(0, 0, -1)")
      val eyev = vector(0, 0, -1)
      And("normalv ← vector(0, 0, -1)")
      val normalv = vector(0, 0, -1)
      And("light ← point_light(point(0, 0, -10), color(1, 1, 1))")
      val light = pointLight(point(0, 0, -10), color(1, 1, 1))
      When("result ← lighting(m, light, position, eyev, normalv)")
      val result = lighting(m, light, position, eyev, normalv)
      Then("result = color(1.9, 1.9, 1.9)")
      assert(result == color(1.9, 1.9, 1.9))
    }

    scenario("Lighting with the eye between light and surface, eye offset 45°") {
      Given("eyev ← vector(0, √2/2, -√2/2)")
      val eyev = vector(0, sqrt(2)/2, -sqrt(2)/2)
      And("normalv ← vector(0, 0, -1)")
      val normalv = vector(0, 0, -1)
      And("light ← point_light(point(0, 0, -10), color(1, 1, 1))")
      val light = pointLight(point(0, 0, -10), color(1, 1, 1))
      When("result ← lighting(m, light, position, eyev, normalv)")
      val result = lighting(m, light, position, eyev, normalv)
      Then("result = color(1.0, 1.0, 1.0)")
      assert(result == color(1.0, 1.0, 1.0))
    }

    scenario("Lighting with eye opposite surface, light offset 45°") {
      Given("eyev ← vector(0, 0, -1)")
      val eyev = vector(0, 0, -1)
      And("normalv ← vector(0, 0, -1)")
      val normalv = vector(0, 0, -1)
      And("light ← point_light(point(0, 10, -10), color(1, 1, 1))")
      val light = pointLight(point(0, 10, -10), color(1, 1, 1))
      When("result ← lighting(m, light, position, eyev, normalv)")
      val result = lighting(m, light, position, eyev, normalv)
      Then("result = color(0.7364, 0.7364, 0.7364)")
      assert(result == color(0.7364, 0.7364, 0.7364))
    }

    scenario("Lighting with eye in the path of the reflection vector") {
      Given("eyev ← vector(0, -√2/2, -√2/2)")
      val eyev = vector(0, -sqrt(2)/2, -sqrt(2)/2)
      And("normalv ← vector(0, 0, -1)")
      val normalv = vector(0, 0, -1)
      And("light ← point_light(point(0, 10, -10), color(1, 1, 1))")
      val light = pointLight(point(0, 10, -10), color(1, 1, 1))
      When("result ← lighting(m, light, position, eyev, normalv)")
      val result = lighting(m, light, position, eyev, normalv)
      Then("result = color(1.6364, 1.6364, 1.6364)")
      assert(result == color(1.6364, 1.6364, 1.6364))
    }

    scenario("Lighting with the light behind the surface") {
      Given("eyev ← vector(0, 0, -1)")
      val eyev = vector(0, 0, -1)
      And("normalv ← vector(0, 0, -1)")
      val normalv = vector(0, 0, -1)
      And("light ← point_light(point(0, 0, 10), color(1, 1, 1))")
      val light = pointLight(point(0, 0, 10), color(1, 1, 1))
      When("result ← lighting(m, light, position, eyev, normalv)")
      val result = lighting(m, light, position, eyev, normalv)
      Then("result = color(0.1, 0.1, 0.1)")
      assert(result == color(0.1, 0.1, 0.1))
    }

    scenario("Lighting with the surface in shadow") {
      Given("eyev ← vector(0, 0, -1)")
      val eye = vector(0, 0, -1)
      And("normalv ← vector(0, 0, -1)")
      val normal = vector(0, 0, -1)
      And("light ← point_light(point(0, 0, -10), color(1, 1, 1))")
      val light = pointLight(point(0, 0, -10), color(1, 1, 1))
      And("in_shadow ← true")
      val inShadow = true
      When("result ← lighting(m, light, position, eyev, normalv, in_shadow)")
      val result = lighting(m, light, position, eye, normal, inShadow)
      Then("result = color(0.1, 0.1, 0.1)")
      assert(result == color(0.1, 0.1, 0.1))
    }

    scenario("Lighting with a pattern applied") {
      Given("m.pattern ← stripe_pattern(color(1, 1, 1), color(0, 0, 0))")
      And("m.ambient ← 1")
      And("m.diffuse ← 0")
      And("m.specular ← 0")
      val m = Material(
        //color = Color.White,
        ambient = 1,
        diffuse = 0,
        specular = 0,
        pattern = Some(stripePattern(color(1, 1, 1), color(0, 0, 0)))
      )
      And("eyev ← vector(0, 0, -1)")
      val eyev = vector(0, 0, -1)
      And("normalv ← vector(0, 0, -1)")
      val normalv = vector(0, 0, -1)
      And("light ← point_light(point(0, 0, -10), color(1, 1, 1))")
      val light = pointLight(point(0, 0, -10), color(1, 1, 1))
      When("c1 ← lighting(m, light, point(0.9, 0, 0), eyev, normalv, false)")
      val c1 = lighting(m, light, point(0.9, 0, 0), eyev, normalv, false)
      And("c2 ← lighting(m, light, point(1.1, 0, 0), eyev, normalv, false)")
      val c2 = lighting(m, light, point(1.1, 0, 0), eyev, normalv, false)
      Then("c1 = color(1, 1, 1)")
      assert(c1 == color(1, 1, 1))
      And("c2 = color(0, 0, 0)")
      assert(c2 == color(0, 0, 0))
    }

    scenario("Reflectivity for the default material") {
      Given(" m ← material()")
      Then("m.reflective = 0.0")
      val m = material()
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
