package raytracer

class LightSpec extends BaseSpec {
  feature("Lights") {
    scenario("A point light has a position and intensity") {
      val intensity = color(1, 1, 1)
      Given(s"intensity ← $intensity")
      val position = point(0, 0, 0)
      And(s"position ← $position")
      When(" light ← point_light(position, intensity)")
      val light = pointLight(position, intensity)
      Then("light.position = position")
      assert(light.position == position)
      And("light.intensity = intensity")
      assert(light.intensity == intensity)
    }
  }
}

