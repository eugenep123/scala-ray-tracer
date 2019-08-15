package raytracer

import raytracer.math.Transforms.{RotationY, Translation, ViewTransform}

import scala.math.sqrt
import raytracer.math._

class CameraSpec extends BaseSpec {

  feature("Camera") {

    scenario("Constructing a camera") {
      Given("hsize ← 160")
      And("vsize ← 120")
      And("field_of_view ← π/2")
      When("c ← camera(hsize, vsize, field_of_view)")
      Then("c.hsize = 160")
      And("c.vsize = 120")
      And("c.field_of_view = π/2")
      And("c.transform = identity_matrix")
      val hsize = 160
      val vsize = 120
      val fieldOfView = π / 2
      val c = Camera(hsize, vsize, fieldOfView)
      assert(c.width == 160)
      assert(c.height == 120)
      assert(c.fieldOfView == π/2)
      assert(c.transform == Matrix.identity)
    }

    scenario("The pixel size for a horizontal canvas") {
      Given("c ← camera(200, 125, π/2)")
      Then("c.pixel_size = 0.01")
      val c = Camera(200, 125, π/2)
      assert(c.pixelSize ~= 0.01)
    }

    scenario("The pixel size for a vertical canvas") {
      Given("c ← camera(125, 200, π/2)")
      Then("c.pixel_size = 0.01")
      val c = Camera(125, 200, π/2)
      assert(c.pixelSize ~= 0.01)
    }

    scenario("Constructing a ray through the center of the canvas") {
      Given("c ← camera(201, 101, π/2)")
      When("r ← ray_for_pixel(c, 100, 50)")
      Then("r.origin = point(0, 0, 0)")
      And("r.direction = vector(0, 0, -1)")
      val c = Camera(201, 101, π/2)
      val r = c.rayForPixel(100, 50)
      assert(r.origin == point(0, 0, 0))
      assert(r.direction == vector(0, 0, -1))
    }

    scenario("Constructing a ray through a corner of the canvas") {
      Given("c ← camera(201, 101, π/2)")
      When("r ← ray_for_pixel(c, 0, 0)")
      Then("r.origin = point(0, 0, 0)")
      And("r.direction = vector(0.66519, 0.33259, -0.66851)")
      val c = Camera(201, 101, π/2)
      val r = c.rayForPixel(0, 0)
      assert(r.origin == point(0, 0, 0))
      assert(r.direction == vector(0.66519, 0.33259, -0.66851))
    }

    scenario("Constructing a ray when the camera is transformed") {
      Given("c ← camera(201, 101, π/2)")
      When("c.transform ← rotation_y(π/4) * translation(0, -2, 5)")
      And("r ← ray_for_pixel(c, 100, 50)")
      Then("r.origin = point(0, 2, -5)")
      And("r.direction = vector(√2/2, 0, -√2/2)")
      val transform = RotationY(π/4) * Translation(0, -2, 5)
      val c = Camera(201, 101, π/2, transform)
      val r = c.rayForPixel(100, 50)
      assert(r.origin == point(0, 2, -5))
      assert(r.direction == vector(sqrt(2)/2, 0, -sqrt(2)/2))
    }

    scenario("Rendering a world with a camera") {
      Given("w ← default_world()")
      And("c ← camera(11, 11, π/2)")
      And("from ← point(0, 0, -5)")
      And("to ← point(0, 0, 0)")
      And("up ← vector(0, 1, 0)")
      And("c.transform ← view_transform(from, to, up)")
      When("image ← render(c, w)")
      Then("pixel_at(image, 5, 5) = color(0.38066, 0.47583, 0.2855)")
      val w = defaultWorld()
      val from = point(0, 0, -5)
      val to = point(0, 0, 0)
      val up = vector(0, 1, 0)
      val transform = ViewTransform(from, to, up)
      val c = Camera(11, 11, π/2, transform)
      val image = c.renderToCanvas(w)
      assert(image(5, 5) == Color(0.38066, 0.47583, 0.2855))
    }
  }
}
