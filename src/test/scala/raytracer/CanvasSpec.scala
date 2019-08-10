package raytracer

class CanvasSpec extends BaseSpec {

  def canvasToPpm(c: Canvas): Seq[String] = resource.PpmWriter(c)

  feature("Canvas") {
    scenario("Creating a canvas") {
      Given("c ← canvas(10, 20)")
      Then("c.width = 10")
      And("c.height = 20")
      And("every pixel of c is color(0, 0, 0)")
      val c = Canvas(10, 20)
      assert(c.width == 10)
      assert(c.height == 20)
      c.foreach((x, y, c) => assert(c == Color.Black))
    }

    scenario("Writing pixels to a canvas") {
      Given("c ← canvas(10, 20)")
      And("red ← color(1, 0, 0)")
      When("writePixel(c, 2, 3, red)")
      Then("pixel_at(c, 2, 3) = red")
      val c = Canvas(10, 20)
      assert(Color.Red == Color(1, 0, 0))
      c.writePixel(2, 3, Color.Red)
      assert(c(2, 3) == Color.Red)
    }

    scenario("Constructing the PPM header") {
      Given("c ← canvas(5, 3)")
      When("ppm ← canvas_to_ppm(c)")
      Then("lines 1-3 of ppm are")
      val c = Canvas(5, 3)
      val ppm = canvasToPpm(c)
      assert(ppm.take(3) == Seq("P3", "5 3", "255"))
    }

    scenario("Constructing the PPM pixel data") {
      Given("c ← canvas(5, 3)")
      And("c1 ← color(1.5, 0, 0)")
      And("c2 ← color(0, 0.5, 0)")
      And("c3 ← color(-0.5, 0, 1)")
      When("write_pixel(c, 0, 0, c1)")
      And("write_pixel(c, 2, 1, c2)")
      And("write_pixel(c, 4, 2, c3)")
      And("ppm ← canvas_to_ppm(c)")
      Then("lines 4-6 of ppm are")
      val c = Canvas(5, 3)
      val c1 = Color(1.5, 0, 0)
      val c2 = Color(0, 0.5, 0)
      val c3 = Color(-0.5, 0, 1)
      c.writePixel(0, 0, c1)
      c.writePixel(2, 1, c2)
      c.writePixel(4, 2, c3)
      val ppm = canvasToPpm(c)
      assert(ppm.drop(3).take(3) == Seq(
        "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
        "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0",
        "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
      ))
    }

    scenario("Splitting long lines in PPM files") {
      Given("c ← canvas(10, 2)")
      When("every pixel of c is set to color(1, 0.8, 0.6)")
      And(" ppm ← canvas_to_ppm(c)")
      Then("lines 4-7 of ppm are")
      val c = Canvas(10, 2)
      c.fill(Color(1, 0.8, 0.6))
      val ppm = canvasToPpm(c)
      assert(ppm.drop(3).take(4) == Seq(
        "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204",
        "153 255 204 153 255 204 153 255 204 153 255 204 153",
        "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204",
        "153 255 204 153 255 204 153 255 204 153 255 204 153"
      ))
    }

    scenario("PPM files are terminated by a newline") {
      Given("c ← canvas(5, 3)")
      When("ppm ← canvas_to_ppm(c)")
      Then("the last character of ppm is a newline")
      val c = Canvas(5, 3)
      val ppm = canvasToPpm(c)
      assert(ppm.last == "\n")
    }
  }
}
