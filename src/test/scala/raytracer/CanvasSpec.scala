package raytracer

import raytracer.resource.ppm.{PPMParser, PPMWriter}
import org.scalatest.prop.TableDrivenPropertyChecks._

class CanvasSpec extends BaseSpec {

  def canvasToPpm(c: Canvas): Seq[String] = PPMWriter(c)

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

    scenario("Reading a file with the wrong magic number") {
      val content = """P32
                      |1 1
                      |255
                      |0 0 0""".stripMargin
      Given(s"ppm ← a file containing: \n$content")
      Then("canvas_from_ppm(ppm)")
      assert(PPMParser.parse(content).isEmpty)
    }

    scenario("Reading a PPM returns a canvas of the right size") {
      val content = """P3
                      |10 2
                      |255
                      |0 0 0  0 0 0  0 0 0  0 0 0  0 0 0
                      |0 0 0  0 0 0  0 0 0  0 0 0  0 0 0
                      |0 0 0  0 0 0  0 0 0  0 0 0  0 0 0
                      |0 0 0  0 0 0  0 0 0  0 0 0  0 0 0""".stripMargin

      Given(s"ppm ← a file containing: \n$content")
      When("canvas ← canvas_from_ppm(ppm)")
      Then("canvas.width = 10")
      And("canvas.height = 2")

      val canvas = PPMParser.parse(content).get
      assert(canvas.width == 10)
      assert(canvas.height == 2)
    }

    scenario("Reading pixel data from a PPM file") {
      val content = """P3
                      |4 3
                      |255
                      |255 127 0  0 127 255  127 255 0  255 255 255
                      |0 0 0  255 0 0  0 255 0  0 0 255
                      |255 255 0  0 255 255  255 0 255  127 127 127""".stripMargin
      Given(s"ppm ← a file containing:\n$content")
      When("canvas ← canvas_from_ppm(ppm)")

      val examples = Table(
        ( "x", "y" , "color"                    ),
        ( 0  , 0   , Color(1, 0.498, 0)         ),
        ( 1  , 0   , Color(0, 0.498, 1)         ),
        ( 2  , 0   , Color(0.498, 1, 0)         ),
        ( 3  , 0   , Color(1, 1, 1)             ),
        ( 0  , 1   , Color(0, 0, 0)             ),
        ( 1  , 1   , Color(1, 0, 0)             ),
        ( 2  , 1   , Color(0, 1, 0)             ),
        ( 3  , 1   , Color(0, 0, 1)             ),
        ( 0  , 2   , Color(1, 1, 0)             ),
        ( 1  , 2   , Color(0, 1, 1)             ),
        ( 2  , 2   , Color(1, 0, 1)             ),
        ( 3  , 2   , Color(0.498, 0.498, 0.498) )
      )

      val canvas = PPMParser.parse(content).get

      forAll(examples) { (x, y, color) =>
        Then(s"pixel_at(canvas, $x, $y) = $color")
        val c = canvas(x, y)
        assert(c == color)
      }
    }

    scenario("PPM parsing ignores comment lines") {
      val ppm = """P3
                  |# this is a comment
                  |2 1
                  |# this, too
                  |255
                  |# another comment
                  |255 255 255
                  |# oh, no, comments in the pixel data!
                  |255 0 255""".stripMargin

      Given(s"ppm ← a file containing:\n$ppm")

      When("canvas ← canvas_from_ppm(ppm)")
      Then("pixel_at(canvas, 0, 0) = color(1, 1, 1)")
       And("pixel_at(canvas, 1, 0) = color(1, 0, 1)")

      val canvas = PPMParser.parse(ppm).get
      assert(canvas(0, 0) == Color(1, 1, 1))
      assert(canvas(1, 0) == Color(1, 0, 1))
    }

    scenario("PPM parsing allows an RGB triple to span line") {
      val ppm ="""P3
                 |1 1
                 |255
                 |51
                 |153
                 |
                 |204""".stripMargin

      Given(s"ppm ← a file containing:\n$ppm")
      When("canvas ← canvas_from_ppm(ppm)")
      Then("pixel_at(canvas, 0, 0) = color(0.2, 0.6, 0.8)")

      val canvas = PPMParser.parse(ppm).get
      assert(canvas(0, 0) == Color(0.2, 0.6, 0.8))
    }

    scenario("PPM parsing respects the scale setting") {
      val ppm = """P3
                  |2 2
                  |100
                  |100 100 100  50 50 50
                  |75 50 25  0 0 0""".stripMargin
      Given(s"ppm ← a file containing:\n$ppm")

      When("canvas ← canvas_from_ppm(ppm)")
      Then("pixel_at(canvas, 0, 1) = color(0.75, 0.5, 0.25)")

      val canvas = PPMParser.parse(ppm).get
      assert(canvas(0, 1) == Color(0.75, 0.5, 0.25))
    }
  }
}
