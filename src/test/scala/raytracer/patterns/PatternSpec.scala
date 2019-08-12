package raytracer.patterns

import raytracer.math.{Matrix, UVMaps}
import raytracer.math.Transform._
import raytracer.shapes.{Cube, Shape}
import raytracer.{BaseSpec, Color}
import org.scalatest.prop.TableDrivenPropertyChecks._
import raytracer.resource.ppm.PPMParser

import scala.math.sqrt

class PatternSpec extends BaseSpec {
  val white = Color.White
  val black = Color.Black

  feature("Patterns") {

    scenario("Creating a stripe pattern") {
      Given("pattern ← stripe_pattern(white, black)")
      Then("pattern.a = white")
      And("pattern.b = black")
      val pattern = Pattern.stripe(white, black)
      assert(pattern.a == white)
      assert(pattern.b == black)
    }

    scenario("A stripe pattern is constant in y") {
      Given("pattern ← stripe_pattern(white, black)")
      Then("stripe_at(pattern, point(0, 0, 0)) = white")
      And("stripe_at(pattern, point(0, 1, 0)) = white")
      And("stripe_at(pattern, point(0, 2, 0)) = white")
      val pattern = Pattern.stripe(white, black)
      assert(pattern.colorAt(point(0, 0, 0)) == white)
      assert(pattern.colorAt(point(0, 1, 0)) == white)
      assert(pattern.colorAt(point(0, 2, 0)) == white)
    }

    scenario("A stripe pattern is constant in z") {
      Given("pattern ← stripe_pattern(white, black)")
      Then("stripe_at(pattern, point(0, 0, 0)) = white")
      And("stripe_at(pattern, point(0, 0, 1)) = white")
      And("stripe_at(pattern, point(0, 0, 2)) = white")
      val pattern = Pattern.stripe(white, black)
      assert(pattern.colorAt(point(0, 0, 0)) == white)
      assert(pattern.colorAt(point(0, 0, 1)) == white)
      assert(pattern.colorAt(point(0, 0, 2)) == white)
    }

    scenario("A stripe pattern alternates in x") {
      Given("pattern ← stripe_pattern(white, black)")
      Then("stripe_at(pattern, point(0, 0, 0)) = white")
      And("stripe_at(pattern, point(0.9, 0, 0)) = white")
      And("stripe_at(pattern, point(1, 0, 0)) = black")
      And("stripe_at(pattern, point(-0.1, 0, 0)) = black")
      And("stripe_at(pattern, point(-1, 0, 0)) = black")
      And("stripe_at(pattern, point(-1.1, 0, 0)) = white")
      val pattern = Pattern.stripe(white, black)
      assert(pattern.colorAt(point(0, 0, 0)) == white)
      assert(pattern.colorAt(point(0.9, 0, 0)) == white)
      assert(pattern.colorAt(point(1, 0, 0)) == black)
      assert(pattern.colorAt(point(-0.1, 0, 0)) == black)
      assert(pattern.colorAt(point(-1, 0, 0)) == black)
      assert(pattern.colorAt(point(-1.1, 0, 0)) == white)
    }

    scenario("Stripes with an object transformation") {
      Given("object ← sphere()")
      And("set_transform(object, scaling(2, 2, 2))")
      And("pattern ← stripe_pattern(white, black)")
      When("c ← stripe_at_object(pattern, object, point(1.5, 0, 0))")
      Then("c = white")
      val obj = sphere(Scaling(2, 2, 2))
      val pattern = Pattern.stripe(white, black)
      val c = pattern.colorAt(point(1.5, 0, 0), obj)
      assert(c == white)
    }

    scenario("Stripes with a pattern transformation") {
      Given("object ← sphere()")
      And("pattern ← stripe_pattern(white, black)")
      And("set_pattern_transform(pattern, scaling(2, 2, 2))")
      Then("c = white")
      val obj = sphere()
      val pattern = Pattern.stripe(white, black)
        .setTransform(Scaling(2, 2, 2))
      val c = pattern.colorAt(point(1.5, 0, 0), obj)
      assert(c == white)
    }

    scenario("Stripes with both an object and a pattern transformation") {
      Given("object ← sphere()")
      And("set_transform(object, scaling(2, 2, 2))")
      And("pattern ← stripe_pattern(white, black)")
      And("set_pattern_transform(pattern, translation(0.5, 0, 0))")
      When("c ← stripe_at_object(pattern, object, point(2.5, 0, 0))")
      Then("c = white")

      val pattern = Pattern.stripe(white, black).setTransform(Translation(0.5, 0, 0))
      val obj = Shape()
        .scale(2, 2, 2)
        .setPattern(pattern)
        .sphere

      val c = pattern.colorAt(point(2.5, 0, 0), obj)
      assert(c == white)
    }

    scenario("The default pattern transformation") {
      Given("pattern ← test_pattern()")
      Then("pattern.transform = identity_matrix")
      val pattern = TestPattern()
      assert(pattern.transform == Matrix.identity)
    }

    scenario(" Assigning a transformation") {
      Given("pattern ← test_pattern()")
      Then("pattern.transform = translation(1, 2, 3)")
      When("set_pattern_transform(pattern, translation(1, 2, 3))")
      val pattern = TestPattern().setTransform(Translation(1, 2, 3))
      assert(pattern.transform == Translation(1, 2, 3).matrix)
    }

    scenario("A pattern with an object transformation") {
      Given("shape ← sphere()")
      And(" set_transform(shape, scaling(2, 2, 2))")
      And("pattern ← test_pattern()")
      When("c ← pattern_at_shape(pattern, shape, point(2, 3, 4))")
      Then("c = color(1, 1.5, 2)")
      val shape = sphere(Scaling(2, 2, 2))
      val pattern = TestPattern()
      val c = pattern.colorAt(point(2, 3, 4), shape)
      assert(c == Color(1, 1.5, 2))
    }

    scenario("A pattern with a pattern transformation") {
      Given("shape ← sphere()")
      And("pattern ← test_pattern()")
      And("set_pattern_transform(pattern, scaling(2, 2, 2))")
      When("c ← pattern_at_shape(pattern, shape, point(2, 3, 4))")
      Then("c = color(1, 1.5, 2)")
      val shape = sphere()
      val pattern = TestPattern().setTransform(Scaling(2, 2, 2))
      val c = pattern.colorAt(point(2, 3, 4), shape)
      assert(c == Color(1, 1.5, 2))
    }

    scenario("A pattern with both an object and a pattern transformation") {
      Given("shape ← sphere()")
      And("set_transform(shape, scaling(2, 2, 2))")
      And("pattern ← test_pattern()")
      And("set_pattern_transform(pattern, translation(0.5, 1, 1.5))")
      When("c ← pattern_at_shape(pattern, shape, point(2.5, 3, 3.5))")
      Then("c = color(0.75, 0.5, 0.25)")
      val shape = sphere(Scaling(2, 2, 2))
      val pattern = TestPattern().setTransform(Translation(0.5, 1, 1.5))
      val c = pattern.colorAt(point(2.5, 3, 3.5), shape)
      assert(c == Color(0.75, 0.5, 0.25))
    }

    scenario("A gradient linearly interpolates between colors") {
      Given("pattern ← gradient_pattern(white, black)")
      Then("pattern_at(pattern, point(0, 0, 0)) = white")
      And("pattern_at(pattern, point(0.25, 0, 0)) = color(0.75, 0.75, 0.75)")
      And("pattern_at(pattern, point(0.5, 0, 0)) = color(0.5, 0.5, 0.5)")
      And("pattern_at(pattern, point(0.75, 0, 0)) = color(0.25, 0.25, 0.25)")
      val pattern = Pattern.gradient(white, black)
      assert(pattern.colorAt(point(0, 0, 0)) == white)
      assert(pattern.colorAt(point(0.25, 0, 0)) == Color(0.75, 0.75, 0.75))
      assert(pattern.colorAt(point(0.5, 0, 0)) == Color(0.5, 0.5, 0.5))
      assert(pattern.colorAt(point(0.75, 0, 0)) == Color(0.25, 0.25, 0.25))
    }

    scenario("A ring should extend in both x and z") {
      Given("pattern ← ring_pattern(white, black)")
      Then("pattern_at(pattern, point(0, 0, 0)) = white")
      And("pattern_at(pattern, point(1, 0, 0)) = black")
      And("pattern_at(pattern, point(0, 0, 1)) = black")
      // 0.708 = just slightly more than √2/2
      And(" pattern_at(pattern, point(0.708, 0, 0.708)) = black")
      val pattern = Pattern.ring(white, black)
      assert(pattern.colorAt(point(0, 0, 0)) == white)
      assert(pattern.colorAt(point(1, 0, 0)) == black)
      assert(pattern.colorAt(point(0.708, 0, 0.708)) == black)
    }

    scenario("Checkers should repeat in x") {
      Given("pattern ← checkers_pattern(white, black)")
      Then("pattern_at(pattern, point(0, 0, 0)) = white")
      And("pattern_at(pattern, point(0.99, 0, 0)) = white")
      And("pattern_at(pattern, point(1.01, 0, 0)) = black")
      val pattern = Pattern.checkers(white, black)
      assert(pattern.colorAt(point(0, 0, 0)) == white)
      assert(pattern.colorAt(point(0.99, 0, 0)) == white)
      assert(pattern.colorAt(point(1.01, 0, 0)) == black)
    }

    scenario("Checkers should repeat in y") {
      Given("pattern ← checkers_pattern(white, black)")
      Then("pattern_at(pattern, point(0, 0, 0)) = white")
      And("pattern_at(pattern, point(0, 0.99, 0)) = white")
      And("pattern_at(pattern, point(0, 1.01, 0)) = black")
      val pattern = Pattern.checkers(white, black)
      assert(pattern.colorAt(point(0, 0, 0)) == white)
      assert(pattern.colorAt(point(0, 0.99, 0)) == white)
      assert(pattern.colorAt(point(0, 1.01, 0)) == black)
    }

    scenario("Checkers should repeat in z") {
      Given("pattern ← checkers_pattern(white, black)")
      Then("pattern_at(pattern, point(0, 0, 0)) = white")
      And("pattern_at(pattern, point(0, 0, 0.99)) = white")
      And("pattern_at(pattern, point(0, 0, 1.01)) = black")
      val pattern = Pattern.checkers(white, black)
      assert(pattern.colorAt(point(0, 0, 0)) == white)
      assert(pattern.colorAt(point(0, 0, 0.99)) == white)
      assert(pattern.colorAt(point(0, 0, 1.01)) == black)
    }


    scenario("Checker pattern in 2D") {
      val examples = Table(
        ("u", "v", "expected"),
        (0.0, 0.0, black),
        (0.5, 0.0, white),
        (0.0, 0.5, white),
        (0.5, 0.5, black),
        (1.0, 1.0, black)
      )

      forAll(examples) { (u, v, expected) =>
        Given("checkers ← uv_checkers(2, 2, black, white)")
        When(s"color ← uv_pattern_at(checkers, $u, $v)")
        Then(s"color = $expected")

        val checkers = UVPattern.Checkers(2, 2, black, white)
        val color = checkers.colorAt(u, v)
        assert(color == expected)
      }
    }

    scenario("Using a spherical mapping on a 3D point") {
      val examples = Table(
        ("point", "u", "v"),
        (point(0, 0, -1), 0.0, 0.5),
        (point(1, 0, 0), 0.25, 0.5),
        (point(0, 0, 1), 0.5, 0.5),
        (point(-1, 0, 0), 0.75, 0.5),
        (point(0, 1, 0), 0.5, 1.0),
        (point(0, -1, 0), 0.5, 0.0),
        (point(sqrt(2) / 2, sqrt(2) / 2, 0), 0.25, 0.75)
      )

      forAll(examples) { (point, expectedU, expectedV) =>
        Given(s"p ← $point")
        When("(u, v) ← spherical_map(p)")
        Then(s"u = $expectedU")
        And(s"v = $expectedV")

        val p = point
        val (u, v) = UVMaps.spherical(p)
        assert(u == expectedU)
        assert(v == expectedV)
      }
    }

    scenario(s"Using a texture map pattern with a spherical map") {
      val examples = Table(
        ("point", "color"),
        (point(0.4315, 0.4670, 0.7719), white),
        (point(-0.9654, 0.2552, -0.0534), black),
        (point(0.1039, 0.7090, 0.6975), white),
        (point(-0.4986, -0.7856, -0.3663), black),
        (point(-0.0317, -0.9395, 0.3411), black),
        (point(0.4809, -0.7721, 0.4154), black),
        (point(0.0285, -0.9612, -0.2745), black),
        (point(-0.5734, -0.2162, -0.7903), white),
        (point(0.7688, -0.1470, 0.6223), black),
        (point(-0.7652, 0.2175, 0.6060), black)
      )

      forAll(examples) { (point, color) =>
        Given("checkers ← uv_checkers(16, 8, black, white)")
        And("pattern ← texture_map(checkers, spherical_map)")
        Then(s"pattern_at(pattern, $point) = $color")

        val checkers = UVPattern.Checkers(16, 8, black, white)
        val pattern = TextureMapPattern(checkers, UVMapping.Spherical)
        assert(pattern.colorAt(point) == color)
      }
    }


    scenario("Using a planar mapping on a 3D point") {
      val examples = Table(
        ("point", "u", "v"),
        (point(0.25, 0, 0.5), 0.25, 0.5),
        (point(0.25, 0, -0.25), 0.25, 0.75),
        (point(0.25, 0.5, -0.25), 0.25, 0.75),
        (point(1.25, 0, 0.5), 0.25, 0.5),
        (point(0.25, 0, -1.75), 0.25, 0.25),
        (point(1, 0, -1), 0.0, 0.0),
        (point(0, 0, 0), 0.0, 0.0)
      )

      forAll(examples) { (point, expectedU, expectedV) =>
        Given(s"p ← $point")
        When("(u, v) ← planar_map(p)")
        Then(s"u = $expectedU")
        And(s"v = $expectedV")

        val (u, v) = UVMaps.planar(point)
        assert(u == expectedU)
        assert(v == expectedV)
      }
    }

    scenario("Using a cylindrical mapping on a 3D point") {
      val examples = Table(
        ("point", "u", "v"),
        (point(0, 0, -1), 0.0, 0.0),
        (point(0, 0.5, -1), 0.0, 0.5),
        (point(0, 1, -1), 0.0, 0.0),
        (point(0.70711, 0.5, -0.70711), 0.125, 0.5),
        (point(1, 0.5, 0), 0.25, 0.5),
        (point(0.70711, 0.5, 0.70711), 0.375, 0.5),
        (point(0, -0.25, 1), 0.5, 0.75),
        (point(-0.70711, 0.5, 0.70711), 0.625, 0.5),
        (point(-1, 1.25, 0), 0.75, 0.25),
        (point(-0.70711, 0.5, -0.70711), 0.875, 0.5)
      )

      forAll(examples) { (point, expectedU, expectedV) =>
        Given(s"p ← $point")
        When("(u, v) ← cylindrical_map(p)")
        Then(s"u = $expectedU")
        And(s"v = $expectedV")

        val p = point
        val (u, v) = UVMaps.cylindrical(p)
        assert(u == expectedU)
        assert(v == expectedV)
      }
    }

    scenario("Layout of the 'align check' pattern") {
      Given("main ← color(1, 1, 1)")
      And("ul ← color(1, 0, 0)")
      And("ur ← color(1, 1, 0)")
      And("bl ← color(0, 1, 0)")
      And("br ← color(0, 1, 1)")
      And("pattern ← uv_align_check(main, ul, ur, bl, br)")

      val main = Color(1, 1, 1)
      val ul = Color(1, 0, 0)
      val ur = Color(1, 1, 0)
      val bl = Color(0, 1, 0)
      val br = Color(0, 1, 1)
      val pattern = UVPattern.AlignCheck(main, ul, ur, bl, br)
      val table = Table(
        ("u", "v", "expected"),
        (0.5, 0.5, main),
        (0.1, 0.9, ul),
        (0.9, 0.9, ur),
        (0.1, 0.1, bl),
        (0.9, 0.1, br)
      )
      forAll(table) { (u, v, expected) =>
        When(s"c ← uv_pattern_at(pattern, $u, $v)")
        Then(s"c = $expected")

        val c = pattern.colorAt(u, v)
        assert(c == expected)
      }
    }


    scenario("Identifying the face of a cube from a point") {
      import Cube.Face._
      val examples = Table(
        ("point", "face"),
        (point(-1, 0.5, -0.25), Left),
        (point(1.1, -0.75, 0.8), Right),
        (point(0.1, 0.6, 0.9), Front),
        (point(-0.7, 0, -2), Back),
        (point(0.5, 1, 0.9), Up),
        (point(-0.2, -1.3, 1.1), Down)
      )

      forAll(examples) { (point, expectedFace) =>
        When(s"face ← face_from_point($point)")
        Then(s"face = $expectedFace")

        val face = Cube.face(point)
        assert(face == expectedFace)
      }
    }

    scenario("UV mapping the front face of a cube") {
      val examples = Table(
        ("point", "u", "v"),
        (point(-0.5, 0.5, 1), 0.25, 0.75),
        (point(0.5, -0.5, 1), 0.75, 0.25)
      )
      forAll(examples) { (point, expectedU, expectedV) =>
        When(s"(u, v) ← cube_uv_front($point)")
        Then(s"u = $expectedU")
        And(s"v = $expectedV")

        val (u, v) = UVMaps.cubeFront(point)
        assert(u == expectedU)
        assert(v == expectedV)
      }
    }


    scenario("UV mapping the back face of a cub") {
      val examples = Table(
        ("point", "u", "v"),
        (point(0.5, 0.5, -1), 0.25, 0.75),
        (point(-0.5, -0.5, -1), 0.75, 0.25)
      )
      forAll(examples) { (point, expectedU, expectedV) =>
        When(s"(u, v) ← cube_uv_back($point)")
        Then(s"u = $expectedU")
        And(s"v = $expectedV")

        val (u, v) = UVMaps.cubeBack(point)
        assert(u == expectedU)
        assert(v == expectedV)
      }
    }

    scenario("UV mapping the left face of a cube") {
      val examples = Table(
        ("point", "u", "v"),
        (point(-1, 0.5, -0.5), 0.25, 0.75),
        (point(-1, -0.5, 0.5), 0.75, 0.25)
      )
      forAll(examples) { (point, expectedU, expectedV) =>
        When(s"(u, v) ← cube_uv_left($point)")
        Then(s"u = $expectedU")
        And(s"v = $expectedV")

        val (u, v) = UVMaps.cubeLeft(point)
        assert(u == expectedU)
        assert(v == expectedV)
      }
    }

    scenario("UV mapping the right face of a cube") {
      val examples = Table(
        ("point", "u", "v"),
        (point(1, 0.5, 0.5), 0.25, 0.75),
        (point(1, -0.5, -0.5), 0.75, 0.25)
      )
      forAll(examples) { (point, expectedU, expectedV) =>
        When(s"(u, v) ← cube_uv_right($point)")
        Then(s"u = $expectedU")
        And(s"v = $expectedV")

        val (u, v) = UVMaps.cubeRight(point)
        assert(u == expectedU)
        assert(v == expectedV)
      }
    }

    scenario("UV mapping the upper face of a cube") {
      val examples = Table(
        ("point", "u", "v"),
        (point(-0.5, 1, -0.5), 0.25, 0.75),
        (point(0.5, 1, 0.5), 0.75, 0.25)
      )
      forAll(examples) { (point, expectedU, expectedV) =>
        When(s"(u, v) ← cube_uv_up($point)")
        Then(s"u = $expectedU")
        And(s"v = $expectedV")

        val (u, v) = UVMaps.cubeUp(point)
        assert(u == expectedU)
        assert(v == expectedV)
      }
    }

    scenario("UV mapping the lower face of a cube") {
      val examples = Table(
        ("point", "u", "v"),
        (point(-0.5, -1, 0.5), 0.25, 0.75),
        (point(0.5, -1, -0.5), 0.75, 0.25)
      )
      forAll(examples) { (point, expectedU, expectedV) =>
        When(s"(u, v) ← cube_uv_up($point)")
        Then(s"u = $expectedU")
        And(s"v = $expectedV")

        val (u, v) = UVMaps.cubeDown(point)
        assert(u == expectedU)
        assert(v == expectedV)
      }
    }

    scenario("Finding the colors on a mapped cube") {
      When("left ← uv_align_check(yellow, cyan, red, blue, brown)")
      And("front ← uv_align_check(cyan, red, yellow, brown, green)")
      And("right ← uv_align_check(red, yellow, purple, green, white)")
      And("back ← uv_align_check(green, purple, cyan, white, blue)")
      And("up ← uv_align_check(brown, cyan, purple, red, yellow)")
      And("down ← uv_align_check(purple, brown, green, blue, white)")
      And("pattern ← cube_map(left, front, right, back, up, down)")
      import Color._
      import Cube.Face._
      val examples = Table(
        ("face", "point", "color"),
        (Left, point(-1, 0, 0), Yellow),
        (Left, point(-1, 0.9, -0.9), Cyan),
        (Left, point(-1, 0.9, 0.9), Red),
        (Left, point(-1, -0.9, -0.9), Blue),
        (Left, point(-1, -0.9, 0.9), Brown),
        (Front, point(0, 0, 1), Cyan),
        (Front, point(-0.9, 0.9, 1), Red),
        (Front, point(0.9, 0.9, 1), Yellow),
        (Front, point(-0.9, -0.9, 1), Brown),
        (Front, point(0.9, -0.9, 1), Green),
        (Right, point(1, 0, 0), Red),
        (Right, point(1, 0.9, 0.9), Yellow),
        (Right, point(1, 0.9, -0.9), Purple),
        (Right, point(1, -0.9, 0.9), Green),
        (Right, point(1, -0.9, -0.9), white),
        (Back, point(0, 0, -1), Green),
        (Back, point(0.9, 0.9, -1), Purple),
        (Back, point(-0.9, 0.9, -1), Cyan),
        (Back, point(0.9, -0.9, -1), white),
        (Back, point(-0.9, -0.9, -1), Blue),
        (Up, point(0, 1, 0), Brown),
        (Up, point(-0.9, 1, -0.9), Cyan),
        (Up, point(0.9, 1, -0.9), Purple),
        (Up, point(-0.9, 1, 0.9), Red),
        (Up, point(0.9, 1, 0.9), Yellow),
        (Down, point(0, -1, 0), Purple),
        (Down, point(-0.9, -1, 0.9), Brown),
        (Down, point(0.9, -1, 0.9), Green),
        (Down, point(-0.9, -1, -0.9), Blue),
        (Down, point(0.9, -1, -0.9), white)
      )
      forAll(examples) { (face, point, color) =>
        Then(s"pattern_at(pattern, $point) = $color")

        val left = UVPattern.AlignCheck(Yellow, Cyan, Red, Blue, Brown)
        val front = UVPattern.AlignCheck(Cyan, Red, Yellow, Brown, Green)
        val right = UVPattern.AlignCheck(Red, Yellow, Purple, Green, white)
        val back = UVPattern.AlignCheck(Green, Purple, Cyan, white, Blue)
        val up = UVPattern.AlignCheck(Brown, Cyan, Purple, Red, Yellow)
        val down = UVPattern.AlignCheck(Purple, Brown, Green, Blue, white)
        val pattern = Pattern.cubeMap(left, front, right, back, up, down)
        assert(Cube.face(point) == face)
        val c = pattern.colorAt(point)
        assert(c == color)
      }
    }

    scenario("Checker pattern in 2D - image") {

      val ppm =
        """P3
          |10 10
          |10
          |0 0 0  1 1 1  2 2 2  3 3 3  4 4 4  5 5 5  6 6 6  7 7 7  8 8 8  9 9 9
          |1 1 1  2 2 2  3 3 3  4 4 4  5 5 5  6 6 6  7 7 7  8 8 8  9 9 9  0 0 0
          |2 2 2  3 3 3  4 4 4  5 5 5  6 6 6  7 7 7  8 8 8  9 9 9  0 0 0  1 1 1
          |3 3 3  4 4 4  5 5 5  6 6 6  7 7 7  8 8 8  9 9 9  0 0 0  1 1 1  2 2 2
          |4 4 4  5 5 5  6 6 6  7 7 7  8 8 8  9 9 9  0 0 0  1 1 1  2 2 2  3 3 3
          |5 5 5  6 6 6  7 7 7  8 8 8  9 9 9  0 0 0  1 1 1  2 2 2  3 3 3  4 4 4
          |6 6 6  7 7 7  8 8 8  9 9 9  0 0 0  1 1 1  2 2 2  3 3 3  4 4 4  5 5 5
          |7 7 7  8 8 8  9 9 9  0 0 0  1 1 1  2 2 2  3 3 3  4 4 4  5 5 5  6 6 6
          |8 8 8  9 9 9  0 0 0  1 1 1  2 2 2  3 3 3  4 4 4  5 5 5  6 6 6  7 7 7
          |9 9 9  0 0 0  1 1 1  2 2 2  3 3 3  4 4 4  5 5 5  6 6 6  7 7 7  8 8 8""".stripMargin

      Given(s"ppm ← a file containing:\n$ppm")

      And("canvas ← canvas_from_ppm(ppm)")
      And("pattern ← uv_image(canvas)")
      val canvas = PPMParser.parse(ppm).get
      val pattern = UVPattern.Image(canvas)

      val examples = Table(
        ("u", "v", "expected"),
        (0.0, 0.0, Color(0.9, 0.9, 0.9)),
        (0.3, 0.0, Color(0.2, 0.2, 0.2)),
        (0.6, 0.3, Color(0.1, 0.1, 0.1)),
        (1.0, 1.0, Color(0.9, 0.9, 0.9))
      )

      forAll(examples) { (u, v, expected) =>
        When(s"color ← uv_pattern_at(pattern, $u, $v)")
        Then(s"color = $expected")

        val color = pattern.colorAt(u, v)
        assert(color == expected)
      }
    }
  }
}
