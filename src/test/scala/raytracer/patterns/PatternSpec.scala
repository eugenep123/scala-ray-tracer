package raytracer.patterns

import raytracer.math.Matrix
import raytracer.math.Transform._
import raytracer.shapes.Shape
import raytracer.{BaseSpec, Color}

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
  }
}
