package raytracer.math

import java.lang.Math.sqrt

import raytracer.BaseSpec
import raytracer.math.Transform._

class TransformSpec extends BaseSpec {

  feature("Matrix Transformations") {

    scenario("Multiplying by a translation matrix") {
      Given("transform ← translation(5, -3, 2)")
      And("p ← point(-3, 4, 5)")
      Then("transform * p = point(2, 1, 7)")
      val transform = Translation(5, -3, 2)
      val p = point(-3, 4, 5)
      val expected = point(2, 1, 7)
      assert(transform * p == expected)
    }

    scenario("Multiplying by the inverse of a translation matrix") {
      Given("transform ← translation(5, -3, 2)")
      And("inv ← inverse(transform)")
      And("p ← point(-3, 4, 5)")
      Then("inv * p = point(-8, 7, 3)")
      val transform = Translation(5, -3, 2)
      val inv = transform.inverse
      val p = point(-3, 4, 5)
      val expected = point(-8, 7, 3)
      assert(inv * p == expected)
    }

    scenario("Translation does not affect vectors") {
      Given("transform ← translation(5, -3, 2)")
      And("v ← vector(-3, 4, 5)")
      Then("transform * v = v")
      val transform = Translation(5, -3, 2)
      val v = vector(-3, 4, 5)
      assert(transform * v == v)
    }

    scenario("A scaling matrix applied to a point") {
      Given("transform ← scaling(2, 3, 4)")
      And("p ← point(-4, 6, 8)")
      Then("transform * p = point(-8, 18, 32)")
      val transform = Scaling(2, 3, 4)
      val p = point(-4, 6, 8)
      val expected = point(-8, 18, 32)
      assert(transform * p == expected)
    }

    scenario("A scaling matrix applied to a vector") {
      Given("transform ← scaling(2, 3, 4)")
      And("v ← vector(-4, 6, 8)")
      Then("transform * v = vector(-8, 18, 32)")
      val transform = Scaling(2, 3, 4)
      val v = vector(-4, 6, 8)
      val expected = vector(-8, 18, 32)
      assert(transform * v == expected)
    }

    scenario("Multiplying by the inverse of a scaling matrix") {
      Given("transform ← scaling(2, 3, 4)")
      And("inv ← inverse(transform)")
      And("v ← vector(-4, 6, 8)")
      Then("inv * v = vector(-2, 2, 2)")
      val transform = Scaling(2, 3, 4)
      val inv = transform.inverse
      val v = vector(-4, 6, 8)
      val expected = vector(-2, 2, 2)
      assert(inv * v == expected)
    }

    scenario("Reflection is scaling by a negative value") {
      Given("transform ← scaling(-1, 1, 1)")
      And("p ← point(2, 3, 4)")
      Then("transform * p == point(-2, 3, 4)")
      val transform = Scaling(-1, 1, 1)
      val p = point(2, 3, 4)
      val expected = point(-2, 3, 4)
      assert(transform * p == expected)
    }

    scenario("Rotating a point around the x axi") {
      Given("p ← point(0, 1, 0)")
      And("halfQuarter ← rotationX(π / 4)")
      And("full_quarter ← rotation_x(π / 2)")
      Then("half_quarter * p = point(0, √2/2, √2/2)")
      And("full_quarter * p = point(0, 0, 1)")
      val p = point(0, 1, 0)
      val halfQuarter = RotationX(π / 4)
      val fullQuarter = RotationX(π / 2)
      assert(halfQuarter * p == point(0, sqrt(2)/2, sqrt(2)/2))
      assert(fullQuarter * p == point(0, 0, 1))
    }

    scenario("The inverse of an x-rotation rotates in the opposite direction") {
      Given("p ← point(0, 1, 0)")
      And("half_quarter ← rotation_x(π / 4)")
      And("inv ← inverse(half_quarter)")
      Then("inv * p = point(0, √2/2, -√2/2)")
      val p = point(0, 1, 0)
      val halfQuarter = RotationX(π / 4)
      val inv = halfQuarter.inverse
      val expected = point(0, sqrt(2)/2, -sqrt(2)/2)
      assert(inv * p == expected)
    }

    scenario("Rotating a point around the y axi") {
      Given("p ← point(0, 0, 1)")
      And("half_quarter ← rotation_y(π / 4)")
      And("full_quarter ← rotation_y(π / 2)")
      Then("half_quarter * p = point(√2/2, 0, √2/2)")
      And("full_quarter * p = point(1, 0, 0)")
      val p = point(0, 0, 1)
      val halfQuarter = RotationY(π / 4)
      val fullQuarter = RotationY(π / 2)
      assert(halfQuarter * p == point(sqrt(2)/2, 0, sqrt(2)/2))
      assert(fullQuarter * p == point(1, 0, 0))
    }

    scenario("Rotating a point around the z axi") {
      Given("p ← point(0, 1, 0)")
      And("half_quarter ← rotation_z(π / 4)")
      And("full_quarter ← rotation_z(π / 2)")
      Then("half_quarter * p = point(-√2/2, √2/2, 0)")
      And("full_quarter * p = point(-1, 0, 0)")
      val p = point(0, 1, 0)
      val half_quarter = RotationZ(π / 4)
      val full_quarter = RotationZ(π / 2)
      assert(half_quarter * p == point(-sqrt(2)/2, sqrt(2)/2, 0))
      assert(full_quarter * p == point(-1, 0, 0))
    }

    scenario("A shearing transformation moves x in proportion to y") {
      Given("transform ← shearing(1, 0, 0, 0, 0, 0)")
      And("p ← point(2, 3, 4)")
      Then("transform * p = point(5, 3, 4)")
      val transform = Shearing(1, 0, 0, 0, 0, 0)
      val p = point(2, 3, 4)
      assert(transform * p == point(5, 3, 4))
    }

    scenario("shearing transformation moves x in proportion to z") {
      Given("transform ← shearing(0, 1, 0, 0, 0, 0)")
      And("p ← point(2, 3, 4)")
      Then("transform * p = point(6, 3, 4)")
      val transform = Shearing(0, 1, 0, 0, 0, 0)
      val p = point(2, 3, 4)
      assert(transform * p == point(6, 3, 4))
    }

    scenario("A shearing transformation moves y in proportion to x") {
      Given("transform ← shearing(0, 0, 1, 0, 0, 0)")
      And("p ← point(2, 3, 4)")
      Then("transform * p = point(2, 5, 4)")
      val transform = Shearing(0, 0, 1, 0, 0, 0)
      val p = point(2, 3, 4)
      assert(transform * p == point(2, 5, 4))
    }

    scenario("A shearing transformation moves y in proportion to z") {
      Given("transform ← shearing(0, 0, 0, 1, 0, 0)")
      And("p ← point(2, 3, 4)")
      Then("transform * p = point(2, 7, 4)")
      val transform = Shearing(0, 0, 0, 1, 0, 0)
      val p = point(2, 3, 4)
      assert(transform * p == point(2, 7, 4))
    }

    scenario("A shearing transformation moves z in proportion to x") {
      Given("transform ← shearing(0, 0, 0, 0, 1, 0)")
      And("p ← point(2, 3, 4)")
      Then("transform * p = point(2, 3, 6)")
      val transform = Shearing(0, 0, 0, 0, 1, 0)
      val p = point(2, 3, 4)
      assert(transform * p == point(2, 3, 6))
    }

    scenario("A shearing transformation moves z in proportion to y") {
      Given("transform ← shearing(0, 0, 0, 0, 0, 1)")
      And("p ← point(2, 3, 4)")
      Then("transform * p = point(2, 3, 7)")
      val transform = Shearing(0, 0, 0, 0, 0, 1)
      val p = point(2, 3, 4)
      assert(transform * p == point(2, 3, 7))
    }

    scenario("Individual transformations are applied in sequence") {
      Given("p ← point(1, 0, 1)")
      And("A ← rotation_x(π / 2)")
      And("B ← scaling(5, 5, 5)")
      And("C ← translation(10, 5, 7)")
      When("p2 ← A * p")
      Then("p2 = point(1, -1, 0)")
      When("p3 ← B * p2")
      Then("p3 = point(5, -5, 0)")
      When("p4 ← C * p3")
      Then("p4 = point(15, 0, 7)")
      val p = point(1, 0, 1)
      val A = RotationX(π / 2)
      val B = Scaling(5, 5, 5)
      val C = Translation(10, 5, 7)

      // apply rotation first
      val p2 = A * p
      assert(p2 == point(1, -1, 0))

      //then apply scaling
      val p3 = B * p2
      assert(p3 == point(5, -5, 0))

      // then apply translation
      val p4 = C * p3
      assert(p4 == point(15, 0, 7))
    }

    scenario("Chained transformations must be applied in reverse order") {
      Given("p ← point(1, 0, 1)")
      And("A ← rotation_x(π / 2)")
      And("B ← scaling(5, 5, 5)")
      And("C ← Translation(10, 5, 7)")
      When("T ← C * B * A")
      Then("T * p = point(15, 0, 7)")
      val p = point(1, 0, 1)
      val A = RotationX(π / 2)
      val B = Scaling(5, 5, 5)
      val C = Translation(10, 5, 7)
      val T = C * B * A
      assert(T * p == point(15, 0, 7))
    }

    scenario("Chained transformations with the builder") {
      Given("p ← point(1, 0, 1)")
      And("A ← rotation_x(π / 2)")
      And("B ← scaling(5, 5, 5)")
      And("C ← translation(10, 5, 7)")
      When("T ← C * B * A")
      Then("T * p = point(15, 0, 7)")
      val p = point(1, 0, 1)
      val T = Transform()
        .rotateX(π / 2)
        .scale(5, 5, 5)
        .translate(10, 5, 7)
        .build()
      assert(T * p == point(15, 0, 7))
    }

    scenario("The transformation matrix for the default orientation") {
      Given("from ← point(0, 0, 0)")
      And("to ← point(0, 0, -1)")
      And("up ← vector(0, 1, 0)")
      When("t ← view_transform(from, to, up)")
      Then("t = identity_matrix")
      val from = point(0, 0, 0)
      val to = point(0, 0, -1)
      val up = vector(0, 1, 0)
      val t = ViewTransform(from, to, up)
      assert(t.matrix == Matrix.identity)
    }

    scenario("A view transformation matrix looking in positive z direction") {
      Given("from ← point(0, 0, 0)")
      And("to = point(0, 0, 1)")
      And("up ← vector(0, 1, 0)")
      When("t ← view_transform(from, to, up)")
      Then("t = scaling(-1, 1, -1)")
      val from = point(0, 0, 0)
      val to = point(0, 0, 1)
      val up = vector(0, 1, 0)
      val t = ViewTransform(from, to, up)
      assert(t.matrix == Scaling(-1, 1, -1).matrix)
    }

    scenario("The view transformation moves the world") {
      Given("from ← point(0, 0, 8)")
      And("to ← point(0, 0, 0)")
      And("up ← vector(0, 1, 0)")
      When("t ← viewTransform(from, to, up)")
      Then("t = translation(0, 0, -8)")
      val from = point(0, 0, 8)
      val to = point(0, 0, 0)
      val up = vector(0, 1, 0)
      val t = ViewTransform(from, to, up)
      assert(t.matrix == Translation(0, 0, -8).matrix)
    }

    scenario("An arbitrary view transformation") {
      Given("from ← point(1, 3, 2)")
      And("to ← point(4, -2, 8)")
      And("up ← vector(1, 1, 0)")
      When("t ← viewTransform(from, to, up)")
      val m =
        """
          | -0.50709 | 0.50709 |  0.67612 | -2.36643 |
          |  0.76772 | 0.60609 |  0.12122 | -2.82843 |
          | -0.35857 | 0.59761 | -0.71714 |  0.00000 |
          |  0.00000 | 0.00000 |  0.00000 |  1.00000 |
        """.matrix
      Then("t is the following 4x4 matrix: $m")
      val from = point(1, 3, 2)
      val to = point(4, -2, 8)
      val up = vector(1, 1, 0)
      val t = ViewTransform(from, to, up)

      assert(t.matrix  == m)
    }

  }
}
