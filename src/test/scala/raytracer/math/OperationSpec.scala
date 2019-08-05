package raytracer.math

import java.lang.Math.sqrt

import raytracer.BaseSpec

class OperationSpec extends BaseSpec {

  feature("Matrix Transformations") {

    scenario("Multiplying by a translation matrix") {
      val transform = translation(5, -3, 2)
      Given(s"transform ← translation(5, -3, 2): $transform")
      val p = point(-3, 4, 5)
      And(s"p ← $p")

      val expected = point(2, 1, 7)
      Then("transform * p = $expected")
      assert(transform * p == expected)
    }

    scenario("Multiplying by the inverse of a translation matrix") {
      val transform = translation(5, -3, 2)
      Given(s"transform ← translation(5, -3, 2): $transform")
      val inv = inverse(transform)
      And(s"inv ← inverse(transform): $inv")
      val p = point(-3, 4, 5)
      And(s"p ← $p")
      val expected = point(-8, 7, 3)
      Then(s"inv * p = $expected")
      assert(inv * p == expected)
    }

    scenario("Translation does not affect vectors") {
      val transform = translation(5, -3, 2)
      Given(s"transform ← translation(5, -3, 2): $transform")
      val v = vector(-3, 4, 5)
      And(s"v ← $v")
      Then("transform * v = v")
      assert(transform * v == v)
    }

    scenario("A scaling matrix applied to a point") {
      val transform = scaling(2, 3, 4)
      Given(s"transform ← scaling(2, 3, 4): $transform")
      val p = point(-4, 6, 8)
      And(s"p ← $p")
      val expected = point(-8, 18, 32)
      Then("transform * p = point(-8, 18, 32)")
      assert(transform * p == expected)
    }

    scenario("A scaling matrix applied to a vector") {
      val transform = scaling(2, 3, 4)
      Given(s"transform ← scaling(2, 3, 4): $transform")
      val v = vector(-4, 6, 8)
      And(s"v ← $v")
      val expected = vector(-8, 18, 32)
      Then(s"transform * v = $expected")
      assert(transform * v == expected)
    }

    scenario("Multiplying by the inverse of a scaling matrix") {
      val transform = scaling(2, 3, 4)
      Given(s"transform ← scaling(2, 3, 4): $transform")
      val inv = inverse(transform)
      And(s"inv ← inverse(transform): $inv")
      val v = vector(-4, 6, 8)
      And(s"v ← $v")
      val expected = vector(-2, 2, 2)
      Then(s"inv * v = $expected")
      assert(inv * v == expected)
    }

    scenario("Reflection is scaling by a negative value") {
      val transform = scaling(-1, 1, 1)
      Given(s"transform ← scaling(-1, 1, 1): $transform")
      val p = point(2, 3, 4)
      And(s"p ← $p")
      val expected = point(-2, 3, 4)
      Then(s"transform * p == $expected")
      assert(transform * p == expected)
    }

    scenario("Rotating a point around the x axis") {
      val p = point(0, 1, 0)
      Given(s"p ← $p")
      val halfQuarter = rotationX(π / 4)
      val fullQuarter = rotationX(π / 2)
      And(s"halfQuarter ← rotationX(π / 4): $halfQuarter")
      And(s"full_quarter ← rotation_x(π / 2): $fullQuarter")
      Then("half_quarter * p = point(0, √2/2, √2/2)")
      assert(halfQuarter * p == point(0, sqrt(2)/2, sqrt(2)/2))
      And("full_quarter * p = point(0, 0, 1)")
      assert(fullQuarter * p == point(0, 0, 1))
    }

    scenario("The inverse of an x-rotation rotates in the opposite direction") {
      val p = point(0, 1, 0)
      Given(s"p ← $p")
      val halfQuarter = rotationX(π / 4)
      And(s"half_quarter ← rotation_x(π / 4): $halfQuarter")
      val inv = inverse(halfQuarter)
      And(s"inv ← inverse(half_quarter): $inv")
      val expected = point(0, sqrt(2)/2, -sqrt(2)/2)
      Then(s"inv * p = point(0, √2/2, -√2/2): $expected")
      assert(inv * p == expected)
    }

    scenario("Rotating a point around the y axis") {
      val p = point(0, 0, 1)
      Given("p ← point(0, 0, 1)")
      val halfQuarter = rotationY(π / 4)
      And(s"half_quarter ← rotation_y(π / 4): $halfQuarter")
      val fullQuarter = rotationY(π / 2)
      And(s"full_quarter ← rotation_y(π / 2): $fullQuarter")
      Then("half_quarter * p = point(√2/2, 0, √2/2)")
      assert(halfQuarter * p == point(sqrt(2)/2, 0, sqrt(2)/2))
      And("full_quarter * p = point(1, 0, 0)")
      assert(fullQuarter * p == point(1, 0, 0))
    }

    scenario("Rotating a point around the z axis") {
      val p = point(0, 1, 0)
      Given(s"p ← $p")
      val half_quarter = rotationZ(π / 4)
      And("half_quarter ← rotation_z(π / 4)")
      val full_quarter = rotationZ(π / 2)
      And("full_quarter ← rotation_z(π / 2)")
      Then("half_quarter * p = point(-√2/2, √2/2, 0)")
      assert(half_quarter * p == point(-sqrt(2)/2, sqrt(2)/2, 0))
      And("full_quarter * p = point(-1, 0, 0)")
      assert(full_quarter * p == point(-1, 0, 0))
    }

    scenario("A shearing transformation moves x in proportion to y") {
      val transform = shearing(1, 0, 0, 0, 0, 0)
      Given(s"transform ← shearing(1, 0, 0, 0, 0, 0): $transform")
      val p = point(2, 3, 4)
      And(s"p ← $p")
      Then("transform * p = point(5, 3, 4)")
      assert(transform * p == point(5, 3, 4))
    }

    scenario("shearing transformation moves x in proportion to z") {
      val transform = shearing(0, 1, 0, 0, 0, 0)
      Given("transform ← shearing(0, 1, 0, 0, 0, 0)")
      val p = point(2, 3, 4)
      And(s"p ← $p")
      Then("transform * p = point(6, 3, 4)")
      assert(transform * p == point(6, 3, 4))
    }

    scenario("A shearing transformation moves y in proportion to x") {
      val transform = shearing(0, 0, 1, 0, 0, 0)
      Given("transform ← shearing(0, 0, 1, 0, 0, 0)")
      val p = point(2, 3, 4)
      And(s"p ← $p")
      Then("transform * p = point(2, 5, 4)")
      assert(transform * p == point(2, 5, 4))
    }

    scenario("A shearing transformation moves y in proportion to z") {
      val transform = shearing(0, 0, 0, 1, 0, 0)
      Given("transform ← shearing(0, 0, 0, 1, 0, 0)")
      val p = point(2, 3, 4)
      And(s"p ← $p")
      Then("transform * p = point(2, 7, 4)")
      assert(transform * p == point(2, 7, 4))
    }

    scenario("A shearing transformation moves z in proportion to x") {
      val transform = shearing(0, 0, 0, 0, 1, 0)
      Given("transform ← shearing(0, 0, 0, 0, 1, 0)")
      val p = point(2, 3, 4)
      And(s"p ← $p")
      Then("transform * p = point(2, 3, 6)")
      assert(transform * p == point(2, 3, 6))
    }

    scenario("A shearing transformation moves z in proportion to y") {
      val transform = shearing(0, 0, 0, 0, 0, 1)
      Given("transform ← shearing(0, 0, 0, 0, 0, 1)")
      val p = point(2, 3, 4)
      And(s"p ← $p")
      Then("transform * p = point(2, 3, 7)")
      assert(transform * p == point(2, 3, 7))
    }

    scenario("Individual transformations are applied in sequence") {
      val p = point(1, 0, 1)
      Given(s"p ← $p")
      And("A ← rotation_x(π / 2)")
      And("B ← scaling(5, 5, 5)")
      And("C ← translation(10, 5, 7)")
      val A = rotationX(π / 2)
      val B = scaling(5, 5, 5)
      val C = translation(10, 5, 7)

      // apply rotation first

      When("p2 ← A * p")
      Then("p2 = point(1, -1, 0)")
      val p2 = A * p
      assert(p2 == point(1, -1, 0))

      //then apply scaling
      When("p3 ← B * p2")
      Then("p3 = point(5, -5, 0)")
      val p3 = B * p2
      assert(p3 == point(5, -5, 0))

      // then apply translation
      When("p4 ← C * p3")
      Then("p4 = point(15, 0, 7)")
      val p4 = C * p3
      assert(p4 == point(15, 0, 7))
    }

    scenario("Chained transformations must be applied in reverse order") {
      Given("p ← point(1, 0, 1)")
      val p = point(1, 0, 1)
      And("A ← rotation_x(π / 2)")
      val A = rotationX(π / 2)
      And("B ← scaling(5, 5, 5)")
      val B = scaling(5, 5, 5)
      And("C ← translation(10, 5, 7)")
      val C = translation(10, 5, 7)
      When("T ← C * B * A")
      val T = C * B * A
      Then("T * p = point(15, 0, 7)")
      assert(T * p == point(15, 0, 7))
    }

    scenario("Chained transformations with the builder") {
      Given("p ← point(1, 0, 1)")
      val p = point(1, 0, 1)
      And("A ← rotation_x(π / 2)")
      And("B ← scaling(5, 5, 5)")
      And("C ← translation(10, 5, 7)")
      When("T ← C * B * A")
      val T = Builder
        .rotateX(π / 2)
        .scale(5, 5, 5)
        .translate(10, 5, 7)
        .build()
      Then("T * p = point(15, 0, 7)")
      assert(T * p == point(15, 0, 7))
    }

    scenario("The transformation matrix for the default orientation") {
      Given("from ← point(0, 0, 0)")
      val from = point(0, 0, 0)
      And("to ← point(0, 0, -1)")
      val to = point(0, 0, -1)
      And("up ← vector(0, 1, 0)")
      val up = vector(0, 1, 0)
      When("t ← view_transform(from, to, up)")
      val t = viewTransform(from, to, up)
      Then("t = identity_matrix")
      assert(t.matrix == identityMatrix)
    }

    scenario("A view transformation matrix looking in positive z direction") {
      Given("from ← point(0, 0, 0)")
      val from = point(0, 0, 0)
      And("to = point(0, 0, 1)")
      val to = point(0, 0, 1)
      And("up ← vector(0, 1, 0)")
      val up = vector(0, 1, 0)
      When("t ← view_transform(from, to, up)")
      val t = viewTransform(from, to, up)
      Then("t = scaling(-1, 1, -1)")
      assert(t.matrix == scaling(-1, 1, -1).matrix)
    }

    scenario("The view transformation moves the world") {
      Given("from ← point(0, 0, 8)")
      val from = point(0, 0, 8)
      And("to ← point(0, 0, 0)")
      val to = point(0, 0, 0)
      And("up ← vector(0, 1, 0)")
      val up = vector(0, 1, 0)
      When("t ← viewTransform(from, to, up)")
      val t = viewTransform(from, to, up)
      Then("t = translation(0, 0, -8)")
      assert(t.matrix == translation(0, 0, -8).matrix)
    }

    scenario("An arbitrary view transformation") {

      Given("from ← point(1, 3, 2)")
      val from = point(1, 3, 2)
      And("to ← point(4, -2, 8)")
      val to = point(4, -2, 8)
      And("up ← vector(1, 1, 0)")
      val up = vector(1, 1, 0)
      When("t ← viewTransform(from, to, up)")
      val t = viewTransform(from, to, up)
      val m =
        """
          | -0.50709 | 0.50709 |  0.67612 | -2.36643 |
          |  0.76772 | 0.60609 |  0.12122 | -2.82843 |
          | -0.35857 | 0.59761 | -0.71714 |  0.00000 |
          |  0.00000 | 0.00000 |  0.00000 |  1.00000 |
        """.matrix
      Then(s"t is the following 4x4 matrix: $m")
      assert(t.matrix  == m)
    }

  }
}
