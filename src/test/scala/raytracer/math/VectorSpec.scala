package raytracer.math

import java.lang.Math.sqrt

import raytracer.BaseSpec

class VectorSpec extends BaseSpec {

  feature("Vectors") {

    scenario("Adding two vectors") {
      Given("a1 ←vector(3, -2, 5)")
      And("a2 ← vector(-2, 3, 1)")
      Then("a1 + a2 = vector(1, 1, 6)")
      val a1 = vector(3, -2, 5)
      val a2 = vector(-2, 3, 1)
      val expected = vector(1, 1, 6)
      assert((a1 + a2) == expected)
    }

    scenario("Subtracting two vectors") {
      Given("v1← vector(3, 2, 1)")
      And("v2 ← vector(5, 6, 7)")
      Then("v1 - v2 =  vector(-2, -4, -6)")
      val v1 = vector(3, 2, 1)
      val v2 = vector(5, 6, 7)
      assert((v1 - v2) == vector(-2, -4, -6))
    }

    scenario("Subtracting a vector from the zero vector") {
      Given("zero ← vector(0, 0, 0)")
      And("v ← vector(1, -2, 3)")
      Then("zero - v = vector(-1, 2, -3)")
      val zero = vector(0, 0, 0)
      val v = vector(1, -2, 3)
      assert((zero - v) == vector(-1, 2, -3))
    }

    scenario("Multiplying a tuple by a scalar") {
      Given("a ←vector(1, -2, 3)")
      Then("a * 3.5 = vector(3.5, -7, 10.5)")
      val a = vector(1, -2, 3)
      assert(a * 3.5 == vector(3.5, -7, 10.5))
    }

    scenario("Multiplying a tuple by a fraction") {
      Given("a ← vector(1, -2, 3")
      Then("a * 0.5 =  vector(0.5, -1, 1.5)")
      val a = vector(1, -2, 3)
      assert(a * 0.5 ==  vector(0.5, -1, 1.5))
    }

    scenario("Dividing a tuple by a scalar") {
      Given("a ← vector(1, -2, 3)")
      Then("a / 2 = vector(0.5, -1, 1.5)")
      val a = vector(1, -2, 3)
      assert(a / 2 == vector(0.5, -1, 1.5))
    }

    scenario("Computing the magnitude of vector(1, 0, 0)") {
      Given("v ← vector(1, 0, 0)")
      Then("magnitude(v) = 1")
      val v = vector(1, 0, 0)
      assert(v.magnitude ~= 1)
    }

    scenario("Computing the magnitude of vector(0, 1, 0)") {
      Given("v ← vector(0, 1, 0)")
      Then("v.magnitude = 1")
      val v = vector(0, 1, 0)
      assert(v.magnitude ~= 1)
    }

    scenario("Computing the magnitude of vector(0, 0, 1)") {
      Given("v ← vector(0, 0, 1)")
      Then("v.magnitude = 1")
      val v = vector(0, 0, 1)
      assert(v.magnitude ~= 1)
    }

    scenario("Computing the magnitude of vector(1, 2, 3)") {
      Given("v ← vector(1, 2, 3)")
      Then("v.magnitude = √14")
      val v = vector(1, 2, 3)
      assert(v.magnitude ~= sqrt(14))
    }

    scenario("Computing the magnitude of vector(-1, -2, -3)") {
      Given("v ← vector(-1, -2, -3)")
      Then("v.magnitude = √14")
      val v = vector(-1, -2, -3)
      assert(v.magnitude ~= sqrt(14))
    }

    scenario("Normalizing vector(4, 0, 0) gives (1, 0, 0)") {
      Given("v ← vector(4, 0, 0)")
      Then("normalize(v) = vector(1, 0, 0)")
      val v = vector(4, 0, 0)
      assert(v.normalize == vector(1, 0, 0))
    }

    scenario("Normalizing vector(1, 2, 3)") {
      Given("v ← vector(1, 2, 3)")
      Then("normalize(v) = vector(1/√14,   2/√14,   3/√14)")
      val v = vector(1, 2, 3)
      val expected = vector(0.26726, 0.53452, 0.80178)
      assert(v.normalize == expected)
    }

    scenario("The magnitude of a normalized vector") {
      Given("v ← vector(1, 2, 3)")
      When("norm ← normalize(v)")
      Then("magnitude(norm) = 1")
      val v = vector(1, 2, 3)
      assert(v.normalize.magnitude == 1.0)
    }

    scenario("The dot product of two tuples") {
      Given("a ← vector(1, 2, 3)")
      Given("b ← vector(2, 3, 4)")
      Then("dot(a, b) = 20")
      val a = vector(1, 2, 3)
      val b = vector(2, 3, 4)
      assert(a.dot(b) == 20)
    }

    scenario("The cross product of two vectors") {
      Given("a ← vector(1, 2, 3)")
      Given("b ← vector(2, 3, 4)")
      Then("cross(a, b) = vector(-1, 2, -1)")
      Then("cross(b, a) = vector(1, -2, 1)")
      val a = vector(1, 2, 3)
      val b = vector(2, 3, 4)
      assert(a.cross(b) == vector(-1, 2, -1))
      assert(b.cross(a) == vector(1, -2, 1))
    }

    scenario("Reflecting a vector approaching at 45°") {
      Given("v ← vector(1, -1, 0)")
      Given("n ← vector(0, 1, 0)")
      When("r ← reflect(v, n)")
      Then("r = vector(1, 1, 0)")
      val v = vector(1, -1, 0)
      val n = vector(0, 1, 0)
      val r = v.reflect(n)
      assert(r == vector(1, 1, 0))
    }

    scenario("Reflecting a vector off a slanted surface°") {
      Given("v ← vector(0, -1, 0)")
      And("n ← vector(√2/2, √2/2, 0)")
      When("r ← reflect(v, n)")
      Then("r = vector(1, 0, 0)")
      val v = vector(0, -1, 0)
      val n = vector(sqrt(2) / 2, sqrt(2) / 2, 0)
      val r = v.reflect(n)
      assert(r == vector(1, 0, 0))
    }

  }
}
