package raytracer.math

import java.lang.Math.sqrt

import raytracer.BaseSpec

class VectorSpec extends BaseSpec {

  feature("Vectors") {

    scenario("Adding two vectors") {
      val a1 = vector(3, -2, 5)
      val a2 = vector(-2, 3, 1)
      val expected = vector(1, 1, 6)
      Given(s"a1 ← $a1")
      And(s"a2 ← $a2")
      Then(s"a1 + a2 = $expected")
      assert((a1 + a2) == expected)
    }

    scenario("Subtracting two vectors") {
      val v1 = vector(3, 2, 1)
      val v2 = vector(5, 6, 7)
      val expected = vector(-2, -4, -6)
      Given(s"v1← $v1")
      And(s"v2 ← $v2")
      Then(s"v1 - v2 = $expected")
      assert((v1 - v2) == expected)
    }

    scenario("Subtracting a vector from the zero vector") {
      val zero = vector(0, 0, 0)
      val v = vector(1, -2, 3)
      val expected = vector(-1, 2, -3)
      Given(s"zero ← $zero")
      And(s"v ← $v")
      Then(s"zero - v = $expected")
      assert((zero - v) == expected)
    }

    scenario("Multiplying a tuple by a scalar") {
      val a = vector(1, -2, 3)
      val expected = vector(3.5, -7, 10.5)
      Given(s"a ← $a")
      Then(s"a * 3.5 = $expected")
      assert(a * 3.5 == expected)
    }

    scenario("Multiplying a tuple by a fraction") {
      val a = vector(1, -2, 3)
      val expected = vector(0.5, -1, 1.5)
      Given(s"a ← $a")
      Then(s"a * 0.5 = $expected")
      assert(a * 0.5 == expected)
    }

    scenario("Dividing a tuple by a scalar") {
      val a = vector(1, -2, 3)
      val expected = vector(0.5, -1, 1.5)
      Given(s"a ← $a")
      Then(s"a / 2 = $expected")
      assert(a / 2 == expected)
    }

    scenario("Computing the magnitude of vector(1, 0, 0)") {
      val v = vector(1, 0, 0)
      Given(s"v ← $v")
      Then(s"magnitude(v) = 1")
      assert(magnitude(v) ~= 1)
    }

    scenario("Computing the magnitude of vector(0, 1, 0)") {
      val v = vector(0, 1, 0)
      Given(s"v ← $v")
      Then(s"magnitude(v) = 1")
      assert(magnitude(v) ~= 1)
    }

    scenario("Computing the magnitude of vector(0, 0, 1)") {
      val v = vector(0, 0, 1)
      Given(s"v ← $v")
      Then(s"magnitude(v) = 1")
      assert(magnitude(v) ~= 1)
    }

    scenario("Computing the magnitude of vector(1, 2, 3)") {
      val v = vector(1, 2, 3)
      Given(s"v ← $v")
      Then(s"magnitude(v) = √14")
      assert(magnitude(v) ~= sqrt(14))
    }

    scenario("Computing the magnitude of vector(-1, -2, -3)") {
      val v = vector(-1, -2, -3)
      Given(s"v ← $v")
      Then(s"magnitude(v) = √14")
      assert(magnitude(v) ~= sqrt(14))
    }

    scenario("Normalizing vector(4, 0, 0) gives (1, 0, 0)") {
      val v = vector(4, 0, 0)
      val expected = vector(1, 0, 0)
      Given(s"v ← $v")
      Then(s"normalize(v) = $expected")
      assert(normalize(v) == expected)
    }

    scenario("Normalizing vector(1, 2, 3)") {
      val v = vector(1, 2, 3)
      //vector(1/√14,   2/√14,   3/√14)
      val expected = vector(0.26726, 0.53452, 0.80178)
      Given(s"v ← $v")
      Then(s"normalize(v) = $expected")
      assert(normalize(v) == expected)
    }

    scenario("The magnitude of a normalized vector") {
      val v = vector(1, 2, 3)
      Given(s"v ← $v")
      When("norm ← normalize(v)")
      Then(s"magnitude(norm) = 1")
      assert(v.normalize.magnitude == 1.0)
    }

    scenario("The dot product of two tuples") {
      val a = vector(1, 2, 3)
      val b = vector(2, 3, 4)
      Given(s"a ← $a")
      Given(s"b ← $b")
      Then(s"dot(a, b) = 20")
      assert(dot(a, b) == 20)
    }

    scenario("The cross product of two vectors") {
      val a = vector(1, 2, 3)
      val b = vector(2, 3, 4)
      Given(s"a ← $a")
      Given(s"b ← $b")
      val ab = vector(-1, 2, -1)
      val ba = vector(1, -2, 1)
      Then(s"cross(a, b) = $ab")
      assert(cross(a, b) == ab)
      Then(s"cross(b, a) = $ba")
      assert(cross(b, a) == ba)
    }

    scenario("Reflecting a vector approaching at 45°") {
      val v = vector(1, -1, 0)
      val n = vector(0, 1, 0)
      Given(s"v ← $v")
      Given(s"n ← $n")
      When("r ← reflect(v, n)")
      val r = reflect(v, n)
      val expected = vector(1, 1, 0)
      Then(s"r = $expected")
      assert(r == expected)
    }

    scenario("Reflecting a vector off a slanted surface°") {
      val v = vector(0, -1, 0)
      val n = vector(sqrt(2) / 2, sqrt(2) / 2, 0)
      Given(s"v ← $v")
      And(s"n ← vector(√2/2, √2/2, 0)")
      When("r ← reflect(v, n)")
      val r = reflect(v, n)
      val expected = vector(1, 0, 0)
      Then(s"r = $expected")
      assert(r == expected)
    }

  }
}
