package raytracer.math

import raytracer.BaseSpec

class TupleSpec extends BaseSpec {

  feature("Tuples") {
    scenario("constructing tuples from an array") {
      val m = Matrix(
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 8, 7, 6,
        5, 4, 3, 2
      )
      Given(s"an array $m")
      val t0 = m.row(1)
      val expected = Vector(5, 6, 7, 8)
      Then(s"Tuple(3, a) == $expected")
      assert(t0 == expected)
    }

    scenario("A tuple with w=1.0 is a point") {
      val x = 4.3
      val y = -4.2
      val z = 3.1
      val w = 1.0
      val a = tuple(x, y, z, w)
      Given(s"a ← tuple($x, $y, $z, $w)")
      Then(s"a.x = $x")
      assert(a.x == x)
      Then(s"a.y = $y")
      assert(a.y == y)
      Then(s"a.z = $z")
      assert(a.z == z)
      Then(s"a.w = $w")
      assert(a.w == w)
      Then(s"a is a point")
      assert(a.isPoint)
      Then(s"a is not a vector")
      assert(!a.isVector)
    }

    scenario("A tuple with w=0 is a vector") {
      val x = 4.3
      val y = -4.2
      val z = 3.1
      val w = 0.0
      val a = tuple(x, y, z, w)
      Given(s"a ← tuple($x, $y, $z, $w)")
      Then(s"a.x = $x")
      assert(a.x == x)
      Then(s"a.y = $y")
      assert(a.y == y)
      Then(s"a.z = $z")
      assert(a.z == z)
      Then(s"a.w = $w")
      assert(a.w == w)
      Then(s"a is not a point")
      assert(!a.isPoint)
      Then(s"a is a vector")
      assert(a.isVector)
    }

    // Interop with points and vectors

    scenario("vector() creates tuples with w=0") {
      val v = vector(4, -4, 3)
      val t = tuple(4, -4, 3, 0)
      Given(s"v = $v")
      Then(s"v = $t")
      assert(v == t)
    }

    scenario("point() creates tuples with w=1") {
      val p = point(4, -4, 3)
      val t = tuple(4, -4, 3, 1)
      Given(s"p = $p")
      Then(s"$p = $t")
      assert(p == t)
    }
  }
}
