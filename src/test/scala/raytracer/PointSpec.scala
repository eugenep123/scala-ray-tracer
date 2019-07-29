package raytracer

class PointSpec extends BaseSpec {

  feature("points") {
    scenario("point() creates a point") {
      val x = 4.3
      val y = -4.2
      val z = 3.1
      val a = point(x, y, z)
      Given(s"p ← point($x, $y, $z)")
      Then(s"p.x = $x")
      assert(a.x == x)
      Then(s"p.y = $y")
      assert(a.y == y)
      Then(s"p.z = $z")
    }

    scenario("Subtracting two points") {
      val p1 = point(3, 2, 1)
      val p2 = point(5, 6, 7)
      val expected = vector(-2, -4, -6)
      Given(s"p1 ← $p1")
      And(s"p2 ← $p2")
      Then(s"p1 - p2 = $expected")
      assert((p1 - p2) == expected)
    }

    scenario("Adding a vector to a point") {
      val p = point(3,-2,5)
      val v = vector(-2,3,1)
      Given(s"p ← $p")
      And(s"v ← $v")
      val expected = point(1, 1, 6)
      Then(s"p + v = $expected")
      assert((p + v) == expected)
    }

    scenario("Subtracting a vector from a point") {
      val p = point(3, 2, 1)
      val v = vector(5, 6, 7)
      val expected = point(-2, -4, -6)
      Given(s"p ← $p")
      And(s"v ← $v")
      Then(s"p - v = $expected")
      assert((p - v) == expected)
    }

    scenario("Negating a point") {
      val a = point(1, -2, 3)
      val expected = point(-1, 2, -3)
      Given(s"a ← $a")
      Then(s"-a = $expected")
      assert(-a == expected)
    }
  }
}


