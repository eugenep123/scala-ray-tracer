package raytracer
package shapes

import org.scalatest.prop.TableDrivenPropertyChecks._

class CubeSpec extends BaseSpec {

  feature("Cubes") {
    val intersections =
      Table(
        ("descrirption", "origin", "direction", "t1", "t2"),  // First tuple defines column names
        ( "+x"     , point(5, 0.5, 0)  , vector(-1, 0, 0) ,  4 ,  6 ),
        ( "-x"     , point(-5, 0.5, 0) , vector(1, 0, 0)  ,  4 ,  6 ),
        ( "+y"     , point(0.5, 5, 0)  , vector(0, -1, 0) ,  4 ,  6 ),
        ( "-y"     , point(0.5, -5, 0) , vector(0, 1, 0)  ,  4 ,  6 ),
        ( "+z"     , point(0.5, 0, 5)  , vector(0, 0, -1) ,  4 ,  6 ),
        ( "-z"     , point(0.5, 0, -5) , vector(0, 0, 1)  ,  4 ,  6 ),
        ( "inside" , point(0, 0.5, 0)  , vector(0, 0, 1)  , -1 ,  1 )
      )

    forAll (intersections) {
      (desc: String, origin: Point3D, direction: Vector3D, t1: Int, t2: Int) =>

      scenario(s"A ray intersects a cube: $desc") {
        Given("c ← cube()")
        And(s"r ← ray($origin, $direction)")
        When(" xs ← local_intersect(c, r)")
        Then(" xs.count = 2")
        And(s"xs[0].t = $t1")
        And(s"xs[1].t = $t2")
        val c = cube()
        val r = ray(origin, direction)
        val xs = localIntersect(c, r)
        assert(xs.size == 2)
        assert(xs.head.t ~= t1.toDouble)
        assert(xs(1).t == t2.toDouble)
      }
    }

    val misses = Table(
      ( "origin"         , "direction"                    ),
      ( point(-2, 0, 0)  , vector(0.2673, 0.5345, 0.8018) ),
      ( point(0, -2, 0)  , vector(0.8018, 0.2673, 0.5345) ),
      ( point(0, 0, -2)  , vector(0.5345, 0.8018, 0.2673) ),
      ( point(2, 0, 2)   , vector(0, 0, -1)               ),
      ( point(0, 2, 2)   , vector(0, -1, 0)               ),
      ( point(2, 2, 0)   , vector(-1, 0, 0)               )
    )

    forAll(misses) { (origin: Point3D, direction: Vector3D) =>
      scenario(s"A ray misses a cube: $origin - $direction") {

        Given("c ← cube()")
        And(s"r ← ray($origin, $direction)")
        When("xs ← local_intersect(c, r)")
        Then("xs.count = 0")
        val c = cube()
        val r = ray(origin, direction)
        val xs = localIntersect(c, r)
        assert(xs.isEmpty)
      }
    }

    val normals = Table(
      ( "point"              , "normal"         ),
      ( point(1, 0.5, -0.8)  , vector(1, 0, 0)  ),
      ( point(-1, -0.2, 0.9) , vector(-1, 0, 0) ),
      ( point(-0.4, 1, -0.1) , vector(0, 1, 0)  ),
      ( point(0.3, -1, -0.7) , vector(0, -1, 0) ),
      ( point(-0.6, 0.3, 1)  , vector(0, 0, 1)  ),
      ( point(0.4, 0.4, -1)  , vector(0, 0, -1) ),
      ( point(1, 1, 1)       , vector(1, 0, 0)  ),
      ( point(-1, -1, -1)    , vector(-1, 0, 0) )
    )

    forAll(normals) { (p: Point3D, n: Vector3D) =>

      scenario(s"The normal on the surface of a cube: ($p, $n)") {
        Given("c ← cube()")
        And("p ← <point>")
        When("normal ← local_normal_at(c, p)")
        Then("normal = <normal>")
        val c = cube()
        val normal = localNormalAt(c, p)
        assert(n == normal)
      }
    }

    scenario("A cube has a bounding box") {
      Given("shape ← cube()")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-1, -1, -1)")
      And("box.max = point(1, 1, 1)")

      val box = cube().bounds
      assert(box.minimum == point(-1, -1, -1))
      assert(box.maximum == point(1, 1, 1))
    }


  }
}
/*

 */