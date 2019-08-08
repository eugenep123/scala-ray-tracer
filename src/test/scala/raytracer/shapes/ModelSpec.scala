package raytracer
package shapes

import org.scalatest.{Matchers, WordSpec}
import raytracer.math.{Matrix, TransformBuilder, π}

class ModelSpec extends WordSpec with Matchers with TestHelpers {

  "The model" should {

    "equality checks" should {

      def assertEquals(a: Shape, b: Shape): Unit = {
        a.transform shouldEqual b.transform
        a.material shouldEqual b.material
        a.hashCode shouldEqual b.hashCode
        a shouldEqual b
      }

      "support Matrices" in {
        val m1: Matrix = translation(0, -1, 0).matrix
        val m2: Matrix = TransformBuilder().translate(0, -1, 0).build()
        m1 shouldEqual m2
        m1.hashCode() shouldEqual m2.hashCode()
      }

      "support plans" in {
        val a = plane(translation(0, -1, 0))
        val b = Shape().translate(0, -1, 0).plane
        assertEquals(a, b)
      }

      "support cubes" in {
        val a = cube(translation(0, -1, 0))
        val b = Shape().translate(0, -1, 0).cube
        assertEquals(a, b)
      }
    }

    "create cylinders" in {
      val c = Shape()
        .translate(0, 0, -1)
        .rotateY(-π/6)
        .rotateZ(-π/2)
        .scale(0.25, 1, 0.25)
        .cylinder(0, 1)
      assert(c.minimum == 0)
      assert(c.maximum == 1)
    }

    "create objects correctly" in {
      val lower = plane(
        translation(0, -1, 0),
        Material().setReflective(1)
      )
      val upper = plane(
        translation(0, 1, 0),
        Material().setReflective(1)
      )
      val light = pointLight(point(0, 0, 0), color(1, 1, 1))
      val w = World(Seq(lower, upper), Seq(light))


      val w2 = World.empty
        .setLight(pointLight(point(0, 0, 0), color(1, 1, 1)))
        .add(
          Shape().setReflective(1).translate(0, -1, 0).plane
        )
        .add(
          Shape().translate(0, 1, 0).setReflective(1).plane
        )

      w2 shouldEqual w
    }

    "test translations" in {
      val s1 = Shape()
        .scale(2, 2, 2)
        .translate(2, 5, -3)
        .sphere

      val s2 = Sphere(translation(2, 5, -3) * scaling(2, 2, 2))

      assert(s1.transform == s2.transform)
      assert(s1 == s2)
    }

    "cubes with no shadows" in {
      val c = Shape().cube(false)

      c.castsShadow shouldEqual false
    }
  }
}
