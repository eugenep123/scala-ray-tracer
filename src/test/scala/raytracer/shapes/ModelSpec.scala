package raytracer
package shapes

import org.scalatest.{Matchers, WordSpec}
import raytracer.math.{Matrix, TransformBuilder}

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
  }
}
