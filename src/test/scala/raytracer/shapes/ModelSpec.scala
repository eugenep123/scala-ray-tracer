package raytracer
package shapes

import org.scalatest.{Matchers, WordSpec}
import raytracer.math.{Matrix, TransformBuilder}

class ModelSpec extends WordSpec with Matchers with TestHelpers {

  "The model" should {

    "2 matrices should be equal and have the same hash code" in {
      val m1: Matrix = translation(0, -1, 0).matrix
      val m2: Matrix = TransformBuilder().translate(0, -1, 0).build()
      m1 shouldEqual m2
      m1.hashCode() shouldEqual m2.hashCode()

    }
    "any 2 object should be equal if all their ars match (except parent)" in {

      val p1 = plane(translation(0, -1, 0))
      val p2 = Shape().translate(0, -1, 0).plane

      p1.transform shouldEqual p2.transform
      p1.material shouldEqual p2.material
      p1.hashCode shouldEqual p2.hashCode
      p1 shouldEqual p2
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
