package raytracer

import org.scalatest.{Matchers, WordSpec}

class ModelSpec extends WordSpec with Matchers with TestHelpers {

  "The model" should {

    "create objects correctly" in {
      val lower = plane()
        .updateMaterial(_.setReflective(1))
        .setTransform(translation(0, -1, 0))
      val upper = plane()
        .updateMaterial(_.setReflective(1))
        .setTransform(translation(0, 1, 0))
      val light = pointLight(point(0, 0, 0), color(1, 1, 1))
      val w = World(Seq(lower, upper), Seq(light))


      val w2 = World.empty
        .setLight(pointLight(point(0, 0, 0), color(1, 1, 1)))
        .add(plane()
          .updateMaterial(_.setReflective(1))
          .setTransform(translation(0, -1, 0))
        )
        .add(plane()
          .updateMaterial(_.setReflective(1))
          .setTransform(translation(0, 1, 0))
        )

      w2 shouldEqual w
    }
  }
}
