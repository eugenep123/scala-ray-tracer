package chapters

import raytracer.Operation.{Translation, ViewTransform}
import raytracer._
import raytracer.Operations._
import Shapes._

object Chapter11 extends App{

  val width = 400
  val height = 400

  val floor = plane()
    .setTransform(Translation(0, -1, 0))
    .updateMaterial(
      _.setTransparency(0.5)
        .setTransparency(0.5)
        .setRefractiveIndex(1.5)
    )
  val ball = sphere()
    .setTransform(Translation(0, -3.5, -0.5))
    .updateMaterial(
      _.setColor(1, 0, 0)
        .setAmbient(0.5)
    )
  val w = Defaults.world.add(floor).add(ball)
//  val w = Defaults.world.copy(objects = Seq(floor, ball))

  val camera = {
    //    new Camera(
    //    400,
    //    400,
    //    π / 3,
    //    viewTransform(
    //      from = Point3D(0, 0, -3),
    //      to = Point3D(0, 0, 0),
    //      up = Vector3D(0, 1, 0))
    //  )
    ////  val camera = Camera(400, 200, 0.785,
    ////    ViewTransform(
    ////      Point3D(0, 0, -3),
    ////      Point3D(0, 0, 0),
    ////      Vector3D(0, 1, 0)
    ////    ).matrix
    ////  )
    ////  val camera = Defaults.camera
    new Camera(
      width,
      height,
      PI / 3,
      viewTransform(
        Point3D(-2.6, 1.5, -3.9),
        Point3D(-0.6, 1, -0.8),
        Vector3D(0, 1, 0)
      )
    )
  }
  val scene = Scene(w, camera)
  scene.saveTo(Settings.folder)
}
