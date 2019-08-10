package chapters

import raytracer.math._
import raytracer.shapes.Shapes._
import raytracer._
import Transform._


object Chapter11 extends App{

  val width = 400
  val height = 400

  val floor = plane(
    Translation(0, -1, 0),
    Material().setTransparency(0.5)
      .setTransparency(0.5)
      .setRefractiveIndex(1.5)
  )
  val ball = sphere(
    Translation(0, -3.5, -0.5),
    Material().setColor(1, 0, 0).setAmbient(0.5)
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
      ViewTransform(
        Point3D(-2.6, 1.5, -3.9),
        Point3D(-0.6, 1, -0.8),
        Vector3D(0, 1, 0)
      )
    )
  }
  val scene = Scene(w, camera)
  scene.saveTo(Settings.folder)
}
