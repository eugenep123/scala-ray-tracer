package chapters

import raytracer.Scene
import raytracer.math.Point3D

object Chapter15 extends App {


  val resource = "/examples/triangles/teapot.yaml"

  val scene = Scene
    .fromResourceYaml(resource)
    .withDimensions(1024,768)


  val obj = scene.world.objects.head
  val bounds = obj.bounds
  println(bounds)
  println(bounds.center)


  val toCenter = Point3D.origin - bounds.center
  println(toCenter)





  scene.saveToAndOpen(Settings.folder)
}
