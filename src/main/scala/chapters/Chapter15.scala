package chapters

import raytracer.Scene
import raytracer.math.Point3D
import raytracer.shapes._

object Chapter15 extends App {


  val resource = "/scenes/examples/triangles/teapot.yml"

  val scene = Scene
    .fromResourceYaml(resource)
    .withDimensions(1024,768)


  val obj = scene.world.objects
    .collect { case g: Group => g }
    .head
  val bounds = obj.bounds
  println(bounds)
  println(bounds.center)


  val toCenter = Point3D.origin - bounds.center
  println(toCenter)





  scene.saveToAndOpen(Settings.folder)
}
