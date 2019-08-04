package chapters

import raytracer.Scene

object Chapter15 extends App {


  val resource = "/examples/triangles/teapot.yaml"

  val scene = Scene
    .fromResourceYaml(resource)
    .withDimensions(400,400)


  scene.saveTo(Settings.folder)
}
