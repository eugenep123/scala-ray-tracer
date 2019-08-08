package chapters

import raytracer.Scene

object BoundingBoxes extends App {

  val resource = "/scenes/examples/bounding-boxes.yml"
//  val resource = "/scenes/examples/dragon/one-dragon.yaml"
  val scene = Scene
    .fromResourceYaml(resource)
    .withDimensions(1024,768)
  //    .withDimensions(2000,2000)
  scene.saveToAndOpen(Settings.folder)
}
