package chapters

import raytracer.Scene

object BoundingBoxes extends App {

//  val resource = "/scenes/examples/bounding-boxes.yml"
  val resource = "/scenes/examples/dragon/one-dragon.yml"
  val scene = Scene
    .fromResourceYaml(resource)
//    .withDimensions(2880, 1800)
      .withDimensions(1024, 768)
  scene.saveToAndOpen(Settings.folder)
}
