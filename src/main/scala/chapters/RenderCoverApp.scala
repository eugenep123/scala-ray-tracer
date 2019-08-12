package chapters

import raytracer.Scene


object RenderCoverApp extends App {

  val resource = "/cover.yml"
//  val resource = "/scenes/examples/reflect-refract.yml"
//  val resource = "/scenes/examples/refracted-sphere.yml"  //broken
//  val resource = "/scenes/examples/table.yml"
//  val resource = "/scenes/examples/cylinders.yml"
//val resource = "/scenes/examples/chapter7.yml"
//  val resource = "/scenes/examples/group.yml" //Render time: 531141.445412 ms (1024x768)!!!
//  val resource = "/scenes/examples/bounds/reflect-refract-with-bounds.yml"
//  val resource = "/scenes/examples/metal.yml"  //broken

  val scene = Scene
    .fromResourceYaml(resource)
    .withDimensions(1024,768)
//    .withDimensions(2000,2000)
  scene.saveToAndOpen(Settings.folder)
}


