package chapters

import raytracer.Scene


object RenderCoverApp extends App {

//  val resource = "/cover.yaml"
//  val resource = "/examples/reflect-refract.yml"
//  val resource = "/examples/refracted-sphere.yaml"  //broken
//  val resource = "/examples/table.yml"
//  val resource = "/examples/cylinders.yml"
//val resource = "/examples/chapter7.yaml"
  val resource = "/examples/group.yaml" //Render time: 531141.445412 ms (1024x768)!!!
//  val resource = "/examples/bounds/reflect-refract-with-bounds.yaml"
  val scene = Scene
    .fromResourceYaml(resource)
    .withDimensions(1024,768)
//    .withDimensions(400,400)
  scene.saveTo(Settings.folder)
}


