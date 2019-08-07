package ui.threaded

import raytracer.Scene


object ThreadWatch extends App {


//  val model = new Model()
//  val resource = "/cover.yaml"
  val resource = "/scenes/examples/group.yaml"
  val scene = Scene
    .fromResourceYaml(resource)
    .withDimensions(1024,768)

  val model = new SceneModel(scene)

  Control.start(model)
  new Thread(model).start()

}


