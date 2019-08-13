package chapters

import raytracer.Scene

object TextureMappingApp extends App {

  val resources = Seq(
//    "/scenes/texture-mapping/align-check-plane.yml",
//    "/scenes/texture-mapping/checkered-cube.yml",
//    "/scenes/texture-mapping/checkered-cylinder.yml",
//    "/scenes/texture-mapping/checkered-plane-map.yml",
//    "/scenes/texture-mapping/checkered-sphere.yml",
    "/scenes/texture-mapping/earth.yml",
//    "/scenes/texture-mapping/orrery.yml",
//    "/scenes/texture-mapping/skybox.yml"
  )

  resources foreach { resource =>
    println(s"Reading: $resource")
    val scene = Scene
      .fromResourceYaml(resource)
      .withDimensions(1024,768)
    scene.saveToAndOpen(Settings.folder)
  }
}
