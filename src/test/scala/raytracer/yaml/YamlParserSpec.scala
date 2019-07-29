package raytracer.yaml

import org.scalatest.{Matchers, WordSpec}
import raytracer.Operation._
import raytracer._
import raytracer.yaml.ast._

class YamlParserSpec extends WordSpec with Matchers with TestHelpers {

  val resourceName = "/cover.yaml"
  "The yaml parser" should {
    "parse add camera" in {
      val yaml =
        """
          |- add: camera
          |  width: 100
          |  height: 100
          |  field-of-view: 0.785
          |  from: [ -6, 6, -10 ]
          |  to: [ 6, 0, 6 ]
          |  up: [ -0.45, 1, 0 ]
          |
          """.stripMargin
      val result = YamlParser.parse(yaml).get
      val camera = result.cameras.head
      camera shouldEqual CameraObject(width = 100,
                                      height = 100,
                                      fieldOfView = 0.785,
                                      from = point(-6, 6, -10),
                                      to = point(6, 0, 6),
                                      up = Vector3D(-0.45, 1, 0))
    }

    "parse all items" in {
      val result = YamlParser.readResource(resourceName)
      println(result)
      val cube = "cube"
      val purple = "purple"
      val large = "large"
      val medium = "medium"
      val small = "small"
      val white = "white"
      val blue = "blue"
      val red = "red"
//      result.get shouldEqual coverScene

    }

    "support patterns" in {
      val result = YamlParser.readResource("/examples/reflect-refract.yml")
      println(result.get)
    }

    "support spheres" in {
      val result = YamlParser.readResource("/examples/refracted-sphere.yaml")
      println(result.get)
    }

    "support cones and groups" in {
      val result = YamlParser.readResource("/examples/group.yaml")
      println(result.get)
      val scene = result.get.build
      println(scene)
    }

//    "parse camera" in {
//      val yaml =
//        """
//          |- add: camera
//          |  width: 400
//          |  height: 400
//          |  field-of-view: 1.0471975512 # pi/3
//          |  from: [ 0, 2.5, 0 ]
//          |  to: [ 0, 0, 0 ]
//          |  up: [ 0, 1, 0 ]
//        """.stripMargin
//      val result = YamlParser.parse(yaml).get
//      println(result)
//      val camera = result.build.camera
//      println(camera)
//      println(camera.transform)
//      println(camera.transform.inverse)
//
//    }

    "parse cylinders" in {
      val yaml =
        """
          |- add: cylinder
          |  min: 0
          |  max: 0.75
          |  closed: true
          |  transform:
          |    - [ scale, 0.5, 1, 0.5 ]
          |    - [ translate, -1, 0, 1 ]
          |  material:
          |    color: [0, 0, 0.6]
          |    diffuse: 0.1
          |    specular: 0.9
          |    shininess: 300
          |    reflective: 0.9
        """.stripMargin

      val result = YamlParser.parse(yaml).get
      println(result)
    }

    "shapes with no transforms should have the identity" in {

      val s = SimpleShapeObject(
        "sphere",
        Some(MaterialValue(None,None,Some(0.1),None,None,Some(300.0),Some(1.0),Some(1.0),Some(1.52),None)),
        TransformList(List())
      )
      val shape = s.build(None, None)(DefineLookup.empty)
      shape.transform shouldEqual identityMatrix
    }
  }

}
