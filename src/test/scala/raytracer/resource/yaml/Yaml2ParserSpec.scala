package raytracer.resource.yaml

import org.scalatest.{Matchers, WordSpec}
import raytracer.{Color, TestHelpers}
import raytracer.math.{Operation, Point3D, Vector3D}


class Yaml2ParserSpec extends WordSpec with Matchers with TestHelpers {
  import AST._
  import YamlParser.parse2
  val resourceName = "/cover.yaml"

  "The Yaml reader" should {

    "read AddCamera" in {
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
      val expected = AddCamera(100, 100, 0.785, Point3D(-6, 6, -10), Point3D(6, 0, 6), Vector3D(-0.45, 1, 0))
      parse2(yaml).get shouldEqual List(expected)
    }

    "read AddLight" in {
      val yaml = """- add: light
                   |  at: [ -10, 10, -10 ]
                   |  intensity: [ 1, 1, 1 ]""".stripMargin
      val expected = AddLight(Point3D(-10, 10, -10), Color.White)
      parse2(yaml).get shouldEqual List(expected)
    }

    "read shape ref" in {
      val yaml = """- add: wacky
                   |  transform:
                   |    - [ rotate-y, 0.1745 ]
                   |  material:
                   |    color: [ 0.2, 0.9, 0.6 ]
                   |    ambient: 0.2
                   |    diffuse: 0.8
                   |    specular: 0.7
                   |    shininess: 20""".stripMargin
      val expected = ShapeReference("wacky",
        TransformValue(List(TransformOperation(Operation.RotationY(0.1745)))),
        Some(MaterialObject(
          Some(Color(0.2, 0.9, 0.6)), Some(0.8), Some(0.2), Some(0.7),
          Some(20), None, None, None, None)))
      parse2(yaml).get shouldEqual List(expected)
    }

    "read sphere" in {
      val yaml = """- add: sphere
                   |  material:
                   |    color: [1, 0.25, 0.25]
                   |    ambient: 0
                   |    specular: 0
                   |    diffuse: 0.5
                   |    reflective: 0.5""".stripMargin
      val material = MaterialObject(
        Some(Color(1, 0.25, 0.25)), Some(0.5), Some(0), Some(0),
        None, Some(0.5), None, None, None)
      val expected = SphereValue(TransformValue(Nil), Some(material))
      parse2(yaml).get shouldEqual List(expected)
    }

    "read define material" in {
      val yaml = """- define: blue-material
                   |  extend: white-material
                   |  value:
                   |    color: [ 0.537, 0.831, 0.914 ]
                   |""".stripMargin
      val expected = DefineMaterial("blue-material", Some("white-material"),
        MaterialObject(color = Some(Color(0.537, 0.831, 0.914))))
      parse2(yaml).get shouldEqual List(expected)
    }
  }
}
