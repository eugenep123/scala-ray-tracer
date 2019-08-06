package raytracer.resource.yaml

import org.scalatest.{Matchers, WordSpec}
import raytracer.math.{Operation, Point3D, Vector3D}
import raytracer.{Color, TestHelpers}

class YamlSpec extends WordSpec with Matchers with TestHelpers {
  import AST._
  import Yaml.parseItems
  val resourceName = "/cover.yaml"

  import Operation._

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
      val expected = AddCamera(100,
                               100,
                               0.785,
                               Point3D(-6, 6, -10),
                               Point3D(6, 0, 6),
                               Vector3D(-0.45, 1, 0))
      parseItems(yaml).get shouldEqual List(expected)
    }

    "read AddLight" in {
      val yaml = """- add: light
                   |  at: [ -10, 10, -10 ]
                   |  intensity: [ 1, 1, 1 ]""".stripMargin
      val expected = AddLight(Point3D(-10, 10, -10), Color.White)
      parseItems(yaml).get shouldEqual List(expected)
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
      val expected = ShapeReference(
        "wacky",
        Some(TransformList(List(TransformOperation(RotationY(0.1745))))),
        Some(MaterialObject(Some(Color(0.2, 0.9, 0.6)),
               Some(0.8), Some(0.2), Some(0.7), Some(20),
               None, None, None, None))
      )
      parseItems(yaml).get shouldEqual List(expected)
    }

    "ready transform refs" in {
      val yaml = """- add: cube
                   |  material: blue-material
                   |  transform:
                   |    - large-object
                   |    - [ translate, 8.5, 1.5, -0.5 ]""".stripMargin
      val transform = TransformList(
        Seq(TransformReference("large-object"),
            TransformOperation(Translation(8.5, 1.5, -0.5))))
      val expected =
        AddCube(Some(transform), Some(MaterialReference("blue-material")))
      parseItems(yaml).get shouldEqual List(expected)
    }

    "read sphere" in {
      val yaml = """- add: sphere
                   |  material:
                   |    color: [1, 0.25, 0.25]
                   |    ambient: 0
                   |    specular: 0
                   |    diffuse: 0.5
                   |    reflective: 0.5""".stripMargin
      val material = MaterialObject(Some(Color(1, 0.25, 0.25)),
                                    Some(0.5),
                                    Some(0),
                                    Some(0),
                                    None,
                                    Some(0.5),
                                    None,
                                    None,
                                    None)
      val expected = AddSphere(None, Some(material))
      parseItems(yaml).get shouldEqual List(expected)
    }

    "read define material" in {
      val yaml = """- define: blue-material
                   |  extend: white-material
                   |  value:
                   |    color: [ 0.537, 0.831, 0.914 ]
                   |""".stripMargin
      val expected =
        DefineMaterial("blue-material",
                       Some("white-material"),
                       MaterialObject(color = Some(Color(0.537, 0.831, 0.914))))
      parseItems(yaml).get shouldEqual List(expected)
    }

    "read define transform" in {
      val yaml = """- define: large-object
                   |  value:
                   |    - standard-transform
                   |    - [ scale, 3.5, 3.5, 3.5 ]""".stripMargin
      val expected =
        DefineTransform("large-object",
          TransformList(Seq(
            TransformReference("standard-transform"),
            TransformOperation(Scaling(3.5, 3.5, 3.5)))))
      parseItems(yaml).get shouldEqual List(expected)
    }

    "read shape defines" in {
      val yaml = """- define: leg
                   |  value:
                   |    add: group
                   |    children:
                   |      - add: sphere
                   |        transform:
                   |          - [ scale, 0.25, 0.25, 0.25 ]
                   |          - [ translate, 0, 0, -1 ]
                   |      - add: cylinder
                   |        min: 0
                   |        max: 1
                   |        closed: false
                   |        transform:
                   |          - [ scale, 0.25, 1, 0.25 ]
                   |          - [ rotate-z, -1.5708 ]
                   |          - [ rotate-y, -0.5236 ]
                   |          - [ translate, 0, 0, -1 ]
                   |""".stripMargin

      val sphere = AddSphere(
        Some(TransformList(Seq(
          TransformOperation(Scaling(0.25, 0.25, 0.25)),
          TransformOperation(Translation(0, 0, -1))))), None)

      val cylinder = AddCylinder(0, 1, false,
        Some(TransformList(Seq(
          Scaling(0.25, 1, 0.25),
          RotationZ(-1.5708),
          RotationY(-0.5236),
          Translation(0, 0, -1)
        ).map(TransformOperation.apply))), None)
      val group = AddGroup(Seq(sphere, cylinder), None, None)
      val expected = DefineShape("leg", group)
      parseItems(yaml).get shouldEqual List(expected)
    }

    "resolve material correctly" in {
      val yaml = """- define: white-material
                   |  value:
                   |    color: [ 1, 1, 1 ]
                   |    diffuse: 0.7
                   |    ambient: 0.1
                   |    specular: 0.0
                   |    reflective: 0.1
                   |
                   |- define: blue-material
                   |  extend: white-material
                   |  value:
                   |    color: [ 0.537, 0.831, 0.914 ]""".stripMargin

      val white = DefineMaterial("white-material", None,
        MaterialObjectBuilder()
          .color(Color.White)
          .diffuse(0.7)
          .ambient(0.1)
          .specular(0.0)
          .reflective(0.1))
      val blue = DefineMaterial("blue-material", Some("white-material"),
        MaterialObjectBuilder().color(0.537, 0.831, 0.914)
      )
      val expected = Seq(white, blue)
      parseItems(yaml).get shouldEqual expected

    }
  }
}


