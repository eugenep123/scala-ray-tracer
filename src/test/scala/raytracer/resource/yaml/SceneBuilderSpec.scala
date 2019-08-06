package raytracer.resource.yaml

import org.scalatest.{Matchers, OptionValues, WordSpec}
import raytracer.{Color, Material, TestHelpers}
import raytracer.resource.yaml.AST._

class SceneBuilderSpec extends WordSpec with Matchers with TestHelpers with OptionValues {

  "The scene builder" should {


    "support material building" in {
      val a = MaterialObjectBuilder()
        .color(Color.White)
        .diffuse(0.7)
        .ambient(0.1)
        .specular(0.0)
        .reflective(0.1)
        .obj

      val b = MaterialObject(
        color = Some(Color.White),
        diffuse = Some(0.7),
        ambient = Some(0.1),
        specular = Some(0.0),
        reflective = Some(0.1)
      )

      a shouldEqual b
    }

    "should combine materials" in {
      val a = MaterialObject(
        color = Some(Color.White),
        diffuse = Some(0.7),
        ambient = Some(0.1),
        specular = Some(0.0),
        reflective = Some(0.1)
      )

      val b = MaterialObject()
    }

    "correctly resolve materials" in {


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
      val items = Seq(white, blue)
      val builder = SceneBuilder(items)

      builder.lookup.materials shouldEqual Map(
        "white-material" -> white,
        "blue-material" -> blue
      )

      // Should return all values for the keys in the inheritance chain
      builder.resolveMaterialList("white-material") shouldEqual Seq(white.value)
      builder.resolveMaterialList("blue-material") shouldEqual Seq(white.value, blue.value)


      // All properties should be merged together
      val combined = builder.resolveMaterial("blue-material")
      val expectedObj = MaterialObjectBuilder()
        .color(Color.White)
        .diffuse(0.7)
        .ambient(0.1)
        .specular(0.0)
        .reflective(0.1)
        .color(0.537, 0.831, 0.914).obj

      combined shouldEqual expectedObj


      // Build a actual material from the ref
      val material = builder.buildMaterial("blue-material")
      val expected = Material()
        .setDiffuse(0.7)
        .setAmbient(0.1)
        .setSpecular(0.0)
        .setReflective(0.1)
        .setColor(0.537, 0.831, 0.914)
      material.value shouldEqual expected
    }


  }

}
