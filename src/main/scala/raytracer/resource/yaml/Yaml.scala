package raytracer.resource
package yaml
import java.util

import org.yaml.snakeyaml.Yaml
import raytracer.Scene
import raytracer.resource.yaml.AST.YamlValue
import raytracer.resource.yaml.reader.YamlReader.YamlValueReader
import raytracer.resource.yaml.reader._

import scala.collection.JavaConverters._

object Yaml {

  import ParseResult._

  private def readMaps(yamlStr: String): Seq[YamlMap] = {
    val yaml = new Yaml
    val all = yaml.loadAll(yamlStr)
    all.asScala.flatMap { x =>
      val al = x.asInstanceOf[java.util.ArrayList[util.LinkedHashMap[String, Any]]]
      al.asScala.map(_.asScala.toMap)
    }.toList
  }

  def parse(yamlString: String): ParseResult[SceneBuilder] =
    parseItems(yamlString).map(SceneBuilder.apply)

  def parseItems(yamlString: String): ParseResult[Seq[YamlValue]] = {
    for {
      maps <- wrap(readMaps(yamlString)).withMessage("Failed to parse yaml string")
      items <- sequence[YamlMap, AST.YamlValue](maps, YamlValueReader.readMap)
    } yield items
  }

  def parseResource(name: String): ParseResult[SceneBuilder] = {
    parse(getResourceString(name))
  }

  def read(yaml: String): Scene = {
    parse(yaml).get.build
  }

  def readFromResource(name: String): Scene = {
    parseResource(name).get.build
  }
}