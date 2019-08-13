package raytracer.resource
package yaml
import java.util

import org.yaml.snakeyaml.Yaml
import raytracer.Scene
import raytracer.resource.yaml.AST.YamlValue
import raytracer.resource.yaml.reader.AstReaders.YamlValueReader
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

  def parse(yamlString: String)(implicit loader: ResourceLoader): ParseResult[SceneBuilder] =
    parseItems(yamlString).map(SceneBuilder.apply(_))

  def parseItems(yamlString: String): ParseResult[Seq[YamlValue]] = {
    for {
      maps <- wrap(readMaps(yamlString)).withMessage("Failed to parse yaml string")
      items <- sequence[YamlMap, AST.YamlValue](maps, YamlValueReader.readMap)
    } yield items
  }

  def parseResource(name: String)(implicit loader: ResourceLoader): ParseResult[SceneBuilder] = {
    parse(getResourceString(name))
  }

  def read(yaml: String)(implicit loader: ResourceLoader): Scene = {
    parse(yaml).get.build
  }

  def readFromResource(name: String)(implicit loader: ResourceLoader): Scene = {
    parseResource(name).get.build
  }
}