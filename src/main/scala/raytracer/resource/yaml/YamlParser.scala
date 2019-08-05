package raytracer.resource
package yaml
import java.util

import org.yaml.snakeyaml.Yaml

import scala.collection.JavaConverters._

object YamlParser {

  import ParseResult._
  import ValueReaders._
  import ast._

  def readResource(name: String): ParseResult[SceneAst] = {
    parse(getResourceString(name))
  }

  def parse(yamlString: String): ParseResult[SceneAst] = {
    for {
      maps <- wrap(parseYamlMaps(yamlString)).withMessage("Failed to parse yaml string")
      items <- sequence[YamlMap, YamlItem](maps, YamlItemReader.readMap)
    } yield SceneAst(items)
  }

  def parseYamlMaps(yamlStr: String): Seq[YamlMap] = {
    val yaml = new Yaml
    val all = yaml.loadAll(yamlStr)
    all.asScala.flatMap { x =>
      val al = x.asInstanceOf[java.util.ArrayList[util.LinkedHashMap[String, Any]]]
      al.asScala.map(_.asScala.toMap)
    }.toList
  }


}