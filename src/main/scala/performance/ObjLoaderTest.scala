package performance

import raytracer.Material
import raytracer.math.Transform
import raytracer.resource.{ResourceLoader, getResourceString}
import raytracer.shapes.Group
import org.scalameter.measure
import performance.obj.{ObjBuilder, ObjParser, TriangleMesh}

object ObjLoaderTest extends App {


  val transform = Transform().identity.build()
  val material = Material()
  val filename = "dragon.obj"
  val divideThreshold = 20

  def divide(g: Group) = {
    val group2 = new Group(transform, material)
    group2.addChildren(g.children)
    val threshold = scala.math.max(group2.size / 20, divideThreshold)
//    val threshold = 1
    // subdivide bounding boxes
    val time2 = measure {
      group2.divide(threshold)
      println("done..")
    }
    println(s"Divided '$filename' in $time2 (threshold = $threshold)")
  }

  def loadOldObject(): Unit = {
    val time1 = measure {
      println(s"Loading: $filename...")
      val loader = ResourceLoader.default
      val group = loader.loadObject(filename) // no transforms
      divide(group)
    }
    println(s"Loaded '$filename' in $time1")
  }

  def loadNew() = {
    val time = measure {
      val resourceName = s"/objects/$filename"
      val content = getResourceString(resourceName)
      val parser = new ObjParser(ObjBuilder)
      val result = parser.parse(content)
      val group = result.toGroup()
      divide(group)
    }
    println(s"New: loaded '$filename' in $time (threshold = 1)")
  }

  loadOldObject()
  loadNew()
}