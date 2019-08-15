package performance

import org.scalameter.measure
import performance.obj.TriangleMesh
import raytracer.Material
import raytracer.math.Transform
import raytracer.resource.waveform.{ObjBuilder, ObjParser}
import raytracer.resource.{ResourceLoader, getResourceString}
import raytracer.shapes.Group

object ObjLoaderTest extends App {


  val transform = Transform().identity.build()
  val material = Material()
  val filename = "dragon.obj"
//  val filename = "teapot.obj"
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
      val group = loader.loadObject(filename, false) // no transforms
      divide(group)
    }
    println(s"Loaded '$filename' in $time1")
  }

  def loadNew() = {
    val time = measure {
      val resourceName = s"/objects/$filename"
      val content = getResourceString(resourceName)
      val parser = new ObjParser(new ObjBuilder)
      val result = parser.parse(content)
      val mesh = TriangleMesh.build(result)
      val bounds = mesh.bounds
      println(bounds)
//      val group = result.toGroup()
//      divide(group)
    }
    println(s"New: loaded '$filename' in $time (threshold = 1)")
  }
//Divided 'dragon.obj' in 79089.937715 ms (threshold = 1174)
  //Loaded 'dragon.obj' in 79972.387794 ms
//  loadOldObject()
  loadNew()
}
