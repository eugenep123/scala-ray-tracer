package performance

import raytracer.Material
import raytracer.math.Transform
import raytracer.resource.ResourceLoader
import raytracer.shapes.Group
import org.scalameter.measure

object ObjLoaderTest extends App {


  val transform = Transform().identity.build()
  val material = Material()
  val filename = "dragon.obj"
  val divideThreshold = 20

  def loadOldObject(): Unit = {
    val time1 = measure {
      println(s"Loading: $filename...")
      val loader = ResourceLoader.default
      val group = loader.loadObject(filename) // no transforms
      val group2 = new Group(transform, material)
      group2.addChildren(group.children)
      //    val threshold = scala.math.max(group2.size / 20, divideThreshold)
      val threshold = 1
      // subdivide bounding boxes
      val time2 = measure {
        group2.divide(threshold)
      }
      println(s"Divided '$filename' in $time2 (threshold = $threshold)")
    }
    println(s"Loaded '$filename' in $time1")
  }


  loadOldObject()

}
