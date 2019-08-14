package performance.obj

import raytracer.resource._
import raytracer.resource.waveform.{ObjBuilder, ObjParser}

object TestObjectBuilder extends App {

  //  val filename = "dragon.obj"
    val filename = "teapot.obj"
    val resourceName = s"/objects/$filename"
    val content = getResourceString(resourceName)
  //  val filename = "../assets/teapot/teapot.obj"
  //  val content = readFile(filename)
  //
  //
  //
//    val parser = new ObjParser(new ObjBuilder)
  //  val result = parser.parse(content)
  //  println(result)
  val parser = new ObjParser(ObjBuilder)
  val result = parser.parse(content)

  val mesh = TriangleMesh.build(result)
  println(mesh)

  val group = result.toGroup()
}
