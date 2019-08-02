package raytracer
package files
import shapes._

// https://github.com/seanrowens/oObjLoader
object ObjFileParser {

  case class Result(
    vertices: Array[Point3D]) {


    def groups: Seq[Group] = ???
    def defaultGroup: Group = ???

    def toGroup: Group = ???
    def normals: Seq[Vector3D] = ???
  }

  def parse(content: String): Result = ???
}


