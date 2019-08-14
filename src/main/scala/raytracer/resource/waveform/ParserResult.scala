package raytracer.resource.waveform

import raytracer.math.{Point3D, Vector3D}
import raytracer.shapes.{Group, Shape, Triangle, TriangleData}

import scala.collection.mutable.ArrayBuffer

class ParserResult extends ObjBuilder {
  private val _vertices = ArrayBuffer.empty[Point3D]
  private val _normals = ArrayBuffer.empty[Vector3D]
  private var _currentGroup = Option.empty[Group]
  private val _groups = ArrayBuffer.empty[Group]

  //Note: it is significant that the array is 1-based,
  def vertices(index: Int): Point3D = {
    require(index <= _vertices.size, s"Vertex $index out or range, should be [1-${_vertices.size}]")
    _vertices(index - 1)
  }
  //Note: it is significant that the array is 1-based,
  def normals(index: Int): Vector3D = {
    require(index <= _normals.size, s"Vertex $index out or range, should be [1-${_normals.size}]")
    _normals(index - 1)
  }

  def groups: Seq[Group] = _groups.toVector
  def defaultGroup: Group = _groups.head

  def toGroup: Group = {
    val xs = _groups.filterNot(_.isEmpty)
    xs.size match {
      case 0 => Group()
      case 1 =>
        xs.head match {
          case g: Group => g
          case other =>
            val g = Group()
            g.addChild(other)
            g
        }
      case _ =>
        val g = Group()
        xs.foreach(g addChild _)
        g
    }
  }

  private def newGroup(): Group = {
    val g = Group()
    _currentGroup = Some(g)
    _groups.append(g)
    g
  }
  private def currentGroup: Group = {
    _currentGroup.getOrElse(newGroup())
  }

  private def addShape(s: Shape): Unit = {
    currentGroup.addChild(s)
  }
  override def addVector(x: Double, y: Double, z: Double, w: Double): Unit = {
    require(w == 1.0) // For now only allow points
    _vertices.append(Point3D(x, y, z))
  }

  override def addVertexNormal(x: Double, y: Double, z: Double): Unit = {
    _normals.append(Vector3D(x, y, z))
  }

  override def addFace(xs: Seq[Int]): Unit = {
    require(xs.size >= 3, "Face cannot be < 3")
    val points = xs.map(vertices)
    points match {
      case Seq(p1, p2, p3) => addShape(Triangle(p1, p2, p3))
      case _ =>
        TriangleData.fanTriangulation(points).foreach(t => addShape(Triangle.data(t)))
    }
  }

  override def addFace2(xs: Seq[(Int, Int)]): Unit = {
    require(xs.size >= 3, s"Face size should be larger than 3, found: ${xs.size}")
    val pairs = xs.map { case (v, n) => (vertices(v), normals(n)) }
    pairs match {
      case Seq((p1,n1), (p2, n2), (p3, n3)) =>
        addShape(Triangle.smooth(p1, p2, p3, n1, n2, n3))
      case _ =>
        TriangleData.fanTriangulationS(pairs).foreach(t => addShape(Triangle.data(t)))
    }
  }
  override def addFace3(xs: Seq[(Int, Int, Int)]): Unit = {
    require(xs.size >= 3, s"Face size should be larger than 3, found: ${xs.size}")
    val pairs = xs.map { case (v, _, n) => (vertices(v), normals(n)) } //TODO: textures
    pairs match {
      case Seq((p1,n1), (p2, n2), (p3, n3)) =>
        addShape(Triangle.smooth(p1, p2, p3, n1, n2, n3))
      case _ =>
        TriangleData.fanTriangulationS(pairs).foreach(t => addShape(Triangle.data(t)))
    }
  }

  override def textureCoordinates(u: Double, v: Option[Double], w: Option[Double]): Unit = {
    //TODO: implement me
  }


  override def useMaterial(material: String): Unit = {
    println(s"Ignoring: use material:  $material") //TODO
  }


  override def useMaterialLib(materialLibFile: String): Unit = {
    println(s"Ignoring: use material Lib:  $materialLibFile") //TODO
  }

  override def addGroup(name: String): Unit = {
    newGroup()
    //TODO: model group names
  }

  override def addObject(name: String): Unit = {
    println(s"Ignoring add object: $name") //todo
  }
}
