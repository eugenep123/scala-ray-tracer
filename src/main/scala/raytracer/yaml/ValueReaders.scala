package raytracer.yaml

import java.util.{ArrayList => JavaArrayList}

import raytracer.{Color, Operation, Point3D, Vector3D}

import scala.collection.JavaConverters._

object ValueReaders {
  import Converters._
  import ParseResult._
  import raytracer.yaml.ast._

  implicit val ColorReader: ValueReader[Color] = (value: Any) => fromTuple3(Color.apply)(value)
  implicit val Point3DReader: ValueReader[Point3D] = (value: Any) => fromTuple3(Point3D.apply)(value)
  implicit val Vector3DReader: ValueReader[Vector3D] = (value: Any) => fromTuple3(Vector3D.apply)(value)

  implicit object ReferenceReader extends ValueReader[Reference] {
    override def read(value: Any): ParseResult[Reference] = {
      Converters.toStr(value).flatMap { s =>
        s.lastIndexOf("-") match {
          case i if i > 0 => success(Reference(s.take(i), s.drop(i + 1)))
          case _ => success(Reference(s, "shape"))
            //conversionError("Reference", s)
        }
      }
    }
  }

  abstract class BaseMapReader[A](what: String) extends ValueReader[A] {
    final override def read(value: Any): ParseResult[A] = {
      value match {
        case m: java.util.Map[String, Any] =>
          readMap(m.asScala.toMap)
        case m: Map[String, Any] =>
          readMap(m)
        case other =>
          fail(s"Failed to read $what, expected map, found: '$other'")
      }
    }
    def readMap(map: YamlMap): ParseResult[A]
  }

  abstract class BaseListReader[A](what: String) extends ValueReader[A] {
    override def read(value: Any): ParseResult[A] = {
      value match {
        case a: JavaArrayList[Double] =>
          readSeq(a.asScala.toVector)
        case v: Seq[Any] =>
          readSeq(v.toVector)
        case other =>
          fail(s"Failed to read $what, expected array, found: '$other'")
      }
    }
    def readSeq(xs: Vector[Any]): ParseResult[A]
  }

  case class ListReader[A](what: String, itemReader: ValueReader[A]) extends BaseListReader[Seq[A]](what) {
    override def readSeq(xs: Vector[Any]): ParseResult[Seq[A]] = {
      sequence(xs, itemReader.read)
    }
  }

  val ColorListReader = new ListReader[Color]("color-list", ColorReader)

  implicit val MaterialObjectReader: ReferenceOrObjectReader[MaterialObject] =
    new ReferenceOrObjectReader[MaterialObject](
      MaterialReference.apply, MaterialValueReader.asInstanceOf[ValueReader[MaterialObject]]
    )

  implicit val TransformListReader: ValueReader[TransformList] =
    (value: Any) => TransformObjectSeqReader.read(value).map(TransformList.apply)

  object TransformObjectSeqReader extends BaseListReader[Seq[TransformObject]]("transform") {
    private val transformRefReader = ReferenceReader.map(TransformReference.apply)

    override def readSeq(xs: Vector[Any]): ParseResult[Seq[TransformObject]] = {
      ParseResult.sequence(xs, readOne)
    }
    def readOne(value: Any): ParseResult[TransformObject] = {
      value match {
        case s: String => transformRefReader.read(s)
        case other => TransformOperationReader.read(other)
      }
    }
  }

  object TransformOperationReader extends BaseListReader[TransformOperation]("transform-op") {
    override def readSeq(xs: Vector[Any]): ParseResult[TransformOperation] = {
      for {
        action <- ParseResult.fromOption(xs.headOption.map(_.toString), "Expected non empty array")
        doubles <- toVectorDouble(xs.drop(1))
        op <- readOp(action, doubles)
      } yield TransformOperation(op)
    }

    def readOp(action: String, values: Vector[Double]): ParseResult[Operation] = {
      import Operation._
      (action, values) match {
        case ("translate",  Vector(x, y, z))  => success(Translation(x, y, z))
        case ("scale",      Vector(x, y, z))  => success(Scaling(x, y, z))
        case ("rotate-x",   Vector(rad))      => success(RotationX(rad))
        case ("rotate-y",   Vector(rad))      => success(RotationY(rad))
        case ("rotate-z",   Vector(rad))      => success(RotationZ(rad))
        //        case ("shear", Vector(x, y, z)) => success(Shearing(x, y, z))
        case other =>
          fail(s"Invalid transform: $action")
      }
    }
  }

  abstract class AbstractPatternReader[A <: PatternValue](what: String) extends BaseMapReader[A]("what") {

    def readPattern(map: YamlMap, patternType: String, transform: TransformList): ParseResult[A]

    override def readMap(map: YamlMap): ParseResult[A] = {
      for {
        patternType <- map.readString("type")
        transformOpt <- map.readOpt[TransformList]("transform")
        transform = transformOpt getOrElse TransformList(Nil)
        pattern <- readPattern(map, patternType, transform)
      } yield pattern
    }

    def readColorPair(map: YamlMap): ParseResult[(Color, Color)] = {
      for {
        xs <- map.read("colors")(ColorListReader)
        pair <- toPair[Color](xs)
      } yield pair
    }

  }

  implicit object PatternValueReader extends AbstractPatternReader[PatternValue]("pattern") {
    override def readPattern(map: YamlMap, patternType: String, transform: TransformList): ParseResult[PatternValue] = {
      patternType match {
        case "checkers" => readColor2Pattern(map, transform, CheckersPatternValue.apply)
        case "stripes" => readColor2Pattern(map, transform, StripePatternValue.apply)
        case "gradient" => readColor2Pattern(map, transform, GradientPatternValue.apply)
        case other => fail(s"Unsupported pattern: $other")
      }
    }

    def readColor2Pattern(
      map: YamlMap,
      transform: TransformList,
      f: (Color, Color, TransformList) => PatternValue): ParseResult[PatternValue] = {
      readColorPair(map) map { pair =>
        val (a, b) = pair
        f(a, b, transform)
      }
    }
  }

  implicit object MaterialValueReader extends BaseMapReader[MaterialValue]("Material") {
    override def readMap(map: YamlMap): ParseResult[MaterialValue] = {
      for {
        extend <- map.readOpt[Reference]("extend")
        color <- map.readOpt[Color]("color")
        diffuse <- map.readDoubleOpt("diffuse")
        ambient <- map.readDoubleOpt("ambient")
        specular <- map.readDoubleOpt("specular")
        shininess <- map.readDoubleOpt("shininess")
        reflective <- map.readDoubleOpt("reflective")
        transparency <- map.readDoubleOpt("transparency")
        refractiveIndex <- map.readDoubleOpt("refractive-index")
        patternOpt <- map.readOpt[PatternValue]("pattern")
      } yield MaterialValue(
        extend, color, diffuse, ambient, specular, shininess,
        reflective, transparency, refractiveIndex, patternOpt)
    }
  }

  case class ReferenceOrObjectReader[A](
    makeRef: Reference => A,
    readValue: ValueReader[A]) extends ValueReader[A] {
    override def read(value: Any): ParseResult[A] = {
      value match {
        case s: String => ReferenceReader.read(s).map(makeRef)
        case _ => readValue.read(value)
      }
    }
  }



  implicit object AddCameraAstReader extends BaseMapReader[CameraObject]("Add-Camera") {
    override def readMap(map: YamlMap): ParseResult[CameraObject] = {
      for {
        width <- map.readInt("width")
        height <- map.readInt("height")
        fov <- map.readDouble("field-of-view")
        from <- map.read[Point3D]("from")
        to <- map.read[Point3D]("to")
        up <- map.read[Vector3D]("up")
      } yield CameraObject(width, height, fov, from, to, up)
    }
  }

  implicit object AddLightAstReader extends BaseMapReader[LightObject]("AddLightAst") {
    override def readMap(map: YamlMap): ParseResult[LightObject] = {
      for {
        at <- map.read[Point3D]("at")
        intensity <- map.read[Color]("intensity")
      } yield LightObject(at, intensity)
    }
  }

  implicit object ShapeObjectReader extends BaseMapReader[ShapeObject]("shape") {
    override def readMap(map: YamlMap): ParseResult[ShapeObject] = {
      for {
        shapeType <- map.readString("add")
        material <- map.readOpt[MaterialObject]("material")
        transformOpt <- map.readOpt[TransformList]("transform")
        transform = transformOpt getOrElse TransformList(Nil)
        shape <- readShape(map, shapeType, material, transform)
      } yield shape
    }
    val shapeListReader = ListReader("shapes-list", this)
    protected def readShape(
      map: YamlMap,
      shapeType: String,
      material: Option[MaterialObject],
      transform: TransformList): ParseResult[ShapeObject] = {
      shapeType match {
        case "cylinder" | "cone" =>
          for {
            minimum <- map.readDouble("min")
            maximum <- map.readDouble("max")
            closed <- map.readBool("closed")
            isCylinder = (shapeType == "cylinder")
          } yield CylindricObject(material, transform, minimum, maximum, closed, isCylinder)
        case "sphere" | "cube" | "plane" | "cylinder" =>
          val shape = SimpleShapeObject(shapeType, material, transform)
          success(shape)
        case "group" =>
          for {
            children <- map.read[Seq[ShapeObject]]("children")(shapeListReader)
          } yield ShapesGroup(material, transform, children)
        case "obj" =>
          for {
            filename <- map.readString("file")
          } yield ObjFileShape(material, transform, filename)
        case reference =>
          val ref = ShapeReference(reference, material, transform)
          success(ref)
      }
    }
  }

  implicit object AddObjectReader extends BaseMapReader[AddObject]("AddObject") {
    override def readMap(map: YamlMap): ParseResult[AddObject] = {
      map
        .readString("add")
        .flatMap {
          case "camera" =>
            map.convertTo[CameraObject]
          case "light" =>
            map.convertTo[LightObject]
          case _ =>
            map.convertTo[ShapeObject]
        }
        .withDetail(map)
    }
  }

  implicit object DefineReader extends BaseMapReader[DefineObject]("define") {
    override def readMap(map: YamlMap): ParseResult[DefineObject] = {
      for {
        ref <- map.read[Reference]("define")
        define <- readDefine(map, ref)
      } yield define
    }

    private def readDefine(map: YamlMap, reference: Reference): ParseResult[DefineObject] = {
      reference.defineType match {
        case "material" =>
          for {
            value <- map.read[MaterialValue]("value")
          } yield DefineMaterial(reference, value)

        case "transform" | "object" =>
          for {
            value <- map.read[TransformList]("value")
          } yield DefineTransform(reference, value)
        case "shape" =>
          for {
            value <- map.read[ShapeObject]("value")
          } yield DefineShape(reference, value)
        case _ =>
          fail(s"Invalid define type: '${reference.defineType}'")
      }
    }
  }

  implicit object YamlItemReader extends BaseMapReader[YamlItem]("root item") {
    override def readMap(map: YamlMap): ParseResult[YamlItem] = {
      val result =
        if (map.contains("define")) DefineReader.readMap(map)
        else if (map.contains("add")) AddObjectReader.readMap(map)
        else fail("Unsupported type")
      result.withDetail(map)
    }
  }
}
