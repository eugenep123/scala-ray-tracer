package raytracer.resource.yaml


import raytracer.Color
import raytracer.math.{Operation, Point3D, Vector3D}

// Used to read YamlMaps
object YamlReader {
  import AST._
  import Converters._
  import ParseResult._
  import Readers._

  implicit val ColorReader: ValueReader[Color] = (value: Any) => fromTuple3(Color.apply)(value)
  implicit val Point3DReader: ValueReader[Point3D] = (value: Any) => fromTuple3(Point3D.apply)(value)
  implicit val Vector3DReader: ValueReader[Vector3D] = (value: Any) => fromTuple3(Vector3D.apply)(value)

  def readYamlValue(m: YamlMap): ParseResult[YamlValue] = {
    val result =
      if (m.contains("add")) AddValueReader.readMap(m)
      else if (m.contains("define")) DefineValueReader.readMap(m)
      else fail("Unsupported type")
    result.withDetail(m)
  }

  implicit object TransformTransformValue extends ListReader[TransformValue] {
    override def readSeq(xs: Vector[Any]): ParseResult[TransformValue] = {
      ParseResult.sequence(xs, readTransformItem).map(TransformValue.apply)
    }
    def readTransformItem(value: Any): ParseResult[TransformItem] = {
      value match {
        case ref: String => success(TransformReference(ref))
        case other =>
          // Read operation (list)
          toVectorAny(value).flatMap(readTransformOperation)
      }
    }

    def readTransformOperation(xs: Vector[Any]): ParseResult[TransformOperation] = {
      for {
        action <- ParseResult.fromOption(xs.headOption.map(_.toString), "Expected non empty array")
        doubles <- toVectorDouble(xs.drop(1))
        op <- readOperation(action, doubles)
      } yield TransformOperation(op)
    }

    def readOperation(action: String, values: Vector[Double]): ParseResult[Operation] = {
      import raytracer.math.Operation._
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

  implicit object PatternValueReader extends MapReader[PatternValue] {
    override def readMap(map: YamlMap): ParseResult[PatternValue] = {
      for {
        patternType <- map.readString("type")
        transformOpt <- map.readOpt[TransformValue]("transform")
        transform = transformOpt getOrElse TransformValue(Nil)
        pattern <- readPattern(map, patternType, transform)
      } yield pattern
    }

    def readPattern(map: YamlMap, patternType: String, transform: TransformValue): ParseResult[PatternValue] = {
      patternType match {
        case "checkers" => readColor2Pattern(map, transform, CheckersPatternValue.apply)
        case "stripes" => readColor2Pattern(map, transform, StripePatternValue.apply)
        case "gradient" => readColor2Pattern(map, transform, GradientPatternValue.apply)
        case "ring" => readColor2Pattern(map, transform, RingPatternValue.apply)
        case other => fail(s"Unsupported pattern: $other")
      }
    }

    def readColor2Pattern(
      map: YamlMap,
      transform: TransformValue,
      f: (Color, Color, TransformValue) => PatternValue): ParseResult[PatternValue] = {
      readColorPair(map) map { pair =>
        val (a, b) = pair
        f(a, b, transform)
      }
    }

    def readColorPair(map: YamlMap): ParseResult[(Color, Color)] = {
      for {
        xs <- map.read[Seq[Color]]("colors")
        pair <- toPair[Color](xs)
      } yield pair
    }

  }

  implicit object MaterialValueReader extends ValueReader[MaterialValue] {
    override def read(value: Any): ParseResult[MaterialValue] = {
      value match {
        case ref: String => success(MaterialReference(ref))
        case _ => readMaterialObject(value)
      }
    }

    def readMaterialObject(value: Any): ParseResult[MaterialValue] =
      toMap(value).flatMap(readMaterialObjectMap)

    def readMaterialObjectMap(map: YamlMap): ParseResult[MaterialValue] = {
      for {
        color <- map.readOpt[Color]("color")
        diffuse <- map.readDoubleOpt("diffuse")
        ambient <- map.readDoubleOpt("ambient")
        specular <- map.readDoubleOpt("specular")
        shininess <- map.readDoubleOpt("shininess")
        reflective <- map.readDoubleOpt("reflective")
        transparency <- map.readDoubleOpt("transparency")
        refractiveIndex <- map.readDoubleOpt("refractive-index")
        patternOpt <- map.readOpt[PatternValue]("pattern")
      } yield MaterialObject(
        color, diffuse, ambient, specular, shininess,
        reflective, transparency, refractiveIndex, patternOpt)
    }
  }

  abstract class AddReader[A <: AddValue] extends MapReader[A] {
    override def readMap(map: YamlMap): ParseResult[A] = {
      map.readString("add").flatMap(readAdd(map, _))
    }

    def readAdd(map: YamlMap, addType: String): ParseResult[A]

    def readBaseShape(map: YamlMap)(f: (TransformValue, Option[MaterialValue]) => ParseResult[A]): ParseResult[A] = {
      for {
        material <- map.readOpt[MaterialValue]("material")
        transformOpt <- map.readOpt[TransformValue]("transform")
        transform = transformOpt getOrElse TransformValue(Nil)
        result <- f(transform, material)
      } yield result
    }
  }

  implicit object AddValueReader extends AddReader[AddValue] {

    override def readAdd(map: YamlMap, addType: String): ParseResult[AddValue] = {
      val result = addType match {
        case "camera" => readAddCamera(map)
        case "light" => readAddLight(map)
        case _ => AddShapeReader.readAdd(map, addType)
      }
      result.withDetail(map)
    }

    def readAddCamera(map: YamlMap): ParseResult[AddCamera] = {
      for {
        width <- map.readInt("width")
        height <- map.readInt("height")
        fov <- map.readDouble("field-of-view")
        from <- map.read[Point3D]("from")
        to <- map.read[Point3D]("to")
        up <- map.read[Vector3D]("up")
      } yield AddCamera(width, height, fov, from, to, up)
    }

    def readAddLight(map: YamlMap): ParseResult[AddLight] = {
      for {
        at <- map.read[Point3D]("at")
        intensity <- map.read[Color]("intensity")
      } yield AddLight(at, intensity)
    }

  }

  implicit object AddShapeReader extends AddReader[AddShape] {
    override def readAdd(map: YamlMap, addType: String): ParseResult[AddShape] = {
      if (ShapeValueReader.supports(addType)) ShapeValueReader.readAdd(map, addType)
      else readBaseShape(map)((transform, material) => success(ShapeReference(addType, transform, material)))
    }
  }

  implicit object ShapeValueReader extends AddReader[ShapeValue] {
    val shapes = Set("cylinder", "cone", "sphere", "cube", "plane", "group", "obj")
    def supports(addType: String) = shapes.contains(addType)

    override def readAdd(map: YamlMap, addType: String): ParseResult[ShapeValue] =
      readBaseShape(map)((transform, material) => readShape(map, addType, transform, material))

    def readShape(
      map: YamlMap,
      shapeType: String,
      transform: TransformValue,
      material: Option[MaterialValue]): ParseResult[ShapeValue] = {
      shapeType match {
        case "sphere" => success(SphereValue(transform, material))
        case "cube" => success(CubeValue(transform, material))
        case "plane" => success(PlaneValue(transform, material))
        case "cylinder" | "cone" =>
          for {
            minimum <- map.readDouble("min")
            maximum <- map.readDouble("max")
            closed <- map.readBool("closed")
          } yield {
            if (shapeType == "cone") ConeValue(minimum, maximum, closed, transform, material)
            else CylinderValue(minimum, maximum, closed, transform, material)
          }
        case "obj" =>
          for {
            filename <- map.readString("filename")
          } yield ObjFileValue(filename, transform, material)
        case "group" =>
          for {
            children <- map.read[Seq[AddShape]]("children")
          } yield GroupValue(children, transform, material)
      }

    }

  }

  implicit object DefineValueReader extends MapReader[DefineValue] {
    val types = Set("transform", "material", "shape", "transform", "object")

    override def readMap(map: YamlMap): ParseResult[DefineValue] = {
      for {
        ref <- map.readString("define")
        define <- readDefine(map, ref)
      } yield define
    }

    def typeOfKey(key: String) =
      types.find(t => key.endsWith(s"-$t")).getOrElse(key)

    def readDefine(map: YamlMap, key: String): ParseResult[DefineValue] = {
      val defineType = typeOfKey(key)
      defineType match {
        case "material" =>
          for {
            extend <- map.readStringOpt("extend")
            value <- map.read[MaterialValue]("value")
          } yield DefineMaterial(key, extend, value)

        case "transform" | "object" =>
          for {
            value <- map.read[TransformValue]("value")
          } yield DefineTransform(key, value)
        case "shape" =>
          for {
            value <- map.read[ShapeValue]("value")
          } yield DefineShape(key, value)
        case _ =>
          fail(s"Invalid define type: '$defineType'")
      }
    }
  }
}
