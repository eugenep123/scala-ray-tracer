package raytracer.resource.yaml.reader

import raytracer.Color
import raytracer.math.{Point3D, Transforms, Vector3D}
import raytracer.patterns.UVMapping
import raytracer.resource.yaml.AST._

// Used to read YamlMaps
object AstReaders {
  import ParseResult._
  import raytracer.resource.yaml.reader.Converters._
  import raytracer.resource.yaml.reader.Readers._


  private def readColorPair(map: YamlMap): ParseResult[(Color, Color)] = {
    for {
      xs <- map.read[Seq[Color]]("colors")
      pair <- toPair[Color](xs)
    } yield pair
  }

  // Top level reader
  implicit object YamlValueReader extends MapReader[YamlValue] {
    override def readMap(map: YamlMap): ParseResult[YamlValue] = {
      val result =
        if (map.contains("add")) AddValueReader.readMap(map)
        else if (map.contains("define")) DefineValueReader.readMap(map)
        else fail("Unsupported type")
      result.withDetail(map)
    }
  }

  implicit object TransformListReader extends ListReader[TransformList] {
    override def readSeq(xs: Vector[Any]): ParseResult[TransformList] = {
      ParseResult.sequence(xs, readTransformItem).map(TransformList.apply)
    }
    def readTransformItem(value: Any): ParseResult[TransformItem] = {
      value match {
        case ref: String => success(TransformReference(ref))
        case _ => toVectorAny(value).flatMap(readTransformOperation)
      }
    }

    def readTransformOperation(xs: Vector[Any]): ParseResult[TransformOperation] = {
      for {
        action <- ParseResult.fromOption(xs.headOption.map(_.toString), "Expected non empty array")
        doubles <- toVectorDouble(xs.drop(1))
        op <- readOperation(action, doubles)
      } yield TransformOperation(op)
    }

    def readOperation(action: String, values: Vector[Double]): ParseResult[Transforms] = {
      import raytracer.math.Transforms._
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


  implicit object UVPatternValueReader extends MapReader[UVPatternValue] {
    override def readMap(map: YamlMap): ParseResult[UVPatternValue] = {
      for {
        t <- map.readString("type")
        value <- readUV(map, t)
      } yield value
    }
    def readUV(map: YamlMap, t: String): ParseResult[UVPatternValue] = {
      t match {
        case "align_check" =>
          for {
            c <- map.readMap("colors")
            main <- c.read[Color]("main")
            ul <- c.read[Color]("ul")
            ur <- c.read[Color]("ur")
            bl <- c.read[Color]("bl")
            br <- c.read[Color]("br")
          } yield AlightCheckUVPatternValue(main, ul, ur, bl, br)
        case "checkers" =>
          for {
            width <- map.readInt("width")
            height <- map.readInt("height")
            colors <- readColorPair(map)
          } yield CheckersUVPatternValue(width, height, colors._1, colors._2)
        case "image" =>
          for {
            file <- map.readString("file")
          } yield ImageUVPatternValue(file)
        case _ =>
          fail(s"Unknown uv pattern: '$t'")
      }
    }

  }

  implicit object PatternValueReader extends MapReader[PatternValue] {
    override def readMap(map: YamlMap): ParseResult[PatternValue] = {
      for {
        patternType <- map.readString("type")
        transform <- map.readOpt[TransformList]("transform")
        pattern <- readPattern(map, patternType, transform)
      } yield pattern
    }

    def readPattern(map: YamlMap, patternType: String, transform: TransformOption): ParseResult[PatternValue] = {
      patternType match {
        case "checkers" => readColor2Pattern(map, transform, CheckersPatternValue.apply)
        case "stripes" => readColor2Pattern(map, transform, StripePatternValue.apply)
        case "gradient" => readColor2Pattern(map, transform, GradientPatternValue.apply)
        case "ring" => readColor2Pattern(map, transform, RingPatternValue.apply)
        case "map" =>
          for {
            transform <- map.readOpt[TransformList]("transform")
            typ <- map.readString("mapping")
            pattern <- readMapPattern(map, transform, typ)
          } yield pattern
        case other =>
          fail(s"Unsupported pattern: $other")
      }
    }

    def readMapPattern(map: YamlMap, transform: TransformOption, typ: String): ParseResult[PatternValue] = {
      typ match {
        case "planar" =>
          for {
            uv <- map.read[UVPatternValue]("uv_pattern")
          } yield MapPatternValue(UVMapping.Planar, uv, transform)

        case "cube" =>
          for {
            left <-  map.read[UVPatternValue]("left")
            front <-  map.read[UVPatternValue]("front")
            right <-  map.read[UVPatternValue]("right")
            back <-  map.read[UVPatternValue]("back")
            up <-  map.read[UVPatternValue]("up")
            down <-  map.read[UVPatternValue]("down")
          } yield CubeMapPatternValue(left, front, right, back, up, down, transform)

        case "cylindrical" =>
          for {
            uv <- map.read[UVPatternValue]("uv_pattern")
          } yield MapPatternValue(UVMapping.Cylindrical, uv, transform)
        case "spherical" =>
          for {
            uv <- map.read[UVPatternValue]("uv_pattern")
          } yield MapPatternValue(UVMapping.Spherical, uv, transform)
        case other =>
          fail(s"Unsupported map pattern: $typ")
      }
    }

    def readColor2Pattern(
      map: YamlMap,
      transform: TransformOption,
      f: (Color, Color, TransformOption) => PatternValue): ParseResult[PatternValue] = {
      readColorPair(map) map { pair =>
        val (a, b) = pair
        f(a, b, transform)
      }
    }

  }

  implicit object MaterialValueReader extends ValueReader[MaterialValue] {
    override def read(value: Any): ParseResult[MaterialValue] = {
      value match {
        case ref: String => success(MaterialReference(ref))
        case _ => MaterialObjectReader.read(value)
      }
    }
  }

  implicit object MaterialObjectReader extends MapReader[MaterialObject] {
    override def readMap(map: YamlMap): ParseResult[MaterialObject] = {
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

    def readBaseShape(map: YamlMap)(f: (TransformOption, MaterialOption) => ParseResult[A]): ParseResult[A] = {
      for {
        material <- map.readOpt[MaterialValue]("material")
        transform <- map.readOpt[TransformList]("transform")
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
    val shapes = Set("cylinder", "cone", "sphere", "cube", "plane", "group", "obj")

    override def readAdd(map: YamlMap, addType: String): ParseResult[AddShape] = {
      readBaseShape(map)((transform, material) =>
        if (supports(addType)) readShape(map, addType, transform, material)
        else success(ShapeReference(addType, transform, material))
      )
    }

    def supports(addType: String): Boolean = shapes.contains(addType)

    def readShape(
      map: YamlMap,
      shapeType: String,
      transform: TransformOption,
      material: MaterialOption): ParseResult[AddShape] = {
      shapeType match {
        case "sphere" =>
          success(AddSphere(transform, material))
        case "cube" =>
          for {
            shadow <- map.readBoolOpt("shadow")
          } yield AddCube(transform, material, shadow.getOrElse(true))
        case "plane" =>
          success(AddPlane(transform, material))
        case "cylinder" | "cone" =>
          for {
            minimum <- map.readDouble("min")
            maximum <- map.readDouble("max")
            closed <- map.readBoolOpt("closed")
          } yield {
            if (shapeType == "cone") AddCone(minimum, maximum, closed.getOrElse(false), transform, material)
            else AddCylinder(minimum, maximum, closed.getOrElse(false), transform, material)
          }
        case "obj" =>
          for {
            file <- map.readString("file")
          } yield AddObjFile(file, transform, material)
        case "group" =>
          for {
            children <- map.read[Seq[AddShape]]("children")
          } yield AddGroup(children, transform, material)
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

    def typeOfKey(key: String): String =
      types.find(t => key.endsWith(s"-$t")).getOrElse(key)

    def readDefine(map: YamlMap, key: String): ParseResult[DefineValue] = {
      val defineType = typeOfKey(key)
      defineType match {
        case "material" =>
          for {
            extend <- map.readStringOpt("extend")
            value <- map.read[MaterialObject]("value")
          } yield DefineMaterial(key, extend, value)
        case "transform" | "object" =>
          for {
            value <- map.read[TransformList]("value")
          } yield DefineTransform(key, value)
        case "shape" =>
          for {
            value <- map.read[AddShape]("value")
          } yield DefineShape(key, value)
        case _ =>
          // shape
          for {
            value <- map.read[AddShape]("value")
          } yield DefineShape(key, value)
      }
    }
  }
}
