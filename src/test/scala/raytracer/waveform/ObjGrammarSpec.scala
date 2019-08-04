package raytracer.waveform


import fastparse.Parsed.Success
import fastparse.{P, Parsed, parse}
import org.scalatest.{Matchers, WordSpec}

class ObjGrammarSpec extends WordSpec with Matchers {
  import ObjGrammar._

  def test[T](parser: P[_] => P[T], input: String, expected: T): Unit = {
    val parsed = parse(input, parser(_), startIndex = 0)
    val Parsed.Success(value, successIndex) = parsed
    assert(value == expected)
  }

  "the parser" should {
    "parse doubles" in {
      test(implicit c => decimal, "123", 123d)
      test(implicit c => decimal, "-123 ", -123d)
      test(implicit c => decimal, "12.3", 12.3)
      test(implicit c => decimal, "-123  ", -123)
      test(implicit c => decimal, "-12.3", -12.3d)
      test(implicit c => decimal, "0.3", 0.3)
    }

    "parse index" in {
      test(implicit c => index, "123", 123)
      test(implicit c => index, "0", 0)
      test(implicit c => index, "1 2 ", 1)
    }

    "parse a name" in {
      test(implicit c => name, "123", "123")
      test(implicit c => name, "123sdsd23", "123sdsd23")
      test(implicit c => name, "dsd123", "dsd123")
      test(implicit c => name, "dsd123 ", "dsd123")
    }

    "parse group" in {
      // g FirstGroup
      test(implicit c => group, "g FirstGroup", "FirstGroup")
      test(implicit c => group, "g some-group-23", "some-group-23")

      test(implicit c => group,
        """g some-group-23
          |f 1 2 3
          |g SecondGroup
          |f 1 3 4
          |""".stripMargin, "some-group-23")
    }

    "parse a vertex" in {
      //# List of geometric vertices, with (x, y, z [,w]) coordinates, w is optional and defaults to 1.0.
      //v 0.123 0.234 0.345 1.0
      test(implicit c => vector, "v 0.123  0.234 0.345 1.0 ", (0.123, 0.234, 0.345, 1.0))
      test(implicit c => vector, "v  0.123 0.234  0.345 ", (0.123, 0.234, 0.345, 1.0))
    }

    "parse vertex normal" in {
      test(implicit c => vertexNormal, "vn 0 0 1", (0.0, 0.0, 1.0))
      test(implicit c => vertexNormal, "vn 0.707 0 -0.707", (0.707, 0.0, -0.707))
      test(implicit c => vertexNormal, "vn 1 2 3", (1.0, 2.0, 3.0))
    }

//    "parse a face" in {
//      // # Polygonal face element (see below)
//      //f 1 2 3
//      test(implicit c => faceVertex, "f    1 2  4 5", List(1, 2, 4, 5))
//      test(implicit c => faceVertex, "f 1 2 3", List(1, 2, 3))
//    }
//
//    "parse face with normals" in {
//      test(implicit c => faceVertexNormal, "f 1//3 2//1 3//2", List((1, 3), (2, 1), (3, 2)))
//    }
//
//    "parse face with vertex/texture/normal" in {
//      // f 1/0/3 2/102/1 3/14/2
//      test(implicit c => faceVertexTextureNormal, "f 1/0/3 2/102/1 3/14/2", List((1, 0, 3), (2, 102, 1), (3, 14, 2)))
//    }

    "parse a face" in {
      // # Polygonal face element (see below)
      //f 1 2 3
      test(implicit c => face, "f    1 2  4 5", List(1, 2, 4, 5))
      test(implicit c => face, "f 1 2 3", List(1, 2, 3))
      test(implicit c => face, "f 1//3 2//1 3//2", List((1, 3), (2, 1), (3, 2)))

      // f 1/0/3 2/102/1 3/14/2
      test(implicit c => face, "f 1/0/3 2/102/1 3/14/2", List((1, 0, 3), (2, 102, 1), (3, 14, 2)))
    }

//    "parse multiple lines" in {
//
//      val content =
//        """
//          |v -1 1 0
//          |v -1 0 0
//          |v 1 0 0
//          |v 1 1 0
//          |
//          |g FirstGroup
//          |f 1 2 3
//          |g SecondGroup
//          |f 1 3 4
//          |""".stripMargin
//
//      val Success(value, index) = parse(content, lines(_), true, 0)
//      println(value)
//    }

//    "parse mutiple lines" in {
//      val content = """
//        |v -1 1 0
//        |v -1.0000 0.5000 0.0000
//        |v 1 0 0
//        |v 1 1 0
//        |""".stripMargin
//
//      val Success(value, index) = parse(content, lines(_), true, 0)
//        println(value)
//
//    }

    "parse a file" in {
      val content =
        """
          |# 3ds Max Wavefront OBJ Exporter v0.97b - (c)2007 guruware
          |# File Created: 29.07.2018 11:53:30
          |
          |#mtllib Teapot.mtl fix this later!!
          |
          |#
          |# object Teapot001
          |#
          |
          |v  21.0000 36.0000 -0.0000
          |""".stripMargin

      val result = ObjFileParser.parseContent(content)
    }
  }

}