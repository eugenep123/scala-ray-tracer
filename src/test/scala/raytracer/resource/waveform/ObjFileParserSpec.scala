package raytracer.resource
package waveform

import raytracer.BaseSpec

class ObjFileParserSpec extends BaseSpec {


  def trianglesFileContent = getResourceString("/triangles.obj")

  feature("OBJ File Parser") {

    scenario("Ignoring unrecognized lines") {
      Given(
        """gibberish ← a file containing:
      There was a young lady named Bright
      who traveled much faster than light.
      She set out one day
      in a relative way,
      and came back the previous night.
      """)
      When("parser ← parse_obj_file(gibberish)")
      Then("parser should have ignored 5 lines")

      val gibberish =
        """
          There was a young lady named Bright
          who traveled much faster than light.
          She set out one day
          in a relative way,
          and came back the previous night.
        """
      val result = parseObjFile(gibberish)
      //parser should have ignored 5 lines
    }

    scenario("Vertex records") {
      val content =
        """
          |v -1 1 0
          |v -1.0000 0.5000 0.0000
          |v 1 0 0
          |v 1 1 0
          |""".stripMargin
      Given(s"file ← a file containing:\n$content")

      When("parser ← parse_obj_file(file)")
      Then("parser.vertices[1] = point(-1, 1, 0)")
      And("parser.vertices[2] = point(-1, 0.5, 0)")
      And("parser.vertices[3] = point(1, 0, 0)")
      And("parser.vertices[4] = point(1, 1, 0)")

      val result = parseObjFile(content)
      assert(result.vertices(1) == point(-1, 1, 0))
      assert(result.vertices(2) == point(-1, 0.5, 0))
      assert(result.vertices(3) == point(1, 0, 0))
      assert(result.vertices(4) == point(1, 1, 0))
    }

    scenario("Parsing triangle faces") {
      val content =
        """
          |v -1 1 0
          |v -1 0 0
          |v 1 0 0
          |v 1 1 0
          |
          |f 1 2 3
          |f 1 3 4
          |""".stripMargin
      Given(s"file ← a file containing: \n$content")

      When(" parser ← parse_obj_file(file)")
      And("g ← parser.default_group")
      And("t1 ← first child of g")
      And("t2 ← second child of g")
      Then("t1.p1 = parser.vertices[1]")
      And("t1.p2 = parser.vertices[2]")
      And("t1.p3 = parser.vertices[3]")
      And("t2.p1 = parser.vertices[1]")
      And("t2.p2 = parser.vertices[3]")
      And("t2.p3 = parser.vertices[4]")

      val result = parseObjFile(content)
      val g = result.defaultGroup
      val t1 = g.triangles(0)
      val t2 = g.triangles(1)

      assert(t1.p1 == result.vertices(1))
      assert(t1.p2 == result.vertices(2))
      assert(t1.p3 == result.vertices(3))
      assert(t2.p1 == result.vertices(1))
      assert(t2.p2 == result.vertices(3))
      assert(t2.p3 == result.vertices(4))
    }

    scenario("Triangulating polygons") {
      val content =
        """
          |v -1 1 0
          |v -1 0 0
          |v 1 0 0
          |v 1 1 0
          |v 0 2 0
          |
          |f 1 2 3 4 5
          |""".stripMargin
      Given(s"file ← a file containing: \n$content")

      When("parser ← parse_obj_file(file)")
      And("g ← parser.default_group")
      And("t1 ← first child of g")
      And("t2 ← second child of g")
      And("t3 ← third child of g")
      Then("t1.p1 = parser.vertices[1]")
      And("t1.p2 = parser.vertices[2]")
      And("t1.p3 = parser.vertices[3]")
      And("t2.p1 = parser.vertices[1]")
      And("t2.p2 = parser.vertices[3]")
      And("t2.p3 = parser.vertices[4]")
      And("t3.p1 = parser.vertices[1]")
      And("t3.p2 = parser.vertices[4]")
      And("t3.p3 = parser.vertices[5]")

      val result = parseObjFile(content)
      val g = result.defaultGroup
      val t1 = g.triangles(0)
      val t2 = g.triangles(1)
      val t3 = g.triangles(2)
      assert(t1.p1 == result.vertices(1))
      assert(t1.p2 == result.vertices(2))
      assert(t1.p3 == result.vertices(3))
      assert(t2.p1 == result.vertices(1))
      assert(t2.p2 == result.vertices(3))
      assert(t2.p3 == result.vertices(4))
      assert(t3.p1 == result.vertices(1))
      assert(t3.p2 == result.vertices(4))
      assert(t3.p3 == result.vertices(5))
    }

    scenario("Triangles in groups") {
      Given("file ← the file 'triangles.obj'")
      When("parser ← parse_obj_file(file)")
      And("g1 ← 'FirstGroup' from parser")
      And("g2 ← 'SecondGroup' from parser")
      And("t1 ← first child of g1")
      And("t2 ← first child of g2")
      Then("t1.p1 = parser.vertices[1]")
      And("t1.p2 = parser.vertices[2]")
      And("t1.p3 = parser.vertices[3]")
      And("t2.p1 = parser.vertices[1]")
      And("t2.p2 = parser.vertices[3]")
      And("t2.p3 = parser.vertices[4]")

      val result = parseObjFile(trianglesFileContent)
      val g1 = result.groups(0)
      val g2 = result.groups(1)
      val t1 = g1.triangles.head
      val t2 = g2.triangles.head
      assert(t1.p1 == result.vertices(1))
      assert(t1.p2 == result.vertices(2))
      assert(t1.p3 == result.vertices(3))
      assert(t2.p1 == result.vertices(1))
      assert(t2.p2 == result.vertices(3))
      assert(t2.p3 == result.vertices(4))
    }

    scenario("Converting an OBJ file to a group") {
      Given("file ← the file 'triangles.obj'")
      And(" parser ← parse_obj_file(file)")
      When(" g ← obj_to_group(parser)")
      Then("g includes 'FirstGroup' from parser")
      And("g includes 'SecondGroup' from parser")

      val result = parseObjFile(trianglesFileContent)
      val g = result.toGroup
      assert(g includes result.groups(0))
      assert(g includes result.groups(1))
    }

    scenario("Vertex normal records") {
      val content =
        """
          |vn 0 0 1
          |vn 0.707 0 -0.707
          |vn 1 2 3
          |""".stripMargin
      Given(s"file ← a file containing: \n$content")
      When("parser ← parse_obj_file(file)")
      Then("parser.normals[1] = vector(0, 0, 1)")
      And("parser.normals[2] = vector(0.707, 0, -0.707)")
      And("parser.normals[3] = vector(1, 2, 3)")

      val result = parseObjFile(content)
      assert(result.normals(1) == vector(0, 0, 1))
      assert(result.normals(2) == vector(0.707, 0, -0.707))
      assert(result.normals(3) == vector(1, 2, 3))
    }

    scenario("Faces with normals") {
      val content =
        """
          |v 0 1 0
          |v -1 0 0
          |v 1 0 0
          |
          |vn -1 0 0
          |vn 1 0 0
          |vn 0 1 0
          |
          |f 1//3 2//1 3//2
          |f 1/0/3 2/102/1 3/14/2
          |""".stripMargin
      Given(s"file ← a file containing:\n$content")
      When("parser ← parse_obj_file(file)")
      And("g ← parser.default_group")
      And("t1 ← first child of g")
      And("t2 ← second child of g")
      Then("t1.p1 = parser.vertices[1]")
       And("t1.p2 = parser.vertices[2]")
       And("t1.p3 = parser.vertices[3]")
       And("t1.n1 = parser.normals[3]")
       And("t1.n2 = parser.normals[1]")
       And("t1.n3 = parser.normals[2]")
      And("t2 = t1")


      val result = parseObjFile(content)

      val g = result.defaultGroup
      val t1 = g.smoothTriangles(1)
      val t2 = g.smoothTriangles(1)
      assert(t1.p1 == result.vertices(1))
      assert(t1.p2 == result.vertices(2))
      assert(t1.p3 == result.vertices(3))
      assert(t1.n1 == result.normals(3))
      assert(t1.n2 == result.normals(1))
      assert(t1.n3 == result.normals(2))
      assert(t2 == t1)
    }
  }
}
