package raytracer
package shapes

import raytracer.math.{Matrix, Ray}
import raytracer.math.Transforms.{Scaling, Translation}

class GroupsSpec extends BaseSpec {

  feature("Groups") {

    scenario("Creating a new group") {
      Given("g ← group()")
      Then("g.transform = identity_matrix")
      And("g is empty")

      val g = group()
      assert(g.transform == Matrix.identity)
      assert(g.isEmpty)
    }

    scenario("Adding a child to a group") {
      Given("g ← group()")
      And("s ← test_shape()")
      When("add_child(g, s)")
      Then("g is not empty")
      And("g includes s")
      And("s.parent = g")

      val g = group()
      val s = testShape()
      assert(!g.includes(s))
      assert(g.isEmpty)

      g.addChild(s)
      assert(!g.isEmpty)
      assert(g includes s)
      assert(s.parent.contains(g))
    }

    scenario("Intersecting a ray with an empty group") {
      Given("g ← group()")
      And("r ← ray(point(0, 0, 0), vector(0, 0, 1))")
      When("xs ← local_intersect(g, r)")
      Then("xs is empty")

      val g = group()
      val r = Ray(point(0, 0, 0), vector(0, 0, 1))
      val xs = g.localIntersect(r)
      assert(xs.isEmpty)
    }

    scenario("Intersecting a ray with a non-empty group") {
      Given("g ← group()")
      And("s1 ← sphere()")
      And("s2 ← sphere()")
      And("set_transform(s2, translation(0, 0, -3))")
      And("s3 ← sphere()")
      And("set_transform(s3, translation(5, 0, 0))")
      And("add_child(g, s1)")
      And("add_child(g, s2)")
      And("add_child(g, s3)")
      When("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      And("xs ← local_intersect(g, r)")
      Then("xs.count = 4")
      And("xs[0].object = s2")
      And("xs[1].object = s2")
      And("xs[2].object = s1")
      And("xs[3].object = s1")

      val g = group()
      val s1 = sphere()
      val s2 = sphere(Translation(0, 0, -3))
      val s3 = sphere(Translation(5, 0, 0))

      g.addChild(s1)
      g.addChild(s2)
      g.addChild(s3)
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val xs = g.localIntersect(r)
      assert(xs.size == 4)
      assert(xs(0).obj == s2)
      assert(xs(1).obj == s2)
      assert(xs(2).obj == s1)
      assert(xs(3).obj == s1)
    }

    scenario("Intersecting a transformed group") {
      Given("g ← group()")
      And("set_transform(g, scaling(2, 2, 2))")
      And("s ← sphere()")
      And("set_transform(s, translation(5, 0, 0))")
      And("add_child(g, s)")
      When("r ← ray(point(10, 0, -10), vector(0, 0, 1))")
      And("xs ← intersect(g, r)")
      Then("xs.count = 2")

      val g = group(Scaling(2, 2, 2))

      val s = sphere(Translation(5, 0, 0))
      g.addChild(s)
      val r = Ray(point(10, 0, -10), vector(0, 0, 1))
      val xs = g.intersect(r)
      assert(xs.size == 2)
    }

    scenario("Intersecting ray+group doesn't test children if box is missed") {
      Given("child ← test_shape()")
      And("shape ← group()")
      And("add_child(shape, child)")
      And("r ← ray(point(0, 0, -5), vector(0, 1, 0))")
      When("xs ← intersect(shape, r)")
      Then("child.saved_ray is unset")

      val child = testShape()
      val shape = group().addChild(child)
      val r = Ray(point(0, 0, -5), vector(0, 1, 0))
      val xs = shape.intersect(r)
      assert(child.savedRay.isEmpty)
    }

    scenario("Intersecting ray+group tests children if box is hit") {
      Given("child ← test_shape()")
      And("shape ← group()")
      And("add_child(shape, child)")
      And("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      When("xs ← intersect(shape, r)")
      Then("child.saved_ray is set")

      val child = testShape()
      val shape = group()
      shape.addChild(child)
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val xs = shape.intersect(r)
      assert(child.savedRay.isDefined)
    }

    scenario("A group has a bounding box that contains its children") {
      Given("s ← sphere()")
      And("set_transform(s, translation(2, 5, -3) * scaling(2, 2, 2))")
      And("c ← cylinder()")
      And("c.minimum ← -2")
      And("c.maximum ← 2")
      And("set_transform(c, translation(-4, -1, 4) * scaling(0.5, 1, 0.5))")
      And("shape ← group()")
      And("add_child(shape, s)")
      And("add_child(shape, c)")
      When("box ← bounds_of(shape)")
      Then("box.min = point(-4.5, -3, -5)")
      And("box.max = point(4, 7, 4.5)")

      val s = Shape()
        .scale(2, 2, 2)
        .translate(2, 5, -3)
        .sphere

      val c = Shape()
        .scale(0.5, 1, 0.5)
        .translate(-4, -1, 4)
        .cylinder(-2, 2)

      val shape = Group().addChild(s).addChild(c)
      val box = shape.bounds
      assert(box.minimum == point(-4.5, -3, -5))
      assert(box.maximum == point(4, 7, 4.5))
    }

    scenario("Partitioning a group's children") {
      Given("""s1 ← sphere() with:
              |      | transform | translation(-2, 0, 0) |""".stripMargin)
        And("""s2 ← sphere() with:
              |      | transform | translation(2, 0, 0) |""".stripMargin)
        And("s3 ← sphere()")
        And("g ← group() of [s1, s2, s3]")
       When("(left, right) ← partition_children(g)")
       Then("g is a group of [s3]")
        And("left = [s1]")
        And("right = [s2]")

      val s1 = sphere(Translation(-2, 0, 0))
      val s2 = sphere(Translation(2, 0, 0))
      val s3 = sphere()
      val g = group().addChildren(Seq(s1, s2, s3))
      val (left, right) = g.partitionChildren

      assert(g.children == Seq(s3))
      assert(left == Seq(s1))
      assert(right == Seq(s2))
    }

    scenario("Creating a sub-group from a list of children") {
      Given("s1 ← sphere()")
        And("s2 ← sphere()")
        And("g ← group()")
       When("make_subgroup(g, [s1, s2])")
       Then("g.count = 1")
        And("g[0] is a group of [s1, s2]")

      val s1 = sphere()
      val s2 = sphere()
      val g = group()
      val subGroup = g.makeSubgroup(Seq(s1, s2))
      assert(g.size == 1)
      assert(g.children.head == subGroup)
    }

    scenario("Subdividing a group partitions its children") {
      Given("""s1 ← sphere() with:
              |      | transform | translation(-2, -2, 0) |""".stripMargin)
      And("""s2 ← sphere() with:
            |      | transform | translation(-2, 2, 0) |""".stripMargin)
      And("""s3 ← sphere() with:
            |      | transform | scaling(4, 4, 4) |""".stripMargin)
      And("g ← group() of [s1, s2, s3]")
     When("divide(g, 1)")
     Then("g[0] = s3")
      And("subgroup ← g[1]")
      And("subgroup is a group")
      And("subgroup.count = 2")
      And("subgroup[0] is a group of [s1]")
      And("subgroup[1] is a group of [s2]")

      val s1 = Sphere(Translation(-2, -2, 0))
      val s2 = Sphere(Translation(-2, 2, 0))
      val s3 = Sphere(Scaling(4, 4, 4))
      val g = Group().addChildren(Seq(s1, s2, s3))

      g.divide(1)

      assert(g.children.head == s3)
      val subgroup = g.children(1)

      assert(subgroup.isInstanceOf[Group])
      val sub = subgroup.asInstanceOf[Group]
      assert(sub.size == 2)
      assert(sub.children(0).asInstanceOf[Group].children == Seq(s1))
      assert(sub.children(1).asInstanceOf[Group].children == Seq(s2))
    }


    scenario("Subdividing a group with too few children") {
      Given("""s1 ← sphere() with:
              |      | transform | translation(-2, 0, 0) |""".stripMargin)
        And("""s2 ← sphere() with:
              |      | transform | translation(2, 1, 0) |""".stripMargin)
        And("""s3 ← sphere() with:
              |      | transform | translation(2, -1, 0) |""".stripMargin)
      And("subgroup ← group() of [s1, s2, s3]")
      And("s4 ← sphere()")
      And("g ← group() of [subgroup, s4]")
     When("divide(g, 3)")
     Then("g[0] = subgroup")
      And("g[1] = s4")
      And("subgroup.count = 2")
      And("subgroup[0] is a group of [s1]")
      And("subgroup[1] is a group of [s2, s3]")

      val s1 = sphere(Translation(-2, 0, 0))

      val s2 = sphere(Translation(2, 1, 0))
      val s3 = sphere(Translation(2, -1, 0))

      val subgroup = group().addChildren(Seq(s1, s2, s3))
      val s4 = sphere()

      val g = group().addChildren(Seq(subgroup, s4))
      g.divide(3)

      assert(g.children(0) == subgroup)
      assert(g.children(1) == s4)
      assert(subgroup.size == 2)
      assert(subgroup.children(0).asInstanceOf[Group].children.toList == Seq(s1))
      assert(subgroup.children(1).asInstanceOf[Group].children.toList == Seq(s2, s3))
    }
  }

}



