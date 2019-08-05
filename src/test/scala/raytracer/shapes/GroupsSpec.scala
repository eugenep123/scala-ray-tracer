package raytracer
package shapes

class GroupsSpec extends BaseSpec {

  feature("Groups") {

    scenario("Creating a new group") {
      Given("g ← group()")
      Then("g.transform = identity_matrix")
      And("g is empty")

      val g = group()
      assert(g.transform == identityMatrix)
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

      g.add(s)
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
      val r = ray(point(0, 0, 0), vector(0, 0, 1))
      val xs = localIntersect(g, r)
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
      val s2 = sphere(translation(0, 0, -3))
      val s3 = sphere(translation(5, 0, 0))

      addChild(g, s1)
      addChild(g, s2)
      addChild(g, s3)
      val r = ray(point(0, 0, -5), vector(0, 0, 1))
      val xs = localIntersect(g, r)
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

      val g = group(scaling(2, 2, 2))

      val s = sphere(translation(5, 0, 0))
      addChild(g, s)
      val r = ray(point(10, 0, -10), vector(0, 0, 1))
      val xs = intersect(g, r)
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
      val shape = group().add(child)
      val r = ray(point(0, 0, -5), vector(0, 1, 0))
      val xs = intersect(shape, r)
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
      addChild(shape, child)
      val r = ray(point(0, 0, -5), vector(0, 0, 1))
      val xs = intersect(shape, r)
      assert(child.savedRay.isDefined)
    }
  }

}
/*

Scenario:


Scenario:


Scenario: A group has a bounding box that contains its children
    Given s ← sphere()
  And set_transform(s, translation(2, 5, -3) * scaling(2, 2, 2))
  And c ← cylinder()
  And c.minimum ← -2
  And c.maximum ← 2
  And set_transform(c, translation(-4, -1, 4) * scaling(0.5, 1, 0.5))
  And shape ← group()
  And add_child(shape, s)
  And add_child(shape, c)
  When box ← bounds_of(shape)
  Then box.min = point(-4.5, -3, -5)
  And box.max = point(4, 7, 4.5)

Scenario: Partitioning a group's children
    Given s1 ← sphere() with:
      | transform | translation(-2, 0, 0) |
    And s2 ← sphere() with:
      | transform | translation(2, 0, 0) |
    And s3 ← sphere()
    And g ← group() of [s1, s2, s3]
    When (left, right) ← partition_children(g)
  Then g is a group of [s3]
    And left = [s1]
  And right = [s2]

Scenario: Creating a sub-group from a list of children
  Given s1 ← sphere()
    And s2 ← sphere()
    And g ← group()
  When make_subgroup(g, [s1, s2])
  Then g.count = 1
    And g[0] is a group of [s1, s2]

Scenario: Subdividing a group partitions its children
    Given s1 ← sphere() with:
      | transform | translation(-2, -2, 0) |
    And s2 ← sphere() with:
      | transform | translation(-2, 2, 0) |
    And s3 ← sphere() with:
      | transform | scaling(4, 4, 4) |
    And g ← group() of [s1, s2, s3]
    When divide(g, 1)
    Then g[0] = s3
    And subgroup ← g[1]
    And subgroup is a group
    And subgroup.count = 2
    And subgroup[0] is a group of [s1]
    And subgroup[1] is a group of [s2]

    Scenario: Subdividing a group with too few children
  Given s1 ← sphere() with:
      | transform | translation(-2, 0, 0) |
    And s2 ← sphere() with:
      | transform | translation(2, 1, 0) |
    And s3 ← sphere() with:
      | transform | translation(2, -1, 0) |
    And subgroup ← group() of [s1, s2, s3]
    And s4 ← sphere()
    And g ← group() of [subgroup, s4]
  When divide(g, 3)
  Then g[0] = subgroup
    And g[1] = s4
    And subgroup.count = 2
    And subgroup[0] is a group of [s1]
    And subgroup[1] is a group of [s2, s3]
 */