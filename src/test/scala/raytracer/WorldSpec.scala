package raytracer
import raytracer.math.{Intersection, Ray}
import raytracer.math.Transform.{Scaling, Translation}
import raytracer.patterns.TestPattern
import raytracer.shapes.Shape

import scala.math.sqrt

class WorldSpec extends BaseSpec {

  feature("World") {

    scenario("Creating a world") {
      Given("w ← world()")
      Then("w contains no objects")
      And(" w has no light source")
      val w = World.empty
      assert(w.objects.isEmpty)
      assert(w.lights.isEmpty)
    }

    scenario("The default world") {
      Given("light ← point_light(point(-10, 10, -10), color(1, 1, 1))")
      And("""s1 ← sphere() with:
          | material.color     | (0.8, 1.0, 0.6)        |
          | material.diffuse   | 0.7                    |
          | material.specular  | 0.2                    |
        """.stripMargin)
      And("""s2 ← sphere() with:
            |  transform | scaling(0.5, 0.5, 0.5) |""")
      When("w ← default_world()")
      Then("w.light = light")
      And("w contains s1")
      And("w contains s2")
      val light = PointLight(point(-10, 10, -10), Color(1, 1, 1))
      val s1 = sphere(material = Material(
        Color(0.8, 1.0, 0.6),
        diffuse = 0.7,
        specular = 0.2
      ))
      val s2 = sphere(transform = Scaling(0.5, 0.5, 0.5))
      val w = defaultWorld()
      assert(w.lights.head == light)
      assert(w.objects.contains(s1))
      assert(w.objects.contains(s2))
    }

    scenario("Intersect a world with a ray") {
      Given("w ← default_world()")
      And("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      When("xs ← intersect_world(w, r)")
      Then("xs.size = 4")
      And("xs(0).t = 4")
      And("xs(1).t = 4.5")
      And("xs(2).t = 5.5")
      And("xs(3).t = 6")
      val w = defaultWorld()
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val xs = w.intersect(r)
      assert(xs.size == 4)
      assert(xs.head.t == 4)
      assert(xs(1).t == 4.5)
      assert(xs(2).t == 5.5)
      assert(xs(3).t == 6)
    }

    scenario("Shading an intersection") {
      Given("w ← default_world()")
      And("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      And("shape ← the first object in w")
      And("i ← intersection(4, shape)")
      When("comps ← prepareComputations(i, r)")
      And("c ← shadeHit(w, comps)")
      Then("c = color(0.38066, 0.47583, 0.2855)")
      val w = defaultWorld()
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val shape = w.objects.head
      val i = Intersection(4, shape)
      val comps = prepareComputations(i, r)
      val c = w.shadeHit(comps)
      assert(c == Color(0.38066, 0.47583, 0.2855))
    }

    scenario("The color when a ray misses") {
      Given("w ← default_world()")
      And("r ← ray(point(0, 0, -5), vector(0, 1, 0))")
      When("c ← color_at(w, r)")
      Then("c = color(0, 0, 0)")
      val w = defaultWorld()
      val r = Ray(point(0, 0, -5), vector(0, 1, 0))
      val c = w.colorAt(r)
      assert(c == Color(0, 0, 0))
    }

    scenario("The color when a ray hits") {
      Given("w ← default_world()")
      And("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      When("c ← color_at(w, r)")
      Then("c = color(0.38066, 0.47583, 0.2855)")
      val w = defaultWorld()
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val c = w.colorAt(r)
      assert(c == Color(0.38066, 0.47583, 0.2855))
    }

    scenario("The color with an intersection behind the ray") {
      Given("w ← default_world()")
        And("outer ← the first object in w")
        And("outer.material.ambient ← 1")
        And("inner ← the second object in w")
        And("inner.material.ambient ← 1")
        And("r ← ray(point(0, 0, 0.75), vector(0, 0, -1))")
       When("c ← color_at(w, r)")
       Then("c = inner.material.color")

      val w = defaultWorld()
      val outer = w.objects.head.cloneWith(_.setAmbient(1))
      val inner = w.objects(1).cloneWith(_.setAmbient(1))
      val r = Ray(point(0, 0, 0.75), vector(0, 0, -1))
      val w2 = w.copy(objects = Seq(outer, inner))
      val c = w2.colorAt(r)
      assert(c == inner.material.color)
    }

    scenario("There is no shadow when nothing is collinear with point and light") {
      Given("w ← default_world()")
      And("p ← point(0, 10, 0)")
      Then("is_shadowed(w, p) is false")
      val w = defaultWorld()
      val p = point(0, 10, 0)
      assert(!w.isShadowed(p))
    }

    scenario("The shadow when an object is between the point and the light") {
      Given("w ← default_world()")
      And("p ← point(10, -10, 10)")
      Then("is_shadowed(w, p) is true")
      val w = defaultWorld()
      val p = point(10, -10, 10)
      assert(w.isShadowed(p))
    }

    scenario("There is no shadow when an object is behind the light") {
      Given("w ← default_world()")
      And("p ← point(-20, 20, -20)")
      Then(" w.isShadowed(, p) is false")
      val w = defaultWorld()
      val p = point(-20, 20, -20)
      assert(!w.isShadowed(p))
    }

    scenario("There is no shadow when an object is behind the point") {
      Given("w ← default_world()")
      And("p ← point(-2, 2, -2)")
      Then("w.isShadowed(, p) is false")
      val w = defaultWorld()
      val p = point(-2, 2, -2)
      assert(!w.isShadowed(p))
    }

    scenario("shadeHit() is given an intersection in shadow") {
      Given("w ← default_world()")
      And("w.light ← point_light(point(0, 0, -10), color(1, 1, 1))")
      And("s1 ← sphere()")
      And("s1 is added to w")
      And("""s2 ← sphere() with:
            | transform | translation(0, 0, 10) |""")
      And("s2 is added to w")
      And("r ← ray(point(0, 0, 5), vector(0, 0, 1))")
      And("i ← intersection(4, s2)")
      When("comps ← prepareComputations(i, r)")
      And(" c ← shadeHit(w, comps)")
      Then("c = color(0.1, 0.1, 0.1)")
      val s2 = sphere(Translation(0, 0, 10))
      val w = defaultWorld()
        .setLight(PointLight(point(0, 0, -10), Color(1, 1, 1)))
        .add(sphere())
        .add(s2)
      val r = Ray(point(0, 0, 5), vector(0, 0, 1))
      val i = Intersection(4, s2)
      val comps = prepareComputations(i, r)
      val c = w.shadeHit(comps)
      assert(c == Color(0.1, 0.1, 0.1))
    }

    scenario("Shading an intersection from the inside") {
      Given("w ← default_world()")
      And(" w.light ← point_light(point(0, 0.25, 0), color(1, 1, 1))")
      And("r ← ray(point(0, 0, 0), vector(0, 0, 1))")
      And("shape ← the second object in w")
      And(" i ← intersection(0.5, shape)")
      When("comps ← prepareComputations(i, r)")
      And("c ← shadeHit(w, comps)")
      Then("c = color(0.90498, 0.90498, 0.90498)")
      val w = defaultWorld()
        .setLight(PointLight(point(0, 0.25, 0), Color(1, 1, 1)))
      val r = Ray(point(0, 0, 0), vector(0, 0, 1))
      val shape = w.objects(1)
      val i = Intersection(0.5, shape)
      val comps = prepareComputations(i, r)
      val c = w.shadeHit(comps)
      assert(c == Color(0.90498, 0.90498, 0.90498))
    }

    scenario("The reflected color for a non-reflective material") {
      Given("w ← default_world()")
      And("r ← ray(point(0, 0, 0), vector(0, 0, 1))")
      And(" shape ← the second object in w")
      And("shape.material.ambient ← 1")
      And("i ← intersection(1, shape)")
      When("comps ← prepareComputations(i, r)")
      And("color ← reflected_color(w, comps)")
      Then("color = color(0, 0, 0)")
      val r = Ray(point(0, 0, 0), vector(0, 0, 1))
      val w = defaultWorld()
        .updateObject(1)(
          _.cloneWith(_.setAmbient(1))
        )
      val shape = w.objects(1)
      val i = Intersection(1, shape)
      val comps = prepareComputations(i, r)
      val color = w.reflectedColor(comps)
      assert(color == Color.Black)
    }

    scenario("The reflected color for a reflective material") {
      Given("w ← default_world()")
      And(
        """
          | shape ← plane() with:
          |   material.reflective | 0.5                   |
          |   transform           | translation(0, -1, 0) |""".stripMargin)
      And("shape is added to w")
      And("r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2)) ")
      And("i ← intersection(√2, shape)")
      When("comps ← prepareComputations(i, r)")
      And("color ← reflected_color(w, comps)")
      Then("color = color(0.19032, 0.2379, 0.14274)")

      val shape = Shape().translate(0, -1, 0)  .setReflective(0.5).plane
      val w = defaultWorld().add(shape)
      val r = Ray(point(0, 0, -3), vector(0, -sqrt(2)/2, sqrt(2)/2))
      val i = Intersection(sqrt(2), shape)
      val comps = prepareComputations(i, r)
      val c = w.reflectedColor(comps)
      assert(c == Color(0.19032, 0.2379, 0.14274))
    }

    scenario("shadeHit() with a reflective material") {
      Given("w ← default_world()")
      And(
        """shape ← plane() with:
          |   material.reflective | 0.5                   |
          |   transform           | translation(0, -1, 0) |""".stripMargin)
      And("shape is added to w")
      And("r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2)) ")
      And("i ← intersection(√2, shape)")
      When("comps ← prepareComputations(i, r)")
      And("color ← reflected_color(w, comps)")
      Then("color =  color(0.87677, 0.92436, 0.82918)")

      val shape = plane(Translation(0, -1, 0), Material(reflective = 0.5))
      val w = defaultWorld().add(shape)
      val r = Ray(point(0, 0, -3), vector(0, -sqrt(2)/2, sqrt(2)/2))
      val i = Intersection(sqrt(2), shape)
      val comps = prepareComputations(i, r)
      val c = w.shadeHit(comps)
      assert(c ==  Color(0.87677, 0.92436, 0.82918))
    }

    scenario("color_at() with mutually reflective surfaces") {

      Given("w ← world()")
      And("w.light ← point_light(point(0, 0, 0), color(1, 1, 1))")
      And("""lower ← plane() with:
        |      | material.reflective | 1                     |
        |      | transform           | translation(0, -1, 0) |""")
      And("lower is added to w")
      And("""upper ← plane() with:
        |      | material.reflective | 1                    |
        |      | transform           | translation(0, 1, 0) |""")
      And("upper is added to w")
      And("r ← ray(point(0, 0, 0), vector(0, 1, 0))")
      Then("color_at(w, r) should terminate successfully")

      val w = World.empty
        .setLight(PointLight(point(0, 0, 0), Color(1, 1, 1)))
        .add(plane(Translation(0, -1, 0), Material().setReflective(1)))
        .add(plane(Translation(0, 1, 0), Material().setReflective(1)))

      val r = Ray(point(0, 0, 0), vector(0, 1, 0))

      // WARNING IF NOT SUPPORTED THIS WILL NOT RETURN
      val c = w.colorAt(r)

      println(w)
    }

    scenario("The reflected color at the maximum recursive depth") {
      Given("w ← default_world()")
      And("""shape ← plane() with:
          |        | material.reflective | 0.5                   |
          |        | transform           | translation(0, -1, 0) |""")
      And("shape is added to w")
      And("r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))")
      And("i ← intersection(√2, shape)")
      When("comps ← prepareComputations(i, r)")
      And("color ← reflected_color(w, comps, 0)")
      Then(" color = color(0, 0, 0)")

      val shape = plane(Translation(0, -1, 0), Material().setReflective(0.5))
      val w = defaultWorld()
        .add(shape)
      val r =  Ray(point(0, 0, -3), vector(0, -sqrt(2)/2, sqrt(2)/2))
      val i = Intersection(sqrt(2), shape)
      val comps = prepareComputations(i, r)
      val color = w.reflectedColor(comps, 0)
      assert(color == Color.Black)
    }

    scenario("The refracted color with an opaque surface") {
      Given("w ← default_world()")
      And("shape ← the first object in w")
      And("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      And("xs ← intersections(4:shape, 6:shape)")
      When("comps ← prepareComputations(xs[0], r, xs)")
      And("c ← refracted_color(w, comps, 5)")
      Then("c = color(0, 0, 0)")

      val w = defaultWorld()
      val shape = w.objects.head
      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val xs = intersectionPairs( (4, shape), (6, shape))
      val comps = prepareComputations(xs.head, r, xs)
      val c = w.refractedColor(comps, 5)
      assert(c == Color.Black)
    }
    scenario("The refracted color at the maximum recursive depth") {
      Given("w ← default_world()")
      And("shape ← the first object in w")
      And("""shape has:
        |        | material.transparency     | 1.0 |
        |        | material.refractive_index | 1.5 |""")
      And("r ← ray(point(0, 0, -5), vector(0, 0, 1))")
      And("xs ← intersections(4:shape, 6:shape)")
      When("comps ← prepareComputations(xs[0], r, xs)")
      And("c ← refracted_color(w, comps, 0)")
      Then("c = color(0, 0, 0)")

      val shape =  defaultWorld()
        .objects.head
        .cloneWith(_.setTransparency(1.0).setReflective(1.5))
      val w = defaultWorld().setObjectAt(0, shape)

      val r = Ray(point(0, 0, -5), vector(0, 0, 1))
      val xs = intersectionPairs((4, shape), (6, shape))
      val comps = prepareComputations(xs.head, r, xs)
      val c =  w.refractedColor(comps, 0)
      assert(c == Color.Black)
    }

    scenario("The refracted color under total internal reflection") {
      Given(" w ← default_world()")
      And("shape ← the first object in w")
      And("""shape has:
        |        | material.transparency     | 1.0 |
        |        | material.refractive_index | 1.5 |""")
      And("r ← ray(point(0, 0, √2/2), vector(0, 1, 0))")
      And("xs ← intersections(-√2/2:shape, √2/2:shape)")
//      # NOTE: this time you're inside the sphere, so you need
//      # to look at the second intersection, xs[1], not xs[0]
      When("comps ← prepareComputations(xs[1], r, xs)")
      And("c ← refracted_color(w, comps, 5)")
      Then("c = color(0, 0, 0)")

      val shape = defaultWorld()
        .objects.head
        .cloneWith(_.setTransparency(1.0).setRefractiveIndex(1.5))
      val w = defaultWorld().setObjectAt(0, shape)

      val r = Ray(point(0, 0, sqrt(2)/2), vector(0, 1, 0))
      val xs = intersectionPairs((-sqrt(2)/2, shape), (sqrt(2)/2, shape))

      val comps = prepareComputations(xs(1), r, xs)
      val c = w.refractedColor(comps, 5)
      assert(c == Color.Black)
    }

    scenario("The refracted color with a refracted ray") {
      Given("w ← default_world()")
      And("A ← the first object in w")
      And("""A has:
        |        | material.ambient | 1.0            |
        |        | material.pattern | test_pattern() |""")
      And("B ← the second object in w")
      And("""B has:
        |        | material.transparency     | 1.0 |
        |        | material.refractive_index | 1.5 |""")
      And("r ← ray(point(0, 0, 0.1), vector(0, 1, 0))")
      And("xs ← intersections(-0.9899:A, -0.4899:B, 0.4899:B, 0.9899:A)")
      When("comps ← prepareComputations(xs[2], r, xs)")
      And("c ← refracted_color(w, comps, 5)")
      Then("c = color(0, 0.99888, 0.04725)")

      val A = defaultWorld().first.cloneWith(_.setAmbient(1.0).setPattern(TestPattern()))
      val B = defaultWorld().second.cloneWith(_.setTransparency(1.0).setRefractiveIndex(1.5))
      val w = defaultWorld().copy(objects = List(A, B))

      assert(w.objects(0).material.ambient == 1.0)
      assert(w.objects(0).material.pattern.contains(TestPattern()))

      assert(w.objects(1).material.refractiveIndex == 1.5)
      assert(w.objects(1).material.transparency == 1.0)

      val r = Ray(point(0, 0, 0.1), vector(0, 1, 0))
      val xs = intersectionPairs((-0.9899, A), (-0.4899, B), (0.4899, B), (0.9899, A))
      val comps = prepareComputations(xs(2), r, xs)
      val c = w.refractedColor(comps, 5)

      assert(w.objects(1).material.transparency == 1.0)
      assert(w.objects(1).material.color == Color(1, 1, 1))
      assert(c == Color(0, 0.99888, 0.04725))
    }

    scenario("shadeHit() with a transparent material") {
      Given("w ← default_world()")
      And("""floor ← plane() with:
        |    | transform                 | translation(0, -1, 0) |
        |    | material.transparency     | 0.5                   |
        |    | material.refractive_index | 1.5                   |""")
        And(" floor is added to w")
        And("""ball ← sphere() with:
          |    | material.color     | (1, 0, 0)                  |
          |    | material.ambient   | 0.5                        |
          |    | transform          | translation(0, -3.5, -0.5) |""")
      And("ball is added to w")
      And("r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))")
      And("xs ← intersections(√2:floor)")
      When("comps ← prepareComputations(xs[0], r, xs)")
      And("color ← shadeHit(w, comps, 5)")
      Then("color = Color(0.93642, 0.68642, 0.68642)")

      val floor = plane(
        Translation(0, -1, 0),
        Material()
          .setTransparency(0.5)
          .setRefractiveIndex(1.5)
      )
      val ball = sphere(
        Translation(0, -3.5, -0.5),
        Material().setColor(1, 0, 0).setAmbient(0.5))
      val w = defaultWorld() //.copy(objects = Seq(floor, ball))
        .add(floor)
        .add(ball)
      val r = Ray(point(0, 0, -3), vector(0, -sqrt(2)/2, sqrt(2)/2))
      val xs = intersectionPairs((sqrt(2), floor))
      val comps = prepareComputations(xs.head, r, xs)
      val color = w.shadeHit(comps, 5)
      assert(color == Color(0.93642, 0.68642, 0.68642))
    }

    scenario("shadeHit() with a reflective, transparent material") {
      Given("w ← default_world()")
        And("r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))")
        And("""floor ← plane() with:
            |          | transform                 | translation(0, -1, 0) |
            |          | material.reflective       | 0.5                   |
            |          | material.transparency     | 0.5                   |
            |          | material.refractive_index | 1.5                   |""".stripMargin)
        And("floor is added to w")
        And("""ball ← sphere() with:
              |          | material.color     | (1, 0, 0)                  |
              |          | material.ambient   | 0.5                        |
              |          | transform          | translation(0, -3.5, -0.5) |""".stripMargin)
        And("ball is added to w")
        And("xs ← intersections(√2:floor)")
       When("comps ← prepareComputations(xs[0], r, xs)")
        And("color ← shadeHit(w, comps, 5)")
       Then("color = color(0.93391, 0.69643, 0.69243)")


      val r = Ray(point(0, 0, -3), vector(0, -sqrt(2.0)/2.0, sqrt(2.0)/2.0))

      val floor = Shape()
        .translate(0, -1, 0)
        .setReflective(0.5)
        .setTransparency(0.5)
        .setRefractiveIndex(1.5)
        .plane

      val ball = Shape()
        .setColor(1, 0, 0)
        .setAmbient(0.5)
        .translate(0, -3.5, -0.5)
        .sphere
      val w = defaultWorld()
        .add(floor)
        .add(ball)

      val xs = intersectionPairs((sqrt(2), floor))
      val comps = prepareComputations(xs.head, r, xs)
      val color = w.shadeHit(comps, 5)
      assert(color == Color(0.93391, 0.69643, 0.69243))

    }
  }

}