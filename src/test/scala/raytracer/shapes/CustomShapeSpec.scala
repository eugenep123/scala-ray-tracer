//package raytracer
//package shapes
//
//import scala.collection.mutable
//
//class CustomShapeSpec extends BaseSpec {
//
//  feature("Shapes") {
//    scenario("calculate the path of a shape") {
//      val g1 = group()
//      setTransform(g1, rotationY(π/2))
//
//      val g2 = group()
//      setTransform(g2, scaling(1, 2, 3))
//      addChild(g1, g2)
//
//      val s = sphere()
//      setTransform(s, translation(5, 0, 0))
//      addChild(g2, s)
//
//      val p = point(1.7321, 1.1547, -5.5774)
//      val expected = vector(0.2857, 0.4286, -0.8571)
//      val n = normalAt(s, p)
//      assert(n == expected)
//
//      // Precalculate the transform from g1 => g2 => s
//      assert(s.path == List(g1, g2, s))
//
//      val m = s.path.reverse.map(_.transform.inverse).reduce(_ * _)
//      val localPoint = m * p
//      val localNormal = s.localNormalAt(localPoint, null)
//      val n2 = s.normalToWorld(localNormal)
//      assert(n2 == expected)
//      // If we can cache this reliably per shape, we dont need to multiply all the way fro every shape and point
//
//      type Path = List[Shape]
//
//      val mkTransform = (p: Path) => {
//        println("Calculating")
//        p.reverse.map(_.transform.inverse).reduce(_ * _)
//      }
//      val cache = new PathCache[Matrix](mkTransform)
//
//
//      assert(!cache.contains(s.path))
//      assert(!cache.contains(s.path))
//
//      val m2 = cache.get(s.path)
//      assert(cache.contains(s.path))
//      assert(cache.get(s.path) == m2)
//      val localPoint2 = m2 * p
//      val localNormal2 = s.localNormalAt(localPoint2, null)
//      val n3 = s.normalToWorld(localNormal2)
//      assert(n3 == expected)
//
//    }
//  }
//}
//
//class PathCache[A](calc: List[Shape] => A) {
//  type Path = List[Shape]
//
//  private def mkKey(path: Path) = path.map(_.transform.hashCode()).mkString(",")
//
//  private var store = new mutable.HashMap[String, A]()
//
//  def store(path: Path, value: A): Unit = {
//    val key = mkKey(path)
//    this.store += (key -> value)
//  }
//
//  def contains(path: Path): Boolean = {
//    val key = mkKey(path)
//    store.contains(key)
//  }
//
//  def get(path: Path): A = {
//    val key = mkKey(path)
//    println(s"key: $key")
//    store.get(key) match {
//      case Some(value) => value
//      case _ =>
//        val value = calc(path)
//        this.store += (key -> value)
//        value
//    }
//  }
//}
