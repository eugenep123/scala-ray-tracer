package raytracer
import raytracer.Operations._


object Defaults {

  final val MaxRecursion = 3

  val width = 400
  val height = 400
  val camera: Camera = new Camera(
    width,
    height,
    π / 3,
    viewTransform(
      from = Point3D(0, 1.5, -5),
      to = Point3D(0, 1, 0),
      up = Vector3D(0, 1, 0))
  )

  val material = new Material(
    Color.White,
    ambient = 0.1,
    diffuse = 0.9,
    specular = 0.9,
    shininess = 200.0,
    reflective = 0.0,
    transparency = 0.0,
    refractiveIndex = 1.0,
    pattern = None
  )


  def world: World = {
    val light = PointLight(Point3D(-10, 10, -10), Color(1, 1, 1))
    val s1 = Shapes.sphere(
      material = Material(
      Color(0.8, 1.0, 0.6),
      diffuse = 0.7,
      specular = 0.2
    ))
    val s2 = Shapes.sphere(Operations.scaling(0.5, 0.5, 0.5))
    val objects = Seq(s1, s2)
    new World(objects, Seq(light))
  }

  object Materials {
    val glass = Material(transparency = 1.0, refractiveIndex = 1.5)
    //ambient to 0, and diffuse to (1 - transparency)
    ///(0.5, 0.5, 0) for the Color and Transparency = 1. Also Ambient is 0 or 0.1
    val boundingBoxMaterial = Material(
      Color(0.5, 0.5, 0),
      ambient = 0.1, //0.1, // or 0
      diffuse = (1.0 - 0.8), //0.0,
      specular = 0.0,
      shininess = 0,
      reflective = 0.0,
      transparency = 1.0,
      refractiveIndex = 1
    )
  }

}
