package chapters

import raytracer.math.Operations.viewTransform
import raytracer.{Camera, Color, Material}
import raytracer.shapes.{Csg, Cube, Cylinder, Shape, Sphere}
import raytracer.math._

object RenderDice extends RenderApp {
  val dice = Dice.build()

  override def objects: Seq[Shape] = Seq(dice)

  override def camera: Camera =
    Camera(width, height, 1.5,
      viewTransform(Point3D(2, 4, -7), Point3D(0, 1, 0), Vector3D(0, 1, 0)))
}


object Dice {

  def build(body: Color = Color.Red, dots: Color = Color.White): Shape = {
    val bodyMat = Material().setColor(body)
    val dotsMat = Material().setColor(dots)
    build(bodyMat, dotsMat, 0.18, 0.60)
  }

  def build(matBody: Material, matDots: Material, R: Double, D: Double): Shape = {
  def edge(x: Double, y: Double, z: Double, rotx: Double, rotz: Double): Cylinder = {
      Shape()
        .scale(R, 1, R)
        .rotateZ(rotz)
        .rotateX(rotx)
        .translate(x, y, z)
        .cylinder(-1, +1, true)
    }

    def corner(x: Double, y: Double, z: Double): Sphere = {
      Shape()
        .scale(R)
        .translate(x, y, z)
        .sphere
    }

    def face(x: Double, y: Double, z: Double): Cube = {
      Shape().scale(1 + x, 1 + y, 1 + z).cube
    }

    def number(x: Double, y: Double, z: Double): Sphere = {
      Shape()
        .scale(R)
        .translate(x, y, z)
        .sphere
    }

    val xs = Seq(
      edge(-1, 0, -1, 0, 0),
      edge(-1, 0, +1, 0, 0),
      edge(+1, 0, -1, 0, 0),
      edge(+1, 0, +1, 0, 0),
      edge(-1, -1, 0, PI/2, 0),
      edge(-1, +1, 0, PI/2, 0),
      edge(+1, -1, 0, PI/2, 0),
      edge(+1, +1, 0, PI/2, 0),
      edge(0, -1, -1, 0, PI/2),
      edge(0, +1, -1, 0, PI/2),
      edge(0, -1, +1, 0, PI/2),
      edge(0, +1, +1, 0, PI/2),

      corner(-1, -1, -1),
      corner(-1, +1, -1),
      corner(+1, -1, -1),
      corner(+1, +1, -1),
      corner(-1, -1, +1),
      corner(-1, +1, +1),
      corner(+1, -1, +1),
      corner(+1, +1, +1),

      face(R, 0, 0),
      face(0, R, 0),
      face(0, 0, R)
    )

    val csgBody = Csg.unionAll(xs, matBody).get

    val ys = Seq(
      number(0, 0, -1-R),  // 1
      number(1+R, -D, -D), // 2
      number(1+R, D, D),
      number(0, -1-R, 0), // 3
      number(-D, -1-R, -D),
      number(D, -1-R, D),
      number(-D, 1+R, -D), // 4
      number(-D, 1+R, +D),
      number(+D, 1+R, -D),
      number(+D, 1+R, +D),
      number(-1-R, D, D), // 5
      number(-1-R, -D, D),
      number(-1-R, -D, -D),
      number(-1-R, D, -D),
      number(-1-R, 0, 0),
      number(-D, -D, 1+R), // 6
      number(-D, 0, 1+R),
      number(-D, D, 1+R),
      number(+D, -D, 1+R),
      number(+D, 0, 1+R),
      number(+D, D, 1+R),
    )
    val csgDots = Csg.unionAll(ys, matDots).get

    val csg = Csg(Csg.Difference, csgBody, csgDots,
      Transform()
        .scale(2.0)
        .rotateZ(PI/5).rotateY(PI/5).rotateX(PI/5)
    )

    csg
  }
}