package chapters

import raytracer.shapes._

object Chapter14 extends RenderApp {

  val hexagon = Hexagon.build

  override def objects: Seq[Shape] = Seq(hexagon)
}
