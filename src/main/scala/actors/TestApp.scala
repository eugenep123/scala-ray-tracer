package actors


import chapters.Settings
import raytracer.{Canvas, Color, Scene}

import scala.concurrent.ExecutionContext.Implicits.global

object TestApp extends App {
  import AsyncRenderer._

  val resource = "/examples/reflect-refract.yml"
  val scene = Scene
    .fromResourceYaml(resource)
    .withDimensions(1024,768)


  val handler = new RenderHandler {
    val canvas = Canvas(scene.camera.width, scene.camera.height)
    override def onStart(width: Int, height: Int): Unit = {
      canvas.fill(Color.Black)
    }
    override def onWork(result: PixelResult): Unit = {
      import result._
      canvas.writePixel(pixel.x, pixel.y, color)
      println(result)
    }
    override def onCompleted(): Unit = {
      canvas.save(Settings.folder)
    }
    override def onCanceled(): Unit = {}
  }

  val context = renderAsync(scene, handler)
  context.waitTillDone()
  println("done")





}
