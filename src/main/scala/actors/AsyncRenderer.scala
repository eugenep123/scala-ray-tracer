package actors

import java.util.concurrent.atomic.AtomicBoolean

import actors.TestApp.context
import raytracer.{Camera, Color, Scene, World}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.util.Random

object AsyncRenderer {

  trait RenderHandler {
    def onStart(width: Int, height: Int): Unit
    def onWork(result: PixelResult): Unit
    def onCompleted(): Unit
    def onCanceled(): Unit
  }

  case class PixelToRender(x: Int, y: Int, size: Int)
  case class PixelResult(pixel: PixelToRender, color: Color)

  // For now we just shuffle it (later, we need a progressive loader order
  private def getPixes(camera: Camera): Seq[PixelToRender] = {
    val xs = camera.pixels.map { case (x, y) => PixelToRender(x, y, 1) }
    Random.shuffle(xs)
  }

  def renderPixel(
    canceledRef: AtomicBoolean,
    camera: Camera,
    world: World,
    pixel: PixelToRender): Option[PixelResult] = {
    if (canceledRef.get()) None
    else {
      val ray = camera.rayForPixel(pixel.x, pixel.y)
      val color = world.colorAt(ray)
      val result = PixelResult(pixel, color)
      Some(result)
    }
  }

  def renderAsync(scene: Scene, handler: RenderHandler)(implicit ec: ExecutionContext): RenderRef = {
    val camera = scene.camera
    val world = scene.world
    val cancelRef = new AtomicBoolean(false)
    val pixels = getPixes(camera)
    val future = Future {
      blocking {
        pixels.par
          .map { pixel =>
            renderPixel(cancelRef, camera, world, pixel)
          }
          .foreach { result =>
            result.foreach(handler.onWork)
          }
        if (cancelRef.get()) handler.onCanceled()
        else handler.onCompleted()
      }
    }
    RenderRef(future, cancelRef)
  }

  case class RenderRef(future: Future[Unit], cancelRef: AtomicBoolean) {
    def cancel(): Future[Unit] = {
      cancelRef.set(true)
      future
    }
    def waitTillDone(): Unit = {
      Await.result(context.future, Duration.Inf)
    }
  }
}
