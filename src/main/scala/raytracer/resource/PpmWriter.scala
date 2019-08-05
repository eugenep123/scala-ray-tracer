package raytracer.resource

import raytracer.Canvas

object PpmWriter {
  val MAX_PPM_LINE_LENGTH = 70

  def apply(c: Canvas): Seq[String] = {
    import c._
    val header = Seq(
      "P3",
      s"$width $height",
      "255"
    )

    val body = (0 until height)
      .map(y => (0 until width)
        .map(x => c(x, y).rgbString).mkString(" ")
      )
      .flatMap(splitLine)

    header ++ body :+ "\n"
  }

  private def splitLine(line: String): Seq[String] = {
    if (line.length < MAX_PPM_LINE_LENGTH) Seq(line)
    else {
      val (head, rest) = line.splitAt(
        line.lastIndexWhere(_.isSpaceChar, MAX_PPM_LINE_LENGTH)
      )
      Seq(head) ++ splitLine(rest.trim)
    }
  }


  def save(c: Canvas, folder: String): Unit = {
    import java.io.{File, PrintWriter}
    val name = s"canvas_${System.currentTimeMillis}.ppm"
    val outputFile = new File(s"$folder/$name")
    val writer = new PrintWriter(outputFile)

    val lines = PpmWriter(c)
    lines.foreach { line =>
      writer.write(line)
      writer.write("\n")
    }
    writer.close()
    println(s"File written to : $outputFile")
  }
}
