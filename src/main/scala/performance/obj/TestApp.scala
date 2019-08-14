package performance.obj

import raytracer.resource._

object TestApp extends App {


  val input =
    """mtllib default.mtl
      |
      |#
      |# object Teapot001
      |#
      |
      |v  33.647778 59.807484 4.662212
      |v  33.322739 59.872997 4.617175
      |v  34.018166 59.610893 4.713532
      |v  34.326466 59.610893 -0.000000
      |v  34.417419 59.283257 4.768853
      |v  34.729336 59.283257 -0.000000
      |v 0 1 0
      |v -1 0 0
      |v 1 0 0
      |
      |vn -1 0 0
      |vn 1 0 0
      |vn 0 1 0
      |
      |# blee
      |vt 0.034025 0.519450 -0.167330
      |# 6460 texture coords
      |
      |g Base
      |usemtl default
      |vt 0.001157 0.646129 -0.167330
      |vt 0.033605 0.520720 -0.167330
      |vt 0.034025 0.519450 -0.167330
      |# 6460 texture coords
      |
      |g Base
      |usemtl default
      |f 2/3 1/4 1536/1 1537/2
      |f 1/4 3/6 4/5 1536/1
      |f 3/6 5/8 6/7 4/5
      |f 5/8 7/10 8/9 6/7
      |f 7/10 9/12 10/11 8/9
      |f 9/12 11/14 12/13 10/11
      |f 13/16 1/4 2/3 14/15
      |f 15/17 3/6 1/4 13/16
      |f 15/17 16/18 5/8 3/6
      |f 16/18 17/19 7/10 5/8
      |f 17/19 18/20 9/12 7/10
      |f 18/20 19/21 11/14 9/12
      |f 20/23 13/16 14/15 21/22""".stripMargin


val mtl = """# Default material file.  Created by Morgan McGuire and released into
            |# the Public Domain on July 16, 2011.
            |#
            |# http://graphics.cs.williams.edu/data
            |
            |newmtl default
            |  Ns 10.0000
            |  Ni 1.5000
            |  Tr 0  0
            |  illum 2
            |  Ka 1 1 1
            |  Kd 1 1 1
            |  Ks 0.2 0.2 0.2
            |  Ke 0 0 0
            |  map_Kd default.png
            |# MTL written from /Volumes/HD2/classes/rt/candy1.obj
            |newmtl candy1
            |Kd 0.8 0.010858 0.00458411
            |Ns 256
            |d 1
            |illum 2
            |Ka 0 0 0
            |Ks 0.2 0.2 0.2
            |""".stripMargin



//  val parser = new ObjParser(ObjectHandler.printHandler)
//  parser.parse(input)

val parser = new MaterialLibParser(MaterialHandler.print)
parser.parse(mtl)

////  val filename = "dragon.obj"
//  val filename = "teapot.obj"
//  val resourceName = s"/objects/$filename"
//  val content = getResourceString(resourceName)


//
////  val filename = "../assets/bunny.obj"
//  val filename = "../assets/teapot/teapot.obj"
//  val content = readFile(filename)
//
//
//
//  val parser = new ObjParser(new ObjBuilder)
//  val result = parser.parse(content)
//  println(result)
}
