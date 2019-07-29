package raytracer

import scala.math.abs

/**
  * Optimized 4x4 matrix
  */
final case class Matrix(private val m: Array[Double]) {
  require(m.size == 16)

  val rows: Int = 4
  val columns: Int = 4

  private lazy val inverseResult: Option[(Double, Array[Double])] = Matrix.inverse(m)
  private lazy val inverseOpt = inverseResult.map(pair => Matrix(pair._2))

  def apply(row: Int, col: Int): Double = {
    require(row >= 0 && col >= 0 && row < rows && col < columns, "Index out of bounds")
    m((row * columns) + col)
  }

  def row(index: Int): IndexedSeq[Double] = {
    require(index >= 0 && index < 4, "Row index should be (0..3)")
    (0 until 4).map(c => this(index, c))
  }

  def col(index: Int): IndexedSeq[Double] = {
    require(index >= 0 && index < 4, "Col index should be (0..3)")
    (0 until 4).map(r => this(r, index))
  }

  def determinant: Double = inverseResult.fold(0.0)(_._1)
  def isInvertible: Boolean = determinant != 0.0

  def inverse: Matrix = {
    inverseOpt getOrElse {
      throw new RuntimeException(s"The matrix has no inverse: $this")
    }
  }

  def transpose: Matrix = new Matrix(Matrix.transpose(m))

  def *(rhs: Tuple4): Tuple4 = {
    val x = m(0) * rhs.x + m(1) * rhs.y + m(2) * rhs.z + m(3) * rhs.w
    val y = m(4) * rhs.x + m(5) * rhs.y + m(6) * rhs.z + m(7) * rhs.w
    val z = m(8) * rhs.x + m(9) * rhs.y + m(10) * rhs.z + m(11) * rhs.w
    val w = m(12) * rhs.x + m(13) * rhs.y + m(14) * rhs.z + m(15) * rhs.w
    Tuple4(x, y, z, w)
  }

  def *(rhs: Point3D): Point3D = {
    // w = 1.0
    val x = m(0) * rhs.x + m(1) * rhs.y + m(2) * rhs.z + m(3)   // * rhs.w
    val y = m(4) * rhs.x + m(5) * rhs.y + m(6) * rhs.z + m(7)   //* rhs.w
    val z = m(8) * rhs.x + m(9) * rhs.y + m(10) * rhs.z + m(11) //* rhs.w
    Point3D(x, y, z)
  }

  def *(rhs: Vector3D): Vector3D = {
    // Since w = 0
    val x = m(0) * rhs.x + m(1) * rhs.y + m(2) * rhs.z //+ m(3) * rhs.w
    val y = m(4) * rhs.x + m(5) * rhs.y + m(6) * rhs.z //+ m(7) * rhs.w
    val z = m(8) * rhs.x + m(9) * rhs.y + m(10) * rhs.z //+ m(11) * rhs.w
    Vector3D(x, y, z)
  }

  def *(other: Matrix): Matrix = Matrix(Matrix.multiply(this.m, other.m))

  def ~=(rhs: Matrix): Boolean = Matrix.isEquals(this.m, rhs.m)

  override def equals(other: Any): Boolean = other match {
    case rhs: Matrix => Matrix.isEquals(this.m, rhs.m)
    case _ => false
  }

  override def toString: String = Matrix.show(this, 1)
}


object Matrix {

  val identity: Matrix = Matrix(
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
  )

  def apply(
    m0: Double, m1: Double, m2: Double, m3: Double,
    m4: Double, m5: Double, m6: Double, m7: Double,
    m8: Double, m9: Double, m10: Double, m11: Double,
    m12: Double, m13: Double, m14: Double, m15: Double): Matrix = {
    new Matrix(
      Array(
        m0, m1, m2, m3,
        m4, m5, m6, m7,
        m8, m9, m10, m11,
        m12, m13, m14, m15
      )
    )
  }


  def show(m: Matrix, indent: Int): String = {
    val cols = m.columns
    val tabs = "\t" * indent
    val sx = m.m.map(doubleToString)
    val sep = s"\n$tabs"
    val width = sx.maxBy(_.size).size
    val padded = sx.map(s => " " * (width - s.size) + s)
    padded
      .zipWithIndex
      .map {
        case (s, index) =>
          if ((index % cols == 0) && index != 0) s"|$sep| $s "
          else s"| $s "
      }
      .mkString(sep, "", s"|\n")
  }

  def parse(str: String): Matrix = {
    val array = str.split('|').map(_.trim).filterNot(_.isEmpty).map(_.toDouble)
    Matrix(array)
  }

  private def inverse(m: Array[Double]): Option[(Double, Array[Double])] = {

    val m00 = m(0)
    val m01 = m(1)
    val m02 = m(2)
    val m03 = m(3)

    val m10 = m(4)
    val m11 = m(5)
    val m12 = m(6)
    val m13 = m(7)

    val m20 = m(8)
    val m21 = m(9)
    val m22 = m(10)
    val m23 = m(11)

    val m30 = m(12)
    val m31 = m(13)
    val m32 = m(14)
    val m33 = m(15)

    val A2323 = m22 * m33 - m23 * m32
    val A1323 = m21 * m33 - m23 * m31
    val A1223 = m21 * m32 - m22 * m31
    val A0323 = m20 * m33 - m23 * m30
    val A0223 = m20 * m32 - m22 * m30
    val A0123 = m20 * m31 - m21 * m30
    val A2313 = m12 * m33 - m13 * m32
    val A1313 = m11 * m33 - m13 * m31
    val A1213 = m11 * m32 - m12 * m31
    val A2312 = m12 * m23 - m13 * m22
    val A1312 = m11 * m23 - m13 * m21
    val A1212 = m11 * m22 - m12 * m21
    val A0313 = m10 * m33 - m13 * m30
    val A0213 = m10 * m32 - m12 * m30
    val A0312 = m10 * m23 - m13 * m20
    val A0212 = m10 * m22 - m12 * m20
    val A0113 = m10 * m31 - m11 * m30
    val A0112 = m10 * m21 - m11 * m20

    val det = (
      (m00 * ( m11 * A2323 - m12 * A1323 + m13 * A1223 )) -
      (m01 * ( m10 * A2323 - m12 * A0323 + m13 * A0223 )) +
      (m02 * ( m10 * A1323 - m11 * A0323 + m13 * A0123 )) -
      (m03 * ( m10 * A1223 - m11 * A0223 + m12 * A0123 ))
    )

    if (det == 0.0) None
    else {
      val invDet = 1.0 / det

      val inv = Array(
        invDet *   ( m11 * A2323 - m12 * A1323 + m13 * A1223 ),
        invDet * - ( m01 * A2323 - m02 * A1323 + m03 * A1223 ),
        invDet *   ( m01 * A2313 - m02 * A1313 + m03 * A1213 ),
        invDet * - ( m01 * A2312 - m02 * A1312 + m03 * A1212 ),
        invDet * - ( m10 * A2323 - m12 * A0323 + m13 * A0223 ),
        invDet *   ( m00 * A2323 - m02 * A0323 + m03 * A0223 ),
        invDet * - ( m00 * A2313 - m02 * A0313 + m03 * A0213 ),
        invDet *   ( m00 * A2312 - m02 * A0312 + m03 * A0212 ),
        invDet *   ( m10 * A1323 - m11 * A0323 + m13 * A0123 ),
        invDet * - ( m00 * A1323 - m01 * A0323 + m03 * A0123 ),
        invDet *   ( m00 * A1313 - m01 * A0313 + m03 * A0113 ),
        invDet * - ( m00 * A1312 - m01 * A0312 + m03 * A0112 ),
        invDet * - ( m10 * A1223 - m11 * A0223 + m12 * A0123 ),
        invDet *   ( m00 * A1223 - m01 * A0223 + m02 * A0123 ),
        invDet * - ( m00 * A1213 - m01 * A0213 + m02 * A0113 ),
        invDet *   ( m00 * A1212 - m01 * A0212 + m02 * A0112 )
      )
      Some((det, inv))
    }

  }

  private def isEquals(lhs: Array[Double], rhs: Array[Double]): Boolean = {
    require(lhs.size == rhs.size, "both arrays should be size size")
    (0 until 16).forall(i => abs(lhs(i) - rhs(i)) < EPSILON_TEST)
  }

  private def multiply(a: Array[Double], b: Array[Double]): Array[Double] = {
    @inline def A(row: Int, col: Int): Double = a((row * 4) + col)
    @inline def B(row: Int, col: Int): Double = b((row * 4) + col)
    val out = new Array[Double](16)
    (0 until 4) foreach { i =>
      val ai0 = A(i,0)
      val ai1 = A(i,1)
      val ai2 = A(i,2)
      val ai3 = A(i,3)
      out((i * 4) + 0) = ai0 * B(0,0) + ai1 * B(1,0) + ai2 * B(2,0) + ai3 * B(3,0)
      out((i * 4) + 1) = ai0 * B(0,1) + ai1 * B(1,1) + ai2 * B(2,1) + ai3 * B(3,1)
      out((i * 4) + 2) = ai0 * B(0,2) + ai1 * B(1,2) + ai2 * B(2,2) + ai3 * B(3,2)
      out((i * 4) + 3) = ai0 * B(0,3) + ai1 * B(1,3) + ai2 * B(2,3) + ai3 * B(3,3)
    }
    out
  }

  private def transpose(from: Array[Double]): Array[Double] = {
    val to = new Array[Double](16)
    to(0)  = from(0)
    to(1)  = from(4)
    to(2)  = from(8)
    to(3)  = from(12)
    to(4)  = from(1)
    to(5)  = from(5)
    to(6)  = from(9)
    to(7)  = from(13)
    to(8)  = from(2)
    to(9)  = from(6)
    to(10) = from(10)
    to(11) = from(14)
    to(12) = from(3)
    to(13) = from(7)
    to(14) = from(11)
    to(15) = from(15)
    to
  }
}