package raytracer.math

/**
  * optimized for multiply and inverse operations with as little as possible allocations
  */
final case class Matrix(
  m00: Double, m01: Double, m02: Double, m03: Double,
  m10: Double, m11: Double, m12: Double, m13: Double,
  m20: Double, m21: Double, m22: Double, m23: Double,
  m30: Double, m31: Double, m32: Double, m33: Double) {

  private lazy val inverseResult: Option[(Double, Matrix)] = calculateInverse
  private lazy val inverseOpt = inverseResult.map(_._2)

  def apply(row: Int, col: Int): Double = {
    require(row >= 0 && col >= 0 && row < 4 && col < 4, "Index out of bounds")
    this.row(row)(col)
  }

  def row(index: Int): IndexedSeq[Double] = {
    require(index >= 0 && index < 4, "Row index should be (0..3)")
    index match {
      case 0 => Vector(m00, m01, m02, m03)
      case 1 => Vector(m10, m11, m12, m13)
      case 2 => Vector(m20, m21, m22, m23)
      case 3 => Vector(m30, m31, m32, m33)
    }
  }

  def col(index: Int): IndexedSeq[Double] = {
    require(index >= 0 && index < 4, "Col index should be (0..3)")
    index match {
      case 0 => Vector(m00, m10, m20, m30)
      case 1 => Vector(m01, m11, m21, m31)
      case 2 => Vector(m02, m12, m22, m32)
      case 3 => Vector(m03, m13, m23, m33)
    }
  }

  def determinant: Double = inverseResult.fold(0.0)(_._1)
  def isInvertible: Boolean = determinant != 0.0

  def inverse: Matrix = {
    inverseOpt getOrElse {
      throw new RuntimeException(s"The matrix has no inverse: $this")
    }
  }

  def transpose: Matrix = {
    Matrix(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33
    )
  }

  def *(rhs: Tuple4): Tuple4 = {
    val x = m00 * rhs.x + m01 * rhs.y + m02 * rhs.z + m03 * rhs.w
    val y = m10 * rhs.x + m11 * rhs.y + m12 * rhs.z + m13 * rhs.w
    val z = m20 * rhs.x + m21 * rhs.y + m22 * rhs.z + m23 * rhs.w
    val w = m30 * rhs.x + m31 * rhs.y + m32 * rhs.z + m33 * rhs.w
    Tuple4(x, y, z, w)
  }

  def *(rhs: Point3D): Point3D = {
    // w = 1.0
    val x = m00 * rhs.x + m01 * rhs.y + m02 * rhs.z + m03   // * rhs.w
    val y = m10 * rhs.x + m11 * rhs.y + m12 * rhs.z + m13   //* rhs.w
    val z = m20 * rhs.x + m21 * rhs.y + m22 * rhs.z + m23   //* rhs.w
    Point3D(x, y, z)
  }

  def *(rhs: Vector3D): Vector3D = {
    // Since w = 0
    val x = m00 * rhs.x + m01 * rhs.y + m02 * rhs.z //+ m03 * rhs.w
    val y = m10 * rhs.x + m11 * rhs.y + m12 * rhs.z //+ m13 * rhs.w
    val z = m20 * rhs.x + m21 * rhs.y + m22 * rhs.z //+ m23 * rhs.w
    Vector3D(x, y, z)
  }

  def *(other: Matrix): Matrix = {
    Matrix(
      m00 * other.m00 + m01 * other.m10 + m02 * other.m20 + m03 * other.m30,
      m00 * other.m01 + m01 * other.m11 + m02 * other.m21 + m03 * other.m31,
      m00 * other.m02 + m01 * other.m12 + m02 * other.m22 + m03 * other.m32,
      m00 * other.m03 + m01 * other.m13 + m02 * other.m23 + m03 * other.m33,
      m10 * other.m00 + m11 * other.m10 + m12 * other.m20 + m13 * other.m30,
      m10 * other.m01 + m11 * other.m11 + m12 * other.m21 + m13 * other.m31,
      m10 * other.m02 + m11 * other.m12 + m12 * other.m22 + m13 * other.m32,
      m10 * other.m03 + m11 * other.m13 + m12 * other.m23 + m13 * other.m33,
      m20 * other.m00 + m21 * other.m10 + m22 * other.m20 + m23 * other.m30,
      m20 * other.m01 + m21 * other.m11 + m22 * other.m21 + m23 * other.m31,
      m20 * other.m02 + m21 * other.m12 + m22 * other.m22 + m23 * other.m32,
      m20 * other.m03 + m21 * other.m13 + m22 * other.m23 + m23 * other.m33,
      m30 * other.m00 + m31 * other.m10 + m32 * other.m20 + m33 * other.m30,
      m30 * other.m01 + m31 * other.m11 + m32 * other.m21 + m33 * other.m31,
      m30 * other.m02 + m31 * other.m12 + m32 * other.m22 + m33 * other.m32,
      m30 * other.m03 + m31 * other.m13 + m32 * other.m23 + m33 * other.m33)
  }

  private def calculateInverse: Option[(Radians, Matrix)] = {
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

    val det =
      (m00 * ( m11 * A2323 - m12 * A1323 + m13 * A1223 )) -
      (m01 * ( m10 * A2323 - m12 * A0323 + m13 * A0223 )) +
      (m02 * ( m10 * A1323 - m11 * A0323 + m13 * A0123 )) -
      (m03 * ( m10 * A1223 - m11 * A0223 + m12 * A0123 ))

    if (det == 0.0) None
    else {
      val invDet = 1.0 / det

      val inv = Matrix(
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


  def ~=(other: Matrix): Boolean = {
    (m00 ~= other.m00) && (m01 ~= other.m01) && (m02 ~= other.m02) && (m03 ~= other.m03) &&
      (m10 ~= other.m10) && (m11 ~= other.m11) && (m12 ~= other.m12) && (m13 ~= other.m13) &&
      (m20 ~= other.m20) && (m21 ~= other.m21) && (m22 ~= other.m22) && (m23 ~= other.m23) &&
      (m30 ~= other.m30) && (m31 ~= other.m31) && (m32 ~= other.m32) && (m33 ~= other.m33)
  }

  override def equals(other: Any): Boolean = other match {
    case rhs: Matrix => this ~= rhs
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

  def apply(m: Array[Double]): Matrix = {
    require(m.length == 16, "Only 4x4 matrix supported (length == 16)")
    new Matrix(m(0), m(1), m(2), m(3), m(4), m(5), m(6), m(7), m(8), m(9), m(10), m(11), m(12), m(13), m(14), m(15))
  }

  def parse(str: String): Matrix = {
    val array = str.split('|').map(_.trim).filterNot(_.isEmpty).map(_.toDouble)
    Matrix(array)
  }

  def show(m: Matrix, indent: Int): String = {
    import m._
    val array = Array(m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33)
    val sx = array.map(doubleToString)
    val tabs = "\t" * indent
    val sep = s"\n$tabs"
    val width = sx.maxBy(_.size).size
    val padded = sx.map(s => " " * (width - s.size) + s)
    padded
      .zipWithIndex
      .map {
        case (s, index) =>
          if ((index % 4 == 0) && index != 0) s"|$sep| $s "
          else s"| $s "
      }
      .mkString(sep, "", s"|\n")
  }
}