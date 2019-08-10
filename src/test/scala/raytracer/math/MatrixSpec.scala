package raytracer.math

import raytracer.BaseSpec

class MatrixSpec extends BaseSpec {
  feature("Matrices") {

    scenario("Creating a matrix") {
      val a = Matrix(
        1, 2, 3, 4,
        5, 6, 7, 8,
        9, 8, 7, 6,
        5, 4, 3, 2
      )
      val b =
        """
          | 1 | 2 | 3 | 4 |
          | 5 | 6 | 7 | 8 |
          | 9 | 8 | 7 | 6 |
          | 5 | 4 | 3 | 2 |
        """.matrix

      assert(a == b)
    }

    scenario("Constructing and inspecting a 4x4 matrix") {
      val m =
        """
          |  1   |  2   |  3   |  4   |
          |  5.5 |  6.5 |  7.5 |  8.5 |
          |  9   | 10   | 11   | 12   |
          | 13.5 | 14.5 | 15.5 | 16.5 |
        """.matrix
      Given(s"the following 4x4 matrix M:$m")
      Then("M[0,0] = 1")
      And("M[0,3] = 4")
      And("M[1,0] = 5.5")
      And("M[1,2] = 7.5")
      And("M[2,2] = 11")
      And("M[3,0] = 13.5")
      And("M[3,2] = 15.5")
      assert(m(0, 0) ~= 1)

      assert(m(0, 3) ~= 4)

      assert(m(1, 0) ~= 5.5)

      assert(m(1, 2) ~= 7.5)

      assert(m(2, 2) ~= 11)

      assert(m(3, 0) ~= 13.5)

      assert(m(3, 2) ~= 15.5)
    }
/*
    scenario("A 2x2 matrix ought to be representable") {
      val m =
        """
          | -3 |  5 |
          |  1 | -2 |
        """.matrix
      Given(s"the following 2x2 matrix M:$m")

      Then(s"m.numRows == 2")
      assert(m.rows == 2)

      And(s"m.numColumns == 2")
      assert(m.columns == 2)

      Then("M[0,0] = -3")
      assert(m(0, 0) == -3)

      And("M[0,1] = 5")
      assert(m(0, 1) == 5)

      And("M[1,0] = 1")
      assert(m(1, 0) == 1)

      And("M[1,1] = -2")
      assert(m(1, 1) == -2)

//      And("m.row(1) == tuple(1, -2)")
//      assert(m.row(1) == tuple(1, -2))

//      And("m.column(1) == tuple(5, -2)")
//      assert(m.column(1) == tuple(5, -2))

    }

    scenario("A 3x3 matrix ought to be representable") {
      val m =
        """
          | -3 |  5 |  0 |
          |  1 | -2 | -7 |
          |  0 |  1 |  1 |
        """.matrix
      Given(s"the following 2x2 matrix M:$m")

      Then(s"m.rows == 3")
      assert(m.rows == 3)

      And(s"m.columns == 3")
      assert(m.columns == 3)

      Then("M[0,0] = -3")
      assert(m(0, 0) == -3)

      And("M[1,1] = -2")
      assert(m(1, 1) == -2)

      And("M[2,2] = 1")
      assert(m(2, 2) == 1)

    }
*/
    scenario("Matrix should produce a string") {
      val str =
        "\n\t| 1 | 2 | 3 | 4 |\n\t| 5 | 6 | 7 | 8 |\n\t| 9 | 8 | 7 | 6 |\n\t| 5 | 4 | 3 | 2 |\n"
      val m = str.matrix
      Given(s"the following matrix A:$m")
      Then("m.toString == a")
      assert(m.toString == str)
    }

    scenario("Access matrix rows") {
      val m =
        """
          | 1 | 2 | 3 | 4 |
          | 5 | 6 | 7 | 8 |
          | 9 | 8 | 7 | 6 |
          | 5 | 4 | 3 | 2 |
        """.matrix
      Given(s"the following matrix A:$m")

      val numRows = m.col(0).length
      Then(s"m.numRows = $numRows")

      val row0 = Vector[Double](1, 2, 3, 4)
      And(s"m.row(0) == $row0")

      assert(m.row(0) == row0)

      val row1 = Vector[Double](5, 6, 7, 8)
      And(s"m.row(1) == $row1")
      assert(m.row(1) == row1)

      val row2 = Vector[Double](9, 8, 7, 6)
      And(s"m.row(2) == $row2")
      assert(m.row(1) == row1)

      val row3 = Vector[Double](5, 4, 3, 2)
      And(s"m.row(3) == $row3")
      assert(m.row(1) == row1)
    }

//    scenario("Access 4x4 matrix columns") {
//      val m =
//        """
//          | 1 | 2 | 3 | 4 |
//          | 5 | 6 | 7 | 8 |
//          | 9 | 8 | 7 | 6 |
//          | 5 | 4 | 3 | 2 |
//        """.matrix
//      Given(s"the following matrix A:$m")
//      val numColumns = m.columns
//      And(s"m.columns = $numColumns")
//      assert(m.columns == numColumns)
//
//      val col0 = Tuple(1, 5, 9, 5)
//      And(s"m.column(0) == $col0")
//      assert(m.column(0) == col0)
//
//      val col1 = Tuple(2, 6, 8, 4)
//      And(s"m.column(1) == $col1")
//      assert(m.column(1) == col1)
//
//      val col2 = Tuple(3, 7, 7, 3)
//      And(s"m.column(2) == $col2")
//      assert(m.column(2) == col2)
//
//      val col3 = Tuple(4, 8, 6, 2)
//      And(s"m.column(3) == $col3")
//      assert(m.column(3) == col3)
//    }

    scenario("Matrix equality with identical matrices") {
      val a =
        """
          | 1 | 2 | 3 | 4 |
          | 5 | 6 | 7 | 8 |
          | 9 | 8 | 7 | 6 |
          | 5 | 4 | 3 | 2 |
        """.matrix
      val b =
        """
          | 1 | 2 | 3 | 4 |
          | 5 | 6 | 7 | 8 |
          | 9 | 8 | 7 | 6 |
          | 5 | 4 | 3 | 2 |
        """.matrix
      Given(s"the following matrix A:$a")
      And(s"the following matrix B:$b")
      Then("A = B")
      assert(a == b)
    }

    scenario("Matrix equality with different matrices") {
      val a =
        """
          | 1 | 2 | 3 | 4 |
          | 5 | 6 | 7 | 8 |
          | 9 | 8 | 7 | 6 |
          | 5 | 4 | 3 | 2 |
        """.matrix
      val b =
        """
          | 2 | 3 | 4 | 5 |
          | 6 | 7 | 8 | 9 |
          | 8 | 7 | 6 | 5 |
          | 4 | 3 | 2 | 1 |
        """.matrix
      Given(s"the following matrix A:$a")
      And(s"the following matrix B:$b")
      Then("A != B")
      assert(a != b)
    }

    scenario("Multiplying two matrices") {
      val a =
        """
          | 1 | 2 | 3 | 4 |
          | 5 | 6 | 7 | 8 |
          | 9 | 8 | 7 | 6 |
          | 5 | 4 | 3 | 2 |
        """.matrix
      val b =
        """
          | -2 | 1 | 2 |  3 |
          |  3 | 2 | 1 | -1 |
          |  4 | 3 | 6 |  5 |
          |  1 | 2 | 7 |  8 |
        """.matrix

      val expected =
        """
          | 20|  22 |  50 |  48 |
          | 44|  54 | 114 | 108 |
          | 40|  58 | 110 | 102 |
          | 16|  26 |  46 |  42 |
        """.matrix
      Given(s"the following matrix A:$a")
      And(s"the following matrix B:$b")
      Then(s"A * B is the following 4x4 matrix:$expected")
      assert(a * b == expected)
    }

    scenario("A matrix multiplied by a tuple") {
      val a =
        """
          | 1 | 2 | 3 | 4 |
          | 2 | 4 | 4 | 2 |
          | 8 | 6 | 4 | 1 |
          | 0 | 0 | 0 | 1 |
        """.matrix
      Given(s"the following matrix A:$a")
      And("b ← tuple(1, 2, 3, 1)")
      Then(s"A * b =  tuple(18, 24, 33, 1)")
      val b = tuple(1, 2, 3, 1)
      assert(a * b ==  tuple(18, 24, 33, 1))
    }

    scenario("A matrix multiplied by a point") {
      val a =
        """
          | 1 | 2 | 3 | 4 |
          | 2 | 4 | 4 | 2 |
          | 8 | 6 | 4 | 1 |
          | 0 | 0 | 0 | 1 |
        """.matrix
      Given(s"the following matrix A:$a")
      And(s"b ← point(1, 2, 3)")
      Then(s"A * b = point(18, 24, 33)")
      assert(a * point(1, 2, 3) == point(18, 24, 33))
    }

//    scenario("A matrix multiplied by a vector") {
//      val a =
//        """
//          | 1 | 2 | 3 | 4 |
//          | 2 | 4 | 4 | 2 |
//          | 8 | 6 | 4 | 1 |
//          | 0 | 0 | 0 | 1 |
//        """.matrix
//      Given(s"the following matrix A:$a")
//      val b = vector(1, 2, 3)
//      And(s"b ← $b")
//      val expected = vector(18, 24, 33)
//      Then(s"A * b = $expected")
//      assert(a * b == expected)
//    }

    scenario("The identity matrix") {
      val i =
        """
          | 1 | 0 |  0 |  0 |
          | 0 | 1 |  0 |  0 |
          | 0 | 0 |  1 |  0 |
          | 0 | 0 |  0 |  1 |
        """.matrix
      Given(s"i ← $i")
      Then("i == identity")
      assert(i == Matrix.identity)
    }

    scenario("Multiplying a matrix by the identity matrix") {
      val m =
        """
          | 0 | 1 |  2 |  4 |
          | 1 | 2 |  4 |  8 |
          | 2 | 4 |  8 | 16 |
          | 4 | 8 | 16 | 32 |
        """.matrix
      Given(s"the following matrix A: $m")
      Then("A * identity_matrix = A")
      assert(m * Matrix.identity == m)
    }

    scenario("Multiplying the identity matrix by a tuple") {
      val a = tuple(1, 2, 3, 4)
      Given(s"a ← $a")
      Then("identity_matrix * a = a")
      assert(Matrix.identity * a == a)
    }

    scenario("Transposing a matrix") {

      val a =
        """
          | 0 | 9 | 3 | 0 |
          | 9 | 8 | 0 | 8 |
          | 1 | 8 | 5 | 3 |
          | 0 | 0 | 5 | 8 |
        """.matrix
      Given(s"the following matrix A: $a")
      val expected =
        """
          | 0 | 9 | 1 | 0 |
          | 9 | 8 | 8 | 0 |
          | 3 | 0 | 5 | 5 |
          | 0 | 8 | 3 | 8 |
        """.matrix
      Then(s"transpose(a) is the following matrix: $expected")
      assert(a.transpose == expected)
    }

    scenario("Transposing the identity matrix") {
      Given("A ← transpose(identity_matrix)")
      Then("A = identity_matrix")
      assert(Matrix.identity.transpose == Matrix.identity)
    }
    /*
        scenario("Calculating the determinant of a 2x2 matrix") {
          val a =
            """
              |  1 | 5 |
              | -3 | 2 |
            """.matrix
          Given(s"the following 2x2 matrix A: $a")
          Then("determinant(A) = 17")
          assert(determinant(a) == 17)
        }

        scenario("A submatrix of a 3x3 matrix is a 2x2 matrix") {
          val a =
            """
              |  1 | 5 |  0 |
              | -3 | 2 |  7 |
              |  0 | 6 | -3 |
            """.matrix
          Given(s"the following 3x3 matrix A: $a")
          val expected =
            """
              | -3 | 2 |
              |  0 | 6 |
            """.matrix
          Then(s"submatrix(A, 0, 2) is the following 2x2 matrix: $expected")
          assert(submatrix(a, 0, 2) == expected)
        }

        scenario("A submatrix of a 4x4 matrix is a 3x3 matrix") {
          val a =
            """
              | -6 |  1 |  1 |  6 |
              | -8 |  5 |  8 |  6 |
              | -1 |  0 |  8 |  2 |
              | -7 |  1 | -1 |  1 |
            """.matrix
          Given(s"the following 4x4 matrix A: $a")

          val expected =
            """
              | -6 |  1 | 6 |
              | -8 |  8 | 6 |
              | -7 | -1 | 1 |
            """.matrix
          Then(s"submatrix(A, 2, 1) is the following 3x3 matrix: $expected")
          assert(submatrix(a, 2, 1) == expected)
        }

        scenario("Calculating a minor of a 3x3 matrix") {
          val a =
            """
              |  3 |  5 |  0 |
              |  2 | -1 | -7 |
              |  6 | -1 |  5 |
            """.matrix
          Given(s"the following 3x3 matrix A: $a")

          val b = submatrix(a, 1, 0)
          And("B ← submatrix(A, 1, 0)")

          Then("determinant(B) = 25")
          assert(determinant(b) == 25)

          And("minor(A, 1, 0) = 25")
          assert(minor(a, 1, 0) == 25)
        }

        scenario("Calculating a cofactor of a 3x3 matrix") {
          val A  =
            """
              |  3 |  5 |  0 |
              |  2 | -1 | -7 |
              |  6 | -1 |  5 |
            """.matrix
          Given(s"the following 3x3 matrix A: $A")

          Then("minor(A, 0, 0) = -12")
          assert(minor(A, 0, 0) == -12)

          And("cofactor(A, 0, 0) = -12")
          assert(cofactor(A, 0, 0) == -12)

          And("minor(A, 1, 0) = 25")
          assert(minor(A, 1, 0) == 25)

          And("cofactor(A, 1, 0) = -25")
          assert(cofactor(A, 1, 0) == -25)
        }

        scenario("Calculating the determinant of a 3x3 matrix") {
          val A =
            """
              |  1 |  2 |  6 |
              | -5 |  8 | -4 |
              |  2 |  6 |  4 |
            """.matrix
          Given(s"the following 3x3 matrix A: $A")

          Then("cofactor(A, 0, 0) = 56")
          assert(cofactor(A, 0, 0) == 56)

          And("cofactor(A, 0, 1) = 12")
          assert(cofactor(A, 0, 1) == 12)

          And("cofactor(A, 0, 2) = -46")
          assert(cofactor(A, 0, 2) == -46)

          And("determinant(A) = -196")
          assert(determinant(A) == -196)
        }
    */
    scenario("Calculating the determinant of a 4x4 matrix") {
      val A =
        """
          | -2 | -8 |  3 |  5 |
          | -3 |  1 |  7 |  3 |
          |  1 |  2 | -9 |  6 |
          | -6 |  7 |  7 | -9 |
        """.matrix
      Given(s"the following 4x4 matrix A: $A")

//      Then("cofactor(A, 0, 0) = 690")
//      assert(cofactor(A, 0, 0) == 690)
//
//      And("cofactor(A, 0, 1) = 447")
//      assert(cofactor(A, 0, 1) == 447)
//
//      And("cofactor(A, 0, 2) = 210")
//      assert(cofactor(A, 0, 2) == 210)
//
//      And("cofactor(A, 0, 3) = 51")
//      assert(cofactor(A, 0, 3) == 51)

      And("determinant(A) = -4071")
      assert(A.determinant == -4071)
    }

    scenario("Testing an invertible matrix for invertibility") {
      val A =
        """
          |  6 |  4 |  4 |  4 |
          |  5 |  5 |  7 |  6 |
          |  4 | -9 |  3 | -7 |
          |  9 |  1 |  7 | -6 |
        """.matrix
      Given(s"the following 4x4 matrix A: $A")

      Then("determinant(A) = -2120")
      assert(A.determinant == -2120)

      And("A is invertible")
      assert(A.isInvertible)
    }

    scenario("Testing a non-invertible matrix for invertibility") {
      val A =
        """
          | -4 |  2 | -2 | -3 |
          |  9 |  6 |  2 |  6 |
          |  0 | -5 |  1 | -5 |
          |  0 |  0 |  0 |  0 |
        """.matrix
      Given(s"the following 4x4 matrix A: $A")

      Then("determinant(A) = 0")
      assert(A.determinant == 0.0)

      And("A is not invertible")
      assert(!A.isInvertible)
    }

    scenario("Calculating the inverse of a matrix") {
      val A =
        """
          | -5 |  2 |  6 | -8 |
          |  1 | -5 |  1 |  8 |
          |  7 |  7 | -6 | -7 |
          |  1 | -3 |  7 |  4 |
        """.matrix
      Given(s"the following 4x4 matrix A: $A")

      And("B ← inverse(A)")
      val B = A.inverse

      Then("determinant(A) = 532")
      assert(A.determinant == 532)

//      And("cofactor(A, 2, 3) = -160")
//      assert(cofactor(A, 2, 3) ~= -160)

      And("B[3,2] = -160/532")
      assert(B(3,2) ~= -160d/532d)

//      And("cofactor(A, 3, 2) = 105")
//      assert(cofactor(A, 3, 2) ~= 105)

      And(s"B[2,3] = 105/532")
      assert(B(2, 3) ~= 105.0/532.0)

      val expected =
        """
          |  0.21805 |  0.45113 |  0.24060 | -0.04511 |
          | -0.80827 | -1.45677 | -0.44361 |  0.52068 |
          | -0.07895 | -0.22368 | -0.05263 |  0.19737 |
          | -0.52256 | -0.81391 | -0.30075 |  0.30639 |
        """.matrix
      And(s"B is the following 4x4 matrix: $expected")
      assert(B == expected)
    }

    scenario("Calculating the inverse of another matrix") {
      val A =
        """
          |  8 | -5 |  9 |  2 |
          |  7 |  5 |  6 |  1 |
          | -6 |  0 |  9 |  6 |
          | -3 |  0 | -9 | -4 |
        """.matrix
      Given(s"the following 4x4 matrix A: $A")

      val expected =
        """
          | -0.15385 | -0.15385 | -0.28205 | -0.53846 |
          | -0.07692 |  0.12308 |  0.02564 |  0.03077 |
          |  0.35897 |  0.35897 |  0.43590 |  0.92308 |
          | -0.69231 | -0.69231 | -0.76923 | -1.92308 |
        """.matrix
      Then(s"inverse(A) is the following 4x4 matrix: $expected")
    }

    scenario("Calculating the inverse of a third matrix") {
      val A =
        """
          |  9 |  3 |  0 |  9 |
          | -5 | -2 | -6 | -3 |
          | -4 |  9 |  6 |  4 |
          | -7 |  6 |  6 |  2 |
        """.matrix
      Given(s"the following 4x4 matrix A: $A")
      val expected =
        """
          | -0.04074 | -0.07778 |  0.14444 | -0.22222 |
          | -0.07778 |  0.03333 |  0.36667 | -0.33333 |
          | -0.02901 | -0.14630 | -0.10926 |  0.12963 |
          |  0.17778 |  0.06667 | -0.26667 |  0.33333 |
        """.matrix
      Then(s"inverse(A) is the following 4x4 matrix: $expected")
    }

    scenario(" Multiplying a product by its inverse") {
      val A =
        """
          |  3 | -9 |  7 |  3 |
          |  3 | -8 |  2 | -9 |
          | -4 |  4 |  4 |  1 |
          | -6 |  5 | -1 |  1 |
        """.matrix
      Given(s"the following 4x4 matrix A: $A")

      val B =
        """
          |  8 |  2 |  2 |  2 |
          |  3 | -1 |  7 |  0 |
          |  7 |  0 |  5 |  4 |
          |  6 | -2 |  0 |  5 |
        """.matrix
      And(s"the following 4x4 matrix B: $B")

      val C = A * B
      And(s"C ← A * B: $C")
      Then("C * inverse(B) = A")
      assert((C * B.inverse) == A)

    }

    scenario("bug: matrix not invertable") {
      val m =
        """
          |    0 |    0 |    0 |    0 |
          |    0 |    0 |    0 |    0 |
          |    0 |    1 |    0 | -2.5 |
          |    0 |    0 |    0 |    1 |
        """.matrix
    }

  }
}
