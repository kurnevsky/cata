package net.kurnevsky

import atto.syntax.parser._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._

class ExprCheck extends FunSuite with Checkers {
  test("print should be inverse of parse") {
    import ExprGen.shrink
    check {
      forAll(ExprGen.expr) { expr =>
        val exprStr = ExprF.print(expr)
        ExprF.Parser.expr.parseOnly(exprStr).option.get == expr
      }
    }
  }
}
