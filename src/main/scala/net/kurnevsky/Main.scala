package net.kurnevsky

import atto.syntax.parser._

object Main extends App {
  val exprStr = "-1+2*(1+2)+(+(1))"
  val expr = ExprF.Parser.expr.parseOnly(exprStr).option.get
  println(ExprF.print(expr))
}
