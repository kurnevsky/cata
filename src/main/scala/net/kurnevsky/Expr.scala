package net.kurnevsky

trait ExprType {
  type Expr = Fix[ExprF]
}

object ConstExpr extends (Int ⇒ Expr) {
  override def apply(value: Int): Expr =
    Fix[ExprF](ConstExprF(value))
}

object BinaryOpExpr extends ((BinaryOp, Expr, Expr) ⇒ Expr) {
  override def apply(op: BinaryOp, left: Expr, right: Expr): Expr =
    Fix[ExprF](BinaryOpExprF(op, left, right))
}

object UnaryOpExpr extends ((UnaryOp, Expr) ⇒ Expr) {
  override def apply(op: UnaryOp, value: Expr) =
    Fix[ExprF](UnaryOpExprF(op, value))
}

object BracketsExpr extends (Expr ⇒ Expr) {
  override def apply(value: Expr): Expr =
    Fix[ExprF](BracketsExprF(value))
}
