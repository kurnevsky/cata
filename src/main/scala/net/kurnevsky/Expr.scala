package net.kurnevsky

trait ExprType {
  type Expr = Fix[ExprF]
}

object ConstExpr extends (Int ⇒ Expr) {
  override def apply(value: Int): Expr =
    Fix[ExprF](ConstExprF(value))
  def unapply(expr: Expr) = expr.value match {
    case constExprF: ConstExprF => ConstExprF.unapply(constExprF)
    case _ => None
  }
}

object BinaryOpExpr extends ((BinaryOp, Expr, Expr) ⇒ Expr) {
  override def apply(op: BinaryOp, left: Expr, right: Expr): Expr =
    Fix[ExprF](BinaryOpExprF(op, left, right))
  def unapply(expr: Expr) = expr.value match {
    case binaryOpExprF: BinaryOpExprF[Expr] ⇒ BinaryOpExprF.unapply(binaryOpExprF)
    case _ => None
  }
}

object UnaryOpExpr extends ((UnaryOp, Expr) ⇒ Expr) {
  override def apply(op: UnaryOp, value: Expr) =
    Fix[ExprF](UnaryOpExprF(op, value))
  def unapply(expr: Expr) = expr.value match {
    case unaryOpExprF: UnaryOpExprF[Expr] ⇒ UnaryOpExprF.unapply(unaryOpExprF)
    case _ => None
  }
}

object BracketsExpr extends (Expr ⇒ Expr) {
  override def apply(value: Expr): Expr =
    Fix[ExprF](BracketsExprF(value))
  def unapply(expr: Expr) = expr.value match {
    case bracketsExprF: BracketsExprF[Expr] ⇒ BracketsExprF.unapply(bracketsExprF)
    case _ => None
  }
}
