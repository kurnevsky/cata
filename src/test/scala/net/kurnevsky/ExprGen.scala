package net.kurnevsky

import org.scalacheck._

object ExprGen {
  implicit val shrink: Shrink[Expr] = Shrink {
    case ConstExpr(value) ⇒ Shrink.shrink(value).filter(_ >= 0).map(ConstExpr)
    case BinaryOpExpr(_, left, right) ⇒ left +: right +: (Shrink.shrink(left) ++ Shrink.shrink(right))
    case UnaryOpExpr(_, value) ⇒ value +: Shrink.shrink(value)
    case BracketsExpr(value) ⇒ value +: Shrink.shrink(value)
  }

  val unaryOp: Gen[UnaryOp] =
    Gen.oneOf(UnaryOp.Minus, UnaryOp.Plus)
  val addOp: Gen[BinaryOp] =
    Gen.oneOf(BinaryOp.Add, BinaryOp.Sub)
  val mulOp: Gen[BinaryOp] =
    Gen.oneOf(BinaryOp.Mul, BinaryOp.Div)
  val constExpr: Gen[Expr] =
    Gen.choose(0, 100).map(ConstExpr)
  def bracketsExpr(depth: Int): Gen[Expr] =
    if (depth <= 0)
      constExpr
    else
      Gen.oneOf(
        addExpr(depth - 1, true).map(BracketsExpr),
        constExpr
      )
  def unaryOpExpr(depth: Int, unary: Boolean): Gen[Expr] =
    if (unary)
      Gen.oneOf(
        for {
          op ← unaryOp
          value ← bracketsExpr(depth)
        } yield UnaryOpExpr(op, value),
        bracketsExpr(depth)
      )
    else
      bracketsExpr(depth)
  def mulExpr(depth: Int, unary: Boolean): Gen[Expr] =
    if (depth <= 0)
      unaryOpExpr(depth, unary)
    else
      Gen.oneOf(
        for {
          op ← mulOp
          left ← mulExpr(depth - 1, unary)
          right ← unaryOpExpr(depth, false)
        } yield BinaryOpExpr(op, left, right),
        unaryOpExpr(depth, unary)
      )
  def addExpr(depth: Int, unary: Boolean): Gen[Expr] =
    if (depth <= 0)
      mulExpr(depth, unary)
    else
      Gen.oneOf(
        for {
          op ← addOp
          left ← addExpr(depth - 1, unary)
          right ← mulExpr(depth, false)
        } yield BinaryOpExpr(op, left, right),
        mulExpr(depth, unary)
      )
  val expr: Gen[Expr] =
    addExpr(8, true)
}
