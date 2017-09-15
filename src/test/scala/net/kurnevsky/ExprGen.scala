package net.kurnevsky

import org.scalacheck._

object ExprGen {
  val unaryOp: Gen[UnaryOp] =
    Gen.oneOf(UnaryOp.Minus, UnaryOp.Plus)
  val addOp: Gen[BinaryOp] =
    Gen.oneOf(BinaryOp.Add, BinaryOp.Sub)
  val mulOp: Gen[BinaryOp] =
    Gen.oneOf(BinaryOp.Mul, BinaryOp.Div)
  val constExpr: Gen[Expr] =
    Gen.choose(0, Int.MaxValue).map(ConstExpr)
  def bracketsExpr(level: Int): Gen[Expr] =
    if (level <= 0)
      constExpr
    else
      Gen.oneOf(
        addExpr(level - 1),
        constExpr
      )
  def unaryOpExpr(level: Int): Gen[Expr] =
    Gen.oneOf(
      for {
        op ← unaryOp
        value ← bracketsExpr(level)
      } yield UnaryOpExpr(op, value),
      bracketsExpr(level)
    )
  def mulExpr(level: Int): Gen[Expr] =
    if (level <= 0)
      unaryOpExpr(level)
    else
      Gen.oneOf(
        for {
          op ← mulOp
          left ← mulExpr(level - 1)
          right ← unaryOpExpr(level)
        } yield BinaryOpExpr(op, left, right),
        unaryOpExpr(level)
      )
  def addExpr(level: Int): Gen[Expr] =
    if (level <= 0)
      mulExpr(level)
    else
      Gen.oneOf(
        for {
          op ← addOp
          left ← addExpr(level - 1)
          right ← mulExpr(level)
        } yield BinaryOpExpr(op, left, right),
        mulExpr(level)
      )
  val expr: Gen[Expr] =
    addExpr(4)
}
