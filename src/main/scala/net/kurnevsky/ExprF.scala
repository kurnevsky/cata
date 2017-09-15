package net.kurnevsky

import atto.parser.character.char
import atto.parser.combinator.{endOfInput, opt}
import atto.parser.numeric.int
import atto.syntax.parser._
import atto.Parser
import cats.Functor

sealed trait ExprF[+T]
final case class ConstExprF(value: Int) extends ExprF[Nothing]
final case class BinaryOpExprF[+T](op: BinaryOp, left: T, right: T) extends ExprF[T]
final case class UnaryOpExprF[+T](op: UnaryOp, value: T) extends ExprF[T]
final case class BracketsExprF[+T](value: T) extends ExprF[T]

object ExprF {
  object Parser {
    def toTree(arg: (Expr, List[(BinaryOp, Expr)])): Expr = arg match {
      case (head, tail) ⇒
        tail.foldLeft(head)((acc, e) ⇒ BinaryOpExpr(e._1, acc, e._2))
    }
    val addOp: Parser[BinaryOp] =
      char('+') >| BinaryOp.Add | char('-') >| BinaryOp.Sub
    val mulOp: Parser[BinaryOp] =
      char('*') >| BinaryOp.Mul | char('/') >| BinaryOp.Div
    val unaryOp: Parser[UnaryOp] =
      char('+') >| UnaryOp.Plus | char('-') >| UnaryOp.Minus
    val constExpr: Parser[Expr] =
      int.filter(_ >= 0) -| ConstExpr
    lazy val bracketsExpr: Parser[Expr] =
      char('(') ~> addExpr -| BracketsExpr <~ char(')') | constExpr
    lazy val unaryOpExpr: Parser[Expr] =
      opt(unaryOp) ~ bracketsExpr -| {
        case (Some(op), value) ⇒ UnaryOpExpr(op, value)
        case (None, value) ⇒ value
      }
    lazy val mulExpr: Parser[Expr] =
      unaryOpExpr ~ (mulOp ~ bracketsExpr).many -| toTree
    lazy val addExpr: Parser[Expr] =
      mulExpr ~ (addOp ~ mulExpr).many -| toTree
    lazy val expr: Parser[Expr] =
      addExpr <~ endOfInput
  }

  implicit object FunctorExprF extends Functor[ExprF] {
    override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case ConstExprF(value) ⇒ ConstExprF(value)
      case BinaryOpExprF(op, left, right) ⇒ BinaryOpExprF(op, f(left), f(right))
      case UnaryOpExprF(op, value) ⇒ UnaryOpExprF(op, f(value))
      case BracketsExprF(value) ⇒ BracketsExprF(f(value))
    }
  }

  def eval(expr: Expr): Int = expr.cata[Int] {
    case ConstExprF(value) ⇒ value
    case BinaryOpExprF(BinaryOp.Add, left, right) ⇒ left + right
    case BinaryOpExprF(BinaryOp.Sub, left, right) ⇒ left - right
    case BinaryOpExprF(BinaryOp.Mul, left, right) ⇒ left * right
    case BinaryOpExprF(BinaryOp.Div, left, right) ⇒ left / right
    case UnaryOpExprF(UnaryOp.Minus, value) ⇒ -value
    case UnaryOpExprF(UnaryOp.Plus, value) ⇒ value
    case BracketsExprF(value) ⇒ value
  }

  def print(expr: Expr): String = expr.cata[String] {
    case ConstExprF(value) ⇒ value.toString
    case BinaryOpExprF(BinaryOp.Add, left, right) ⇒ s"$left+$right"
    case BinaryOpExprF(BinaryOp.Sub, left, right) ⇒ s"$left-$right"
    case BinaryOpExprF(BinaryOp.Mul, left, right) ⇒ s"$left*$right"
    case BinaryOpExprF(BinaryOp.Div, left, right) ⇒ s"$left/$right"
    case UnaryOpExprF(UnaryOp.Minus, value) ⇒ s"-$value"
    case UnaryOpExprF(UnaryOp.Plus, value) ⇒ s"+$value"
    case BracketsExprF(value) ⇒ s"($value)"
  }
}
