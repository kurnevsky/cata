package net.kurnevsky.dummy

import atto.parser.character.char
import atto.parser.combinator.{endOfInput, opt}
import atto.parser.numeric.int
import atto.syntax.parser._
import atto.Parser
import net.kurnevsky.{UnaryOp, BinaryOp}

sealed trait Expr
final case class ConstExpr(value: Int) extends Expr
final case class BinaryOpExpr(op: BinaryOp, left: Expr, right: Expr) extends Expr
final case class UnaryOpExpr(op: UnaryOp, value: Expr) extends Expr
final case class BracketsExpr(value: Expr) extends Expr

object Expr {
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

  def eval(expr: Expr): Int = expr match {
    case ConstExpr(value) ⇒ value
    case BinaryOpExpr(BinaryOp.Add, left, right) ⇒ eval(left) + eval(right)
    case BinaryOpExpr(BinaryOp.Sub, left, right) ⇒ eval(left) - eval(right)
    case BinaryOpExpr(BinaryOp.Mul, left, right) ⇒ eval(left) * eval(right)
    case BinaryOpExpr(BinaryOp.Div, left, right) ⇒ eval(left) / eval(right)
    case UnaryOpExpr(UnaryOp.Minus, value) ⇒ -eval(value)
    case UnaryOpExpr(UnaryOp.Plus, value) ⇒ eval(value)
    case BracketsExpr(value) ⇒ eval(value)
  }

  def print(expr: Expr): String = expr match {
    case ConstExpr(value) ⇒ value.toString
    case BinaryOpExpr(BinaryOp.Add, left, right) ⇒ s"${print(left)}+${print(right)}"
    case BinaryOpExpr(BinaryOp.Sub, left, right) ⇒ s"${print(left)}-${print(right)}"
    case BinaryOpExpr(BinaryOp.Mul, left, right) ⇒ s"${print(left)}*${print(right)}"
    case BinaryOpExpr(BinaryOp.Div, left, right) ⇒ s"${print(left)}/${print(right)}"
    case UnaryOpExpr(UnaryOp.Minus, value) ⇒ s"-${print(value)}"
    case UnaryOpExpr(UnaryOp.Plus, value) ⇒ s"+${print(value)}"
    case BracketsExpr(value) ⇒ s"(${print(value)})"
  }
}
