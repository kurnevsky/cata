package net.kurnevsky

sealed trait BinaryOp
object BinaryOp {
  case object Add extends BinaryOp
  case object Sub extends BinaryOp
  case object Mul extends BinaryOp
  case object Div extends BinaryOp
}
