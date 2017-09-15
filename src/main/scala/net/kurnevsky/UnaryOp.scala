package net.kurnevsky

sealed trait UnaryOp
object UnaryOp {
  case object Plus extends UnaryOp
  case object Minus extends UnaryOp
}
