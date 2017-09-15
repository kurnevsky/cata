package net.kurnevsky

import cats.Functor
import cats.syntax.functor._

import scala.language.higherKinds

final case class Fix[F[_]](value: F[Fix[F]]) extends AnyVal {
  def cata[B](f: F[B] => B)(implicit F: Functor[F]): B =
    f(value.map(_.cata(f)))
}
