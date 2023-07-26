package spice4s.client.models

import spice4s.client.util._
import cats.implicits._

sealed abstract case class Type private (value: String)

object Type {
  val validationRegex = "^([a-z][a-z0-9_]{1,61}[a-z0-9]/)?[a-z][a-z0-9_]{1,62}[a-z0-9]$".r

  def unsafeFromString(value: String): Type = new Type(value) {}

  def apply(value: String): Validation[Type] =
    unsafeFromString(value).validateRegex(validationRegex)(_.value)

  def decode(x: String): Decoded[Option[Type]] =
    opt(x).map(unsafeFromString).pure[Decoded]
}
