package spice4s.client.models

import cats.data._
import scalapb.validate._
import spice4s.client.util._
import cats.implicits._

sealed abstract case class Id private (value: String)

object Id {
  val validationRegex = "^([a-zA-Z0-9/_|\\-=+]{1,})?$".r

  def unsafeFromString(value: String): Id = new Id(value) {}

  def apply(value: String): Validation[Id] =
    unsafeFromString(value).validateRegex(validationRegex)(_.value)

  def decode(x: String): Decoded[Option[Id]] =
    opt(x).map(unsafeFromString).pure[Decoded]
}
