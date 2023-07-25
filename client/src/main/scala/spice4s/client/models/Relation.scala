package spice4s.client.models

import cats.data._
import scalapb.validate._
import spice4s.client.util._

sealed abstract case class Relation private (value: String) {
  def encode = value
}

object Relation {
  val validationRegex = "^([a-z][a-z0-9_]{1,62}[a-z0-9])?$".r

  def unsafeFromString(value: String): Relation = new Relation(value) {}

  def apply(value: String): Validated[List[ValidationFailure], Relation] =
    unsafeFromString(value).validateRegex(validationRegex)(_.value)
}
