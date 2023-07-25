package spice4s.client.models

import cats.data._
import scalapb.validate._
import spice4s.client.util._

sealed abstract case class Relation private (value: String)

object Relation {
  val validationRegex = "^([a-z][a-z0-9_]{1,62}[a-z0-9])?$".r

  def unsafeFromString(value: String): Relation = new Relation(value) {}

  def apply(value: String): Validation[Relation] =
    unsafeFromString(value).validateRegex(validationRegex)(_.value)
}
