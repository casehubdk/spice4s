package spice4s.client.models

import cats.data._
import scalapb.validate._
import spice4s.client.util._

sealed abstract case class SubjectType private (value: String) extends Encodable[String] {
  def encode = value
}

object SubjectType {
  val validationRegex = "^([a-z][a-z0-9_]{1,61}[a-z0-9]/)?[a-z][a-z0-9_]{1,62}[a-z0-9]$".r

  def unsafeFromString(value: String): SubjectType = new SubjectType(value) {}

  def apply(value: String): Validated[List[ValidationFailure],SubjectType] = 
    unsafeFromString(value).validateRegex(validationRegex)
}
