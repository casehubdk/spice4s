package spice4s.client.models

import cats.data._
import scalapb.validate._
import spice4s.client.util._

sealed abstract case class ResourceType private (value: String) extends Encodable[String] {
  def encode = value
}

object ResourceType {
  val validationRegex = "^([a-z][a-z0-9_]{1,61}[a-z0-9]/)?[a-z][a-z0-9_]{1,62}[a-z0-9]$".r

  def unsafeFromString(value: String): ResourceType = new ResourceType(value) {}

  def apply(value: String): Validated[List[ValidationFailure],ResourceType] = 
    unsafeFromString(value).validateRegex(validationRegex)
}
