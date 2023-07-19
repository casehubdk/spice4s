package spice4s.client.models

import cats.data._
import scalapb.validate._
import spice4s.client.util._

sealed abstract case class ResourceId private (value: String) extends Encodable[String] {
  def encode = value
}

object ResourceId {
  val validationRegex = "^([a-zA-Z0-9/_|\\-=+]{1,})?$".r

  def unsafeFromString(value: String): ResourceId = new ResourceId(value) {}

  def apply(value: String): Validated[List[ValidationFailure],ResourceId] = 
    unsafeFromString(value).validateRegex(validationRegex)
}
