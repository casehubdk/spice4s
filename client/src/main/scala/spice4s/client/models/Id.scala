package spice4s.client.models

import cats.data._
import scalapb.validate._
import spice4s.client.util._

sealed abstract case class Id private (value: String) extends Encodable[String] {
  def encode = value
}

object Id {
  val validationRegex = "^([a-zA-Z0-9/_|\\-=+]{1,})?$".r

  def unsafeFromString(value: String): Id = new Id(value) {}

  def apply(value: String): Validated[List[ValidationFailure],Id] = 
    unsafeFromString(value).validateRegex(validationRegex)
}
