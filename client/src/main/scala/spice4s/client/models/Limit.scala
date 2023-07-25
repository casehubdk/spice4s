package spice4s.client.models

import com.authzed.api.v1.{permission_service => ps}
import spice4s.client.util._
import cats.implicits._
import scalapb.validate.ValidationFailure

sealed abstract case class Limit private (value: Int)

object Limit {
  def unsafeFromString(value: Int): Limit = new Limit(value) {}

  def apply(value: Int): Validation[Limit] =
    if (value >= 0 || value <= 1000) unsafeFromString(value).valid
    else invalid("limit", value, "Limit must be between 0 and 1000")
}
