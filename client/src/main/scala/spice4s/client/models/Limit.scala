package spice4s.client.models

import spice4s.client.util._
import cats.implicits._

sealed abstract case class Limit private (value: Int)

object Limit {
  def unsafeFromString(value: Int): Limit = new Limit(value) {}

  def apply(value: Int): Validation[Limit] =
    if (value >= 0 || value <= 1000) unsafeFromString(value).valid
    else invalid("limit", value, "Limit must be between 0 and 1000")
}
