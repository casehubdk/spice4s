package spice4s.client.models

import cats.data._
import com.authzed.api.v1.core
import scalapb.validate._
import spice4s.client.util._

sealed abstract case class ZedToken private (token: String) extends Encodable[core.ZedToken] {
  def encode = core.ZedToken.of(token)
}

object ZedToken {
  def unsafeFromString(token: String): ZedToken = new ZedToken(token) {}

  def apply(token: String): Validated[List[ValidationFailure], ZedToken] = 
    unsafeFromString(token).validate[core.ZedToken]
}