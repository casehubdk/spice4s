package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.core
import spice4s.client.util._

sealed abstract case class ZedToken private (token: String) {
  def encode: core.ZedToken = core.ZedToken.of(token)
}

object ZedToken {
  def unsafeFromString(token: String): ZedToken = new ZedToken(token) {}

  def apply(token: String): Validation[ZedToken] =
    unsafeFromString(token).validator(_.encode)

  def decode(x: core.ZedToken): Decoded[Option[ZedToken]] =
    opt(x.token).map(unsafeFromString).pure[Decoded]
}
