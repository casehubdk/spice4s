package spice4s.client.models

import com.authzed.api.v1.core
import spice4s.client.util._
import cats.implicits._

sealed abstract case class Cursor private (token: String) {
  def encode = core.Cursor.of(token)
}

object Cursor {
  def unsafeFromString(token: String): Cursor = new Cursor(token) {}

  def apply(token: String): Option[Cursor] =
    if (token.isEmpty) None
    else Some(unsafeFromString(token))

  def decode(x: core.Cursor): Decoded[Option[Cursor]] =
    opt(x.token).map(unsafeFromString).pure[Decoded]
}
