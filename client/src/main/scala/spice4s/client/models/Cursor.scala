package spice4s.client.models

sealed abstract case class Cursor private (token: String)

object Cursor {
  def unsafeFromString(token: String): Cursor = new Cursor(token) {}

  def apply(token: String): Option[Cursor] =
    if (token.isEmpty) None
    else Some(unsafeFromString(token))
}
