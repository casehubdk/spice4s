package spice4s.client.util

trait Encodable[A] {
  def encode: A
}
