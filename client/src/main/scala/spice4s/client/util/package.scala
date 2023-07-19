package spice4s.client

import scalapb.validate._
import cats.data._
import cats.implicits._

package object util {
  implicit class EncodableOps[A](private val value: A) {
    def validate[V: Validator](implicit ev: A <:< Encodable[V]): Validated[List[ValidationFailure],A] =
      Validator[V].validate(value.encode) match {
        case Failure(violations) => violations.invalid
        case Success => value.valid
      }
  }
}

