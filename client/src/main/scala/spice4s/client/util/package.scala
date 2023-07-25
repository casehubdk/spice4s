package spice4s.client

import scalapb.validate._
import cats.data._
import cats.implicits._
import scala.util.matching.Regex

package object util {
  implicit class AnyUtilOps[A](private val value: A) {
    def validator[V: Validator](implicit ev: A <:< Encodable[V]): Validated[List[ValidationFailure], A] =
      Validator[V].validate(value.encode) match {
        case Failure(violations) => violations.invalid
        case Success             => value.valid
      }

    def validateRegex(regex: Regex)(implicit ev: A <:< Encodable[String]): Validated[List[ValidationFailure], A] =
      if (regex.matches(value.encode)) value.valid
      else
        List(
          ValidationFailure(
            "regex-field",
            value.encode,
            s"$value with encoded value ${value.encode} does not match regex ${regex.toString}"
          )
        ).invalid
  }

  // def validate[A: Validator](a: A): Validated[List[ValidationFailure], Unit] =
  //     Validator[A].validate(a) match {
  //       case Failure(violations) => violations.invalid
  //       case Success => ().valid
  //     }

}
