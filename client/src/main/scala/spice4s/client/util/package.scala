package spice4s.client

import scalapb.validate._
import cats.data._
import cats.implicits._
import scala.util.matching.Regex

package object util {
  implicit class AnyUtilOps[A](private val value: A) {
    def validator[V: Validator](f: A => V): Validated[List[ValidationFailure], A] =
      Validator[V].validate(f(value)) match {
        case Failure(violations) => violations.invalid
        case Success             => value.valid
      }

    def validateRegex(regex: Regex)(f: A => String): Validated[List[ValidationFailure], A] =
      if (regex.matches(f(value))) value.valid
      else
        List(
          ValidationFailure(
            "regex-field",
            f(value),
            s"$value with encoded value ${f(value)} does not match regex ${regex.toString}"
          )
        ).invalid
  }
}
