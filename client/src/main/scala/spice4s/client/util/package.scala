/*
 * Copyright 2023 CaseHubDK
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spice4s.client

import cats._
import scalapb.validate._
import cats.data._
import cats.implicits._
import scala.util.matching.Regex
import cats.ApplicativeError

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
        invalid(
          "regex-field",
          f(value),
          s"$value with encoded value ${f(value)} does not match regex ${regex.toString}"
        )
  }

  type Validation[A] = Validated[List[ValidationFailure], A]

  def invalid[A](name: String, a: Any, reason: String): Validation[A] =
    List(ValidationFailure(name, a.toString, reason)).invalid

  final case class DecoderError(message: String, path: Chain[String]) {
    def prepend(p: String): DecoderError = copy(path = p +: path)
  }
  final case class DecoderException(
      errors: NonEmptyChain[DecoderError]
  ) extends Exception(errors.map(x => s"${x.message} at ${x.path.intercalate(".")}").toList.mkString("\n"))

  type Decoded[A] = ValidatedNec[DecoderError, A]
  def raise[A](message: String): Decoded[A] =
    DecoderError(message, Chain.empty).invalidNec

  def raiseIn[F[_], A](fa: Decoded[A])(implicit F: ApplicativeError[F, Throwable]): F[A] =
    F.fromValidated(fa.leftMap(DecoderException(_)))

  def indexed[G[_]: Traverse, A](xs: G[Decoded[A]]): Decoded[G[A]] =
    xs.zipWithIndex.traverse { case (d, i) => field(s"[$i]")(d) }

  def field[A](name: String)(fa: Decoded[A]): Decoded[A] =
    fa.leftMap(_.map(_.prepend(name)))

  def req[A](x: Option[A]): Decoded[A] =
    x.fold[Decoded[A]](raise("value was None"))(_.validNec)

  def opt[A: Monoid: Eq](a: A): Option[A] =
    if (a.isEmpty) None
    else Some(a)
}
