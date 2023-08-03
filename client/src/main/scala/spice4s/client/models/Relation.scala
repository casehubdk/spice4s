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

package spice4s.client.models

import cats.implicits._
import spice4s.client.util._

sealed abstract case class Relation private (value: String)

object Relation {
  val validationRegex = "^([a-z][a-z0-9_]{1,62}[a-z0-9])?$".r

  def unsafeFromString(value: String): Relation = new Relation(value) {}

  def apply(value: String): Validation[Relation] =
    unsafeFromString(value).validateRegex(validationRegex)(_.value)

  def decode(x: String): Decoded[Option[Relation]] =
    opt(x).map(unsafeFromString).pure[Decoded]
}
