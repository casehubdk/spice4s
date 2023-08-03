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

import spice4s.client.util._
import cats.implicits._

sealed abstract case class Id private (value: String)

object Id {
  val validationRegex = "^([a-zA-Z0-9/_|\\-=+]{1,})?$".r

  def unsafeFromString(value: String): Id = new Id(value) {}

  def apply(value: String): Validation[Id] =
    unsafeFromString(value).validateRegex(validationRegex)(_.value)

  def decode(x: String): Decoded[Option[Id]] =
    opt(x).map(unsafeFromString).pure[Decoded]
}
