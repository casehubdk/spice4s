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
import com.authzed.api.v1.core
import spice4s.client.util._

sealed abstract case class ZedToken private (token: String) {
  def encode: core.ZedToken = core.ZedToken.of(token)
}

object ZedToken {
  def unsafeFromString(token: String): ZedToken = new ZedToken(token) {}

  def apply(token: String): Validation[ZedToken] =
    if (token.size >= 1) unsafeFromString(token).valid
    else invalid("zed-token", "ZedToken must be at least 1 character long")

  def decode(x: core.ZedToken): Decoded[Option[ZedToken]] =
    opt(x.token).map(unsafeFromString).pure[Decoded]
}
