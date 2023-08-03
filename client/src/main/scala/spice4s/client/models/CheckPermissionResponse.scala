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
import com.authzed.api.v1.{permission_service => ps}

final case class CheckPermissionResponse(
    checkedAt: ZedToken,
    permissionship: CheckPermissionResponse.Permissionship
)

object CheckPermissionResponse {
  sealed trait Permissionship extends Product with Serializable
  object Permissionship {
    case object NoPermission extends Permissionship
    case object HasPermission extends Permissionship
    case object ConditionalPermission extends Permissionship

    def decode(x: ps.CheckPermissionResponse.Permissionship): Decoded[Permissionship] = {
      import ps.CheckPermissionResponse.Permissionship._
      x match {
        case PERMISSIONSHIP_NO_PERMISSION                 => NoPermission.validNec
        case PERMISSIONSHIP_HAS_PERMISSION                => HasPermission.validNec
        case PERMISSIONSHIP_CONDITIONAL_PERMISSION        => ConditionalPermission.validNec
        case PERMISSIONSHIP_UNSPECIFIED | Unrecognized(_) => raise("permissionship unspecified")
      }
    }
  }

  def decode(x: ps.CheckPermissionResponse): Decoded[CheckPermissionResponse] =
    (
      field("checkedAt")(req(x.checkedAt) andThen ZedToken.decode andThen req),
      field("permissionship")(Permissionship.decode(x.permissionship))
    ).mapN(CheckPermissionResponse.apply)
}
