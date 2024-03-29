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

final case class LookupResourcesResponse(
    lookedUpAt: ZedToken,
    resourceObjectId: String,
    permissionship: LookupPermissionship,
    afterResultCursor: Option[Cursor]
)

object LookupResourcesResponse {
  def decode(x: ps.LookupResourcesResponse): Decoded[LookupResourcesResponse] =
    (
      field("looked_up_at")(req(x.lookedUpAt) andThen ZedToken.decode andThen req),
      x.resourceObjectId.validNec,
      field("permissionship")(LookupPermissionship.decode(x.permissionship)),
      field("after_result_cursor")(x.afterResultCursor.flatTraverse(Cursor.decode))
    ).mapN(LookupResourcesResponse.apply)
}
