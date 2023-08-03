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

final case class ReadRelationshipsResponse(
    token: ZedToken,
    relationship: Relationship,
    cursor: Cursor
)

object ReadRelationshipsResponse {
  def decode(x: ps.ReadRelationshipsResponse): Decoded[ReadRelationshipsResponse] =
    (
      field("token")(req(x.readAt) andThen ZedToken.decode andThen req),
      field("relationship")(req(x.relationship) andThen Relationship.decode),
      field("cursor")(req(x.afterResultCursor) andThen Cursor.decode andThen req)
    ).mapN(ReadRelationshipsResponse.apply)
}
