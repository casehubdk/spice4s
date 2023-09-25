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

import com.authzed.api.v1.{permission_service => ps}
import cats.data._
import cats.implicits._
import cats._

final case class WriteRelationshipsRequest(
    updates: NonEmptyList[RelationshipUpdate],
    preconditions: List[Precondition]
) {
  def encode = ps.WriteRelationshipsRequest.of(
    updates.map(_.encode).toList,
    preconditions.map(_.encode)
  )
}

object WriteRelationshipsRequest {
  implicit lazy val semigroupInstance: Semigroup[WriteRelationshipsRequest] =
    new Semigroup[WriteRelationshipsRequest] {
      def combine(x: WriteRelationshipsRequest, y: WriteRelationshipsRequest) =
        WriteRelationshipsRequest(
          x.updates |+| y.updates,
          x.preconditions |+| y.preconditions
        )
    }
}
