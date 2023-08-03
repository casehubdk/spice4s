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

final case class DeleteRelationshipsResponse(
    deletedAt: ZedToken,
    deletionProgress: DeleteRelationshipsResponse.DeletionProgress
)

object DeleteRelationshipsResponse {
  sealed trait DeletionProgress extends Product with Serializable
  object DeletionProgress {
    case object Complete extends DeletionProgress
    case object Partial extends DeletionProgress

    def decode(x: ps.DeleteRelationshipsResponse.DeletionProgress): Decoded[DeletionProgress] = {
      import ps.DeleteRelationshipsResponse.DeletionProgress._
      x match {
        case DELETION_PROGRESS_COMPLETE                      => Complete.validNec
        case DELETION_PROGRESS_PARTIAL                       => Partial.validNec
        case DELETION_PROGRESS_UNSPECIFIED | Unrecognized(_) => raise("deletionProgress unspecified")
      }
    }
  }

  def decode(x: ps.DeleteRelationshipsResponse): Decoded[DeleteRelationshipsResponse] =
    (
      field("deletedAt")(req(x.deletedAt) andThen ZedToken.decode andThen req),
      field("deletionProgress")(DeletionProgress.decode(x.deletionProgress))
    ).mapN(DeleteRelationshipsResponse.apply)
}
