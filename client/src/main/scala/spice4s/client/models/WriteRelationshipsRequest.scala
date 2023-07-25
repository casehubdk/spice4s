package spice4s.client.models

import com.authzed.api.v1.{permission_service => ps}
import cats.data._

final case class WriteRelationshipsRequest(
    updates: NonEmptyList[RelationshipUpdate],
    preconditions: List[Precondition]
) {
  def encode = ps.WriteRelationshipsRequest.of(
    updates.map(_.encode).toList,
    preconditions.map(_.encode)
  )
}
