package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class ReadRelationshipsRequest(
    consistency: Option[Consistency],
    relationshipFilter: RelationshipFilter,
    limit: Option[Limit],
    cursor: Option[Cursor]
) {
  def encode = ps.ReadRelationshipsRequest.of(
    consistency.map(_.encode),
    relationshipFilter.encode.some,
    limit.foldMap(_.value),
    cursor.map(_.encode)
  )
}
