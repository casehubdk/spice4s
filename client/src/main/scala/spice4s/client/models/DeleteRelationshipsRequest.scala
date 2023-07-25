package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class DeleteRelationshipsRequest(
    filter: RelationshipFilter,
    preconditions: List[Precondition],
    limit: Option[Limit],
    allowPartialDeletions: Option[Boolean]
) {
  def encode = ps.DeleteRelationshipsRequest.of(
    filter.encode.some,
    preconditions.map(_.encode),
    limit.foldMap(_.value),
    allowPartialDeletions.getOrElse(false)
  )
}
