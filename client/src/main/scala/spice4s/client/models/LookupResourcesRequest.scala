package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class LookupResourcesRequest(
    consistency: Option[Consistency],
    resourceObjectType: Type,
    permission: Relation,
    subjectReference: SubjectReference,
    limit: Option[Limit],
    cursor: Option[Cursor]
) {
  def encode = ps.LookupResourcesRequest.of(
    consistency.map(_.encode),
    resourceObjectType.value,
    permission.value,
    subjectReference.encode.some,
    None,
    limit.foldMap(_.value),
    cursor.map(_.encode)
  )
}
