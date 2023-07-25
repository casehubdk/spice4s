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
