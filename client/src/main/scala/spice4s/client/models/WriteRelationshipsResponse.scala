package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class WriteRelationshipsResponse(
    token: ZedToken
)

object WriteRelationshipsResponse {
  def decode(x: ps.WriteRelationshipsResponse): Decoded[WriteRelationshipsResponse] =
    (
      field("token")(req(x.writtenAt) andThen ZedToken.decode andThen req)
    ).map(WriteRelationshipsResponse.apply)
}
