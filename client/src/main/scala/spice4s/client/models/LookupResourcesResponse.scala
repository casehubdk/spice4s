package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class LookupResourcesResponse(
    lookedUpAt: ZedToken,
    resourceObjectId: String,
    permissionship: LookupPermissionship,
    afterResultCursor: Cursor
)

object LookupResourcesResponse {
  def decode(x: ps.LookupResourcesResponse): Decoded[LookupResourcesResponse] =
    (
      field("looked_up_at")(req(x.lookedUpAt) andThen ZedToken.decode andThen req),
      x.resourceObjectId.validNec,
      field("permissionship")(LookupPermissionship.decode(x.permissionship)),
      field("after_result_cursor")(req(x.afterResultCursor) andThen Cursor.decode andThen req)
    ).mapN(LookupResourcesResponse.apply)
}
