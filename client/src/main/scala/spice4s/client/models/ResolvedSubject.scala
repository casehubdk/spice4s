package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class ResolvedSubject(
    subjectObjectId: String,
    permissionship: LookupPermissionship
)

object ResolvedSubject {
  def decode(x: ps.ResolvedSubject): Decoded[ResolvedSubject] =
    (
      x.subjectObjectId.validNec,
      field("permissionship")(LookupPermissionship.decode(x.permissionship))
    ).mapN(ResolvedSubject.apply)
}
