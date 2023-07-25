package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class CheckPermissionRequest(
    consistency: Option[Consistency],
    resource: ObjectReference,
    permission: Relation,
    subject: SubjectReference
) {
  def encode = ps.CheckPermissionRequest.of(
    consistency.map(_.encode),
    resource.encode.some,
    permission.value,
    subject.encode.some,
    None
  )
}
