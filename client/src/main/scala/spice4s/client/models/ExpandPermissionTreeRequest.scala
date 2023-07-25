package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class ExpandPermissionTreeRequest(
    consistency: Option[Consistency],
    resource: ObjectReference,
    permission: Relation
) {
  def encode = ps.ExpandPermissionTreeRequest.of(
    consistency.map(_.encode),
    resource.encode.some,
    permission.value
  )
}
