package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class CheckPermissionResponse(
    checkedAt: ZedToken,
    permissionship: CheckPermissionResponse.Permissionship
)

object CheckPermissionResponse {
  sealed trait Permissionship extends Product with Serializable
  object Permissionship {
    case object NoPermission extends Permissionship
    case object HasPermission extends Permissionship
    case object ConditionalPermission extends Permissionship

    def decode(x: ps.CheckPermissionResponse.Permissionship): Decoded[Permissionship] = {
      import ps.CheckPermissionResponse.Permissionship._
      x match {
        case PERMISSIONSHIP_NO_PERMISSION                 => NoPermission.validNec
        case PERMISSIONSHIP_HAS_PERMISSION                => HasPermission.validNec
        case PERMISSIONSHIP_CONDITIONAL_PERMISSION        => ConditionalPermission.validNec
        case PERMISSIONSHIP_UNSPECIFIED | Unrecognized(_) => raise("permissionship unspecified")
      }
    }
  }

  def decode(x: ps.CheckPermissionResponse): Decoded[CheckPermissionResponse] =
    (
      field("checkedAt")(req(x.checkedAt) andThen ZedToken.decode andThen req),
      field("permissionship")(Permissionship.decode(x.permissionship))
    ).mapN(CheckPermissionResponse.apply)
}
