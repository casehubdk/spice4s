package spice4s.client.models

sealed abstract case class ResourceType private (value: String)

object ResourceType {
  import com.authzed.api.v1.{permission_service => perm}
}
