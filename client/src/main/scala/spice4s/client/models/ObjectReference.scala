package spice4s.client.models

import com.authzed.api.v1.core

final case class ObjectReference(
    objectType: Type,
    objectId: Id
) {
  def encode = core.ObjectReference.of(
    objectType.value,
    objectId.value
  )
}
