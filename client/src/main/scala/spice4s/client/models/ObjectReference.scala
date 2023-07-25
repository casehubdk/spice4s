package spice4s.client.models

import com.authzed.api.v1.core
import spice4s.client.util._
import cats.implicits._

final case class ObjectReference(
    objectType: Type,
    objectId: Id
) {
  def encode = core.ObjectReference.of(
    objectType.value,
    objectId.value
  )
}

object ObjectReference {
  def decode(x: core.ObjectReference): Decoded[ObjectReference] =
    (
      field("objectType")(Type.decode(x.objectType) andThen req),
      field("objectId")(Id.decode(x.objectId) andThen req)
    ).mapN(ObjectReference.apply)
}
