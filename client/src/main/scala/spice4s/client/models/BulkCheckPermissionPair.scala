/*
 * Copyright 2023 CaseHubDK
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.{experimental_service => es}

final case class BulkCheckPermissionPair(
    request: BulkCheckPermissionRequestItem,
    response: BulkCheckPermissionPair.Response
)

object BulkCheckPermissionPair {
  sealed trait Response extends Product with Serializable
  object Response {
    final case class Item(value: BulkCheckPermissionResponseItem) extends Response
    final case class Error(value: com.google.rpc.status.Status) extends Response

    def decode(x: es.BulkCheckPermissionPair.Response): Decoded[Response] = {
      import es.BulkCheckPermissionPair.{Response => R}
      x match {
        case R.Item(x)  => BulkCheckPermissionResponseItem.decode(x).map(Item.apply)
        case R.Error(x) => Error(x).pure[Decoded]
        case R.Empty    => raise("empty response")
      }
    }
  }

  def decode(x: es.BulkCheckPermissionPair): Decoded[BulkCheckPermissionPair] =
    (
      field("request")(req(x.request) andThen BulkCheckPermissionRequestItem.decode),
      field("response")(Response.decode(x.response))
    ).mapN(BulkCheckPermissionPair.apply)
}
