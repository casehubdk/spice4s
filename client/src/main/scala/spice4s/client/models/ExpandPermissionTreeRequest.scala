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

import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class ExpandPermissionTreeRequest(
    consistency: Option[Consistency] = None,
    resource: ObjectReference,
    permission: Relation
) {
  def encode = ps.ExpandPermissionTreeRequest.of(
    consistency.map(_.encode),
    resource.encode.some,
    permission.value
  )
}
