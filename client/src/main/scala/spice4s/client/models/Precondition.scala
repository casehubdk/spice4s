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

final case class Precondition(
    operation: Precondition.Operation,
    filter: RelationshipFilter
) {
  def encode = ps.Precondition.of(
    operation.encode,
    filter.encode.some
  )
}

object Precondition {
  sealed trait Operation {
    def encode: ps.Precondition.Operation
  }
  object Operation {
    import ps.Precondition.Operation._
    case object MustNotMatch extends Operation {
      def encode: ps.Precondition.Operation = OPERATION_MUST_NOT_MATCH
    }
    case object MustMatch extends Operation {
      def encode: ps.Precondition.Operation = OPERATION_MUST_MATCH
    }
  }
}
