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

final case class LookupSubjectsRequest(
    consistency: Option[Consistency] = None,
    objectReference: ObjectReference,
    permission: Relation,
    subjectObjectType: Type,
    subjectRelation: Option[Relation] = None,
    limit: Option[Limit] = None,
    cursor: Option[Cursor] = None,
    wildcardOption: Option[LookupSubjectsRequest.WildcardOption] = None
) {
  def encode = ps.LookupSubjectsRequest.of(
    consistency.map(_.encode),
    objectReference.encode.some,
    permission.value,
    subjectObjectType.value,
    subjectRelation.foldMap(_.value),
    None,
    limit.foldMap(_.value),
    cursor.map(_.encode),
    wildcardOption.map(_.encode).getOrElse(ps.LookupSubjectsRequest.WildcardOption.WILDCARD_OPTION_UNSPECIFIED)
  )
}

object LookupSubjectsRequest {
  sealed trait WildcardOption {
    def encode: ps.LookupSubjectsRequest.WildcardOption
  }
  object WildcardOption {
    import ps.LookupSubjectsRequest.WildcardOption._
    case object Include extends WildcardOption {
      def encode: ps.LookupSubjectsRequest.WildcardOption = WILDCARD_OPTION_INCLUDE_WILDCARDS
    }
    case object Exclude extends WildcardOption {
      def encode: ps.LookupSubjectsRequest.WildcardOption = WILDCARD_OPTION_EXCLUDE_WILDCARDS
    }
  }
}
