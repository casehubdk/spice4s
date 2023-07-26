package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class LookupSubjectsResponse(
    lookupUpAt: ZedToken,
    subject: ResolvedSubject,
    excludedSubjects: List[ResolvedSubject],
    afterResultCursor: Cursor
)

object LookupSubjectsResponse {
  def decode(x: ps.LookupSubjectsResponse): Decoded[LookupSubjectsResponse] =
    (
      field("looked_up_at")(req(x.lookedUpAt) andThen ZedToken.decode andThen req),
      field("subject")(req(x.subject) andThen ResolvedSubject.decode),
      field("excluded_subjects") {
        indexed {
          x.excludedSubjects.toList.map(ResolvedSubject.decode)
        }
      },
      field("after_result_cursor")(req(x.afterResultCursor) andThen Cursor.decode andThen req)
    ).mapN(LookupSubjectsResponse.apply)
}
