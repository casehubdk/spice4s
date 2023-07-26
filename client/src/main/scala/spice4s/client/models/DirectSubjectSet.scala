package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.core

final case class DirectSubjectSet(
    subjects: List[SubjectReference]
)

object DirectSubjectSet {
  def decode(x: core.DirectSubjectSet) =
    (
      field("subjects") {
        indexed {
          x.subjects.toList.map(SubjectReference.decode)
        }
      }
    ).map(DirectSubjectSet.apply)
}
