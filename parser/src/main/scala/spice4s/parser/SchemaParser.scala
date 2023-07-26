package spice4s.parser

import cats.implicits._
import cats.parse.{Parser => P, Numbers => N, Rfc5234 => R, Parser0 => P0}
import cats.data._

object SchemaParser {
  val whiteSpace = R.wsp

  val lineTerminator = R.lf | R.crlf | R.cr

  val lineComment = P.string("//") *> P.anyChar.repUntil(lineTerminator).void
  val blockComment = P.string("/*") *> P.anyChar.repUntil(P.string("*/")).void

  val sep = blockComment | (lineComment *> lineTerminator) | lineTerminator | whiteSpace

  val seps0 = sep.rep0.void

  def p[A](p: P[A]): P[A] = p <* sep.rep0.void
  def t(c: Char): P[Unit] = p(P.char(c))
  def s(s: String): P[Unit] = p(P.string(s))

  val lowerAlpha = P.charIn('a' to 'z')

  val identBody = lowerAlpha | N.digit

  val identSuffix = identBody | P.charIn("_")

  val definition = s("definition")

  //"^([a-z][a-z0-9_]{1,61}[a-z0-9]/)?[a-z][a-z0-9_]{1,62}[a-z0-9]$".r
  val resourceName = P.string {
    def ident(n: Int) = lowerAlpha *> identBody.rep(1, n) *> identSuffix
    (ident(61).soft *> P.char('/')).?.with1 *> ident(62)
  }

  val relation = s("relation")

  //"^([a-z][a-z0-9_]{1,62}[a-z0-9])?$".r
  val relationName = P.string {
    lowerAlpha *> identBody.rep(1, 62) *> identSuffix
  }

  sealed trait ResourceRelationType
  object ResourceRelationType {
    case object Wildcard extends ResourceRelationType
    final case class Relation(relation: String) extends ResourceRelationType
  }
  val resourceRelationType: P[ResourceRelationType] =
    (P.char('#') *> relationName).map(ResourceRelationType.Relation.apply) |
      P.string(":*").as(ResourceRelationType.Wildcard)

  final case class ResourceReference(
      resource: String,
      relation: Option[ResourceRelationType]
  )
  val resourceIndex: P[ResourceReference] =
    (resourceName ~ resourceRelationType.?).map { case (d, r) => ResourceReference(d, r) }

  sealed trait ResourceDef

  final case class RelationDef(
      name: String,
      resources: NonEmptyList[ResourceReference]
  ) extends ResourceDef
  val resourceRelation: P[RelationDef] = (
    p(relation *> relationName) <* t(':'),
    p(resourceIndex).repSep(t('|'))
  ).mapN(RelationDef.apply)

  val permission = s("permission")

  final case class PermissionExpr(xs: NonEmptyList[String])
  val permissionExpr: P[PermissionExpr] =
    relationName.repSep(s("->")).map(PermissionExpr.apply)

  sealed trait PermissionBinOp
  object PermissionBinOp {
    case object Union extends PermissionBinOp
    case object Intersection extends PermissionBinOp
    case object Exclusion extends PermissionBinOp
  }
  val permissionBinOp: P[PermissionBinOp] =
    t('+').as(PermissionBinOp.Union) |
      t('&').as(PermissionBinOp.Intersection) |
      t('-').as(PermissionBinOp.Exclusion)

  final case class PermissionDef(
      name: String,
      head: PermissionExpr,
      tail: List[(PermissionBinOp, PermissionExpr)]
  ) extends ResourceDef
  val permissionDef: P[PermissionDef] = (
    (permission *> p(relationName) <* t('=')) ~
      permissionExpr ~
      (permissionBinOp ~ permissionExpr).rep0
  ).map { case ((name, hd), tl) => PermissionDef(name, hd, tl) }

  val resourceDef: P[ResourceDef] =
    p(resourceRelation) |
      p(permissionDef)

  final case class Resource(
      name: String,
      content: List[ResourceDef]
  )
  val resource: P[Resource] = p {
    (
      (definition *> p(resourceName)) ~
        (t('{') *> resourceDef.rep0 <* t('}'))
    ).map { case (name, content) => Resource(name, content) }
  }

  val schema: P0[List[Resource]] = resource.rep0 <* seps0
}
