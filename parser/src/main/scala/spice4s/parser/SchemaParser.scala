package spice4s.parser

import cats.parse.{Parser => P, Numbers => N, Rfc5234 => R, Parser0 => P0, Caret}
import cats.data._

object SchemaParser {
  val whiteSpace = R.wsp

  val lineTerminator = R.lf | R.crlf | R.cr

  val lineComment = (P.string("//").with1 *> P.anyChar.repUntil(lineTerminator) *> lineTerminator).void
  val blockComment = (P.string("/*").with1 *> P.anyChar.repUntil(P.string("*/")) *> P.string("*/")).void

  val sep = blockComment | lineComment | lineTerminator | whiteSpace

  val seps0 = sep.rep0.void

  def p[A](p: P[A]): P[A] = p <* sep.rep0.void
  def t(c: Char): P[Unit] = p(P.char(c))
  def s(s: String): P[Unit] = p(P.string(s))

  val lowerAlpha = P.charIn('a' to 'z')

  val identSuffix = lowerAlpha | N.digit

  val identBody = identSuffix | P.char('_')

  val definition = s("definition")

  //"^([a-z][a-z0-9_]{1,61}[a-z0-9]/)?[a-z][a-z0-9_]{1,62}[a-z0-9]$".r

  val resourceName = P.string {
    def ident(n: Int) = lowerAlpha *> (identBody.soft <* P.peek(identBody)).rep(1, n) *> identSuffix

    (ident(61).soft *> P.char('/')).?.with1 *> ident(62)
  }

  val relation = s("relation")

  //"^([a-z][a-z0-9_]{1,62}[a-z0-9])?$".r
  val relationName = P.string {
    lowerAlpha *> (identBody.soft <* P.peek(identBody)).rep(1, 62) *> identSuffix
  }

  sealed trait ResourceRelationType
  object ResourceRelationType {
    case object Wildcard extends ResourceRelationType
    final case class Relation(relation: String) extends ResourceRelationType
  }
  val resourceRelationType: P[ResourceRelationType] =
    (P.char('#') *> relationName).map(ResourceRelationType.Relation.apply) |
      P.string(":*").backtrack.as(ResourceRelationType.Wildcard)

  final case class ResourceReference(
      resource: String,
      relation: Option[ResourceRelationType],
      caret: Caret
  )
  val resourceIndex: P[ResourceReference] =
    (P.caret.with1 ~ resourceName ~ resourceRelationType.?).map { case ((c, d), r) => ResourceReference(d, r, c) }

  sealed trait ResourceDef {
    def name: String
  }

  final case class RelationDef(
      name: String,
      resources: NonEmptyList[ResourceReference],
      caret: Caret
  ) extends ResourceDef
  val resourceRelation: P[RelationDef] = (
    P.caret.with1 ~
      (p(relation *> relationName) <* t(':')) ~
      p(resourceIndex).repSep(t('|'))
  ).map { case ((c, name), resources) => RelationDef(name, resources, c) }

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

  sealed trait PermissionExpression
  object PermissionExpression {
    final case class Leaf(lhs: String, rhs: Option[String], caret: Caret) extends PermissionExpression
    final case class BinaryOp(
        op: PermissionBinOp,
        lhs: PermissionExpression,
        rhs: PermissionExpression,
        caret: Caret
    ) extends PermissionExpression
  }

  lazy val permissionExpression: P[PermissionExpression] = P.defer {
    lazy val leaf = p(P.caret.with1 ~ relationName ~ (P.string("->").backtrack *> relationName).?)
      .map { case ((c, lhs), rhs) => PermissionExpression.Leaf(lhs, rhs, c) }

    def tryBinary(p: P[PermissionExpression]): P[PermissionExpression] =
      (p ~ (P.caret.with1 ~ permissionBinOp ~ permissionExpression).?).map {
        case (lhs, None)                 => lhs
        case (lhs, Some(((c, op), rhs))) => PermissionExpression.BinaryOp(op, lhs, rhs, c)
      }

    tryBinary(leaf) | tryBinary(permissionExpression.between(t('('), t(')')))
  }

  final case class PermissionDef(
      name: String,
      expr: PermissionExpression,
      caret: Caret
  ) extends ResourceDef
  val permissionDef: P[PermissionDef] = (
    P.caret.with1 ~ (permission *> p(relationName) <* t('=')) ~ permissionExpression
  ).map { case ((c, name), expr) => PermissionDef(name, expr, c) }

  val resourceDef: P[ResourceDef] = p(resourceRelation) | p(permissionDef)

  final case class Resource(
      name: String,
      content: List[ResourceDef],
      caret: Caret
  ) {
    lazy val lookup = content.map(x => x.name -> x).toMap
  }
  val resource: P[Resource] = p {
    (
      P.caret.with1 ~ (definition *> p(resourceName)) ~ resourceDef.rep0.between(t('{'), t('}'))
    )
      .map { case ((c, name), content) => Resource(name, content, c) }
  }

  val schema: P0[List[Resource]] = {
    seps0 *> resource.rep0
  }
}
