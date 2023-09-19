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

package spice4s.generator

import cats.effect.{Resource => _, _}
import fs2.io.file._
import cats._
import cats.implicits._
import scala.meta._
import spice4s.parser.SchemaParser._
import cats.data._
import cats.parse.Caret
import scala.annotation.tailrec

object Generator extends App {
  final case class Error(message: String, position: Caret)

  def convert(ress: List[Resource]) = {
    val lookup = ress.map(res => res.name -> res).toMap

    def raise[A](msg: String, caret: Caret): ValidatedNec[Error, A] =
      Error(msg, caret).invalidNec

    def verifyResource(res: String, caret: Caret): ValidatedNec[Error, Resource] =
      lookup.get(res) match {
        case Some(x) => x.validNec
        case None    => raise(s"Resource '$res' is not defined", caret)
      }

    def verifyRelPerm(res: Resource, rel: String, caret: Caret): ValidatedNec[Error, Rel] =
      res.lookup.get(rel) match {
        case Some(_) => Rel(res.name, rel).validNec
        case None    => raise(s"Relation '$rel' is not defined on resource '${res.name}'", caret)
      }

    def verifyResourceRel(res: String, rel: String, caret: Caret): ValidatedNec[Error, Rel] =
      verifyResource(res, caret).andThen(res => verifyRelPerm(res, rel, caret))

    ress
      .flatTraverse { res =>
        res.content.traverse { y =>
          val here: ValidatedNec[Error, Solution] = y match {
            case rd: RelationDef =>
              rd.resources.toList.foldMapA { r =>
                r.relation match {
                  case None | Some(ResourceRelationType.Wildcard) =>
                    verifyResource(r.resource, r.caret) as
                      Solution(Set(r.resource), Set.empty, Set.empty)
                  case Some(ResourceRelationType.Relation(rel)) =>
                    verifyResourceRel(r.resource, rel, r.caret).map { rel =>
                      Solution(Set.empty, Set(rel), Set.empty)
                    }
                }
              }
            case pd: PermissionDef =>
              def go(x: PermissionExpression): ValidatedNec[Error, Solution] =
                x match {
                  case PermissionExpression.Leaf(lhs, None, c) =>
                    verifyResourceRel(res.name, lhs, c).map { rel =>
                      Solution(Set.empty, Set(rel), Set.empty)
                    }
                  case PermissionExpression.Leaf(lhs, Some(rhs), c) =>
                    verifyResourceRel(res.name, lhs, c).map { rel =>
                      Solution(Set.empty, Set.empty, Set(Arrow(rel, rhs)))
                    }
                  case PermissionExpression.BinaryOp(_, lhs, rhs, _) =>
                    go(lhs) |+| go(rhs)
                }
              go(pd.expr)
          }

          here.tupleLeft(Rel(res.name, y.name))
        }
      }
      .map(x => State(x.toMap))
  }

  def snake2camel(x: String) = {
    val xs = x.split("_")
    xs.headOption.mkString ++ xs.tail.toList.foldMap(_.capitalize)
  }

  def snake2Type(x: String): Type.Name =
    Type.Name(snake2camel(x).capitalize)

  def snake2Obj(x: String): Term.Name =
    Term.Name(snake2camel(x).capitalize)

  def definitionTypeReference(x: String): Type.Ref =
    x.split("/").toList match {
      case x :: Nil      => snake2Type(x)
      case x :: y :: Nil => Type.Select(snake2Obj(x), snake2Type(y))
      case _             => ???
    }

  def definitionValueReference(x: String): Term.Ref =
    x.split("/").toList match {
      case x :: Nil      => snake2Obj(x)
      case x :: y :: Nil => Term.Select(snake2Obj(x), snake2Obj(y))
      case _             => ???
    }

  def resourceRelationTrait(relation: String, possibleTypes: NonEmptyList[String]) = {
    val traitName = snake2Type(relation)
    val pts = possibleTypes.map { pt =>
      val ext = Init(Type.Apply(traitName, Type.ArgClause(List(definitionTypeReference(pt)))), Name.Anonymous(), Seq.empty)
      q"implicit object ${snake2Obj(relation.capitalize + "_" + pt.capitalize)} extends $ext"
    }

    NonEmptyList(
      q"sealed trait ${traitName}[A]",
      pts.toList
    )
  }

  def resourceRelationMethod(
      thisType: Type.Name,
      companion: Term.Name,
      relation: String,
      tpe: Either[Term.Name, String],
      subjectRelation: Option[String]
  ) = {
    val sr = subjectRelation match {
      case None     => q"None"
      case Some(sr) => q"Some(Relation.unsafeFromString(${Lit.String(sr)}))"
    }
    val relName = relationCompanionName(relation)
    val rel = Term.Select(companion, relName)

    val relTpe = Type.Singleton(rel)

    def impl(that: Type.Ref) =
      q"""PermissionRequest[$thisType, $relTpe, $that](this, $rel, that, $sr)"""

    val n = relation + subjectRelation.foldMap("_" + _)
    tpe match {
      case Right(x) =>
        val thatType = definitionTypeReference(x)
        q"def ${Term.Name(snake2camel(n))}(that: $thatType): PermissionRequest[$thisType, $relTpe, $thatType] = ${impl(thatType)}"
      case Left(companion) =>
        val thatType = Type.Name("A")
        q"""
          def ${Term.Name(snake2camel(n))}[A <: Spice4sResource](
            that: A
          )(implicit ev: ${Type.Select(companion, snake2Type(relation))}[A]): PermissionRequest[$thisType, $relTpe, $thatType] = 
            ${impl(thatType)}
        """
    }
  }

  def doResource(res: Resource, computed: State) = {
    final case class MakeRelation(
        name: String,
        possibleTypes: NonEmptyList[String],
        subjectRelation: Option[String]
    )
    val zs = res.content.flatMap { rd =>
      val mrs = rd match {
        case r: RelationDef =>
          val pureResources = r.resources
            .collect { case ResourceReference(resource, None, _) => resource }
          val labelledEdges = r.resources
            .collect { case ResourceReference(resource, Some(ResourceRelationType.Relation(label)), _) =>
              label -> resource
            }
            .groupMap { case (k, _) => k } { case (_, v) => v }

          pureResources.toNel.toList.map(nel => MakeRelation(rd.name, nel, None)) ++
            labelledEdges.toList
              .collect { case (sr, x :: xs) => MakeRelation(rd.name, NonEmptyList(x, xs), Some(sr)) }
        case pd: PermissionDef =>
          computed.lookup(res.name, pd.name).toNel.toList.map { nel =>
            MakeRelation(pd.name, nel, None)
          }
      }

      mrs tupleLeft rd
    }

    val ys = zs.map { case (rd, mr) =>
      mr.possibleTypes match {
        case NonEmptyList(head, Nil) => (rd, mr, Option(head))
        case _                       => (rd, mr, none)
      }
    }

    val objName = snake2Obj(res.name)

    val caseClassName = snake2Type(res.name)

    val companionContent = ys.collect { case (rd, mr, None) => rd -> mr }

    val companion: Defn.Object = {
      val constants = q"""
        implicit def constants: Spice4sResourceConstants[$caseClassName] = 
          new Spice4sResourceConstants[$caseClassName] {
            def objectType: Type = Type.unsafeFromString(${Lit.String(res.name)})
          }
      """

      val members = constants :: companionContent.flatMap { case (rd, mr) =>
        resourceRelationTrait(rd.name + mr.subjectRelation.foldMap("_" + _), mr.possibleTypes).toList
      } ++ ys.map { case (rd, _, _) => rd.name }.distinct.map(relationDefCompanion)
      q"""
        object ${objName} {
          ..${members}
        }
      """
    }

    val singulars = ys.collect { case (rd, mr, Some(t)) => (rd, mr, t) }

    val singularCls = singulars.map { case (rd, mr, name) =>
      resourceRelationMethod(caseClassName, objName, rd.name, Right(name), mr.subjectRelation)
    }

    val unionCls = companionContent.map { case (rd, mr) =>
      resourceRelationMethod(caseClassName, objName, rd.name, Left(objName), mr.subjectRelation)
    }

    val combined = q"def constants: Spice4sResourceConstants[$caseClassName] = ${objName}.constants" ::
      (singularCls ++ unionCls)
    List(
      q"""
        case class $caseClassName(id: Id) extends Spice4sResource {
            ..${combined}
          }
      """
    ) ++ List(companion)
  }

  def relationCompanionName(rd: String): Term.Name =
    snake2Obj(rd + "Relation")

  def relationDefCompanion(rd: String): Defn.Object = {
    val n = Lit.String(rd)
    val defName = relationCompanionName(rd)
    q"""
      implicit object ${defName} extends Spice4sRelation {
        def relation = Relation.unsafeFromString($n)
      }
    """
  }

  def convertSchema(schema: String) = {
    val res = spice4s.parser.Parse.parseWith(spice4s.parser.SchemaParser.schema)(schema)

    res match {
      case Left(err) => err.leftNec
      case Right(ress) =>
        val s = convert(ress)
        s match {
          case Validated.Invalid(errs) =>
            errs.map { err =>
              val (msg, _, _) = spice4s.parser.ParserUtil.showVirtualTextLine(schema, err.position.offset)
              s"${err.message}:\n" + msg + "\n"
            }.asLeft
          case Validated.Valid(state) =>
            val computed = compute(state)
            val namespaced = ress.map { res =>
              val parts = res.name.split("/")
              parts.toList match {
                case ns :: name :: Nil => (Some(ns) -> res.copy(name = name))
                case _                 => (None -> res)
              }
            }
            namespaced
              .groupMap { case (ns, _) => ns } { case (_, res) => res }
              .toList
              .flatMap { case (ns, ress) =>
                val body = ress.flatMap(doResource(_, computed))

                ns match {
                  case None => body
                  case Some(ns) =>
                    List(
                      q"""
                        object ${snake2Obj(ns)} {
                          ..$body
                        }
                      """
                    )
                }
              }
              .asRight
        }
    }
  }

  def generate(schema: String): EitherNec[String, String] =
    convertSchema(schema).map { xs =>
      val prefix = List(
        q"package spice4s.generated",
        q"import spice4s.client.models._",
        q"import spice4s.generator.core._"
      )
      val all = prefix ++ xs
      all.map(_.syntax).mkString("\n\n")
    }

  def generateFromTo[F[_]: Files](from: Path, to: Path)(implicit F: Async[F]): F[Option[NonEmptyChain[String]]] =
    Files[F]
      .readUtf8(from)
      .compile
      .string
      .map(generate)
      .flatMap {
        case Left(err) => F.pure(Some(err))
        case Right(output) =>
          fs2
            .Stream(output)
            .through(fs2.text.utf8.encode[F])
            .through(Files[F].writeAll(to))
            .compile
            .drain as none
      }

  final case class Rel(d: String, r: String) {
    def ->(r2: String): Arrow = Arrow(this, r2)
    override def toString = s"$d#$r"
  }
  final case class Arrow(rel: Rel, r: String) {
    override def toString = s"$rel->$r"
  }
  final case class Solution(
      xs: Set[String],
      rs: Set[Rel],
      arrows: Set[Arrow]
  ) {
    override def toString = s"xs: {${xs.mkString(",")}}, rs: {${rs.mkString(",")}}, arrows: {${arrows.mkString(",")}}"

    def arrow(a: String, valid: Set[Rel]) = Solution(
      Set.empty,
      xs.map(x => Rel(x, a)) & valid,
      rs.map(r => Arrow(r, a))
    )
  }
  object Solution {
    implicit lazy val solutionMonoid: Monoid[Solution] = new Monoid[Solution] {
      def empty = Solution(Set.empty, Set.empty, Set.empty)
      def combine(x: Solution, y: Solution) = Solution(
        x.xs ++ y.xs,
        x.rs ++ y.rs,
        x.arrows ++ y.arrows
      )
    }
  }

  def funktion(sol: Solution): (List[(Rel, Solution)], List[(Arrow, Solution)]) = {
    sol.rs.toList.map { r =>
      r -> sol.copy(rs = sol.rs /* - r*/ )
    } -> sol.arrows.toList.map { a =>
      a -> sol.copy(arrows = sol.arrows /*- a*/ )
    }
  }

  def go(s: State): Option[State] = {
    s.xs.toList.collectFirstSome { case (k, v) =>
      val (rs2, as2) = funktion(v)
      rs2.collectFirstSome { case (r, sol) =>
        val sol2 = s.xs(r)
        val sol3 = sol2 |+| sol
        if (sol3 != v) {
          Some(State(s.xs + (k -> sol3)))
        } else {
          None
        }
      } orElse as2.collectFirstSome { case (Arrow(r, a), sol) =>
        val sol2 = s.xs(r)
        val sol3 = sol2.arrow(a, s.xs.keySet) |+| sol
        if (sol3 != v) {
          Some(State(s.xs + (k -> sol3)))
        } else {
          None
        }
      }
    }
  }

  @tailrec
  def compute(init: State): State = go(init) match {
    case None    => init
    case Some(s) => compute(s)
  }

  final case class State(xs: Map[Rel, Solution]) {
    override def toString =
      s"State(\n${xs.map { case (k, v) => s"$k -> $v" }.map("\t" + _).mkString("\n")})"

    def lookup(resource: String, rel: String): List[String] =
      xs.get(Rel(resource, rel)).foldMap(_.xs).toList
  }
}
