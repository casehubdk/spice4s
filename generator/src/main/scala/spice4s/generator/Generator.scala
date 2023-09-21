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

/*
case class CaseClass(id: ID) extends Spice4sResource { 
  def companion: Spice4sCompanion[CaseClass] = CaseClass

  def relation1(that: That): PermissionRequest[CaseClass, Relation1.type, That] = 
    PermissionRequest(this, Relation1, that, None)
  def relation2[A <: Spice4sResource](that: A)(implicit ev: Relation2[A]): PermissionRequest[CaseClass, Relation2.type, A] = 
    PermissionRequest(this, Relation2, that, None)
}

object CaseClass extends Spice4sCompanion[CaseClass] {
  implicit val constants: Spice4sResourceConstants[CaseClass] = new Spice4sResourceConstants[CaseClass] {
    def objectType = Type.unsafeFromString("case_class")
  }

  implicit object Relation1 extends Spice4sRelation[CaseClass, Relation1.type, That] {
    def resource = CaseClass
    def relation = Relation.unsafeFromString("relation1")
    def subResource = That
  }

  sealed abstract class Relation2Union[A <: Spice4sResource](val a: Spice4sCompanion[A]) extends Spice4sUnion[A]
  implicit object Relation2That extends Relation2Union[That](That)
  implicit object Relation2Other extends Relation2Union[Other](Other)

  implicit object Relation2 extends Spice4sUnionRelation[CaseClass, Relation2.type, Relation2Union] {
    def resource = CaseClass
    def relation = Relation.unsafeFromString("relation2")
    def subs = NonEmptyList.of(Relation2That, Relation2Other)
  }
}

case class That(id: ID) extends Spice4sResource {
  def companion: Spice4sCompanion[That] = That
}

object That extends Spice4sCompanion[That] {
  implicit val constants: Spice4sResourceConstants[That] = new Spice4sResourceConstants[That] {
    def objectType = Type.unsafeFromString("that")
  }
}

case class Other(...
...
*/
object Generator extends App {
  final case class Error(message: String, position: Caret)

    case class ResourceId(
      name: String,
      namespace: Option[String]
    ) {
      def combined = namespace.foldMap(_ + "_") + name
      def idName = combined + "_id"
      def typename: Type.Name = snake2Type(idName)
      def typeclass: Term.Name = Term.Name(snake2camel(idName))
      def smartConstructorName: Term.Name = Term.Name(snake2camel(combined))
      def typePath = 
        namespace.map(x => Type.Select(snake2Obj(x), snake2Type(name))).getOrElse(snake2Type(name))
      def termPath = 
        namespace.map(x => Term.Select(snake2Obj(x), snake2Obj(name))).getOrElse(snake2Obj(name))
    }

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

  case class PossibleType(typename: String) {
    val (typenamePrefix, actualTypename) = {
      val (lst, prefix) = typename.split("/").toList.toNel
        .map(xs => (xs.last, xs.init))
        .getOrElse((typename, Nil))
      val fullPrefix = Term.Name("self") :: prefix.map(snake2Obj)
      fullPrefix.foldLeft(Option.empty[Term.Ref]) { case (z, nxt) =>
        z.map(Term.Select(_, nxt)).orElse(Some(nxt))
      } -> lst
    }

    def tpe = {
      val r = snake2Type(actualTypename)
      typenamePrefix.map(Type.Select(_, r)).getOrElse(r)
    }
    def companion = {
      val r = snake2Obj(actualTypename)
      typenamePrefix.map(Term.Select(_, r)).getOrElse(r)
    }
    
    def unionName(parentUnionName: String) = parentUnionName + "_" + typename
    def unionCompanion(parentUnionName: String) = snake2Obj(unionName(parentUnionName))
    def unionImpl(parentUnionName: String) = {
      val ext = 
        Init(
        Type.Apply(snake2Type(parentUnionName), Type.ArgClause(List(tpe))),
        Name.Anonymous(),
        Seq(Term.ArgClause(List(companion)))
      )
      q"""implicit object ${unionCompanion(parentUnionName)} extends $ext"""
    }
  }
  
  final case class MakeRelation(
      rd: ResourceDef,
      possibleTypes: NonEmptyList[PossibleType],
      subjectRelation: Option[String]
  ) {
    def baseName = rd.name + subjectRelation.foldMap("_" + _)

    def companion = snake2Obj(baseName)
    def typename = Type.Singleton(companion)

    def unionName = baseName + "_union"
    def unionCompanion = snake2Obj(unionName)
    def unionType = snake2Type(unionName)

    def relationBaseName = baseName + "_relation_type"
    def relationCompanion = snake2Obj(relationBaseName)
    def relationTpe = Type.Singleton(relationCompanion)

    def caseClassMethod(caseClass: Type.Name, parentCompanion: Term.Name) = {
      val methodName = Term.Name(snake2camel(baseName))
      val sr = subjectRelation.map(x => q"Some(Relation.unsafeFromString(${Lit.String(x)}))").getOrElse(q"None")
      possibleTypes match {
        case NonEmptyList(x, Nil) => 
          q"""
            def $methodName(that: ${x.tpe}): PermissionRequest[
              $caseClass,
              $parentCompanion.$relationCompanion.type,
              ${x.tpe}
            ] = $parentCompanion.$companion(this, that, $sr)
          """
        case _ => 
          q"""
            def $methodName[A <: Spice4sResource](that: A)(implicit ev: $companion.$unionType[A]): PermissionRequest[
              $caseClass,
              $parentCompanion.$relationCompanion.type,
              ${Type.Name("A")}
            ] = $parentCompanion.$companion(this, that, $sr)
          """
      }
    }

    def unionHierachy = 
      (
        q"""
          sealed abstract class ${unionType}[A <: Spice4sResource](val a: Spice4sCompanion[A]) extends Spice4sUnion[A]
        """ :: possibleTypes.toList.map(_.unionImpl(unionName))
      ).filter(_ => possibleTypes.size > 1)

    def relationType = 
      q"""
        implicit object ${relationCompanion} extends Spice4sRelationType {
          def relation = Relation.unsafeFromString(${Lit.String(rd.name)})
        }
      """

    def relationObj(caseClass: Type.Name, parentCompanion: Term.Name) = {
      val xs = List(
        q"""
          def resource = $parentCompanion
        """,
        q"""
          def relation = $relationCompanion
        """
      )

      possibleTypes match {
        case NonEmptyList(x, Nil) => 
          q"""
            implicit object ${companion} extends Spice4sRelation[
              ${caseClass},
              $relationTpe,
              ${x.tpe}
            ] {
              def subResource = ${x.companion}
              ..$xs
            }
        """
        case _ => 
          q"""
            implicit object ${companion} extends Spice4sUnionRelation[
              ${caseClass},
              $relationTpe,
              ${unionType}
            ] {
              def subs = NonEmptyList.of(..${possibleTypes.toList.map(_.companion)})
              ..$xs
            }
        """
      }
    }
  }

  def resourceCaseClass(
    caseClass: Type.Name, 
    companion: Term.Name,
    mrs: List[MakeRelation]
  ): Defn.Class = 
    q"""
    case class ${caseClass}(id: Id) extends Spice4sResource {
      def companion: Spice4sCompanion[$caseClass] = ${companion}
      ..${mrs.map(_.caseClassMethod(caseClass, companion))}
    }
    """

  def resourceCompanion(
    spiceName: String,
    caseClass: Type.Name, 
    companion: Term.Name,
    mrs: List[MakeRelation]
  ): Defn.Object = {
    val ext = Init(
      Type.Apply(Type.Name("Spice4sCompanion"), Type.ArgClause(List(caseClass))),
      Name.Anonymous(),
      Seq.empty
    )
    q"""
      object ${companion} extends $ext {
        def constants: Spice4sConstants[$caseClass] = new Spice4sConstants[$caseClass] {
          def objectType = Type.unsafeFromString(${Lit.String(spiceName)})
        }
        ..${mrs.flatMap(mr => mr.unionHierachy ++ List(mr.relationType, mr.relationObj(caseClass, companion)))}
      }
    """
  }

  def doResource(res: Resource, computed: State): List[Defn] = {
    val zs: List[MakeRelation] = res.content.flatMap { 
        case r: RelationDef =>
          val pureResources = r.resources
            .collect { case ResourceReference(resource, None, _) => resource }
          val labelledEdges = r.resources
            .collect { case ResourceReference(resource, Some(ResourceRelationType.Relation(label)), _) =>
              label -> resource
            }
            .groupMap { case (k, _) => k } { case (_, v) => v }

          pureResources.toNel.toList.map(nel => MakeRelation(r, nel.map(PossibleType(_)), None)) ++
            labelledEdges.toList
              .collect { case (sr, x :: xs) => MakeRelation(r, NonEmptyList(x, xs).map(PossibleType(_)), Some(sr)) }
        case pd: PermissionDef =>
          computed.lookup(res.name, pd.name).toNel.toList.map { nel =>
            MakeRelation(pd, nel.map(PossibleType(_)), None)
          }
    }

    val caseClass = snake2Type(res.name)
    val companion = snake2Obj(res.name)
    List(
      resourceCaseClass(caseClass, companion, zs),
      resourceCompanion(res.name, caseClass, companion, zs)
    )
  }

  type Effect[A] = WriterT[EitherNec[String, *], List[ResourceId], A]
  def raise[A](errs: NonEmptyChain[String]): Effect[A] = WriterT.liftF(errs.asLeft)
  def pure[A](a: A): Effect[A] = Monad[Effect].pure(a)
  def tell(a: List[ResourceId]): Effect[Unit] = WriterT.tell(a)
  def convertSchema(schema: String): Effect[List[Defn]] = {
    val res = spice4s.parser.Parse.parseWith(spice4s.parser.SchemaParser.schema)(schema)

    res match {
      case Left(err) => raise(NonEmptyChain.of(err))
      case Right(ress) =>
        val s = convert(ress)
        s match {
          case Validated.Invalid(errs) =>
            raise(
              errs.map { err =>
                val (msg, _, _) = spice4s.parser.ParserUtil.showVirtualTextLine(schema, err.position.offset)
                s"${err.message}:\n" + msg + "\n"
              }
            )
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
              .flatTraverse { case (ns, ress) =>
                ress.flatTraverse{ x =>
                  val rid = ResourceId(x.name, ns)
                  tell(List(rid)) >> pure(doResource(x, computed))
                }.map{ body => 
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
              }
        }
    }
  }

  def generate(schema: String): EitherNec[String, String] =
    convertSchema(schema).run.map { case (names, xs) =>
      val allNames = names.toSet

      val prefix = List(
        q"package spice4s.generated",
        q"import spice4s.client.models._",
        q"import spice4s.generator.core._",
        q"import spice4s.encoder._"
      )

      
        val params = allNames.toList
          .map{ n => 
            val k = n.typeclass.value.head.toLower.toString() + n.typeclass.value.tail
            val ap = Type.Apply(Type.Name("Spice4sIdEncoder"), Type.ArgClause(List(n.typename)))
            Term.Param(List(Mod.ValParam()), Term.Name(k), Some(ap), None)
          }

        val smartConstructors = allNames.map{ rid =>
          q"""
            def ${rid.smartConstructorName}(a: ${rid.typename}): ${rid.typePath} = 
              ${rid.termPath}(${rid.typeclass}.encode(a))
          """
        }

      val body = q"""
      abstract class Schema[..${allNames.map(_.typename).toList.map(Type.Param(Nil, _, Nil, Type.Bounds(None, None), Nil, Nil))}](
          ..$params
      ) { self =>
        ..${xs ++ smartConstructors}
      }
      """

      val all = prefix ++ List(body)

      all.foldMap(_.syntax + "\n")
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
