package spice4s.generator

import cats._
import cats.mtl._
import cats.implicits._
import scala.meta._
import spice4s.parser.SchemaParser._
import cats.data._
import spice4s.client.models.Relation
import cats.parse.Caret
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Generator extends App {
  final case class Error(message: String, position: Caret)

  def convert(ress: List[Resource]) = {
    import Test._

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

  Test.schemas.map { x =>
    val res = spice4s.parser.Parse.parseWith(spice4s.parser.SchemaParser.schema)(x)
    res match {
      case Left(err) => println(err)
      case Right(ress) =>
        val s = convert(ress)
        s match {
          case Validated.Invalid(errs) =>
            println {
              errs.foldMap { err =>
                val (msg, _, _) = spice4s.parser.ParserUtil.showVirtualTextLine(x, err.position.offset)
                s"${err.message}:\n" + msg + "\n"
              }
            }
          case Validated.Valid(value) =>
            // println(value)
            Try(Test.compute(value)) match {
              // case Failure(exception) if exception.getMessage().contains("key not found") => ()
              case Failure(exception) => throw exception
              case Success(value) =>
                println(value.xs.values.filter(_.xs.isEmpty))
              // println(value)
            }
        }
    }
  }
  def resource: Defn.Trait = q"""
    trait Resource {
      def value: String
    }
  """

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

  // def resourceReferenceTrait(rr: ResourceReference, whoReferresToThis: NonEmptyList[String]) = {
  //   val w = whoReferresToThis.map(snake2Type).map(n => Init(n, Term.Name(""), Seq.empty)).toList
  //   q"sealed trait ${Type.Name(rr.resource)} extends ..$w"
  // }

  /*
   definition user {}

   definition horse {}

   definition org {
     relation member: user | horse
   }

   org:acme@member@user:kathryn
   */

  // object TypeclassExample {
  //   final case class User(value: String) extends Resource

  //   final case class Horse(value: String) extends Resource

  //   final case class Org(value: String) extends Resource {
  //     val memberRelation = Relation.unsafeFromString("member")

  //     def member[F[_], A](that: A)(implicit ev: Org.Member[A]): F[Boolean] = {
  //       ???
  //     }
  //   }
  //   object Org {
  //     protected trait Member[A]
  //     implicit object MemberUser extends Member[User]
  //     implicit object MemberHorse extends Member[Horse]
  //   }

  //   Org("hey") member User("hest")
  // }

  // trait Resource {
  //   def value: String
  // }

  // final case class User(value: String) extends Resource

  // final case class Org(value: String) extends Resource

  final case class RelationRef(resource: String, relation: String)
  type RelationCache = Map[RelationRef, Set[String]]
  type CycleSet = Set[RelationRef]
  type Effect[A] = EitherT[StateT[State[RelationCache, *], CycleSet, *], NonEmptyChain[Error], A]
  val S = Stateful[Effect, RelationCache]
  val C = Stateful[Effect, CycleSet]
  val R = Raise[Effect, NonEmptyChain[Error]]
  val Effect = Monad[Effect]

  // def raise[A](msg: String, caret: Option[Caret] = None): Effect[A] =
  //   R.raise(NonEmptyChain.one(Error(msg, caret)))

  // def relationDefTypeclass(resource: String, rd: RelationDef, lookup: Map[String, Resource]) = {
  //   def inCached(resource: String, key: RelationRef)(fa: => Effect[List[String]]): Effect[List[String]] =
  //     C.inspect(_.contains(key)).flatMap {
  //       case true => Effect.pure(List(resource))
  //       case false =>
  //         S.get.map(_.get(key)).flatMap {
  //           case Some(x) => Effect.pure(x.toList)
  //           case None    => C.modify(_ + key) *> fa
  //         }
  //     }

  //   def getResource(resource: String, caret: Caret): Effect[Resource] =
  //     lookup.get(resource) match {
  //       case None      => raise(s"resource '${resource}' not found", Some(caret))
  //       case Some(res) => Effect.pure(res)
  //     }

  // sealed trait Expandable
  // final case class Perm(x: PermissionExpression) extends Expandable
  // final case class Rels(resource: String, relation: ResourceRelationType) extends Expandable

  // final case class Solution(
  //     definitions: Set[String],
  //     expandable: Set[Expandable]
  // )
  // object Solution {
  //   implicit val monoidForSolution: Monoid[Solution] = new Monoid[Solution] {
  //     def empty: Solution = Solution(Set.empty, Set.empty)
  //     def combine(x: Solution, y: Solution): Solution =
  //       Solution(x.definitions | y.definitions, x.expandable | y.expandable)
  //   }
  // }

  // def goResource(res: Resource, rel: String): Effect[List[String]] =
  //   res.lookup.get(rel) match {
  //     case Some(rd: RelationDef) => goRelationDef(res.name, rd)
  //     case None                  => raise(s"relation '${rel}' not found in resource '${res.name}'", Some(res.caret))
  //   }

  // def goRelationDef(resource: String, rd: RelationDef): Effect[List[String]] = {
  //   val key = RelationRef(resource, rd.name)
  //   inCached(resource, key) {
  //     rd.resources.toList.parFlatTraverse { rr =>
  //       getResource(rr.resource, rr.caret).flatMap { res =>
  //         rr.relation match {
  //           case None | Some(ResourceRelationType.Wildcard) => Effect.pure(List(rr.resource))
  //           case Some(ResourceRelationType.Relation(rel))   => goResource(res, rel)
  //         }
  //       }
  //     }
  //   }
  // }

  // def goPermissionDef(resource: Resource, pd: PermissionDef) = {
  //   val key = RelationRef(resource.name, pd.name)

  //   def goPermissionExpression(pe: PermissionExpression) = pe match {
  //     case PermissionExpression.Leaf(lhs, None, c)      => getResource(lhs, c).map(_ => List(lhs))
  //     case PermissionExpression.Leaf(lhs, Some(rhs), c) =>

  //   }

  //   pd.caret
  // }

  /*
    definition organization {
    }

    definition user {
      relation org = organization
    }

    definition hest {
      relation uuuu = user | organization

      permission aaaa = uuuu->org + aaaa->org + uuuu + hest->org
    }

    final case class Organization(value: String)

    final case class User(value: String) {
      def org[F[_]](o: Organization): F[Boolean] = ???
    }

    final caes class Hest(value: String) {
      def uuuu[F[_], A](a: A)(implicit ev: Hest.Uuuu[A]): F[Boolean] = ???

      def aaaa[F[_], A](a: A)(implicit ev: Hest.Aaaa[A]): F[Boolean] = ???
    }
    object Hest {
      sealed trait Uuuu[A]
      implicit object UuuuUser extends Uuuu[User]
      implicit object UuuuOrganization extends Uuuu[Organization]

      sealed trait Aaaa[A]
      implicit object AaaaUser extends Aaaa[User]
      implicit object AaaaOrganization extends Aaaa[Organization]
      implciit object AaaaHest extends Aaaa[Hest]
    }
   */

  // val uniqueTypes = rd.resources.parTraverse { rr =>
  // }
  // q"sealed trait Hset"
  // }

  def relationDefRelation(rd: RelationDef): Defn.Val = {
    val n = Term.Name(rd.name)
    val defName = Term.Name(n.value + "Relation")
    q"val ${Pat.Var(defName)} = Relation.unsafeFromString($n)"
  }

  // println(resourceReferenceTrait(ResourceReference("")))

  // println(definitionReference("hest/hest").syntax)

  // def hesteInits = List(init"Resource", init"Resource2(hest)")

  // def definition(
  //     name: String,
  //     exts: List[Init]
  //     // partOf: List[String],
  //     // relations: List[(String, Non

  // ): Defn.Class = q"""
  // final case class ${Type.Name(name)}(value: String) extends ..$exts with Resource {
  // }
  // """

  // def relation(rd: RelationDef) = q"""
  // def ${Type.Name(rd.name)}(that: )
  // """

  // println(definition("Hest", List(init"A.B")))

  // def object
}

object S {

  trait Cont[A]
  case class Type[A](fields: List[Field[A, ?]]) extends Cont[A]
  case object Leaf extends Cont[String]

  trait Attribute[A, B]
  case class Field[A, B](f: A => B, cont: Cont[B], attributes: List[Attribute[A, B]] = Nil)

  case class Entity(name: String)

  val entity = Type[Entity](
    List(
      Field[Entity, String](_.name, Leaf)
    )
  )

  case class Contract(name: String, entity: Entity)

  val contract = Type[Contract](
    List(
      Field[Contract, String](_.name, Leaf),
      Field[Contract, Entity](_.entity, entity)
    )
  )

  trait QueryAttribute[A, B] {
    type F[A]
    type A
    def queryFragment: QueryFragment[F, A]
  }

  sealed trait QueryFragment[F[_], A]
  object QueryFragment {
    sealed trait JoinType[F[_]]
    object JoinType {
      case object One extends JoinType[Lambda[X => X]]
      case object Opt extends JoinType[Option]
    }

    case class Join[F[_]](
        table: String,
        on: String => String,
        jt: JoinType[F]
    ) extends QueryFragment[F, String]

    case class FlatMap[F[_], G[_], A, B](
        fa: QueryFragment[F, A],
        f: A => QueryFragment[G, B]
    ) extends QueryFragment[Lambda[X => F[G[X]]], B]
  }
}

object Test {
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
          // println(k -> r)
          Some(State(s.xs + (k -> sol3)))
        } else {
          None
        }
      } orElse as2.collectFirstSome { case (Arrow(r, a), sol) =>
        val sol2 = s.xs(r)
        val sol3 = sol2.arrow(a, s.xs.keySet) |+| sol
        if (sol3 != v) {
          // println(k -> (r -> a))
          Some(State(s.xs + (k -> sol3)))
        } else {
          None
        }
      }
    }
  }

  var n = 0
  @tailrec
  def compute(init: State): State = go(init) match {
    case None => init
    case Some(s) =>
      n = n + 1
      if (n > 1000) throw new Exception("too many")
      // println(s)
      compute(s)
  }

  final case class State(xs: Map[Rel, Solution]) {
    override def toString =
      s"State(\n${xs.map { case (k, v) => s"$k -> $v" }.map("\t" + _).mkString("\n")})"
  }

  /*
{
	role#member -> ({user}, {group#admin,group#membership}, {})
	role#allowed -> ({}, {role#member}, {})
	user#parent -> ({user}, {}, {})
	group#admin -> ({user}, {}, {})
	group#member -> ({user}, {}, {})
	group#membership -> ({}, {group#admin, group#member}, {group#admin->parent})
	hest#parent -> ({hest}, {hest#computed}, {})
	hest#computed -> ({}, {hest#parent}, {hest#parent->computed})
}
   */

  implicit class StringOps(private val s: String) {
    def ##(s2: String): Rel = Rel(s, s2)
  }

  // println(state)
  // println(compute(state))

  lazy val state = State(
    Map(
      "role" ## "member" -> Solution(
        Set("user"),
        Set("group" ## "admin", "group" ## "membership"),
        Set("hest" ## "parent" -> "computed", "hest" ## "computed2" -> "computed")
      ),
      "role" ## "allowed" -> Solution(
        Set.empty,
        Set("role" ## "member"),
        Set.empty
      ),
      "user" ## "parent" -> Solution(
        Set("user"),
        Set.empty,
        Set.empty
      ),
      "group" ## "admin" -> Solution(
        Set("user"),
        Set.empty,
        Set.empty
      ),
      "group" ## "member" -> Solution(
        Set("user"),
        Set.empty,
        Set.empty
      ),
      "group" ## "membership" -> Solution(
        Set.empty,
        Set("group" ## "admin", "group" ## "member"),
        Set("group" ## "admin" -> "parent")
      ),
      "hest" ## "parent" -> Solution(
        Set("hest"),
        Set("hest" ## "computed"),
        Set.empty
      ),
      "hest" ## "computed" -> Solution(
        Set.empty,
        Set("hest" ## "parent"),
        Set("hest" ## "parent" -> "computed", "hest" ## "computed2" -> "computed")
      ),
      "hest" ## "computed2" -> Solution(
        Set.empty,
        Set("hest" ## "parent"),
        Set("hest" ## "parent" -> "computed", "hest" ## "computed" -> "computed2")
      ),
      "hest" ## "computed3" -> Solution(
        Set.empty,
        Set.empty,
        Set("hest" ## "computed3" -> "computed3")
      )
    )
  )

  lazy val schemas = List(
    """
/** user represents a user */
definition user {
  relation abe: abekat
}

/** group represents a group **/
definition group {
    /** member is a member of a group, which can be a user or the membership of another group */
    relation member: user | group#member
}

/** document represents a document */
definition document {
    /** writer is a writer of the document */
    relation writer: user | group#member

    /** reader is a reader of the document */
    relation reader: user | group#member

    /** write indicates which user can write to the document */
    permission write = writer

    /** read indicates which user can read the document */
    permission read = reader + write
}
""",
    """
definition role {
	relation member: user | group#membership
	permission allowed = member
}

definition user {}

definition group {
	relation admin: user
	relation member: user
	permission membership = admin + member
}
""",
    """
definition platform {
	relation administrator: user
	permission super_admin = administrator
}

definition organization {
	relation platform: platform
	permission admin = platform->super_admin
}

definition resource {
	relation owner: user | organization
	permission admin = owner + owner->admin
}

definition user {}
""",
    """
definition user {}

definition portfolio {
	relation reader: user
	permission read = reader
}

definition folder {
	relation parent_portfolio: portfolio
	relation reader: user
	permission read = reader + parent_portfolio->read
}

definition document {
	relation parent_folder: folder
	relation reader: user

	/** read defines whether a user can read the document */
	permission read = reader + parent_folder->read
}
""",
    """
definition folder {
	relation parent: folder
	relation reader: user

	// Note that since `parent` refers to `folder` (which is this type), `parent->read` will call
	// this same permission, therefore following `read` for *that folder's* parent.
	permission read = reader + parent->read
}

definition user {}
""",
    """
definition organization {
	relation admin: user
	permission read_all_docs = admin
}

definition folder {
	relation parent: folder | organization
	relation reader: user

	// NOTE: since parent is either a folder OR an organization, if we want to check
	// in both, we need to make sure `read_all_docs` has the *same name* in both folder
	// and organization.
	permission read_all_docs = reader + parent->read_all_docs
}

definition document {
	relation parent: folder | organization
	relation reader: user

	permission view = reader + parent->read_all_docs
}

definition user {}
""",
    """
  definition user {}

  definition resource {
      relation manager: user | usergroup#member | usergroup#manager
      relation viewer: user | usergroup#member | usergroup#manager

      permission manage = manager
      permission view = viewer + manager
  }

  definition usergroup {
      relation manager: user | usergroup#member | usergroup#manager
      relation direct_member: user | usergroup#member | usergroup#manager

      permission member = direct_member + manager
  }

  definition organization {
      relation group: usergroup
      relation administrator: user | usergroup#member | usergroup#manager
      relation direct_member: user

      relation resource: resource

      permission admin = administrator
      permission member = direct_member + administrator + group->member
  }
""",
    """
  definition user {}

  definition role {
      relation spanner_databaseoperations_cancel: user:*
      relation spanner_databaseoperations_delete: user:*
      relation spanner_databaseoperations_get: user:*
      relation spanner_databaseoperations_list: user:*
      relation spanner_databaseroles_list: user:*
      relation spanner_databaseroles_use: user:*
      relation spanner_databases_beginorrollbackreadwritetransaction: user:*
      relation spanner_databases_beginpartitioneddmltransaction: user:*
      relation spanner_databases_beginreadonlytransaction: user:*
      relation spanner_databases_create: user:*
      relation spanner_databases_drop: user:*
      relation spanner_databases_get: user:*
      relation spanner_databases_getddl: user:*
      relation spanner_databases_getiampolicy: user:*
      relation spanner_databases_list: user:*
      relation spanner_databases_partitionquery: user:*
      relation spanner_databases_partitionread: user:*
      relation spanner_databases_read: user:*
      relation spanner_databases_select: user:*
      relation spanner_databases_setiampolicy: user:*
      relation spanner_databases_update: user:*
      relation spanner_databases_updateddl: user:*
      relation spanner_databases_userolebasedaccess: user:*
      relation spanner_databases_write: user:*
      relation spanner_instances_get: user:*
      relation spanner_instances_getiampolicy: user:*
      relation spanner_instances_list: user:*
      relation spanner_sessions_create: user:*
      relation spanner_sessions_delete: user:*
      relation spanner_sessions_get: user:*
      relation spanner_sessions_list: user:*
  }

  definition role_binding {
      relation user: user
      relation role: role

      permission spanner_databaseoperations_cancel = user & role->spanner_databaseoperations_cancel
      permission spanner_databaseoperations_delete = user & role->spanner_databaseoperations_delete
      permission spanner_databaseoperations_get = user & role->spanner_databaseoperations_get
      permission spanner_databaseoperations_list = user & role->spanner_databaseoperations_list
      permission spanner_databaseroles_list = user & role->spanner_databaseroles_list
      permission spanner_databaseroles_use = user & role->spanner_databaseroles_use
      permission spanner_databases_beginorrollbackreadwritetransaction = user & role->spanner_databases_beginorrollbackreadwritetransaction
      permission spanner_databases_beginpartitioneddmltransaction = user & role->spanner_databases_beginpartitioneddmltransaction
      permission spanner_databases_beginreadonlytransaction = user & role->spanner_databases_beginreadonlytransaction
      permission spanner_databases_create = user & role->spanner_databases_create
      permission spanner_databases_drop = user & role->spanner_databases_drop
      permission spanner_databases_get = user & role->spanner_databases_get
      permission spanner_databases_getddl = user & role->spanner_databases_getddl
      permission spanner_databases_getiampolicy = user & role->spanner_databases_getiampolicy
      permission spanner_databases_list = user & role->spanner_databases_list
      permission spanner_databases_partitionquery = user & role->spanner_databases_partitionquery
      permission spanner_databases_partitionread = user & role->spanner_databases_partitionread
      permission spanner_databases_read = user & role->spanner_databases_read
      permission spanner_databases_select = user & role->spanner_databases_select
      permission spanner_databases_setiampolicy = user & role->spanner_databases_setiampolicy
      permission spanner_databases_update = user & role->spanner_databases_update
      permission spanner_databases_updateddl = user & role->spanner_databases_updateddl
      permission spanner_databases_userolebasedaccess = user & role->spanner_databases_userolebasedaccess
      permission spanner_databases_write = user & role->spanner_databases_write
      permission spanner_instances_get = user & role->spanner_instances_get
      permission spanner_instances_getiampolicy = user & role->spanner_instances_getiampolicy
      permission spanner_instances_list = user & role->spanner_instances_list
      permission spanner_sessions_create = user & role->spanner_sessions_create
      permission spanner_sessions_delete = user & role->spanner_sessions_delete
      permission spanner_sessions_get = user & role->spanner_sessions_get
      permission spanner_sessions_list = user & role->spanner_sessions_list
  }

  definition project {
      relation granted: role_binding

      // Synthetic Instance Relations
      permission granted_spanner_instances_get = granted->spanner_instances_get
      permission granted_spanner_instances_getiampolicy = granted->spanner_instances_getiampolicy
      permission granted_spanner_instances_list = granted->spanner_instances_list

      // Synthetic Database Relations
      permission granted_spanner_databases_beginorrollbackreadwritetransaction = granted->spanner_databases_beginorrollbackreadwritetransaction
      permission granted_spanner_databases_beginpartitioneddmltransaction = granted->spanner_databases_beginpartitioneddmltransaction
      permission granted_spanner_databases_beginreadonlytransaction = granted->spanner_databases_beginreadonlytransaction
      permission granted_spanner_databases_create = granted->spanner_databases_create
      permission granted_spanner_databases_drop = granted->spanner_databases_drop
      permission granted_spanner_databases_get = granted->spanner_databases_get
      permission granted_spanner_databases_getddl = granted->spanner_databases_getddl
      permission granted_spanner_databases_getiampolicy = granted->spanner_databases_getiampolicy
      permission granted_spanner_databases_list = granted->spanner_databases_list
      permission granted_spanner_databases_partitionquery = granted->spanner_databases_partitionquery
      permission granted_spanner_databases_partitionread = granted->spanner_databases_partitionread
      permission granted_spanner_databases_read = granted->spanner_databases_read
      permission granted_spanner_databases_select = granted->spanner_databases_select
      permission granted_spanner_databases_setiampolicy = granted->spanner_databases_setiampolicy
      permission granted_spanner_databases_update = granted->spanner_databases_update
      permission granted_spanner_databases_updateddl = granted->spanner_databases_updateddl
      permission granted_spanner_databases_userolebasedaccess = granted->spanner_databases_userolebasedaccess
      permission granted_spanner_databases_write = granted->spanner_databases_write

      // Synthetic Sessions Relations
      permission granted_spanner_sessions_create = granted->spanner_sessions_create
      permission granted_spanner_sessions_delete = granted->spanner_sessions_delete
      permission granted_spanner_sessions_get = granted->spanner_sessions_get
      permission granted_spanner_sessions_list = granted->spanner_sessions_list

      // Synthetic Database Operations Relations
      permission granted_spanner_databaseoperations_cancel = granted->spanner_databaseoperations_cancel
      permission granted_spanner_databaseoperations_delete = granted->spanner_databaseoperations_delete
      permission granted_spanner_databaseoperations_get = granted->spanner_databaseoperations_get
      permission granted_spanner_databaseoperations_list = granted->spanner_databaseoperations_list

      // Synthetic Database Roles Relations
      permission granted_spanner_databaseroles_list = granted->spanner_databaseroles_list
      permission granted_spanner_databaseroles_use = granted->spanner_databaseroles_use
  }

  definition spanner_instance {
      relation project: project
      relation granted: role_binding

      permission get = granted->spanner_instances_get + project->granted_spanner_instances_get
      permission getiampolicy = granted->spanner_instances_getiampolicy + project->granted_spanner_instances_getiampolicy
      permission list = granted->spanner_instances_list + project->granted_spanner_instances_list

      // Synthetic Database Relations
      permission granted_spanner_databases_beginorrollbackreadwritetransaction = granted->spanner_databases_beginorrollbackreadwritetransaction + project->granted_spanner_databases_beginorrollbackreadwritetransaction
      permission granted_spanner_databases_beginpartitioneddmltransaction = granted->spanner_databases_beginpartitioneddmltransaction + project->granted_spanner_databases_beginpartitioneddmltransaction
      permission granted_spanner_databases_beginreadonlytransaction = granted->spanner_databases_beginreadonlytransaction + project->granted_spanner_databases_beginreadonlytransaction
      permission granted_spanner_databases_create = granted->spanner_databases_create + project->granted_spanner_databases_create
      permission granted_spanner_databases_drop = granted->spanner_databases_drop + project->granted_spanner_databases_drop
      permission granted_spanner_databases_get = granted->spanner_databases_get + project->granted_spanner_databases_get
      permission granted_spanner_databases_getddl = granted->spanner_databases_getddl + project->granted_spanner_databases_getddl
      permission granted_spanner_databases_getiampolicy = granted->spanner_databases_getiampolicy + project->granted_spanner_databases_getiampolicy
      permission granted_spanner_databases_list = granted->spanner_databases_list + project->granted_spanner_databases_list
      permission granted_spanner_databases_partitionquery = granted->spanner_databases_partitionquery + project->granted_spanner_databases_partitionquery
      permission granted_spanner_databases_partitionread = granted->spanner_databases_partitionread + project->granted_spanner_databases_partitionread
      permission granted_spanner_databases_read = granted->spanner_databases_read + project->granted_spanner_databases_read
      permission granted_spanner_databases_select = granted->spanner_databases_select + project->granted_spanner_databases_select
      permission granted_spanner_databases_setiampolicy = granted->spanner_databases_setiampolicy + project->granted_spanner_databases_setiampolicy
      permission granted_spanner_databases_update = granted->spanner_databases_update + project->granted_spanner_databases_update
      permission granted_spanner_databases_updateddl = granted->spanner_databases_updateddl + project->granted_spanner_databases_updateddl
      permission granted_spanner_databases_userolebasedaccess = granted->spanner_databases_userolebasedaccess + project->granted_spanner_databases_userolebasedaccess
      permission granted_spanner_databases_write = granted->spanner_databases_write + project->granted_spanner_databases_write

      // Synthetic Sessions Relations
      permission granted_spanner_sessions_create = granted->spanner_sessions_create + project->granted_spanner_sessions_create
      permission granted_spanner_sessions_delete = granted->spanner_sessions_delete + project->granted_spanner_sessions_delete
      permission granted_spanner_sessions_get = granted->spanner_sessions_get + project->granted_spanner_sessions_get
      permission granted_spanner_sessions_list = granted->spanner_sessions_list + project->granted_spanner_sessions_list

      // Synthetic Database Operations Relations
      permission granted_spanner_databaseoperations_cancel = granted->spanner_databaseoperations_cancel + project->granted_spanner_databaseoperations_cancel
      permission granted_spanner_databaseoperations_delete = granted->spanner_databaseoperations_delete + project->granted_spanner_databaseoperations_delete
      permission granted_spanner_databaseoperations_get = granted->spanner_databaseoperations_get + project->granted_spanner_databaseoperations_get
      permission granted_spanner_databaseoperations_list = granted->spanner_databaseoperations_list + project->granted_spanner_databaseoperations_list

      // Synthetic Database Roles Relations
      permission granted_spanner_databaseroles_list = granted->spanner_databaseroles_list + project->granted_spanner_databaseroles_list
      permission granted_spanner_databaseroles_use = granted->spanner_databaseroles_use + project->granted_spanner_databaseroles_use
  }

  definition spanner_database {
      relation instance: spanner_instance
      relation granted: role_binding

      // Database
      permission beginorrollbackreadwritetransaction = granted->spanner_databases_beginorrollbackreadwritetransaction + instance->granted_spanner_databases_beginorrollbackreadwritetransaction
      permission beginpartitioneddmltransaction = granted->spanner_databases_beginpartitioneddmltransaction + instance->granted_spanner_databases_beginpartitioneddmltransaction
      permission beginreadonlytransaction = granted->spanner_databases_beginreadonlytransaction + instance->granted_spanner_databases_beginreadonlytransaction
      permission create = granted->spanner_databases_create + instance->granted_spanner_databases_create
      permission drop = granted->spanner_databases_drop + instance->granted_spanner_databases_drop
      permission get = granted->spanner_databases_get + instance->granted_spanner_databases_get
      permission get_ddl = granted->spanner_databases_getddl + instance->granted_spanner_databases_getddl
      permission getiampolicy = granted->spanner_databases_getiampolicy + instance->granted_spanner_databases_getiampolicy
      permission list = granted->spanner_databases_list + instance->granted_spanner_databases_list
      permission partitionquery = granted->spanner_databases_partitionquery + instance->granted_spanner_databases_partitionquery
      permission partitionread = granted->spanner_databases_partitionread + instance->granted_spanner_databases_partitionread
      permission read = granted->spanner_databases_read + instance->granted_spanner_databases_read
      permission select = granted->spanner_databases_select + instance->granted_spanner_databases_select
      permission setiampolicy = granted->spanner_databases_setiampolicy + instance->granted_spanner_databases_setiampolicy
      permission update = granted->spanner_databases_update + instance->granted_spanner_databases_update
      permission updateddl = granted->spanner_databases_updateddl + instance->granted_spanner_databases_updateddl
      permission userolebasedaccess = granted->spanner_databases_userolebasedaccess + instance->granted_spanner_databases_userolebasedaccess
      permission write = granted->spanner_databases_write + instance->granted_spanner_databases_write

      // Sessions
      permission create_session = granted->spanner_sessions_create + instance->granted_spanner_sessions_create
      permission delete_session = granted->spanner_sessions_delete + instance->granted_spanner_sessions_delete
      permission get_session = granted->spanner_sessions_get + instance->granted_spanner_sessions_get
      permission list_sessions = granted->spanner_sessions_list + instance->granted_spanner_sessions_list

      // Database Operations
      permission cancel_operation = granted->spanner_databaseoperations_cancel + instance->granted_spanner_databaseoperations_cancel
      permission delete_operation = granted->spanner_databaseoperations_delete + instance->granted_spanner_databaseoperations_delete
      permission get_operation = granted->spanner_databaseoperations_get + instance->granted_spanner_databaseoperations_get
      permission list_operations = granted->spanner_databaseoperations_list + instance->granted_spanner_databaseoperations_list

      // Database Roles
      permission list_roles = granted->spanner_databaseroles_list + instance->granted_spanner_databaseroles_list
      permission use_role = granted->spanner_databaseroles_use + instance->granted_spanner_databaseroles_use
  }
""",
    """
  definition platform {
  	relation administrator: user
  	permission super_admin = administrator
  }

  definition organization {
    // The platform is generally a singleton pointing to the same
    // platform object, on which the superuser is in turn granted
    // access.
  	relation platform: platform
  	permission admin = platform->super_admin
  }

  definition resource {
  	relation owner: user | organization
  	permission admin = owner + owner->admin
  }

  definition user {}
""",
    """
  definition user {}

  definition project {
  	relation issue_creator: role#member
  	relation issue_assigner: role#member
  	relation any_issue_resolver: role#member
  	relation assigned_issue_resolver: role#member
  	relation comment_creator: role#member
  	relation comment_deleter: role#member
  	relation role_manager: role#member

  	permission create_issue = issue_creator
  	permission create_role = role_manager
  }

  definition role {
  	relation project: project
  	relation member: user
  	relation built_in_role: project

  	permission delete = project->role_manager - built_in_role->role_manager
  	permission add_user = project->role_manager
  	permission add_permission = project->role_manager - built_in_role->role_manager
  	permission remove_permission = project->role_manager - built_in_role->role_manager
  }

  definition issue {
  	relation project: project
  	relation assigned: user

  	permission assign = project->issue_assigner
  	permission resolve = (project->assigned_issue_resolver & assigned) + project->any_issue_resolver
  	permission create_comment = project->comment_creator

  	// synthetic relation
  	permission project_comment_deleter = project->comment_deleter
  }

  definition comment {
  	relation issue: issue
  	permission delete = issue->project_comment_deleter
  }
""",
    """
definition witharrow {
    permission arrowed = foo + bar->baz
}definition another {}

/*
caveat somecaveat(somecondition uint, somebool bool, somestring string) {
  somecondition == 42 && somebool && somestring == 'hello'
}*/

definition user {}definition mydefinition {
    /**
     * some doc comment
     */
    // relation foo: sometype#... | anothertype#somerel

    // My cool permission
    permission bar = foo + baz - meh
    permission another = (foo - meh) + bar
}definition user {}

definition resource {
    relation viewer: user | /*user: |*/ anothertype
    permission view = viewer
}//definition foo {

definition document {
  relation writer: user | user /*with somecaveat*/ | team#member //with anothercaveat
  relation viewer: user | user:* /*with wildcardcaveat */| user // with someprefix/somecaveat
}definition another {}
/*
caveat somecaveat(somecondition uint, somebool bool) {
  somecondition == 42 && somebool || something == "hi there" &&
  ({
    "themap": 42
  }).contains("whatever")
}

caveat anothercaveat(somemap map<string>, somelist list<int>) {
  somelist.contains("hiya") && somemap?.foo
}*/

definition user {}definition sometenant/somedef {
    relation somerel: anothertenant/someobject
}/**
 * user represents a user that can be granted role(s)
 */
definition user {}

/**
 * document represents a document protected by Authzed.
 */
definition document {
    /**
     * writer indicates that the user is a writer on the document.
     */
    relation writer: user

    /**
     * reader indicates that the user is a reader on the document.
     */
    relation reader: user

    /**
     * edit indicates that the user has permission to edit the document.
     */
    permission edit = writer

    /**
     * view indicates that the user has permission to view the document, if they
     * are a `reader` *or* have `edit` permission.
     */
    permission view = reader + edit
}definition another {}
/*
caveat somecaveat(somecondition uint, somebool bool) {
}*/

definition user {}/**
			 * user is a user
			 */
			definition user {}

			/**
			 * single is a thing
			 */
			definition single {
				/**
				 * some permission
				 */
				permission first = bar + baz
			}
definition user {}
/*
caveat somecaveat(somecondition int) {
  somecondition == 42 `
}*/definition user {}

definition namespace {
    relation adminer: user
    permission admin = adminer
}

definition repository {
    relation namespace: namespace
    relation reader: user
    relation writer: user#anotherrel

    permission read = reader + writer + namespace->admin
    permission write = writer
}definition foo {
    permission bar = baz + meh + (aaa->bbb - ccc->ddd) + (maz & beh)
}definition resource {    
    permission empty = nil
    permission another = foo + nil + bar
    permission third = (aaa + bbb + nil) - ccc - nil
}/*definition foo {
    permission bar = (maz & beh)
}definition foo {
    permission bar = ---
}definition foo {
    permission bar = 
}definition foo {
    relation bar: ---
}definition foo {
    relation bar:
}definition another {}*/

/*
caveat somecaveat(somecondition uint, somebool bool) {
  somemap{

  
}
*/

definition user {}definition user {}

definition resource {
    relation viewer: user | user:* | anothertype
    relation another: user | user:*
    permission view = viewer
}
"""
  )
}
