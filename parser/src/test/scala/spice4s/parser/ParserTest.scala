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

package spice4s.parser

import munit.FunSuite

class ParserTest extends FunSuite {
  schemas.zipWithIndex.foreach { case (schema, i) =>
    test(s"should be able to parse schema $i") {
      Parse.parseWith(SchemaParser.schema)(schema) match {
        case Left(err) => fail(err): Unit
        case Right(_)  => ()
      }
    }
  }

  lazy val schemas = List(
    """
/** user represents a user */
definition user {}

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
    permission arrowed = foo + bar->baz->meh
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
