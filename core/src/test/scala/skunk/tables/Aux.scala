/*
 * Copyright 2024 Foldables
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

package skunk.tables

// @see https://github.com/foldables-io/skunk-tables/issues/7
object Aux:
  opaque type TaskId = Long
  object TaskId:
    def apply(id: Long): TaskId   = id
    val example: TaskId           = TaskId(1L)
    inline given IsColumn[TaskId] = ???

  opaque type TaskTitle = String
  object TaskTitle:
    def apply(t: String): TaskTitle  = t
    val example: TaskTitle           = TaskTitle("fix macro")
    inline given IsColumn[TaskTitle] = ???
