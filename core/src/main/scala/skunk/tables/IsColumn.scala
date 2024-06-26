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

import java.time.*
import java.util.UUID

import scala.compiletime.{summonInline, constValue}

import io.circe.Json

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.collection.MaxLength
import io.github.iltotore.iron.refineEither

import skunk.Codec
import skunk.codec.all.*
import skunk.circe.codec.json.json

/** Type class declaring that a type must be represented as a single column in `Table` */
trait IsColumn[A]:
  def codec: Codec[A]

  override def toString: String = codec.toString

object IsColumn:
  inline def ofCodec[A](c: Codec[A]) = new IsColumn[A]:
    def codec: Codec[A] = c

  def apply[A](using ev: IsColumn[A]): IsColumn[A] = ev

  inline given [A: IsColumn]: IsColumn[Option[A]] = ofCodec(summonInline[IsColumn[A]].codec.opt)

  inline given [A, B](using inline members: IsColumn[A], inline constraint: Constraint[A, B]): IsColumn[A :| B] =
    val codec: Codec[A :| B] =
      members.codec.eimap(a => a.refineEither[B])((s: A :| B) => s.asInstanceOf[A])
    ofCodec[A :| B](codec)

  inline given [B <: Int](using inline constraint: Constraint[String, MaxLength[B]]): IsColumn[String :| MaxLength[B]] =
    ofCodec(varchar(constValue[B]))
      .asInstanceOf[IsColumn[String :| MaxLength[B]]]

  inline given IsColumn[String]         = ofCodec(varchar)
  inline given IsColumn[Short]          = ofCodec(int2)
  inline given IsColumn[Int]            = ofCodec(int4)
  inline given IsColumn[Long]           = ofCodec(int8)
  inline given IsColumn[Boolean]        = ofCodec(bool)
  inline given IsColumn[BigDecimal]     = ofCodec(numeric)
  inline given IsColumn[LocalDate]      = ofCodec(date)
  inline given IsColumn[LocalDateTime]  = ofCodec(timestamp)
  inline given IsColumn[OffsetDateTime] = ofCodec(timestamptz)
  inline given IsColumn[UUID]           = ofCodec(uuid)
  inline given IsColumn[Array[Byte]]    = ofCodec(bytea)

  inline given IsColumn[Json] = ofCodec(json)
