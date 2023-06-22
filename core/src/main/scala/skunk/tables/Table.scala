package skunk.tables

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.quoted.*

import skunk.{Codec, Decoder, Fragment, Void}
import skunk.implicits.*

import skunk.tables.internal.{TableBuilder, TwiddleTCN}

/**
  * `Table` is the core entity for skunk-tables API.
  * It links a product type `T` to a Postgres table with specific name and constraints
  * and provides several utility methods to query that table in a type-safe manner
  *
  * Apart from building ready-to-be-executed tables, it also exposes
  * even lower-level API to e.g. check/get all column names at compile-time
  */
trait Table[T <: Product]:
  self =>

  /** Homogenous tuple of `TypedColumn`; Copy of Select#TypedColumns */
  type TypedColumns <: NonEmptyTuple

  def typedColumns: TypedColumns

  /** Table name */
  def name: Table.Name

  /** Union type of all coumn names */
  type ColumnName

  /** Flat tuple of Scala types used in columns */
  type Columns

  /** `Columns` selectable trait, allowing to statically access typed columns */
  type Select

  def select: Select

  given Conversion[ColumnName, String] =
    name => name.asInstanceOf[String]
  given Conversion[List[ColumnName], List[String]] =
    names => names.map(_.asInstanceOf[String])

  /** Skunk twiddler to transform twiddled tuple, received from `select` into concrete `T` */
  def dissect: Dissect.AuxT[T, Columns, TwiddleTCN[TypedColumns]]

  def count[F[_]]: Query[F, "single", Long] =
    Query.count[F](name)

  def all[F[_]]: Query[F, "many", T] { type Input = Void } =
    Query.all[F, T](name, getColumnNames, decoder)

  def query[F[_], A](q: Select => TypedColumn.Op[A]): Query[F, "many", T] =
    val ops = q.apply(select)
    Query.select[F, A, T](name, getColumnNames, ops, decoder)

  // TODO: make sure we're querying at least one primary or unique columns
  def get[F[_], A](q: Select => TypedColumn.Op[A]): Query[F, "optional", T] =
    val ops = q.apply(select)
    Query.get[F, A, T](name, getColumnNames, ops, decoder)

  def insert[F[_], A](using CanInsert[A, T])(a: A) =
    Insert.insert[F, T, A, TypedColumns](name, a, summon[CanInsert[A, T]])

  def decoder: Decoder[T] =
    Table
      .getCodecs(typedColumns)
      .imap(twiddled => dissect.untwiddle(twiddled))(columns => dissect.twiddle(columns))
      .imap(columns => dissect.from(columns))(t => dissect.to(t))

  /** All column names, in their order */
  def getColumnNames: List[ColumnName] =
    getColumns.map(_.n.toString).asInstanceOf[List[ColumnName]]

  override def toString: String =
    s"Table($name, $select)"

  private def getColumns: List[TypedColumn[?, ?, ?, ?]] =
    typedColumns.toList.asInstanceOf[List[TypedColumn[?, ?, ?, ?]]]

  /** Lower-level API to work with `Fragment` */
  object low:

    /** Table name as a `Fragment` */
    def name: Fragment[Void] =
      self.name.toFragment

    /** Build `Fragment` of `table_name as s` */
    def nameAs(short: String): Fragment[Void] =
      sql"${name} AS #${short}"

    /** All comma-separated columns */
    def columns: Fragment[Void] =
      sql"#${self.getColumnNames.mkString(", ")}"

    /** All comma-separated columns with a prefix (like "p.age, p.name, p.email") */
    def columnsOf(prefix: String): Fragment[Void] =
      sql"#${self.getColumnNames.map(n => s"$prefix.$n").mkString(", ")}"

    /** Pick a (type-checked) set of columns as a comma-separated `Fragment` */
    def pick(toInclude: ColumnName*): Fragment[Void] =
      sql"#${toInclude.mkString(", ")}"

    /** Pick columns as a comma-separated `Fragment`, except some (type-checked) set */
    def except(toExclude: ColumnName*): Fragment[Void] =
      sql"#${getColumnNames.filterNot(name => toExclude.contains(name)).mkString(", ")}"



object Table:

  def apply[T <: Product](using t: Table[T]): t.type = t

  /** The constructor of `Table` */
  inline transparent def of[T <: Product] =
    ${ TableBuilder.init[T] }


  opaque type Name = String

  object Name:
    def apply(str: String): Name = str

    extension (name: Name)
      def unbox: String = name
      def toFragment: Fragment[Void] = sql"#${unbox}"


  private def getCodecs[T <: NonEmptyTuple](t: T): Codec[TwiddleTCN[T]] =
    @tailrec
    def go[TT <: Tuple](tt: TT, codecs: Codec[Any]): Codec[Any] =
      tt match
        case EmptyTuple => codecs.asInstanceOf[Codec[Any]]
        case h *: tail => 
          val prod = codecs
            .product(h.asInstanceOf[TypedColumn[?, ?, ?, ?]].primitive.codec.asInstanceOf[Codec[Any]])
            .imap[Any *: Any *: EmptyTuple] { case (a, b) => a *: b *: EmptyTuple } { case a *: b *: EmptyTuple => (a, b) }
            .asInstanceOf[Codec[Any]]
          go(tail, prod)

    t match
      case h *: tail =>
        go(tail, h.asInstanceOf[TypedColumn[?, ?, ?, ?]].primitive.codec.asInstanceOf[Codec[Any]]).asInstanceOf[Codec[TwiddleTCN[T]]]

