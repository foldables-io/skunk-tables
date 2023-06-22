package skunk.tables.internal

import scala.quoted.*

object Constructor:
  def apply(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.Term = {
    import quotes.reflect.*

    val (repr, constructor, tpeArgs) =
      tpe match {
        case AppliedType(repr, reprArguments) => (repr, repr.typeSymbol.primaryConstructor, reprArguments)
        case notApplied                       => (tpe, tpe.typeSymbol.primaryConstructor, Nil)
      }

    New(Inferred(repr))
      .select(constructor)
      .appliedToTypes(tpeArgs)
  }
