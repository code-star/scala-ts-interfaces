package nl.codestar.scalatsi

import nl.codestar.scalatsi.TypescriptType._
import nl.codestar.scalatsi.output.StyleOptions

object TypescriptTypeSerializer {

  def serialize(tp: TypescriptType)(implicit styleOptions: StyleOptions = StyleOptions()): String = {
    import styleOptions._
    tp match {
      case t: TypescriptNamedType => t.name
      case TSAny                  => "any"
      case TSArray(elements)      => serialize(elements) + "[]"
      case TSBoolean              => "boolean"
      case TSIndexedInterface(indexName, indexType, valueType) =>
        s"{ [ $indexName: ${serialize(indexType)} ]: ${serialize(valueType)}$sc }"
      case TSIntersection(Seq())  => serialize(TSNever)
      case TSIntersection(Seq(e)) => serialize(e)
      case TSIntersection(of)     => s"${of.map(serialize) mkString " | "}"
      case TSNever                => "never"
      case TSNull                 => "null"
      case TSNumber               => "number"
      case TSObject               => "object"
      case TSTuple(members)       => s"[${members.map(serialize) mkString ", "}]"
      case TSString               => "string"
      case TSUndefined            => "undefined"
      case TSUnion(Seq())         => serialize(TSNever)
      case TSUnion(Seq(e))        => serialize(e)
      case TSUnion(of)            => s"(${of.map(serialize) mkString " | "})"
      case TSVoid                 => "void"
      case TSLiteralBoolean(v)    => v.toString()
      case TSLiteralNumber(v)     => v.toString()
      case TSLiteralString(v)     => s""""${v.replace("\"", "\"\"")}""""
    }
  }

  @deprecated("0.2.0", "Use emit[T]()")
  def emit[T](implicit tsType: TSNamedType[T]): String = emit[T]()

  def emit[T](styleOptions: StyleOptions = StyleOptions())(implicit tsType: TSNamedType[T]): String =
    emits(styleOptions, types = Set(tsType.get))

  def emits(types: TypescriptNamedType*): String =
    emits(styleOptions = StyleOptions(), types = types.toSet)

  def emits(styleOptions: StyleOptions = StyleOptions(), types: Set[TypescriptNamedType]): String =
    types
      .flatMap(discoverNestedNames)
      .toSeq
      .sorted
      .map(namedType => emitNamed(namedType)(styleOptions))
      .mkString("\n")

  private object TSInterfaceEntry {
    def unapply(typescriptType: TypescriptType): Option[(TypescriptType, Boolean)] =
      typescriptType match {
        case TSUnion(members) if members.contains(TSUndefined) =>
          Some((TSUnion(members.filter(_ != TSUndefined)), false))
        case other => Some((other, true))
      }
  }

  private def emitNamed(named: TypescriptNamedType)(implicit styleOptions: StyleOptions): String = {
    import styleOptions._
    named match {
      case TSAlias(name, underlying) =>
        s"export type $name = ${serialize(underlying)}"

      case TSEnum(name, const, entries) =>
        val mbs = entries.map({
          case (entryName, Some(i)) => s"  $entryName = $i"
          case (entryName, None)    => s"  $entryName"
        })
        s"""export ${if (const) "const " else ""}enum $name {
           |${mbs.mkString(",\n")}
           |}$sc
       """.stripMargin

      case _: TSTypeReference => ""

      case TSInterfaceIndexed(name, indexName, indexType, valueType) =>
        s"""export interface $name {
           |  [ $indexName: ${serialize(indexType)} ]: ${serialize(valueType)}$sc
           |}
       """.stripMargin

      case TSInterface(name, members) =>
        def symbol(required: Boolean) = if (required) ":" else "?:"

        val mbs = members.map({
          case (memberName, TSInterfaceEntry(tp, required)) =>
            s"  $memberName${symbol(required)} ${serialize(tp)}"
        })

        s"""
           |export interface $name {
           |${mbs.mkString("", s"$sc\n", sc)}
           |}
       """.stripMargin
    }
  }

  private def discoverNestedNames(tp: TypescriptType): Set[TypescriptNamedType] = {
    val me: Set[TypescriptNamedType] = tp match {
      case named: TypescriptNamedType => Set(named)
      case _                          => Set()
    }
    tp match {
      case TypescriptAggregateType(nested) =>
        nested.flatMap(discoverNestedNames) ++ me
      case _ => me
    }
  }
}
