package nl.codestar.scalatsi

import scala.language.higherKinds
import scala.reflect.macros.blackbox

private[scalatsi] class Macros(val c: blackbox.Context) {
  import c.universe._

  /** Tree to use to get a TSType[T] */
  private def getTSType(T: Type): Tree =
    q"""_root_.nl.codestar.scalatsi.TSType.getOrGenerate[$T]"""

  private def mapToNever[T: c.WeakTypeTag]: Tree =
    q"""{
       import _root_.nl.codestar.scalatsi.TSNamedType
       import _root_.nl.codestar.scalatsi.TypescriptType.{TSAlias, TSNever}
       TSNamedType(TSAlias(${ref[T]}, TSNever))
     }"""

  private def ref[T](implicit tt: c.WeakTypeTag[T]): Tree =
    q""" _root_.nl.codestar.scalatsi.TSRef(${tt.tpe.typeSymbol.fullName})"""

  private def macroUtil = new MacroUtil[c.type](c)

  def getImplicitMappingOrGenerateDefault[T: c.WeakTypeTag, TSType[_]](implicit tsTypeTag: c.WeakTypeTag[TSType[_]]): Tree =
    macroUtil.lookupOptionalGenericImplicit[T, TSType] match {
      case Some(value) => value
      case None        => generateDefaultMapping[T]
    }

  def getImplicitInterfaceMappingOrGenerateDefault[T: c.WeakTypeTag, TSType[_]](implicit tsTypeTag: c.WeakTypeTag[TSType[_]]): Tree =
    macroUtil.lookupOptionalGenericImplicit[T, TSType] match {
      case Some(value) => value
      case None        => generateInterfaceFromCaseClass[T]
    }

  private def generateDefaultMapping[T: c.WeakTypeTag]: Tree = {
    val T      = c.weakTypeOf[T]
    val symbol = T.typeSymbol

    def err() = {
      c.abort(
        c.enclosingPosition,
        s"Could not find an implicit TSType[$T] in scope and could not generate one. Did you create and import it?"
      )
    }

    if (!symbol.isClass) {
      err()
    } else {
      val classSymbol = symbol.asClass
      if (classSymbol.isCaseClass)
        generateInterfaceFromCaseClass[T]
      else if (classSymbol.isSealed)
        generateUnionFromSealedTrait[T]
      else err()
    }
  }

  private def primaryConstructor(T: Type): MethodSymbol =
    T.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor =>
        if (!m.isPublic)
          c.abort(c.enclosingPosition, s"Only classes with public primary constructor are supported. Found: $T")
        m
    }.get

  private def caseClassFieldsTypes(T: Type): Seq[(String, Type)] = {
    val paramLists = primaryConstructor(T).paramLists
    val params     = paramLists.head

    if (paramLists.size > 1)
      c.abort(c.enclosingPosition, s"Only one parameter list classes are supported. Found: $T")

    params.foreach { p =>
      if (!p.isPublic)
        c.abort(
          c.enclosingPosition,
          s"Only classes with all public constructor arguments are supported. Found: $T"
        )
    }

    params.map { field =>
      (field.name.toTermName.decodedName.toString, field.infoIn(T))
    }
  }

  def generateInterfaceFromCaseClass[T: c.WeakTypeTag]: Tree = {
    val T      = c.weakTypeOf[T]
    val symbol = getClassOrTraitSymbol(T)

    if (!symbol.isCaseClass)
      c.abort(c.enclosingPosition, s"Expected case class, but found: $T")

    val members = caseClassFieldsTypes(T) map {
      case (name, optional) if optional <:< typeOf[Option[_]] =>
        val typeArg = optional.typeArgs.head
        q"($name, ${getTSType(typeArg)} | TSUndefined)"
      case (name, tpe) =>
        q"($name, ${getTSType(tpe)}.get)"
    }

    q"""{
     import _root_.nl.codestar.scalatsi.TypescriptType.TSUndefined
     import _root_.nl.codestar.scalatsi.{TSNamedType, TSIType}
     import _root_.scala.collection.immutable.ListMap
     TSIType(TSInterface(${ref[T]}, ListMap(
       ..$members
     )))
    }"""
  }

  private def getClassOrTraitSymbol(T: Type): ClassSymbol = {
    val symbol = T.typeSymbol
    if (!symbol.isClass)
      c.abort(c.enclosingPosition, s"Expected class or trait, but found $T")
    symbol.asClass
  }

  def generateUnionFromSealedTrait[T: c.WeakTypeTag]: Tree = {
    val T      = c.weakTypeOf[T]
    val symbol = getClassOrTraitSymbol(T)

    if (!symbol.isSealed)
      c.abort(c.enclosingPosition, s"Expected sealed trait or sealed class, but found: $T")

    symbol.knownDirectSubclasses.toSeq match {
      case Seq() =>
        val symbolType =
          if (symbol.isTrait) "trait"
          else if (symbol.isClass && symbol.isAbstract) "abstract class"
          else "type"
        c.warning(c.enclosingPosition, s"Sealed $symbolType $T has no known subclasses, could not generate union")
        mapToNever[T]
      case Seq(singleChild) =>
        q"""{
         import _root_.nl.codestar.scalatsi.TypescriptType
         import _root_.nl.codestar.scalatsi.TSNamedType
         TSNamedType(TSAlias(${ref[T]}, TypescriptType.nameOrType(${getTSType(singleChild.asType.toType)}.get)))
        }"""
      case children =>
        val operands = children map { symbol =>
          q"TypescriptType.nameOrType(${getTSType(symbol.asType.toType)}.get)"
        }

        q"""{
        import _root_.nl.codestar.scalatsi.TypescriptType
        import TypescriptType.{TSAlias, TSUnion}
        import _root_.nl.codestar.scalatsi.TSNamedType
        import _root_.scala.collection.immutable.Vector
        TSNamedType(TSAlias(${ref[T]}, TSUnion(Vector(..$operands))))
      }"""
    }
  }
}
