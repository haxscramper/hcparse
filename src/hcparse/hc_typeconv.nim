## Conversion of C types to nim

import cxtypes, hc_types
import hnimast

import hmisc/other/[colorlogger]
import hmisc/types/colorstring
import hmisc/algo/[hstring_algo, hseq_mapping]
import hmisc/helpers
import hmisc/macros/iflet

import std/[algorithm, strformat, sequtils, strutils]

import cxcommon

proc getTypeName*(cxtype: CXType, conf: WrapConfig): string

proc toNType*(
  cxtype: CXType,
  conf: WrapConfig): tuple[ntype: NType[PNode], mutable: bool]

proc fromElaboratedPType*(cxtype: CXType, conf: WrapConfig): NType[PNode] =
  # debug cxtype
  let genParams = cxtype.getNumTemplateArguments()
  let decl = cxtype.getTypeDeclaration()
  if genParams > 0:
    case decl.cxKind:
      of ckTypedefDecl:
        # WARNING `template <J, Q> using` is not handled
        result = newPType(cxtype.getTypeName(conf))

      of ckClassDecl, ckStructDecl:
        # debug "Class decl"
        let params = cxtype.genParams()
        result = newPType(cxtype.getTypeName(conf))
        for idx, parm in params:
          if parm.cxKind != tkInvalid:
            result.add parm.toNType(conf).ntype

      else:
        warn "Conversion from elaborated type: ", decl
        debug "  ", decl.cxKind(), " in ", decl.getSpellingLocation()

    conf.fixTypeName(result, conf, 0)

  else:
    result = newPType(getTypeName(cxtype, conf))

proc dropPOD*(cxtype: CXType, conf: WrapConfig): string =
  case cxtype.cxKind:
    of tkElaborated:
      cxtype.fromElaboratedPType(conf).head

    of tkPointer:
      cxtype[].dropPOD(conf)

    of tkTypedef:
      ($cxtype).dropPrefix("const ")

    else:
      ""

proc toPIdentDefs*(cursor: CXCursor, conf: WrapConfig): PIdentDefs =
  var varname = $cursor
  if varname.len == 0:
    varname = "arg" & $cursor.cxType().dropPOD(conf)

  result.idents.add newPIdent(varname.fixIdentName())

  let (ctype, mutable) = cursor.cxType().toNType(conf)
  result.vtype = ctype
  if mutable:
    result.kind = nvdVar


proc getSemanticNamespaces*(
    parent: CXCursor, filterInline: bool = true, withType: bool = true
  ): seq[CXCursor] =

  # info "Semantic namespaces for", parent

  var parent = parent

  if withType:
    result.add parent

  parent = parent.getCursorSemanticParent()

  # info parent

  while parent.cxKind() in {
    # TEST might be necessary to add templated namespacess (fuck, why C++
    # is just so god-awful vomit-inducing garbage?)
    ckNamespace, ckStructDecl, ckClassDecl
  }:
    if filterInline and (parent.isInlineNamespace() == 1):
      discard
    else:
      result.add parent

    parent = parent.getCursorSemanticParent()
    # info parent.cxKind()

  reverse(result)




proc getTypeNamespaces*(
    cxtype: CXType, filterInline: bool = true, withType: bool = true
  ): seq[CXCursor] =
  ## Return list of parent namespaces for given type `cxtype`.
  ## `filterInline` - remove namespaces that are marked as `inline`.
  ## `withType` - return type name too, or only namespaces.

  var parent = cxtype.getTypeDeclaration()

  result = getSemanticNamespaces(
    parent, filterInline = filterInline, withType = withType)

proc requiredGenericParams*(cursor: CXCursor): seq[CXCursor] =
  ## Get list of required generic parameters from cursor pointing to
  ## class or struct declaration
  for subn in cursor:
    if subn.cxKind in {
      ckTemplateTemplateParameter,
      ckTemplateTypeParameter
    }:
      if subn.len > 0:
        # WARNING Just drop all template parameters that are not
        # simply `T`.
        discard

      else:
        result.add subn # WARNING blow up on `a<b>`

proc toCName*(cursor: CXCursor): CName
proc toScopedIdent*(cursor: CXCursor): CScopedIdent =
  for elem in cursor:
    result.add toCName(elem)

proc toCName*(cursor: CXCursor): CName =
  result = CName(cursor: cursor, isGenerated: false)
  for genParam in requiredGenericParams(cursor):
    result.genParams.add toScopedIdent(genParam)

proc toCName*(str: string, genp: seq[CScopedIdent] = @[]): CName =
  result = CName(name: str, isGenerated: true)
  result.genParams = genp

proc toScopedIdent*(name: seq[string]): CScopedIdent =
  for elem in name:
    result.add toCName(elem)


proc toScopedIdent*(name: string): CScopedIdent =
  result.add toCName(name)

proc sameNoGeneric*(ident1, ident2: CScopedIdent): bool =
  result = ident1.len == ident2.len
  if result:
    for (a, b) in zip(ident1, ident2):
      if a.identName() != b.identName():
        return false

proc toFullScopedIdent*(cxtype: CXType): CScopedIdent =
  for ns in getTypeNamespaces(cxtype):
    result.add CName(
      isGenerated: false,
      cursor: ns,
      genParams: requiredGenericParams(
        ns.cxType().getTypeDeclaration()).mapIt(toScopedIdent(it))
    )

# proc isDirectTypeDecl*(cursor: CXCursor): bool =
#   case cursor.cxKind():
#     of 
#   debug cursor.treeRepr()
#   raiseImplementError("") 

proc getTypeName*(cxtype: CXType, conf: WrapConfig): string =
  let curs = cxtype.getTypeDeclaration()
  case curs.cxKind:
    of ckTypedefDecl:
      return $curs.cxType()
    of ckClassDecl, ckStructDecl, ckEnumDecl, ckUnionDecl:
      result = $curs
    else:
      err $curs
      err "Type name for ", curs.treeRepr(conf.unit)
      raiseAssert(
        &"Cannot convert cursor of kind {curs.cxKind} to type")

  result = cxtype.getTypeNamespaces().mapIt(
    dropPrefix($it, toStrPart(["const ", "enum ", "struct ", "union "]))
  ).join("::")

  # debug cxtype.getTypeNamespaces()
  # debug result

proc isMutableRef*(cxtype: CXType): bool =
  case cxType.cxKind:
    of tkLValueReference, tkRValueReference:
      return not (cxType.isConstQualifiedType() == 0)
    of tkTypeDef:
      # TODO implement mutability checking
      let decl = cxtype.getTypeDeclaration()
      if decl.len == 1 and decl[0].cxKind == ckTypeRef:
        discard
    else:
      raiseAssert(&"#[ IMPLEMENT Is {cxtype.cxKind} a mutable ref? ]#")

proc fromCxxTypeName*(name: string): string =
  case name:
    of "long": "clong"
    of "int": "cint"
    of "unsigned long": "culong"
    else: ""

proc toNType*(
  cxtype: CXType,
  conf: WrapConfig): tuple[ntype: NType[PNode], mutable: bool] =
  ## Convert CXType to nim type. Due to differences in how mutability
  ## handled in nim and C it is not entirely possible to map `CXType`
  ## to `NType` without losing this information. Instead `mutable` is
  ## returned, indicating whether or not the type was mutable.
  ## Conversion is performed as follows
  ##
  ## - `T&` is considered mutable and mapped to `var T`
  ## - Any kind of pointer is mapped to immutable since it is not possible
  ##   infer this information from C type anyway.
  ## - Function prototype is mapped to `{.cdecl.}` proc type
  ## - 'special' types are mapped
  ##   - `char*` -> `cstring`
  ##   - `char**` -> `cstringArray`
  ##   - `void*` -> `pointer`
  ## - For C types with elaborated specifier (e.g. `enum E` instead of
  ##   simply `E`) specifiers are simply dropped.
  ##
  ## - TODO :: `const&&` parameters /could/ be mapped to `sink` annotations
  var mutable: bool = false
  let restype = case cxtype.cxKind:
    of tkBool:       newPType("bool")
    of tkInt:        newPType("cint")
    of tkVoid:       newPType("void")
    of tkUInt:       newPType("cuint")
    of tkLongLong:   newPType("clonglong")
    of tkULongLong:  newPType("culonglong")
    of tkDouble:     newPType("cdouble")
    of tkULong:      newPType("culong")
    of tkUChar:      newPType("cuchar")
    of tkChar16:     newPType("uint16") # WARNING C++ type is `char16_t`
    of tkChar32:     newPType("uint32") # WARNING C++ type is `char32_t`
    of tkWChar:      newPType("uint32") # WARNING C++ type is `wchar_t`
    of tkChar_S:     newPType("cchar")
    of tkLong:       newPType("clong")
    of tkUShort:     newPType("cushort")
    of tkNullPtr:    newPType("pointer") # WARNING C++ type is `nullptr_t`
    of tkFloat:      newPType("cfloat")
    of tkLongDouble: newPType("clongdouble")
    of tkShort:      newPType("cshort")
    of tkSChar:      newPType("cschar")
    of tkTypedef:
      result.mutable = cxType.isMutableRef()
      newPType(($cxtype).dropPrefix("const ")) # XXXX typedef processing -

    of tkElaborated, tkRecord, tkEnum:
      # debug "From elaborated type"
      fromElaboratedPType(cxtype, conf)

    of tkPointer:
      case cxtype[].cxkind:
        of tkChar_S:
          newPType("cstring")

        of tkPointer:
          if cxtype[][].cxKind() == tkChar_S:
            newPType("cstringArray")

          else:
            newNType("ptr", [toNType(cxtype[], conf).ntype])

        of tkVoid:
          newPType("pointer")

        of tkFunctionProto:
          toNType(cxtype[], conf).ntype

        else:
          newNType("ptr", [toNType(cxtype[], conf).ntype])

    of tkConstantArray:
      newNType(
        "ptr", [newNType(
          "array", @[
            newPType($cxtype.getNumElements()),
            toNType(cxtype.getElementType(), conf).ntype])])

    of tkIncompleteArray:
      newNType("ptr", [toNType(cxtype.getElementType(), conf).ntype])

    of tkFunctionProto:
      newProcNType[PNode](
        rtype = cxtype.getResultType().toNType(conf).ntype,
        args = cxtype.argTypes.mapIt(toNType(it, conf).ntype),
        pragma = newPPragma("cdecl")
      )

    of tkLValueReference:
      result.mutable = cxType.isMutableRef()
      toNType(cxType[], conf).ntype

    of tkRValueReference: # WARNING I'm not 100% sure this is correct
                          # way to map rvalue references to nim type
                          # system.
      result.mutable = cxType.isMutableRef()
      toNType(cxType[], conf).ntype

    of tkUnexposed:
      let strval = ($cxType).dropPrefix("const ") # WARNING
      if strval.validCxxIdentifier():
        newPtype(strval)

      else:
        # pprintStackTrace()
        let decl = cxtype.getTypeDeclaration()
        var res = newPType($decl)
        if decl.cxKind in {
          # HACK list of necessary kinds is determined by trial and error,
          # I'm still not really sure what `tkUnexposed` actually
          # represents.
          ckClassTemplate, ckClassDecl
        }:
          for elem in decl:
            if elem.cxKind() in {ckTemplateTypeParameter}:
              let (sub, _) = elem.cxType().toNType(conf)
              res.add sub

          # info decl
          # debug res.toNNode()
        else:
          # debug decl.cxKind()
          res = newPType("UNEXPOSED")
          # warn strval, "is not a valid identifier for type name, use UNEXPOSED"

          if decl.cxKind() notin {ckNoDeclFound}:
            warn "No decl found for type"
            logIndented:
              info cxtype.lispRepr()
              debug decl.getSpellingLocation()
              debug decl.cxKind()
              debug decl.treeRepr(conf.unit)


        res

    of tkDependent: newPType("DEPENDENT")

    of tkMemberPointer:
      # WARNING Member pointer
      newPType("!!!")

    else:
      err "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen(), " ",
        cxtype[]

      newPType("!!!")

  result.ntype = restype
  conf.fixTypeName(result.ntype, conf, 0)
  result.mutable = mutable

func hasUnexposed*(nt: NType[PNode]): bool =
  case nt.kind:
    of ntkIdent, ntkGenericSpec:
      nt.head in [ "UNEXPOSED", "DEPENDENT" ] or
      nt.genParams.anyOfIt(it.hasUnexposed())
    of ntkProc:
      nt.arguments.anyOfIt(it.vtype.hasUnexposed())
    else:
      false

func toCppNamespace*(ns: CScopedIdent, withGenerics: bool = true): string =
  ## Generate `importcpp` pattern for scoped identifier
  var buf: seq[string]
  var genIdx: int = 0
  for part in ns:
    if withGenerics and part.genParams.len > 0:
      var genTypes: seq[string]
      for param in part.genParams:
        genTypes.add "'" & $genIdx
        inc genIdx

      buf.add $part.cursor & "<" & genTypes.join(", ") & ">"
    else:
      buf.add $part.cursor

  result = buf.join("::")

func pubFields*(cd: CDecl): seq[CDecl] =
  assert cd.kind in {cdkClass, cdkStruct}
  for member in cd.members:
    if (member.kind == cdkField) and (member.accs == asPublic):
      result.add member

proc isEnum*(cxtype: CXType): bool =
  case cxtype.cxKind():
    of tkEnum:
      return true

    of tkElaborated:
      if cxtype.getTypeDeclaration().cxKind() in {ckEnumDecl}:
        return true

      else:
        return false

    else:
      return false
