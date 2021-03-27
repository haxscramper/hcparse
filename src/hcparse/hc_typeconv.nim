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
      of ckTypedefDecl, ckTypeAliasDecl, ckTypeAliasTemplateDecl:
        # WARNING `template <J, Q> using` is not handled
        result = newPType(cxtype.getTypeName(conf))

      of ckClassDecl, ckStructDecl, ckClassTemplate:
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

  # if cursor.cxKind() == ckClassTemplate:
  #   logDefer info, "Required generic for", cursor
  #   debug cursor.getSpellingLocation()
  #   debug result.len
  #   debug result
  #   debug cursor.cxKind()
    # debug cursor.treeRepr()


proc toCName*(cursor: CXCursor): CName
proc toScopedIdent*(cursor: CXCursor): CScopedIdent =
  for elem in cursor:
    result.add toCName(elem)

proc toCName*(cursor: CXCursor): CName =
  result = CName(cursor: cursor, isGenerated: false)
  for genParam in requiredGenericParams(cursor):
    # HACK add `genParam` explicitly. Was added because template type
    # parameters were ignored otherwise. Not sure if this is a general
    # enough solution, but it worked for now.
    let name = @[toCName(genParam)] & toScopedIdent(genParam)
    result.genParams.add name


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
      if a.getName() != b.getName():
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
    of ckTypedefDecl, ckTypeAliasDecl:
      return $curs.cxType()

    of ckClassDecl, ckStructDecl, ckEnumDecl, ckUnionDecl,
       ckClassTemplate, ckTypeAliasTemplateDecl:
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
        let typenameParts = toStrPart(@[
          "type-parameter", "typename type-parameter",
          "typename rebind<type-parameter",
          "typename"
        ])
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
        elif startsWith($cxType, typenameParts):

          let unprefix = dropPrefix($cxType, typenameParts)

          if allIt(unprefix, it in {'0' .. '9', '-'}):
            res = newPType("TYPE_PARAM " & unprefix)

          else:
            res = newPType("COMPLEX_PARAM")


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
              debug decl.treeRepr()


        res

    of tkDependent: newPType("DEPENDENT")

    of tkMemberPointer:
      # WARNING Member pointer
      newPType("!!!")

    of tkDependentSizedArray:
      warn cxtype
      newNType("array", @[
        newPType("???????????????????????"),
        toNType(cxtype.getElementType(), conf).ntype
      ])

    else:
      err "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen(), " ",
        cxtype[]

      newPType("!!!")

  result.ntype = restype
  conf.fixTypeName(result.ntype, conf, 0)
  result.mutable = mutable

func fixTypeParams*(nt: var NType[PNode], params: seq[NType[PNode]]) =
  func aux(nt: var NType[PNode], idx: var int) =
    case nt.kind:
      of ntkIdent, ntkGenericSpec:
        if startsWith(nt.head, "TYPE_PARAM"):
          nt.head = params[idx].head
          inc idx

        for sub in mitems(nt.genParams):
          aux(sub, idx)

      of ntkProc:
        for arg in mitems(nt.arguments):
          aux(arg.vtype, idx)

      else:
        raiseImplementKindError(nt)


  var idx: int
  aux(nt, idx)

func hasSpecial*(nt: NType[PNode], special: seq[string]): bool =
  case nt.kind:
    of ntkIdent, ntkGenericSpec:
      nt.head in special or
      nt.genParams.anyOfIt(it.hasSpecial(special))
    of ntkProc:
      nt.arguments.anyOfIt(it.vtype.hasSpecial(special))
    else:
      false


func hasUnexposed*(nt: NType[PNode]): bool =
  nt.hasSpecial(@[ "UNEXPOSED", "DEPENDENT" ])


func hasComplexParam*(nt: NType[PNode]): bool =
  nt.hasSpecial(@[ "COMPLEX_PARAM" ])

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

proc toInitCall*(cursor: CXCursor, conf: WrapConfig): PNode =
  proc aux(cursor: CXCursor, ilist: bool): PNode =
    case cursor.cxKind():
      of ckUnexposedExpr:
        # info $cursor.cxType()
        if startsWith($cursor.cxType(), "std::initializer_list"):
          # info "Found init list"
          result = aux(cursor[0], true)

        else:
          result = aux(cursor[0], ilist)

      of ckCallExpr:
        let str = "init" & $cursor.cxType()
        case cursor[0].cxKind():
          of ckUnexposedExpr, ckCallExpr, ckFunctionalCastExpr:
            result = aux(cursor[0], ilist)

          of ckIntegerLiteral, ckNullPtrLiteralExpr:
            result = aux(cursor[0], ilist)

          of ckTypeRef:
            # First found in `clang/Rewriter.h/getRangeSize()`
            result = newPCall(str)
            # assert cursor.len == 1, &[
            #   $cursor.getSpellingLocation(),
            #   "\n",
            #   cursor.treeRepr()
            # ]

          # TEMP
          # of ckCallExpr:
          #   let cType = cursor[0].cxType()
          #   case cType.cxKind():
          #     of tkTypedef:
          #       result = aux(cType.getTypeDeclaration(), ilist)
          #       for arg in cursor[0]:
          #         result.add aux(arg, ilist)

          #     else:
          #       raiseImplementKindError(cType)

          else:
            err cursor[0].cxKind()
            debug "\n" & cursor.treeRepr(conf.unit)
            debug cursor.getSpellingLocation()
            # raiseAssert("#[ IMPLEMENT ]#")

        if isNil(result):
          return

        if result.kind in nkTokenKinds:
          result = newPCall(str, result)

        elif result.kind == nkCall and
             result[0].getStrVal() != str:
          result = newPCall(str, result)

      of ckDeclRefExpr:
        if cursor.cxType().cxKind() == tkFunctionProto:
          result = newPCall($cursor)

        elif cursor.cxType().cxKind() == tkTypedef:
          err "Found typedef used as default value"
          debug cursor.getSpellingLocation()
          discard

        elif cursor.cxType().cxKind() == tkEnum:
          err "Found enum value as default"
          debug cursor.getSpellingLocation()
          discard

        else:
          debug cursor.cxType().getTypeDeclaration().treeRepr()

          debug cursor.getSpellingLocation()
          debug cursor.treeRepr()
          raiseImplementKindError(cursor.cxType())

      of ckFunctionalCastExpr:
        result = aux(cursor[1], ilist)

      of ckNullPtrLiteralExpr:
        result = newPLit(nil)

      of ckInitListExpr:
        # debug "Creating initList"
        # debug ilist
        # raiseAssert("#[ IMPLEMENT ]#")
        if ilist:
          result = newPCall("cxxInitList")

        else:
          result = newPCall("init" & $cursor.cxType())

        for arg in cursor:
          result.add aux(arg, false)

      of ckIntegerLiteral, ckCharacterLiteral, ckFloatingLiteral,
         ckStringLiteral:
        let tokens = cursor.tokenStrings(conf.unit)

        case cursor.cxKind():
          of ckIntegerLiteral:
            result = newPCall("cint", newPLit(parseInt(tokens[0])))

          of ckStringLiteral:
            result = newPCall("cstring", newPLit(tokens[0]))

          of ckCharacterLiteral:
            result = newPLit(tokens[0][1])

          of ckFloatingLiteral:
            result = newPLit(parseFloat(tokens[0]))

          else:
            discard

      of ckLambdaExpr:
        err "FIXME implement conversion to call from lambda expr"
        discard

      of ckTypeRef:
        err "FIXME implement conversion to call from type ref "
        discard

      of ckCStyleCastExpr:
        result = nnkCast.newPTree(
          cursor[0].cxType().toNType(conf).ntype.toNNode(),
          aux(cursor[1], ilist)
        )

      else:
        err "Implement for kind", cursor.cxKind()
        debug cursor.getSpellingLocation()
        debug cursor.tokenStrings(conf.unit)
        debug cursor.treeRepr(conf.unit)
        raiseAssert("#[ IMPLEMENT ]#")

  return aux(cursor, false)


proc setDefaultForArg*(arg: var CArg, cursor: CXCursor, conf: WrapConfig) =
  ## Update default value for argument.
  ## - @arg{arg} :: Non-raw argument to update default for
  ## - @arg{cursor} :: original cursor for argument declaration
  ## - @arg{conf} :: Default wrap configuration

  # info cursor.len
  if cursor.len == 2 and
     cursor[1].cxKind() in {ckUnexposedExpr, ckInitListExpr}:
    # debug cursor.treeRepr(conf.unit)
    let default = toInitCall(cursor[1], conf)
    if not isNil(default):
      arg.default = some(default)
    # debug arg.default
