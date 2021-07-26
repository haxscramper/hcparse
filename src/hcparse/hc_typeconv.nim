## Conversion of C types to nim

import cxtypes, hc_types
import hnimast

import
  hmisc/other/[hlogger],
  hmisc/types/colorstring,
  hmisc/algo/[hstring_algo, hseq_mapping, clformat],
  hmisc/helpers,
  hmisc/macros/iflet

import std/[algorithm, strformat, sequtils, strutils,
            parseutils, tables, decls]

import cxcommon


proc getTypeName*(cxtype: CXType, conf: WrapConf): string

proc toNimType*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): NimType


proc fromElaboratedPType*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): NimType =
  # debug cxtype
  let genParams = cxtype.getNumTemplateArguments()
  let decl = cxtype.getTypeDeclaration()
  if genParams > 0:
    case decl.cxKind:
      of ckTypedefDecl, ckTypeAliasDecl, ckTypeAliasTemplateDecl:
        # WARNING `template <J, Q> using` is not handled
        result = newNimType(cxtype.getTypeName(conf), cxtype)

      of ckClassDecl, ckStructDecl, ckClassTemplate:
        # debug "Class decl"
        let params = cxtype.genParams()
        result = newNimType(cxtype.getTypeName(conf), cxtype)
        for idx, parm in params:
          if parm.cxKind != tkInvalid:
            result.add parm.toNimType(conf, cache)

      else:
        conf.warn "Conversion from elaborated type: ", decl
        conf.debug "  ", decl.cxKind(), " in ", decl.getSpellingLocation()

    conf.fixTypeName(result, conf, 0)

  else:
    result = newNimType(getTypeName(cxtype, conf), cxtype)

proc dropPOD*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): string =
  case cxtype.cxKind:
    of tkElaborated:
      cxtype.fromElaboratedPType(conf, cache).nimName

    of tkPointer:
      cxtype[].dropPOD(conf, cache)

    of tkTypedef:
      ($cxtype).dropPrefix("const ")

    else:
      ""

proc toCArg*(cursor: CXCursor, conf: WrapConf, cache: var WrapCache): CArg =
  var varname = $cursor
  if varname.len == 0:
    varname = "arg" & $cursor.cxType().dropPOD(conf, cache)

  varname = varname.fixIdentName()
  let argType = cursor.cxType().toNimType(conf, cache)
  return initCArg(varname, argType)



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


proc fullScopedIdent*(
    cxtype: CXType, filterInline: bool = true, withType: bool = true):
  CScopedIdent =

  for cursor in getTypeNamespaces(cxtype, filterInline, withType):
    result.add toCName(cursor)


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

proc toScopedIdent*(sem: seq[CxCursor]): CSCopedIdent =
  for elem in sem:
    result.add toCName(elem)

proc toScopedIdent*(name: string): CScopedIdent =
  result.add toCName(name)

proc sameNoGeneric*(ident1, ident2: CScopedIdent): bool =
  result = ident1.len == ident2.len
  if result:
    for (a, b) in zip(ident1, ident2):
      if a.getName() != b.getName():
        return false

proc typeName*(ident: CScopedIdent): seq[string] = ident.mapIt($it.cursor)

proc namespacedName*(name: seq[CxCursor], conf: WrapConf): string =
  name.mapIt(dropPrefix(
    $it, toStrPart(["const ", "enum ", "struct ", "union "]))).join("::")

proc namespacedName*(decl: CxCursor, conf: WrapConf): string =
  ## Create /raw/ identifier from fully namespaces Cxx declaration entry.
  ##
  ## - EXAMPLE :: Given `namespace nsp { struct Str{}; }` and cursor that
  ##   points to the `Str` /declaration/ it should return
  assertKind(decl, {ckClassDecl, ckStructDecl, ckClassTemplate})
  decl.getSemanticNamespaces().namespacedName(conf)

proc namespacedName*(cxtype: CxType, conf: WrapConf): string =
  ## Return fully qualified namespaced name for a type based on the type
  ## instance.
  cxtype.getTypeNamespaces().namespacedName(conf)

proc newNimType*(
    conf: WrapConf,
    semspaces: seq[CxCursor], cxType: CxType): NimType =
  result = newNimType(semSpaces.namespacedName(conf), cxType)
  result.fullIdent = some toScopedIdent(semSpaces)


proc defaultTypeParameter*(
  cursor: CxCursor, cache: var WrapCache, conf: WrapConf): Option[NimType] =
  # Clang represents default template type parameters using flat list that I
  # need to collect back into recursive structure again. The algorithm is pretty
  # similar to recursive descent parsing. Current implementation handles
  # only couple use cases right now - mainly `alloc = std::alloccator<char_t>`

  # Example of the template type parameters `_Alloc = std::allocator<_CharT>`
  #```
  # kind: TemplateTypeParameter _Alloc:
  #   +-> type: _Alloc
  #   +-> TemplateRef:
  #   |   +-> type: <invalid>
  #   |   +-> allocator
  #   +-> TypeRef:
  #       +-> type: _CharT
  #       +-> _CharT
  #```

  let params = toSeq(cursor)

  # conf.info cursor.treeRepr()
  # conf.dump cursor.getSpellingLocation()

  proc foldTypes(idx: var int, cache: var WrapCache): NimType =
    let param = params[idx]
    case param.kind:
      of ckTypeRef:
        result = toNimType(param.cxType(), conf, cache)
        inc idx

      of ckTemplateRef:
        let
          cxtype = param.cxType()
          def = param.getCursorDefinition()
          semspaces = def.getSemanticNamespaces()

        result = conf.newNimType(semSpaces, cxtype)
        # result.fullIdent = toScopedIdent(semspaces)
        # conf.dump def.cxType()
        # conf.dump cxtype, cxtype.getNumTemplateArguments()
        # conf.trace result
        inc idx

        result.genericParams.add foldTypes(idx, cache)

      else:
        raise newUnexpectedKindError(params[idx])

  var idx = 0
  return some foldTypes(idx, cache)

proc setParamsForType*(
    cache: var WrapCache, conf: WrapConf,
    ident: CScopedIdent, params: seq[CxCursor]
  ) =
  ## Set or update default template type parameters for type `ident`

  # This procedure is called multiple times and iteratively builds list of
  # actual default templated parameters based on different type occurencies.
  # This is necessary becase type /declaration/ is not guaranteed to contain all
  # the necessary information. Specific example - `std::baisc_string`. It is
  # defined as regular templated class with not default parameters, which are
  # specified in completely different file.

  # #+caption: `basic_string.h`
  # ```cpp
  # template<typename _CharT, typename _Traits, typename _Alloc>
  #   class basic_string
  # ```
  #
  # #+caption: `stringfwd.h`
  # ```cpp
  # template<typename _CharT, typename _Traits = char_traits<_CharT>,
  #        typename _Alloc = allocator<_CharT> >
  # class basic_string;
  # ```

  if params.len > 0:
    var key: seq[string]
    for part in ident:
      key.add $part.cursor

    if key notin cache.paramsForType:
      cache.paramsForType[key] = @[]

    # Convenience helper to avoid writing `cache.paramsForType[key]`
    # all over the place.
    var list {.byaddr1.} = cache.paramsForType[key]

    for idx, param in params:
      var nimType = param.cxtype.toNimType(conf, cache)

      if list.high < idx:
        list.add NimType(kind: ctkIdent)

      if list[idx].defaultType.isNone():
        # Only assign if default template type parameter is none - current type
        # conversion is likely to have at least as much information (or more).
        conf.fixTypeName(nimType, conf, 0)
        list[idx] = nimType

      if param.len() > 0:
        var default = defaultTypeParameter(param, cache, conf)

        if default.isSome():
          conf.fixTypeName(default.get(), conf, 0)
          list[idx].defaultType = default
          # conf.debug "Set default type for", idx, "type parameter", key.hshow()

          # conf.debug cache.paramsForType[key][idx]


proc replacePartials*(
    nimType: var NimType,
    partials: Table[string, NimType],
    conf: WrapConf) =

  ## Replace templated type names in `nimType` with corresponding ones from
  ## `partials`. This is used to create concrete instantiation of C++ template
  ## type with defaulted parameters

  proc aux(nimType: var NimType) =
    if nimType.kind == ctkIdent:
      if nimType.nimName in partials:
        nimType = partials[nimType.nimName]

      else:
        for subnode in mitems(nimType.genericParams):
          aux(subnode)

  # conf.dump partials
  aux(nimType)

proc getParamsForType*(cache: WrapCache, name: seq[string]): seq[NimType] =
  if name in cache.paramsForType:
    result = cache.paramsForType[name]

proc getParamsForType*(
    cache: var WrapCache, cxtype: CScopedIdent,
    conf: WrapConf,
    paramRange: Slice[int] = 0 .. high(int),
    default: bool = false,
    partial: NimType = NimType(kind: ctkIdent)
  ): seq[NimType] =
  ## One or more (potentially defaulted) generic type parameters for type
  ## `cxtype`. Parameter range starts at first requested parameter /index/.
  ## If end of range equal to `high(int)` return all template parameters.

  let name = cxtype.typeName()

  if name in cache.paramsForType:
    var params {.byaddr1.} = cache.paramsForType[name]
    var partials: Table[string, NimType]

    for paramIdx in paramRange:
      if paramIdx < params.len:
        var res =
          if params[paramIdx].defaultType.isSome() and default:
            params[paramIdx].defaultType.get()

          else:
            params[paramIdx]

        result.add res

      elif paramRange.b == high(int):
        break

      else:
        raise newImplementError(
          &"Type {cxtype} does not have generic parameter indexed {paramIdx}")

    # Collect names of the template type parameters that were explicitly
    # specified in the `partial` instantiation
    let minVal = min(paramRange.a, partial.genericParams.len)
    for partialIdx in 0 ..< minVal:
      partials[params[partialIdx].nimName] = partial.genericParams[partialIdx]

    if partials.len > 0:
      for item in mitems(result):
        # For each element in result replace partial template type parameter
        # with concrete type specialization
        item.replacePartials(partials, conf)

      # conf.dump result

    # NOTE current solution is a hack and it operates on assumption that
    # template type parameter will match, which is not the case. Specific
    # example that works right now due to that assumption:

    # ```cpp
    # template<typename _CharT, typename _Traits = char_traits<_CharT>,
    #        typename _Alloc = allocator<_CharT> >
    # class basic_string;
    # ```

    # When `basic_string` is instantiated it needs to fill default template
    # parameters as well - `_Traits` and `_Alloc` part. First type parameter for
    # basic string was called `_CharT` and (if we instantiated `std::string =
    # basic_string<char>`) it would be mapped to `cchar`. It is a lucky
    # coincidence that `std::char_traits` also uses template parameter called
    # `_CharT` - because of this simple replace `_CharT -> cchar` would give me
    # fully correct instantiation of the std string. In general more
    # sophisticated mechanism must be provided to deal with cases that don't
    # magically agree on template type parameter names.

    # ```
    # Type1[C]
    # Type2[A, B = Type1[A]] // Must keep track of multistep renames `C -> A` (or `A -> C`)
    #                        // most likely this would mean recursive calls to replace partials.
    # ```

    # TODO update this comment when new algorithm is implemented



proc getTypeName*(cxtype: CXType, conf: WrapConf): string =
  let curs = cxtype.getTypeDeclaration()
  case curs.cxKind:
    of ckTypedefDecl, ckTypeAliasDecl:
      return $curs.cxType()

    of ckClassDecl, ckStructDecl, ckEnumDecl, ckUnionDecl,
       ckClassTemplate, ckTypeAliasTemplateDecl:
      result = $curs

    else:
      conf.err $curs
      conf.err "Type name for ", curs.treeRepr(conf.unit)
      raiseAssert(
        &"Cannot convert cursor of kind {curs.cxKind} to type")

  result = namespacedName(cxtype, conf)
  conf.debug cxtype.getTypeNamespaces()
  conf.debug result

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

proc toNimType*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): NimType =
  ## Convert CXType to nim type. Due to differences in how mutability
  ## handled in nim and C it is not entirely possible to map `CXType` to
  ## `NimType` without losing this information. Instead `isMutable` is set
  ## in resulting type, indicating whether or not the type was mutable.
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

  if conf.isComplexType(cxtype, cache):
    return conf.newComplexType(cxType, cache)

  var
    mutable: bool = false
    special: CTypeSpecialKind = ctskNone

  result = case cxtype.cxKind():
    of tkBool:       newNimType("bool",        cxtype)
    of tkInt:        newNimType("cint",        cxtype)
    of tkVoid:       newNimType("void",        cxtype)
    of tkUInt:       newNimType("cuint",       cxtype)
    of tkLongLong:   newNimType("clonglong",   cxtype)
    of tkULongLong:  newNimType("culonglong",  cxtype)
    of tkDouble:     newNimType("cdouble",     cxtype)
    of tkULong:      newNimType("culong",      cxtype)
    of tkUChar:      newNimType("cuchar",      cxtype)
    of tkChar16:     newNimType("cchar16",     cxtype)
    of tkChar32:     newNimType("cchar32",     cxtype)
    of tkWChar:      newNimType("cwchar",      cxtype)
    of tkChar_S:     newNimType("cchar",       cxtype)
    of tkLong:       newNimType("clong",       cxtype)
    of tkUShort:     newNimType("cushort",     cxtype)
    of tkNullPtr:    newNimType("pointer",     cxtype) # WARNING C++ type is `nullptr_t`
    of tkFloat:      newNimType("cfloat",      cxtype)
    of tkLongDouble: newNimType("clongdouble", cxtype)
    of tkShort:      newNimType("cshort",      cxtype)
    of tkSChar:      newNimType("cschar",      cxtype)
    of tkTypedef:
      mutable = cxType.isMutableRef()
      newNimType(($cxtype).dropPrefix("const "), cxtype) # XXXX typedef processing -

    of tkElaborated, tkRecord, tkEnum:
      # debug "From elaborated type"
      fromElaboratedPType(cxtype, conf, cache)

    of tkPointer:
      case cxtype[].cxkind:
        of tkChar_S:
          newNimType("cstring", cxtype)

        of tkPointer:
          if cxtype[][].cxKind() == tkChar_S:
            newNimType("cstringArray", cxtype)

          else:
            newNimType("ptr", [toNimType(cxtype[], conf, cache)], cxtype)

        of tkVoid:
          newNimType("pointer", cxtype)

        of tkFunctionProto:
          toNimType(cxtype[], conf, cache)

        else:
          newNimType("ptr", [toNimType(cxtype[], conf, cache)], cxtype)

    of tkConstantArray:
      newNimType(
        "ptr", [
          newNimType(
            "array", @[
              newNimType($cxtype.getNumElements(), cxtype.getElementType()),
              toNimType(cxtype.getElementType(), conf, cache)
            ], cxType)
        ], cxType)

    of tkIncompleteArray:
      # QUESTION maybe convert to `ptr UncheckedArray?` or add user-defined
      # callback for switching between different behaviors.
      newNimType("ptr", [toNimType(
        cxtype.getElementType(), conf, cache)], cxType)

    of tkFunctionProto:
      newNimType(
        cxtype.argTypes.mapIt(initCArg("", toNimType(it, conf, cache))),
        cxtype.getResultType().toNimType(conf, cache)
      )

    of tkLValueReference:
      mutable = cxType.isMutableRef()
      special = ctskLValueRef
      toNimType(cxType[], conf, cache)

    of tkRValueReference: # WARNING I'm not 100% sure this is correct
                          # way to map rvalue references to nim type
                          # system.
      mutable = cxType.isMutableRef()
      special = ctskRValueRef
      toNimType(cxType[], conf, cache)

    of tkUnexposed:
      let strval = ($cxType).dropPrefix("const ") # WARNING
      let db = "string" in strval

      if strval.validCxxIdentifier():
        newNimType(strval, cxtype)

      else:
        # pprintStackTrace()
        let
          decl = cxtype.getTypeDeclaration()
          name = cxType.namespacedName(conf)
          typenameParts = toStrPart(@[
            "type-parameter", "typename type-parameter",
            "typename rebind<type-parameter",
            "typename"
          ])

        # let log =

        # if log:
        #   conf.dump decl, decl.kind

        var res = newNimType(name, cxType)
        # if "initializer_list" in strval:
        #   # let (start, final) = strval.skipUntil('<')
        #   conf.dump cxType[]
        #   conf.dump name
        #   conf.dump cxType.getTemplateArgumentAsType(0)
        #   res.add newNimType("I")

        # el
        if decl.cxKind in {
          # HACK list of necessary kinds is determined by trial and error,
          # I'm still not really sure what `tkUnexposed` actually
          # represents.
          ckClassTemplate, ckClassDecl
        }:
          for arg in cxType.templateParams():
            res.add toNimType(arg, conf, cache)

          # for param in cache.getParamsForType(
          #   cxType.fullScopedIdent(), conf):
          #   res.add param

        elif startsWith($cxType, typenameParts):
          let unprefix = dropPrefix($cxType, typenameParts)
          if allIt(unprefix, it in {'0' .. '9', '-'}):
            res = newNimType("TYPE_PARAM " & unprefix, cxtype, true)

          else:
            res = newTemplateUndefined(cxType)

        else:
          res = newNimType("UNEXPOSED", cxtype, true)
          if decl.cxKind() notin {ckNoDeclFound}:
            conf.warn "No decl found for type"
            conf.logger.indented:
              conf.info cxtype.lispRepr()
              conf.debug decl.getSpellingLocation()
              conf.debug decl.cxKind()
              conf.debug decl.treeRepr()


        res

    of tkDependent:
      newNimType("DEPENDENT", cxType, true)

    of tkMemberPointer:
      # WARNING Member pointer
      newNimType("!!!", cxType, false)

    of tkDependentSizedArray:
      let cx = $cxtype
      let name = cx[cx.skipUntil('[') + 1 .. ^2].strip()
      newNimType("array", @[
        newNimType(name),
        toNimType(cxtype.getElementType(), conf, cache)
      ], cxType)

    else:
      conf.err "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen(), " ",
        cxtype[]

      newNimType("!!!", cxtype)

  result.isMutable = mutable
  result.specialKind = special
  conf.fixTypeName(result, conf, 0)

func fixTypeParams*(nt: var NimType, params: seq[NimType]) =
  func aux(nt: var NimType, idx: var int) =
    case nt.kind:
      of ctkIdent:
        if startsWith(nt.nimName, "TYPE_PARAM"):
          nt.nimName = params[idx].nimName
          inc idx

        for sub in mitems(nt.genericParams):
          aux(sub, idx)

      of ctkProc:
        for arg in mitems(nt.arguments):
          aux(arg.nimType, idx)



  var idx: int
  aux(nt, idx)


func hasSpecial*(nt: NimType, special: seq[string]): bool =
  case nt.kind:
    of ctkIdent:
      nt.nimName in special or
      nt.genericParams.anyOfIt(it.hasSpecial(special))

    of ctkProc:
      nt.arguments.anyOfIt(it.nimType.hasSpecial(special))


func hasUnexposed*(nt: NimType): bool =
  nt.hasSpecial(@[ "UNEXPOSED", "DEPENDENT" ])


func hasComplexParam*(nt: NimType): bool =
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

proc toInitCall*(
    cursor: CXCursor, conf: WrapConf, cache: var WrapCache): PNode =
  proc aux(cursor: CXCursor, ilist: bool, cache: var WrapCache): PNode =
    case cursor.cxKind():
      of ckUnexposedExpr:
        # info $cursor.cxType()
        if startsWith($cursor.cxType(), "std::initializer_list"):
          # info "Found init list"
          result = aux(cursor[0], true, cache)

        else:
          result = aux(cursor[0], ilist, cache)

      of ckCallExpr:
        let str = "init" & $cursor.cxType()
        case cursor[0].cxKind():
          of ckUnexposedExpr, ckCallExpr, ckFunctionalCastExpr:
            result = aux(cursor[0], ilist, cache)

          of ckIntegerLiteral, ckNullPtrLiteralExpr:
            result = aux(cursor[0], ilist, cache)

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
            conf.err cursor[0].cxKind()
            conf.debug "\n" & cursor.treeRepr(conf.unit)
            conf.debug cursor.getSpellingLocation()
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
          conf.err "Found typedef used as default value"
          conf.debug cursor.getSpellingLocation()
          discard

        elif cursor.cxType().cxKind() == tkEnum:
          conf.err "Found enum value as default"
          conf.debug cursor.getSpellingLocation()
          discard

        else:
          conf.debug cursor.cxType().getTypeDeclaration().treeRepr()

          conf.debug cursor.getSpellingLocation()
          conf.debug cursor.treeRepr()
          raiseImplementKindError(cursor.cxType())

      of ckFunctionalCastExpr:
        result = aux(cursor[1], ilist, cache)

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
          result.add aux(arg, false, cache)

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
        conf.err "FIXME implement conversion to call from lambda expr"
        discard

      of ckTypeRef:
        conf.err "FIXME implement conversion to call from type ref "
        discard

      of ckCStyleCastExpr:
        result = nnkCast.newPTree(
          cursor[0].cxType().toNimType(conf, cache).toNType().toNNode(),
          aux(cursor[1], ilist, cache)
        )

      else:
        conf.err "Implement for kind", cursor.cxKind()
        conf.debug cursor.getSpellingLocation()
        conf.debug cursor.tokenStrings(conf.unit)
        # debug cursor.treeRepr(conf.unit)
        # raiseAssert("#[ IMPLEMENT ]#")

  return aux(cursor, false, cache)


proc setDefaultForArg*(
    arg: var CArg, cursor: CXCursor, conf: WrapConf, cache: var WrapCache) =
  ## Update default value for argument.
  ## - @arg{arg} :: Non-raw argument to update default for
  ## - @arg{cursor} :: original cursor for argument declaration
  ## - @arg{conf} :: Default wrap configuration

  # info cursor.len
  if cursor.len == 2 and
     cursor[1].cxKind() in {ckUnexposedExpr, ckInitListExpr}:
    # debug cursor.treeRepr(conf.unit)
    let default = toInitCall(cursor[1], conf, cache)
    if not isNil(default):
      arg.default = some(default)
    # debug arg.default
