import
  hmisc/core/all,
  hmisc/algo/[hstring_algo],
  hmisc/types/colorstring



import std/[with, sequtils, strutils, strformat]

import
  ./interop_ir/[wrap_store, wrap_icpp],
  ./hc_types,
  ./cxtypes,
  ./hc_typeconv

proc cxxName*(conf: WrapConf, decl: CDecl): CxxName =
  raise newImplementError()

proc cxxName*(cxtype: CxType, cache: var WrapCache): CxxName =
  raise newImplementError()

proc cxxPair*(conf: WrapConf, decl: CDecl): CxxNamePair =
  cxxPair(decl.getNimName(conf), conf.cxxName(decl))

proc cxxPair*(
  conf: WrapConf, cxtype: CxType, cache: var WrapCache): CxxNamePair =
  raise newImplementError()
  # cxxPair(cxtype.getTypeName(conf))

proc toCxxUse*(
  conf: WrapConf, cxtype: CxType, cache: var WrapCache): CxxTypeUse

proc cxxGenParams*(
    conf: WrapConf, ident: CSCopedIdent, cache: var WrapCache
  ): CxxGenParams =

  raise newImplementError()

proc toCxxElaborated*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): CxxTypeUse =

  let decl = cxtype.getTypeDeclaration()
  result = conf.cxxPair(cxtype, cache).cxxTypeUse()
  if cxtype.getNumTemplateArguments() > 0:
    case decl.cxKind:
      of ckTypedefDecl, ckTypeAliasDecl, ckTypeAliasTemplateDecl:
        # WARNING `template <J, Q> using` is not handled
        discard
        # result = conf.cxxPair(cxtype, @[]) # newNimType(cxtype.getTypeName(conf), cxtype)

      of ckTypeDeclKinds:
        # result = newNimType(cxtype.getTypeName(conf), cxtype)
        let params = cxtype.templateParams()
        for idx, parm in params:
          if parm.cxKind != tkInvalid:
            result.add conf.toCxxUse(parm, cache) # parm.toNimType(conf, cache)

      else:
        conf.warn "Conversion from elaborated type: ", decl
        conf.debug "  ", decl.cxKind(), " in ", decl.getSpellingLocation()

  # else:
  #   result = newNimType(getTypeName(cxtype, conf), cxtype)

proc toCxxUse*(conf: WrapConf, cxtype: CxType, cache: var WrapCache): CxxTypeUse =
  if conf.isComplexType(cxtype, cache):
    raise newImplementError()
    # return conf.newComplexType(cxType, cache)

  var
    mutable: bool = false
    special: CTypeSpecialKind = ctskNone
    pair: CxxNamePair

  case cxtype.cxKind():
    of tkBool, tkint, tkvoid, tkuint, tklonglong, tkulonglong,
       tkdouble, tkulong, tkuchar, tkchar16, tkchar32, tkwchar,
       tkchar_s, tklong, tkushort, tknullptr, tkfloat, tklongdouble,
       tkshort, tkschar:
      let name = $cxtype
      pair = cxxPair(name, mapPrimitiveName(name).cxxName())

    of tkTypedef:
      mutable = cxType.isMutableRef()
      pair = cxxPair(($cxtype).dropPrefix("const "), cxxName(cxtype, cache))

    of tkElaborated, tkRecord, tkEnum:
      result = toCxxElaborated(cxtype, conf, cache) # fromElaboratedPType(cxtype, conf, cache)

    of tkPointer:
      result = toCxxUse(conf, cxtype[], cache)
      result = wrap(result, ctkPtr)

    # of tkConstantArray:
    #   newNimType(
    #     "ptr", [
    #       newNimType(
    #         "array", @[
    #           newNimType($cxtype.getNumElements(), cxtype.getElementType()),
    #           toNimType(cxtype.getElementType(), conf, cache)
    #         ], cxType)
    #     ], cxType)

    # of tkIncompleteArray:
    #   # QUESTION maybe convert to `ptr UncheckedArray?` or add user-defined
    #   # callback for switching between different behaviors.
    #   newNimType("ptr", [toNimType(
    #     cxtype.getElementType(), conf, cache)], cxType)

    # of tkFunctionProto:
    #   newNimType(
    #     cxtype.argTypes.mapIt(initCArg("", toNimType(it, conf, cache))),
    #     cxtype.getResultType().toNimType(conf, cache)
    #   )

    # of tkLValueReference:
    #   # NOTE this implementation does not work as expected, becuase `const
    #   # T&` is not a const-qulified type.
    #   #
    #   # mutable = not cxType.isConstQualified()
    #   mutable = not startsWith($cxType, "const")
    #   special = ctskLValueRef

    #   toNimType(cxType[], conf, cache)

    # of tkRValueReference: # WARNING I'm not 100% sure this is correct
    #                       # way to map rvalue references to nim type
    #                       # system.
    #   mutable = cxType.isMutableRef()
    #   special = ctskRValueRef
    #   toNimType(cxType[], conf, cache)

    # of tkUnexposed:
    #   let strval = ($cxType).dropPrefix("const ") # WARNING
    #   let db = "string" in strval

    #   if strval.validCxxIdentifier():
    #     newNimType(strval, cxtype)

    #   else:
    #     let
    #       decl = cxtype.getTypeDeclaration()
    #       name = cxType.namespacedName(conf)
    #       typenameParts = toStrPart(@[
    #         "type-parameter", "typename type-parameter",
    #         "typename rebind<type-parameter",
    #         "typename"
    #       ])


    #     var res = newNimType(name, cxType)
    #     if decl.cxKind in ckTypeDeclKinds:
    #       # HACK list of necessary kinds is determined by trial and error,
    #       # I'm still not really sure what `tkUnexposed` actually
    #       # represents.
    #       for arg in cxType.templateParams():
    #         res.add toNimType(arg, conf, cache)
    #         res.genParams[^1].isParam = true

    #     elif startsWith($cxType, typenameParts):
    #       let unprefix = dropPrefix($cxType, typenameParts)
    #       if allIt(unprefix, it in {'0' .. '9', '-'}):
    #         res = newNimType("TYPE_PARAM " & unprefix, cxtype, true)

    #       else:
    #         res = newTemplateUndefined(cxType)

    #     else:
    #       res = newNimType("UNEXPOSED", cxtype, true)
    #       if decl.cxKind() notin {ckNoDeclFound}:
    #         conf.warn "No decl found for type"
    #         conf.logger.indented:
    #           conf.info cxtype.lispRepr()
    #           conf.debug decl.getSpellingLocation()
    #           conf.debug decl.cxKind()
    #           conf.debug decl.treeRepr()


    #     res

    # of tkDependent:
    #   newNimType("DEPENDENT", cxType, true)

    # of tkMemberPointer:
    #   # WARNING Member pointer
    #   newNimType("!!!", cxType, false)

    # of tkDependentSizedArray:
    #   let cx = $cxtype
    #   let name = cx[cx.skipUntil('[') + 1 .. ^2].strip()
    #   newNimType("array", @[
    #     newNimType(name),
    #     toNimType(cxtype.getElementType(), conf, cache)
    #   ], cxType)

    else:
      conf.err "CANT CONVERT: ".toRed({styleItalic}),
        cxtype.kind, " ", ($cxtype).toGreen(), " ",
        cxtype[]

      # newNimType("!!!", cxtype)

  # result.isMutable = mutable
  # result.specialKind = special

proc toCxxOperator*(
    oper: CDecl,
    conf: WrapConf,
    cache: var WrapCache
  ): CxxProc =
  assert conf.isImportcpp

  result = conf.cxxPair(oper).cxxProc()
  result.kind = oper.classifyOperator(conf)

  case result.kind:
    of cpkRegular, cpkConstructor, cpkDestructor:
      raise newUnexpectedKindError(result)

    of cpkCopyAsgnOp:
      result.nimName = "setFrom"
      result.icpp = icppInfix("=")

    of cpkNewOp, cpkDeleteOp:
      discard

    of cpkAsgnOp:
      result.icpp = icppInfix("=")

    of cpkArrayOp:
      let rtype = oper.cursor.retType()
      let nimReturn = rtype.toNimType(conf, cache)
      if nimReturn.isMutable:
        result.nimName = "[]="
        # WARNING potential source of horrible c++ codegen errors
        result.icpp = initIcpp(ipkNextArg, "[", ipkNextARg, "]=", ipkNextArg)

      else:
        result.icpp = initIcpp(ipkNextArg, "[", ipkNextARg, "]")

    of cpkInfixOp:
      result.icpp = initIcpp &"({toCppNamespace(oper.ident)}(#, #))"

      # if oper.arguments.len < 2:
      #   result.addThis = true

    of cpkArrowOp:
      # WARNING
      result.icpp = initIcpp &"(#.operator->(@))"

    of cpkCallOp:
      # NOTE nim does have experimental support for call
      # operator, but I think it is better to wrap this one as
      # separate function `call()`
      with result:
        nimName = "call"
        kind = cpkCallOp
        icpp = initIcpp(ipkNextArg, "(", ipkArgSplice, ")") # &"#(@)"

    of cpkDerefOp:
      result.nimName = "[]"
      result.icpp = initIcpp &"(*#)"

    of cpkPrefixOp:
      result.icpp = initIcpp($result.cxxName, ipkNextArg)

    of cpkPostfixOp:
      result.icpp = initIcpp(ipkNextArg, $result.cxxName)


    of cpkCommaOp:
      result.nimName = "commaOp"
      result.icpp = initIcpp &"operator,(@)"

    of cpkConvertOp:
      let restype = toCxxUse(conf, oper.cursor.retType(), cache)
      with result:
        nimName = "to" & capitalizeAscii(restype.nimName)
        icpp = initIcpp(ipkArgSplice)
        returnType = resType



    of cpkUserLitOp:
      let restype = toCxxUse(conf, oper.cursor.retType(), cache)
      with result:
        nimName = "to" & capitalizeAscii(restype.nimName)
        icpp = initIcpp &"({oper.cursor}(@))"
        returnType = restype

  # FIXME
  # result.header = conf.makeHeader(oper.cursor, conf)

proc toCxxProc*(
    pr: CDecl,
    conf: WrapConf,
    parent: Option[NimType],
    cache: var WrapCache,
    parentDecl: Option[CDecl]
  ): CxxProc =

  result = cxxProc(conf.cxxPair(pr))

  template endProc(result: GenProc): untyped =
    let generated = newProcVisit(result, conf, cache)
    result.decl.add result
    result.decl.add GenPass(iinfo: currLInfo(), passEntries: generated)


  var addThis = (
    pr.kind == cdkMethod and
    pr.cursor.cxKind notin {
      ckConstructor, ckDestructor, ckConversionFunction })

  result.head.genParams = conf.cxxGenParams(pr.ident, cache) # conf.genParamsForIdent(pr.ident, cache)

  if pr.isOperator:
    # HACK temporary workaround for `new` and `delete` operator handing
    var opKind = classifyOperator(pr, conf)
    if opKind notin {cpkNewOp, cpkDeleteOp}:
      result = pr.toCxxOperator(conf, cache)

  else:
    let icppName = toCppNamespace(pr.ident)
    if parent.isSome():
      assert conf.isImportcpp,
        "Cannot wrap methods for non-cxx targets"

      if pr.cursor.isStatic():
        result.icpp = initIcpp &"({icppName}(@))"

      else:
        result.icpp = initIcpp &"(#.{pr.getNimName(conf)}(@))"

      # result.header = conf.makeHeader(pr.cursor, conf)

    else:
      if conf.isImportcpp:
        result.icpp = initIcpp &"({icppName}(@))"

      else:
        result.icpp = initIcpp &"{icppName}"

      # result.header = conf.makeHeader(pr.cursor, conf)


  if $pr.cursor == "operator=":
    # FIXME check if first argument and parent declaration types are
    # identical. Right now it does not work because
    # `b3Matrix3x3` != `const b3Matrix3x3 &`
    if parentDecl.get().cursor.cxType() == pr.arguments[0].cursor.cxType():
      # By default `operator=` is converted to regular `setFrom` proc to
      # correctly handle multiple overloads (in C++ `operator=` can have
      # different types on RHS and LHS, but this is not the case in nim).
      # *if* assignmed is indeed done from two identical types, then it can
      # be wrapped as actual `=` proc.

      # result.name = "="
      # result.kind = pkOperator
      discard

    else:
      # Otherwise add `self` for wrapped proc
      addThis = parent.isSome()

  if addThis:
    assert parent.isSome()
    result.arguments.add initCArg(
      "self", parent.get(),
      pr.cursor.isConstMethod.tern(nvdVar, nvdLet)
    )

  for argIdx, arg in pr.arguments:
    var argType = arg.cursor.cxType().toNimType(conf, cache)
    if arg.cursor.cxType().isEnum():
      argType.nimName &= conf.rawSuffix()

    if argType.kind in {ctkIdent}:
      argType.genParams.add argType.getPartialParams(conf, cache, false)

      if argType.nimName == "UNEXPOSED":
        # WARNING currently parameters which contain `tkUnexposed`
        # types are not handled but are skipped instead. I don't
        # know how to fix right now.
        result.canAdd = false

      if argType.isComplex.not() and
         parentDecl.isSome() and
         arg.cursor.inheritsGenParamsOf(parentDecl.get().cursor) and
         parent.isSome() and
         (arg.cursor.cxType().kind notin {tkUnexposed})
        :
        # WARNING nested class definitions with additional template
        # parameters are not handled right now. It will break for
        # code like
        # `<Ta> struct A { <Tb> struct B {void func(); }; };`
        # and only add `Tb` as template parameter for `func()`.
        for param in parent.get().genParams:
          argType.add param
        # FAIL most likely broken with recent refactoring

    else:
      # FIXME determine and implement edge case handling for procvar
      # arguments
      conf.warn "Temporarily droppping procvar arguemtn handling"


    if not (opKind == cpkPostfixOp and argIdx > 0):
      var newArg = initCArg(arg.name, argType)
      setDefaultForArg(newArg, arg.cursor, conf, cache)
      result.arguments.add newArg

  if pr.cursor.isVariadic() == 1:
    result.pragma.add newPIdent("varargs")

  if pr.isOperator and pr.classifyOperator(conf) == cpkAsgnOp:
    # HACK Force override return type for assignment operators
    result.returnType = newNimType("void")
    endProc(result)

  # elif pr.cursor.kind in {ckConstructor, ckConversionFunction}:
  #   # Override handling of return types for constructors
  #   if not pr.isOperator:
  #     # But ignore implicit user-defined conversion functions like
  #     # `operator T()`

  #     # result.header = conf.makeHeader(pr.cursor, conf)
  #     let suffix = parent.get().nimName.capitalizeAscii()

  #     block initConstructor:
  #       var result = deepCopy(result)
  #       result.name = "init" & suffix
  #       result.returnType = parent.get()
  #       result.icpp = initIcpp &"{toCppNamespace(parentDecl.get().ident)}(@)"
  #       result.decl.add result

  #     block newRefConstructor:
  #       var result = deepCopy(result)
  #       result.name = "new" & suffix
  #       result.impl = some initDestroyCall(
  #         parent.get(), result.arguments,
  #         toCppNamespace(parentDecl.get().ident),
  #         conf, cache)

  #       result.returnType = newNimType("ref", @[parent.get()])
  #       result.icpp = initIcpp &"new {toCppNamespace(parentDecl.get().ident)}(@)"
  #       result.noPragmas = gpcNoPragma

  #       result.decl.add result

  #     block newPtrConstructor:
  #       var result = deepCopy(result)
  #       result.name = "cnew" & suffix
  #       result.returnType = newNimType("ptr", @[parent.get()])
  #       result.icpp = initIcpp &"new {toCppNamespace(parentDecl.get().ident)}(@)"
  #       result.decl.add result


  #   else:
  #     conf.warn "Discarding wrappers for conversion function"
  #     conf.dump pr.cursor
  #     conf.dump pr.cursor.getSpellingLocation()

  # else:
  #   let re = pr.cursor.retType()
  #   var returnType = toNimType(re, conf, cache)

  #   if returnType.isComplex.not() and
  #      parentDecl.isSome() and
  #      parent.isSome() and
  #      pr.cursor.retType().
  #      getTypeDeclaration().
  #      inheritsGenParamsOf(parentDecl.get().cursor):

  #     returnType.genParams = parent.get().genParams
  #     # WARNING(refactor)

  #   result.returnType = returnType

  #   if returnType.hasUnexposed():
  #     # WARNING dropping all methods that use `tkUnexposed` type
  #     # in return value. This must be fixed in future versions.
  #     result.canAdd = false

  #   if pr.cursor.cxkind == ckDestructor:
  #     # Explicitly calling destructor on object
  #     result.arguments.add initCArg("self", newNimType("ptr", @[parent.get()]))
  #     result.icpp = initIcpp &"~{result.name}()"
  #     result.name = "destroy" & parent.get().nimName
  #     result.declareForward = true

  #   endProc(result)


proc toCxxObject*(cd: CDecl, conf: WrapConf, cache: var WrapCache): GenObject =
  let tdecl = cd.cursor.cxType().getTypeDeclaration()
  assert cd.kind in {
    cdkClass, cdkStruct, cdkUnion, cdkForward}, $cd.kind

  result = GenObject(
    rawName: $cd.cursor,
    name: conf.typeNameForScoped(cd.ident, cache),
    cdecl: cd
  )

  assert result.name.kind == ctkIdent

  updateAggregateInit(cd, conf, cache, result)

  if cd.kind != cdkForward:
    # Add type declaration for nested types
    for entry in cd.nestedTypes:
      case entry.kind:
        of cdkEnum:
          result.nestedEntries.add wrapEnum(entry, conf, cache)

        of cdkStruct, cdkClass, cdkUnion:
          result.nestedEntries.add wrapObject(entry, conf, cache)


        else:
          discard


    updateFieldExport(cd, conf, cache, result)

    let (procs, extra) = wrapMethods(cd, conf, result.name, cache)
    result.memberMethods.add procs
    result.nestedEntries.add extra


proc toCxxEnum*(declEn: CDecl, conf: WrapConf, cache: var WrapCache): CxxEnum =
  var gen = makeGenEnum(
    declEn,
    declEn.enumFields.sortedByIt(it[1]).deduplicate(isSorted = true),
    conf, cache
  )

  cache.genEnums.add gen
  result.add gen
  gen.auxGen.add makeEnumConverters(gen, conf, cache)
