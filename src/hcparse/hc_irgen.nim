import
  hmisc/core/all,
  hmisc/algo/[hstring_algo]



import std/[with]

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

proc cxxPair*(conf: WrapConf, cxtype: CxType, cache: var WrapCache): CxxNamePair =
  raise newImplementError()
  # cxxPair(cxtype.getTypeName(conf))

proc toCxxUse*(conf: WrapConf, cxtype: CxType, cache: var WrapCache): CxxTypeUse

proc toCxxElaborated*(
    cxtype: CXType, conf: WrapConf, cache: var WrapCache): CxxTypeUse =

  let decl = cxtype.getTypeDeclaration()
  if cxtype.getNumTemplateArguments() > 0:
    result = conf.cxxPair(cxtype, @[])
    case decl.cxKind:
      of ckTypedefDecl, ckTypeAliasDecl, ckTypeAliasTemplateDecl:
        # WARNING `template <J, Q> using` is not handled
        discard
        # result = conf.cxxPair(cxtype, @[]) # newNimType(cxtype.getTypeName(conf), cxtype)

      of ckTypeDeclKinds:
        result = newNimType(cxtype.getTypeName(conf), cxtype)
        let params = cxtype.templateParams()
        for idx, parm in params:
          if parm.cxKind != tkInvalid:
            result.add conf.toCxxUse(parm, cache) # parm.toNimType(conf, cache)

      else:
        conf.warn "Conversion from elaborated type: ", decl
        conf.debug "  ", decl.cxKind(), " in ", decl.getSpellingLocation()

  else:
    result = newNimType(getTypeName(cxtype, conf), cxtype)

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
      pair = cxxPair(($cxtype).dropPrefix("const "), conf.cxxName(cxtype))

    of tkElaborated, tkRecord, tkEnum:
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
      # NOTE this implementation does not work as expected, becuase `const
      # T&` is not a const-qulified type.
      #
      # mutable = not cxType.isConstQualified()
      mutable = not startsWith($cxType, "const")
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
        let
          decl = cxtype.getTypeDeclaration()
          name = cxType.namespacedName(conf)
          typenameParts = toStrPart(@[
            "type-parameter", "typename type-parameter",
            "typename rebind<type-parameter",
            "typename"
          ])


        var res = newNimType(name, cxType)
        if decl.cxKind in ckTypeDeclKinds:
          # HACK list of necessary kinds is determined by trial and error,
          # I'm still not really sure what `tkUnexposed` actually
          # represents.
          for arg in cxType.templateParams():
            res.add toNimType(arg, conf, cache)
            res.genParams[^1].isParam = true

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

proc toCxxOperator*(
    oper: CDecl,
    conf: WrapConf,
    cache: var WrapCache
  ): CxxProc =
  assert conf.isImportcpp

  result = conf.cxxPair(oper).cxxProc()

  let kind = oper.classifyOperator(conf)

  case oper.operatorKind:
    of cxoCopyAsgnOp:
      result.nimName = "setFrom"
      result.icpp = icppInfix("=")
      result.kind = cpkAssignOperator

    of cxoNewOp, cxoDeleteOp:
      discard

    of cxoAsgnOp:
      result.icpp = icppInfix("=")

    of cxoArrayOp:
      let rtype = oper.cursor.retType()
      let nimReturn = rtype.toNimType(conf, cache)
      if nimReturn.isMutable:
        result.nimName = "[]="
        # WARNING potential source of horrible c++ codegen errors
        result.icpp = initIcpp(ipkNextArg, "[", ipkNextARg, "]=", ipkNextArg)

      else:
        result.icpp = initIcpp(ipkNextArg, "[", ipkNextARg, "]")

    of cxoInfixOp:
      result.icpp = initIcpp &"({toCppNamespace(oper.ident)}(#, #))"

      if oper.arguments.len < 2:
        result.addThis = true

    of cxoArrowOp:
      # WARNING
      result.icpp = initIcpp &"(#.operator->(@))"

    of cxoCallOp:
      # NOTE nim does have experimental support for call
      # operator, but I think it is better to wrap this one as
      # separate function `call()`
      with result:
        name = "call"
        kind = pkRegular
        icpp = initIcpp &"#(@)"

    of cxoDerefOp:
      result.name = "[]"
      result.icpp = initIcpp &"(*#)"

    of cxoPrefixOp:
      result.icpp = initIcpp &"({result.name}#)" # FIXME use scoped ident
      if oper.arguments.len == 0:
        result.addThis = true

    of cxoPostfixOp:
      result.icpp = initIcpp &"(#{result.name})" # FIXME use scoped ident

      if oper.arguments.len == 1:
        result.addThis = true

    of cxoCommaOp:
      with result:
        name = "commaOp"
        icpp = initIcpp &"operator,(@)"
        kind = pkRegular

    of cxoConvertOp:
      let restype = oper.cursor.retType().toNimType(conf, cache)

      with result:
        name = "to" & capitalizeAscii(restype.nimName)
        icpp = initIcpp"@"
        returnType = resType
        declType = ptkConverter
        kind = pkRegular



    of cxoUserLitOp:
      let restype = oper.cursor.retType().toNimType(conf, cache)

      with result:
        name = "to" & capitalizeAscii(restype.nimName)
        icpp = initIcpp &"({oper.cursor}(@))"
        returnType = restype
        kind = pkRegular

  result.header = conf.makeHeader(oper.cursor, conf)
