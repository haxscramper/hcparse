import
  std/[
    strutils, sequtils, strformat, tables,
    lenientops, parseutils, bitops, with, sets,
    hashes
  ]

import
  ./hc_types, ./cxtypes, ./cxcommon, ./hc_visitors,
  ./libclang_wrap, ./hc_typeconv

import
  hnimast,
  hnimast/[pprint, idents_types],
  hmisc/macros/iflet,
  hmisc/algo/[htemplates, hseq_distance, namegen],
  hmisc/[helpers, hexceptions],
  hmisc/other/[hlogger, oswrap, hjson],
  hmisc/types/colorstring

import htsparse/cpp/cpp
import fusion/matching except addPrefix


func incl*[I](s1: var OrderedSet[I], other: OrderedSet[I]) =
  for item in other:
    incl(s1, item)

func excl*[I](s1: var OrderedSet[I], other: OrderedSet[I]) =
  for item in other:
    excl(s1, item)

proc wrapEnum*(declEn: CDecl, conf: WrapConf, cache: var WrapCache):
  seq[GenEntry]


proc wrapOperator*(
    oper: CDecl,
    conf: WrapConf,
    cache: var WrapCache
  ): tuple[decl: GenProc, addThis: bool] =

  var it = initGenProc(oper, currLInfo())

  it.name = oper.getNimName(conf)
  # it.genParams = genParams # FIXME: was it necessary to have generic
  # parameters conversion for operators?

  assert conf.isImportcpp
  let kind = oper.classifyOperator(conf)
  it.kind = pkOperator

  case oper.operatorKind:
    of cxoCopyAsgnOp:
      it.name = "setFrom"
      it.icpp = &"(# = #)"
      it.kind = pkRegular
      it.iinfo = currLInfo()

    of cxoNewOp, cxoDeleteOp:
      discard

    of cxoAsgnOp:
      it.icpp = &"(# {it.name} #)"
      it.iinfo = currLInfo()

    of cxoArrayOp:
      let rtype = oper.cursor.retType()
      let nimReturn = rtype.toNimType(conf, cache)
      if nimReturn.isMutable:
        it.name = "[]="
        # WARNING potential source of horrible c++ codegen errors
        it.icpp = &"#[#]= #"

      else:
        it.icpp = &"#[#]"

    of cxoInfixOp:
      it.icpp = &"({toCppNamespace(oper.ident)}(#, #))"
      it.iinfo = currLInfo()

      if oper.arguments.len < 2:
        result.addThis = true

    of cxoArrowOp:
      # WARNING
      it.icpp = &"(#.operator->(@))"

    of cxoCallOp:
      # NOTE nim does have experimental support for call
      # operator, but I think it is better to wrap this one as
      # separate function `call()`
      it.name = "call"
      it.kind = pkRegular
      it.icpp = &"#(@)"
      it.iinfo = currLInfo()

    of cxoDerefOp:
      it.name = "[]"
      it.icpp = &"(*#)"
      it.iinfo = currLInfo()

    of cxoPrefixOp:
      it.icpp = &"({it.name}#)" # FIXME use scoped ident
      it.iinfo = currLInfo()
      if oper.arguments.len == 0:
        result.addThis = true

    of cxoPostfixOp:
      it.icpp = &"(#{it.name})" # FIXME use scoped ident
      it.iinfo = currLInfo()

      if oper.arguments.len == 1:
        result.addThis = true

    of cxoCommaOp:
      it.name = "commaOp"
      it.icpp = &"commaOp(@)"
      it.kind = pkRegular
      it.iinfo = currLInfo()

    of cxoConvertOp:
      let restype = oper.cursor.retType().toNimType(conf, cache)

      with it:
        name = "to" & capitalizeAscii(restype.nimName)
        icpp = &"@"
        returnType = resType
        declType = ptkConverter
        kind = pkRegular


      it.iinfo = currLInfo()

    of cxoUserLitOp:
      let restype = oper.cursor.retType().toNimType(conf, cache)

      with it:
        name = "to" & capitalizeAscii(restype.nimName)
        icpp = &"({oper.cursor}(@))"
        returnType = restype
        kind = pkRegular


      it.iinfo = currLInfo()

  it.header = conf.makeHeader(oper.cursor, conf)
  result.decl = it
  result.addThis = result.addThis or
    (kind in {
      cxoAsgnOp, cxoArrayOp, cxoDerefOp, cxoArrowOp, cxoConvertOp
    })



proc initDestroyCall(
    parent: NimType,
    args: seq[CArg],
    constructorCall: string,
    conf: WrapConf, cache: var WrapCache
  ): PNode =

  let
    className = parent.nimName
    argType = newNType("ref", [parent.toNType(conf, cache)]).toNNode()
    destroyCall = newPIdent(
      "destroy" & className.fixIdentName().capitalizeAscii())
    argMixin = args.mapIt("(`" & it.name & "`)").join(", ")
    emitStr = &"new ((void*)result) {constructorCall}({argMixin}); " &
      "/* Placement new */"


  return pquote do:
    newImportAux() # FIXME This procedure is a necessary hack that provides
                   # requries `<new>` header import. Maybe this could be
                   # fixed by somehow grenerating required `header:` pragma
                   # statements?
    new(result, proc(self: `argType`) = `destroyCall`(addr self[]))
    {.emit: `emitStr`.}


proc wrapProcedure*(
    pr: CDecl,
    conf: WrapConf,
    parent: Option[NimType],
    cache: var WrapCache,
    parentDecl: Option[CDecl]
  ): tuple[decl: seq[GenEntry], canAdd: bool] =
  ## Generate wrapped entry for procedure, method, operator, or function
  ## declaration
  ##
  ## - @arg{pr} :: Procedure declaration
  ## - @arg{parent} :: Names of parent type declaration if present.
  ##   Parent class for methods and operator overloads.
  ## - @arg{parent} :: Nim name of the parent type
  ## - @arg{parentDecl} :: Optional parent declaration
  ## - @arg{asNewConstructor} :: Of `pr` is a declaration for constructor
  ##   generate `new` or `init` procedure.

  var it = initGenProc(pr, currLInfo())

  template endProc(it: GenProc): untyped =
    let generated = newProcVisit(it, conf, cache)
    result.decl.add it
    result.decl.add GenPass(iinfo: currLInfo(), passEntries: generated)


  var addThis = (
    pr.kind == cdkMethod and
    pr.cursor.cxKind notin {
      ckConstructor, ckDestructor, ckConversionFunction })

  it.genParams = conf.genParamsForIdent(pr.ident, cache)

  var opKind: CxOperatorKind
  result.canAdd = true
  if pr.isOperator:
    # HACK temporary workaround for `new` and `delete` operator handing
    var opKind = classifyOperator(pr, conf)
    if opKind notin {cxoNewOp, cxoDeleteOp}:
      let (decl, adt) = pr.wrapOperator(conf, cache)
      it = decl
      addThis = adt

    else:
      addThis = false
      result.canAdd = false

  else:
    it.name = pr.getNimName(conf)

    let icppName = toCppNamespace(pr.ident)
    if parent.isSome():
      assert conf.isImportcpp,
        "Cannot wrap methods for non-cxx targets"

      if pr.cursor.isStatic():
        addThis = false
        it.iinfo = currLInfo()
        it.icpp = &"({icppName}(@))"

      else:
        it.iinfo = currLInfo()
        it.icpp = &"(#.{pr.getNimName(conf)}(@))"

      it.header = conf.makeHeader(pr.cursor, conf)

    else:
      if conf.isImportcpp:
        it.iinfo = currLinfo()
        it.icpp = &"({icppName}(@))"

      else:
        it.iinfo = currLInfo()
        it.icpp = &"{icppName}"

      it.header = conf.makeHeader(pr.cursor, conf)


  if $it.cdecl.cursor == "operator=":
    # FIXME check if first argument and parent declaration types are
    # identical. Right now it does not work because
    # `b3Matrix3x3` != `const b3Matrix3x3 &`
    if parentDecl.get().cursor.cxType() == pr.arguments[0].cursor.cxType():
      # By default `operator=` is converted to regular `setFrom` proc to
      # correctly handle multiple overloads (in C++ `operator=` can have
      # different types on RHS and LHS, but this is not the case in nim).
      # *if* assignmed is indeed done from two identical types, then it can
      # be wrapped as actual `=` proc.
      it.name = "="
      it.kind = pkOperator

    else:
      # Otherwise add `self` for wrapped proc
      addThis = parent.isSome()

  if addThis:
    assert parent.isSome()
    it.arguments.add initCArg(
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


    if not (opKind == cxoPostfixOp and argIdx > 0):
      var newArg = initCArg(arg.name, argType)
      setDefaultForArg(newArg, arg.cursor, conf, cache)
      it.arguments.add newArg

  if pr.cursor.isVariadic() == 1:
    it.pragma.add newPIdent("varargs")

  if pr.isOperator and pr.classifyOperator(conf) == cxoAsgnOp:
    # HACK Force override return type for assignment operators
    it.returnType = newNimType("void")
    endProc(it)

  elif pr.cursor.kind in {ckConstructor, ckConversionFunction}:
    # Override handling of return types for constructors
    if not pr.isOperator:
      # But ignore implicit user-defined conversion functions like
      # `operator T()`

      it.header = conf.makeHeader(pr.cursor, conf)
      let suffix = parent.get().nimName.capitalizeAscii()

      block initConstructor:
        var it = deepCopy(it)
        it.name = "init" & suffix
        it.iinfo = currLInfo()
        it.returnType = parent.get()
        it.icpp = &"{toCppNamespace(parentDecl.get().ident)}(@)"
        result.decl.add it

      block newRefConstructor:
        var it = deepCopy(it)
        it.name = "new" & suffix
        it.iinfo = currLInfo()
        it.impl = some initDestroyCall(
          parent.get(), it.arguments,
          toCppNamespace(parentDecl.get().ident),
          conf, cache)

        it.returnType = newNimType("ref", @[parent.get()])
        it.icpp = &"new {toCppNamespace(parentDecl.get().ident)}(@)"
        it.noPragmas = gpcNoPragma

        result.decl.add it

      block newPtrConstructor:
        var it = deepCopy(it)
        it.name = "cnew" & suffix
        it.iinfo = currLInfo()
        it.returnType = newNimType("ptr", @[parent.get()])
        it.icpp = &"new {toCppNamespace(parentDecl.get().ident)}(@)"
        result.decl.add it


    else:
      conf.warn "Discarding wrappers for conversion function"
      conf.dump pr.cursor
      conf.dump pr.cursor.getSpellingLocation()

  else:
    let re = pr.cursor.retType()
    var returnType = toNimType(re, conf, cache)

    if returnType.isComplex.not() and
       parentDecl.isSome() and
       parent.isSome() and
       pr.cursor.retType().
       getTypeDeclaration().
       inheritsGenParamsOf(parentDecl.get().cursor):

      returnType.genParams = parent.get().genParams
      # WARNING(refactor)

    it.returnType = returnType

    if returnType.hasUnexposed():
      # WARNING dropping all methods that use `tkUnexposed` type
      # in return value. This must be fixed in future versions.
      result.canAdd = false

    if pr.cursor.cxkind == ckDestructor:
      # Explicitly calling destructor on object
      it.arguments.add initCArg("self", newNimType("ptr", @[parent.get()]))
      it.icpp = &"~{it.name}()"
      it.name = "destroy" & parent.get().nimName
      it.declareForward = true

    endProc(it)

proc wrapMethods*(
    cd: CDecl,
    conf: WrapConf,
    parent: NimType,
    cache: var WrapCache
  ): tuple[methods: seq[GenProc], extra: seq[GenEntry]] =

  ## - @arg{cd} :: Class declaration to wrap methods for
  ## - @arg{parent} :: Nim name of class declaration

  assert cd.kind in {cdkClass, cdkStruct, cdkUnion},
     $cd.kind & $cd.cursor.getSpellingLocation()

  var
    hasConstructor = false
    hasDestructor = false

  for meth in cd.members:
    if meth.kind != cdkMethod: continue

    let (decl, canAdd) = wrapProcedure(
      meth, conf, some parent, cache, some cd)

    if meth.cursor.kind in { ckConstructor, ckConversionFunction }:
      hasConstructor = true

    elif meth.cursor.kind in { ckDestructor }:
      hasDestructor = true

    if canAdd:

      for d in decl:
        if d.kind == gekProc:
          result.methods.add d.genProc

        else:
          result.extra.add d

  if (parent.nimName notin cache.generatedConstructors):
    cache.generatedConstructors.incl parent.nimName
    var returnType = parent
    returnType.genParams = conf.genParamsForIdent(cd.ident, cache)
    let className = cd.ident.toCppNamespace()

    if not hasDestructor:
      result.extra.add GenProc(
        name: "destroy" & parent.nimName.capitalizeAscii(),
        arguments: @[initCArg("obj", newNimType("ptr", @[returnType]))],
        cdecl: cd,
        iinfo: currLInfo(),
        icpp: &"#.~{className}()",
        header: conf.makeHeader(cd.cursor, conf),
        declareForward: true)

    if (not hasConstructor):
      result.extra.add GenProc(
        name: "cnew" & parent.nimName.capitalizeAscii(),
        returnType: newNimType("ptr", @[returnType]),
        icpp: &"new {className}()",
        cdecl: cd,
        iinfo: currLInfo(),
        header: conf.makeHeader(cd.cursor, conf))

      result.extra.add GenProc(
        name: "new" & parent.nimName.capitalizeAscii(),
        returnType: newNimType("ref", @[returnType]),
        cdecl: cd,
        noPragmas: gpcNoPragma,
        iinfo: currLInfo(),
        impl: some initDestroyCall(
          parent, @[], className, conf, cache))

      result.extra.add GenProc(
        name: "init" & parent.nimName.capitalizeAscii(),
        returnType: returnType,
        cdecl: cd,
        icpp: "{className}()",
        header: conf.makeHeader(cd.cursor, conf),
        iinfo: currLInfo())


proc wrapFunction*(cd: CDecl, conf: WrapConf, cache: var WrapCache):
  seq[GenEntry] =

  var (decl, canAdd) = wrapProcedure(
    cd, conf, none NimType, cache, none CDecl)

  if canAdd:
    result = decl


proc wrapTypeFromNamespace(
    ident: CScopedIdent, conf: WrapConf,
    cache: var WrapCache, cursor: CXCursor
  ): PObjectDecl =
  ## `cursor` points to type declaration being wrapped
  # WARNING for now I assume that 'UNEXPOSED' type only occurs in
  # situations like `std::move_iterator<'0>::pointer` where typedef
  # uses it's semantic parent (class or struct declaration) to get
  # template parameters. This might not be a valid assumption in
  # general case.k

  result = PObjectDecl(
    name: conf.typeNameForScoped(ident, cache).toNType(conf, cache),
    exported: true
  )

  result.pragma = some(newPPragma(
    newExprColonExpr(
      newPIdent(conf.importX()),
      ident.toCppNamespace().newRStrLit()
    ),
    newExprColonExpr(
      newPIdent "header",
      conf.makeHeader(cursor, conf).toNNode())))




proc wrapObject*(cd: CDecl, conf: WrapConf, cache: var WrapCache): GenObject

proc wrapAlias*(
    al: CDecl, parent: CScopedIdent, conf: WrapConf, cache: var WrapCache
  ): seq[GenEntry] =
  # NOTE returning multiple values because of
  # `typedef struct A {} A, *APtr` shit that can result in multple
  # declarations, nested types (that might recursively contain who-knows-what)


  if al.isNewType:
    # typdef with new type declaration
    var baseType: NimType

    if al.aliasNewType.kind in {cdkClass, cdkStruct}:
      let wrapBase = al.aliasNewType.wrapObject(conf, cache)
      result.add wrapBase

      baseType = wrapBase.name

    else:
      var wrapBase = al.aliasNewType.wrapEnum(conf, cache)
      baseType = newNimType(wrapBase[0].genEnum.name)
      conf.debug al.aliasNewType.cursor.treeRepr()
      result.add wrapBase

    for newName in al.newTypes:
      var newType = newNimType($newName)
      newType.genParams = baseType.genParams
      if newType.nimName != baseType.nimName:
        # Alias names might be the same for `typedef struct St {} St;`
        result.add GenAlias(
          iinfo: currLInfo(),
          cdecl: al,
          baseType: baseType,
          newAlias: newType)

  else:
    # Get underlying type for alias
    let aliasof = al.cursor.cxType().getCanonicalType()
    conf.logger.indented:
      # Create new identifier for aliased type
      var newAlias = conf.typeNameForScoped(al.ident, cache)

    # Identifier for old aliased type
    var baseType: NimType
    if getTypeDeclaration(aliasof).cxKind() == ckNodeclFound:
      let name = fromCxxTypeName($aliasof)
      if name.len > 0:
        baseType = newNimType(name, aliasof)

      else:
        baseType = toNimType(aliasof, conf, cache) # .ntype # newPType($aliasof)

    else:
      baseType = conf.typeNameForScoped(conf.fullScopedIdent(aliasof), cache)
      # WARNING mismatched generic parameters between lhs and rhs parts of
      # alias might result in broken wrappers.

    var maxIdx = 0
    for idx, param in al.cursor.cxType().templateParams():
      baseType.genParams[idx] = param.toNimType(conf, cache)
      maxIdx = idx

    for idx in (maxIdx + 1) ..< baseType.genParams.len:
      newAlias.genParams.add baseType.genParams[idx]

    # QUESTION is it necessary?
    # baseType.genParams.add baseType.getPartialParams(conf, cache, defaulted = true)

    fixTypeParams(baseType, newAlias.genParams)

    # NOTE ignore `typedef struct` in C
    result.add GenAlias(
      iinfo: currLInfo(),
      isDistinct: conf.isDistinct(al.ident, conf, cache),
      newAlias: newAlias,
      baseType: baseType,
      cdecl: al)

proc getParentFields*(
    inCursor: CXCursor, obj: PObjectDecl,
    conf: WrapConf, cache: var WrapCache
  ): seq[PProcDecl] = # FIXME return `GenProc` declarations instead of nested cursors

  for class in inCUrsor.getClassBaseCursors():
    for entry in class:
      if entry.kind in {ckFieldDecl}:
        let
          fieldType = entry.cxType().toNimType(conf, cache)
          fldname = $entry

        result.add newPProcDecl(
          name = fldName,
          returnType = some(fieldType.toNType(conf, cache)),
          args = { "self" : obj.name },
          iinfo = currLInfo(),
          pragma = newPPragma(newExprColonExpr(
            newPIdent(conf.importX()), newRStrLit(&"(#.{fldName})")))
        )

        result[^1].genParams.add(obj.name.genParams)

        result[^1].addCodeComment(
          &"Parent field getter passtrough from {class}\n")

        if not entry.cxType().isConstQualifiedDeep():
          # FIXME replace with `GProc` declaration
          result.add newPProcDecl(
            name = fldName,
            iinfo = currLInfo(),
            args = { "self" : obj.name, "val" : fieldType.toNType(
              conf, cache) },
            pragma = newPPragma(newExprColonExpr(
              newPIdent(conf.importX()), newRStrLit(&"(#.{fldName} = @)")))
          )

          result[^1].genParams.add(obj.name.genParams)
          result[^1].signature.arguments[0].kind = nvdVar
          result[^1].addCodeComment(
            &"Parent field assignment passtrough from {class}\n")

          result[^1].kind = pkAssgn



proc getDefaultAccess*(cursor: CXCursor): CXAccessSpecifier =
  case cursor.cxKind():
    of ckClassDecl, ckClassTemplate:
      asPrivate

    of ckStructDecl:
      asPublic

    else:
      raiseAssert("Cannot get default visibility for cursor of kind " & $cursor.cxKind())


proc publicFields*(cd: CDecl): seq[CDecl] =
  assert cd.kind in {cdkClass, cdkStruct, cdkUnion}
  for member in cd.members:
    if (member.kind == cdkField) and (member.access == asPublic):
      result.add member

    elif member.kind in {cdkClass, cdkUnion, cdkStruct} and
         member.isAnonymous:
      # Anonymous nested struct, export all fields
      result.add member.publicFields()


proc updateAggregateInit*(
    cd: CDecl, conf: WrapConf, cache: var WrapCache, gen: var GenObject) =

  if conf.isImportcpp and # QUESTION how to handle aggregate initalization
                          # for C structures? Just declare `{.emit.}`` proc
                          # (with or without designated initalizers)
     cd.isAggregateInit and cd.initArgs.len > 0:

    let pr = initGenProc(cd, currLInfo()).withIt do:
      it.name = "init" & gen.name.nimName
      it.arguments = cd.initArgs
      it.header = conf.makeHeader(cd.cursor, conf)
      it.icpp = &"{toCppNamespace(cd.ident)}({{@}})"
      it.returnType = gen.name

    gen.nestedEntries.add pr


proc updateFieldExport*(
  cd: CDecl, conf: WrapConf, cache: var WrapCache, gen: var GenObject) =
  ## Add getter/setter methods for *all* public fields that are accessible
  ## from this object
  for fld in cd.publicFields():
    var res = GenField(
      # QUESTION `conf.identNameForScoped()?`
      name: fld.lastName(conf),
      rawName: fld.lastName(conf),
      iinfo: currLInfo(),
      cdecl: fld,
      fieldType: fld.cursor.cxType().toNimType(conf, cache),
      isConst: fld.isConst)

    if fld.fieldTypeDecl.getSome(newFieldType):
      var newType = newFieldType
      if newType.isAnonymous:
        newType.ident[^1] = toCName("Anon")

      case newType.kind:
        of cdkStruct, cdkUnion, cdkClass:
          var decl = wrapObject(newType, conf, cache)

          gen.nestedEntries.add decl
          res.fieldType.nimName = decl.name.nimName

        of cdkEnum:
          var decl = wrapEnum(newType, conf, cache)

          res.fieldType.nimName = decl[0].genEnum.name
          gen.nestedEntries.add decl

        else:
          discard

    if fld.cursor.cxType().isEnum():
      res.fieldType.nimName &= conf.rawSuffix()

    gen.memberFields.add res


proc wrapObject*(cd: CDecl, conf: WrapConf, cache: var WrapCache): GenObject =
  let tdecl = cd.cursor.cxType().getTypeDeclaration()
  assert cd.kind in {
    cdkClass, cdkStruct, cdkUnion, cdkForward}, $cd.kind

  result = GenObject(
    rawName: $cd.cursor,
    iinfo: currLInfo(),
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


proc cEnumName(
    str: string, nt: NimType, conf: WrapConf, cache: var WrapCache): string =
  ## Generate name for C enum. QUESTION: enum names don't need to be
  ## perfectly accurate as they are converted to integers and then casted -
  ## I'm not completely sure if this correct.
  result = nt.nimName
  result[0] = result[0].toLowerAscii()
  result &= "_" & str
  result = result.strip(chars = {'_'})
  var resLen = result.len()
  while true:
    result = result.replace("__", "_")
    if result.len != resLen:
      resLen = result.len

    else:
      break

  result = cache.nameCache.getName(result)

proc renameField(
    fld: string, pref, enumPref: string, cache: var WrapCache): string =
  # Drop common prefix for enum declaration and add one generated from
  # enum name
  cache.nameCache.newName(
    fld.
      dropPrefix(pref).
      dropPrefix("_").
      toSnakeCamelCase().
      capitalizeAscii().
      addPrefix(enumPref)
  )

proc makeGenEnum*(
    declEn: CDecl, flds: seq[(CXCursor, BiggestInt)],
    conf: WrapConf, cache: var WrapCache
  ): GenEnum =

  var nt = conf.typeNameForScoped(declEn.ident, cache)
  result = GenEnum(
    isMacroEnum: false,
    cdecl: declEn,
    iinfo: currLInfo(),
    rawName: nt.nimName & conf.rawSuffix(),
    name: nt.nimName
  )

  # Nim proxy proc declaration.
  # Determine common prefix for C++ enum (if any)
  let pref = declEn.enumFields.mapIt($it.field).commonPrefix()

  # Get name of the enum type itsel by combining first letters of
  # `PascalCase` or `snake_case` name.
  let enumPref = conf.prefixForEnum(declEn.ident, conf, cache)

  var prev = BiggestInt(-120948783)
  for (name, val) in flds:
    if val != prev:
      prev = val
      result.values.add GenEnumValue(
        cdecl: CDecl(cursor: name, kind: cdkField, ident: @[]),
        docComment: @[conf.docCommentFor(declEn.ident & toCName(name))],
        iinfo: currLInfo(),
        baseName: $name,
        resCName: cEnumName($name, nt, conf, cache),
        resNimName: renameField($name, pref, enumPref, cache),
        resVal: val,
        stringif: toCppNamespace(declEn.ident) & "::" & $name
      )


proc makeEnumConverters(gen: GenEnum, conf: WrapConf, cache: var WrapCache):
  GenPass =

  # Metadata associated with proxy enum
  var arr = nnkBracket.newPTree()
  var strCase = nnkCaseStmt.newPtree(newPIdent "en")
  var convertImpl = nnkCaseStmt.newPTree(newPIdent "en")

  for value in gen.values:
    convertImpl.add nnkOfBranch.newPTree(
      newPIdent(value.resCName),
      newPIdent(value.resNimName)
    )

    strCase.add nnkOfBranch.newPTree(
      newPIdent(value.resCName),
      nnkAsgn.newPTree(newPIdent("result"), newPLit(value.stringif))
    )

    arr.add pquote do:
      (
        name: `newPLit(value.baseName)`, # Name of the original enum
        cEnum: `newPIdent(value.resCName)`, # Original enum value
        cName: `newPLit(value.stringif)`, # Namespaced C++ enum name
        value: cint(`newPLit(value.resVal)`) # Integer value for field
      )

  let
    enName = newPIdent(gen.name)
    arrName = newPIdent("arr" & gen.name & "mapping")
    reverseConvName = newPident("to" & gen.rawName)
    convName = newPIdent("to" & gen.name)
    implName = newPIdent(gen.rawName)


  let helpers = pquote do:
    const `arrName`: array[`enName`, tuple[
      name: string,
      cEnum: `implName`,
      cName: string,
      value: cint
    ]] = `arr`

    proc toCInt*(en: `enName`): cint {.inline.} =
      ## Convert proxy enum to integer value
      `arrName`[en].value

    proc toCInt*(en: set[`enName`]): cint {.inline.} =
      ## Convert set of enums to bitmasked integer
      for val in en:
        result = bitor(result, `arrName`[val].value)

    proc `$`*(en: `implName`): string {.inline.} =
      ## Return namespaced name of the original enum
      `strCase`

    func `convName`*(en: `implName`): `enName` {.inline.} =
      `convertImpl`

    converter `reverseConvName`*(en: `enName`): `implName` {.inline.} =
      `arrName`[en].cEnum

  return GenPass(
    iinfo: currLInfo(),
    passEntries: @[newWrappedEntry(
      toNimDecl(helpers), wepInProcs, currLInfo(), gen.cdecl)])



proc wrapEnum*(
    declEn: CDecl, conf: WrapConf, cache: var WrapCache): seq[GenEntry] =
  ## Generate wrapper for enum declaration
  ##
  ## Generates wrapper for enum declaration, using wrap configuration.
  ## Wrapping is performed in two steps - underlying C enum is wrapped
  ## as-is, and additional nim `enum` is generated. Nim version does not
  ## have holes, which allows it to be used in `array`, iterated upon etc.
  ##
  ## In order to perform conversion between 'proxy' and underlying enum
  ## several helper procs are introduces, such as `toInt`.
  var gen = makeGenEnum(
    declEn,
    declEn.enumFields.sortedByIt(it[1]).deduplicate(isSorted = true),
    conf, cache
  )

  cache.genEnums.add gen
  result.add gen
  gen.auxGen.add makeEnumConverters(gen, conf, cache)

proc evalTokensInt(strs: seq[string]): Option[int64] =
  let str = strs.join(" ")

  proc aux(node: CppNode): Option[int64] =
    case node.kind:
      of cppNumberLiteral:
        try:
          return some parseBiggestInt(str[node.slice()])

        except ValueError as e:
          # err e.msg
          discard

      of cppBinaryExpression:
        if (Some(@lhs), Some(@rhs)) ?= (aux(node[0]), aux(node[1])):
          var invert = (node[0].slice().b + 2) .. (node[1].slice().a - 2)
          let op = strutils.strip(str[invert])
          result = case op:
            of "<<": some lhs shl rhs
            of ">>": some lhs shr rhs
            of "+": some lhs + rhs
            of "-": some lhs - rhs
            of "/": some lhs div rhs
            of "*": some lhs * rhs
            else: raise newImplementError(
              "Operator is not implemented for Cxx AST evaluation: ", op)

      of cppTranslationUnit, cppSyntaxError, cppExpressionStatement,
         cppParenthesizedExpression:
        return aux(node[0])

      of cppStringLiteral, cppCastExpression:
        discard

      else:
        raise newImplementError(
          "Cannot evaluate integer expression from tree:\n",
          node.treeRepr(str)
        )



  result = aux(parseCppString(str))

func capitalAscii*(str: string): string =
  toLowerAscii(str).capitalizeAscii()

func capitalAscii*(strs: seq[string]): seq[string] =
  for str in strs:
    result.add toLowerAscii(str).capitalizeAscii()

proc wrapMacroEnum*(
  values: seq[CDecl], conf: WrapConf, cache: var WrapCache): seq[GenEntry] =

  let prefix = commonPrefix(mapIt(values, $it.cursor)).dropSuffix("_")
  let enumPref = conf.prefixForEnum(@[toCName(prefix)], conf, cache)
  var enumFields: seq[GenEnumValue]
  for val in values:
    assert val.kind == cdkMacro
    let toks = val.cursor.tokenStrings(conf.unit)
    # FIXME range breaks on `#define func(arg)`
    let idx = 1
    let value = evalTokensInt(toks[idx ..^ 1])
    let name = enumPref & toks[0].splitCamel()[1..^1].capitalAscii().join("")

    if value.isSome():
      enumFields.add GenEnumValue(
        cdecl: val,
        iinfo: currLInfo(),
        resNimName: name,
        resVal: value.get(),
        stringif: toks[0]
      )

  if enumFields.len > 0:
    let name = capitalAscii(enumPref) & capitalAscii(prefix)
    var en = GenEnum(
      isMacroEnum: true,
      name: name,
      proxyName: name,
      iinfo: currLInfo(),
      cdecl: nil,
      values: enumFields.sortedByIt(it.resVal)
    )

    result.add en
    let enName = newPIdent(en.name)

    var helpers = pquote do:
      proc toCInt*(en: `enName`): cint {.inline.} =
        ## Convert proxy enum to integer value
        cint(en.int)

      proc toCInt*(en: set[`enName`]): cint {.inline.} =
        ## Convert set of enums to bitmasked integer
        for val in en:
          result = bitor(result, val.cint)

    result.add GenPass(
      iinfo: currLInfo(),
      passEntries: @[
        newWrappedEntry(toNimDecl(helpers), wepInProcs, currLInfo())])



proc wrapMacros*(
  declMacros: seq[CDecl], conf: WrapConf, cache: var WrapCache): seq[GenEntry] =
  var prefix: seq[string]
  var buf: seq[CDecl]
  var lastSplit: seq[string]
  conf.logger.indented:
    for decl in declMacros:
      # This implementation ties to find macro names with common prefixes,
      # group them together.
      let split = split($decl.cursor, "_")
      let pref = commonPrefix(@[lastSplit, split])
      # If has any common prefix with last split, add it
      if pref != prefix and lastSplit.len > 0:
        buf = @[]

      if prefix.len == 0 or pref == prefix:
        if pref.len > 0:
          buf.add decl
          prefix = pref

      elif prefix.len > 0 and pref != prefix:
        prefix = pref
        if buf.len > 1:
          result.add wrapMacroEnum(buf, conf, cache)
          buf = @[]

      lastSplit = split

    if buf.len > 1:
      try:
        result.add wrapMacroEnum(buf, conf, cache)

      except ImplementError as e:
        conf.err "Cannot wrap macro collection to enum"
        conf.debug e.msg



proc wrapApiUnit*(
    api: CApiUnit, conf: WrapConf,
    cache: var WrapCache, index: hc_types.FileIndex
  ): seq[GenEntry] =
  ## Generate wrapper for api unit.
  var macrolist: seq[CDecl]
  for decl in api.decls:
    if cache.canWrap(decl.cursor):
      cache.markWrap(decl.cursor)

    else:
      continue

    case decl.kind:
      of cdkClass, cdkStruct, cdkUnion:
        conf.logger.thisScope("Class wrapping")

        let spec = decl.cursor.getSpecializedCursorTemplate()

        if spec.cxKind() != ckFirstInvalid:
          discard

        else:
          result.add decl.wrapObject(conf, cache)


      of cdkAlias:
        conf.logger.indented:
          result.add decl.wrapAlias(decl.ident, conf, cache)

      of cdkEnum:
        result.add decl.wrapEnum(conf, cache)

      of cdkFunction:
        for f in decl.wrapFunction(conf, cache):
          result.add f

      of cdkMacro:
        macrolist.add decl

      of cdkMethod, cdkField:
        discard

      of cdkForward:
        result.add GenForward(cdecl: decl, iinfo: currLInfo())


  result.add wrapMacros(macrolist, conf, cache)

proc getNType*(carg: CArg): NimType =
  if carg.isRaw:
    raiseAssert("#[ IMPLEMENT ]#")

  else:
    return carg.nimType

proc toNNode*(gen: GenEntry, conf: WrapConf, cache: var WrapCache): seq[WrappedEntry]

proc toNNode*(
  gen: GenProc, conf: WrapConf, cache: var WrapCache): PProcDecl =

  var parentClass: seq[CxCursor]
  for it in items(gen.cdecl.ident):
    if it.cursor.kind in { ckClassTemplate }:
      parentClass.add it.cursor

  result = newPProcDecl(
    name = gen.name.fixIdentName(),
    iinfo = gen.iinfo,
    exported = true,
    returnType = some(
      gen.returnType.toNType(
        conf, cache, asResult = true, noDefaulted = parentClass)),
    declType = gen.declType,
    kind = gen.kind
  )

  var allParams: OrderedSet[string]

  for arg in gen.arguments:
    allParams.incl arg.nimType.
      allGenericParams().mapIt(it.toNType(conf, cache).head).toOrderedSet()

    result.signature.arguments.add newNIdentDefs(
      vname = arg.name.fixIdentName(),
      value = arg.default,
      vtype = arg.getNType().toNType(
        conf, cache, noDefaulted = parentClass),
      kind = arg.varkind)

  for param in conf.genParamsForIdent(gen.cdecl.ident, cache):
    result.genParams.add toNType(param, conf, cache)

  allParams.excl result.genParams.mapIt(it.head).toOrderedSet()

  for param in allParams:
    result.genParams.add newPType(param)

  result.docComment = gen.docComment.join("\n")
  result.signature.pragma = gen.pragma

  if gpcNoImportcpp notin gen.noPragmas:
    result.signature.pragma.add(
      newExprColonExpr(
        newPIdent(conf.importX()),
        newRStrLit(gen.icpp)))

  # If procedure returns mutable lvalue reference ot the same type as
  # passed for the first argument we make it discardable. Most likely
  # original function was returning lvalue ref in order to allow for method
  # chaining. For example `std::string.append()` returns `std::string&` and
  # documentation says it has return value of `*this`
  if notNil(gen.returnType) and
     gen.returnType.specialKind == ctskLValueRef and
     gen.arguments.len > 0 and
     sameNoTy(
       result.argumentType(0),
       result.returnType().get(),
       noParams = true):

    result.signature.pragma.add(newPident("discardable"))


  if gpcNoHeader notin gen.noPragmas:
    result.signature.pragma.add(
      newExprColonExpr(newPIdent "header", gen.header.toNNode()))

  if gen.impl.isSome():
    result.impl = gen.impl.get()

proc toNNode*(
    gen: GenEnum, conf: WrapConf, cache: var WrapCache): seq[WrappedEntry] =
  if gen.isMacroEnum:
    var rawEnum = newPEnumDecl(gen.proxyName, iinfo = currLInfo())
    rawEnum.exported = true

    for value in gen.values:
      rawEnum.addField(
        value.resNimName.fixIdentName(),
        some newPLit(value.resVal),
        docComment = conf.docCommentFor(value.cdecl.ident))

    result.add newWrappedEntry(
      rawEnum.toNimDecl(), wepInTypes, currLInfo())

  else:
    block:
      var rawEnum = newPEnumDecl(gen.rawName, iinfo = currLInfo())
      rawEnum.addDocComment gen.docComment.join("\n")
      rawEnum.exported = true

      let importName =
        if conf.isImportcpp or gen.isCTypedef:
          toCppNamespace(gen.cdecl.ident)

        else:
          "enum " & toCppNamespace(gen.cdecl.ident)

      rawEnum.pragma.add newPIdentColonString(conf.importX(), importName)

      rawEnum.pragma.add nnkExprColonExpr.newPTree(
        newPIdent("header"),
        conf.makeHeader(gen.cdecl.cursor, conf).toNNode()
      )

      rawEnum.exported = true

      for value in gen.values:
        rawEnum.addField(
          value.resCName.fixIdentName(), some newPLit(value.resVal))

      result.add newWrappedEntry(
        rawEnum.toNimDecl(), wepInTypes, currLInfo(), gen.cdecl)

    block:
      var nimEnum = newPEnumDecl(gen.name, iinfo = currLInfo())
      nimEnum.addDocComment gen.docComment.join("\n")
      nimEnum.exported = true
      for value in gen.values:
        nimEnum.addField(
          value.resNimName, docComment = value.docComment.join("\n"))

      result.add newWrappedEntry(
        nimEnum.toNimDecl(), wepInTypes, currLInfo())

  for aux in gen.auxGen:
    result.add toNNode(aux, conf, cache)

proc addProcDecl*(
    wrapped: var seq[WrappedEntry],
    gen: GenProc,
    conf: WrapConf,
    cache: var WrapCache
  ) =

  if gen.impl.isNone():
    wrapped.add toNNode(gen, conf, cache).
      toNimDecl().
      newWrappedEntry(
        if gen.declareForward: wepAfterTypesBeforeProcs else: wepInProcs,
        gen.iinfo, gen.cdecl)

  else:
    wrapped.add toNNode(gen, conf, cache).toNimDecl().newWrappedEntry(
      wepInProcs, gen.iinfo, gen.cdecl)

    if gen.declareForward:
      wrapped.add toNNode(gen, conf, cache).
        copyForward().
        toNimDecl().
        newWrappedEntry(wepAfterTypesBeforeProcs, currLInfo(), gen.cdecl)


proc toNNode*(
    gen: GenObject, conf: WrapConf; cache: var WrapCache
  ): seq[WrappedEntry] =
  var decl = newPObjectDecl(
    gen.name.nimName, iinfo = currLInfo())

  let scoped = conf.typeNameForScoped(
    gen.cdecl.ident, cache).toNType(conf, cache)

  decl.name = scoped

  assert decl.name.kind == ntkIdent, $decl.name.kind
  assert gen.cdecl.kind in {cdkStruct, cdkUnion, cdkClass},
      $gen.cdecl.ident & " " & $gen.cdecl.kind

  decl.pragma = some newPPragma(
    newPIdent("bycopy"),
    nnkExprColonExpr.newPTree(
      newPIdent(conf.importX()),
      newPLit(gen.cdecl.icpp)),
    nnkExprColonExpr.newPTree(
      newPIdent("header"),
      conf.makeHeader(gen.cdecl.cursor, conf).toNNode()
    )
  )

  if gen.cdecl.kind == cdkUnion:
    decl.pragma.get().add newPIdent("union")

  for field in gen.memberFields:
    if field.isConst:
      block getterImplementation:
        var getImpl = newPProcDecl(field.name)
        with getImpl:
          returnType = field.fieldType.toNType(conf, cache)
          iinfo = currLInfo()
          genParams = gen.name.genParams.mapIt(it.toNType(conf, cache))
          pragma = newPPragma(
            newPIdent("noinit"),
            # newPIdentColonString(conf.importX(), &"#.{field.rawName}"),
            newExprColonExpr(
              newPident("header"),
              conf.makeHeader(field.cdecl.cursor, conf).toNNode()
            )
          )

          impl = toNNode(newPPRagma(
            newPIdentColonString("emit", &"return `self`.{field.rawName};")
          ))

        getImpl.addArgument("self", gen.name.toNType(conf, cache))

        result.add newWrappedEntry(
          toNimDecl(getImpl), wepInProcs, currLInfo(), field.cdecl)

      block setterImplementation:
        var setImpl = newPProcDecl(field.name, kind = pkAssgn)
        with setImpl:
          iinfo = currLInfo()
          genParams = gen.name.genParams.mapIt(it.toNType(conf, cache))
          pragma = newPPragma(
            newPIdentColonString(
              "error",
              &"Cannot assign to field '{field.name}' - declared `const` in '{field.cdecl.ident}'"
            )
          )

        setImpl.addArgument("self", gen.name.toNtype(conf, cache))
        setImpl.addArgument("value", field.fieldType.toNType(conf, cache))

        result.add newWrappedEntry(
          toNimDecl(setImpl), wepInProcs, currLInfo(), field.cdecl)

    else:
      var newField = PObjectField(
        docComment: field.docComment.join("\n"),
        isTuple: false,
        isExported: true,
        name: field.name,
        pragma: some newPPragma(
          newPIdentColonString(
            conf.importX, field.cdecl.lastName(conf))),
        fldType: field.fieldType.toNType(conf, cache)
      )

      updateComments(newField, field.cdecl, conf, cache)

      decl.flds.add newField

  for nested in gen.nestedEntries:
    result.add nested.toNNode(conf, cache)

  var obj = newWrappedEntry(
    toNimDecl(decl), wepInTypes, gen.iinfo, gen.cdecl)

  result.add obj

  for meth in gen.memberMethods:
    result.addProcDecl(meth, conf, cache)


proc toNNode*(
    gen: GenAlias, conf: WrapConf, cache: var WrapCache): AliasDecl[PNode] =

  result = AliasDecl[PNode](
    iinfo: gen.iinfo,
    docComment: gen.docComment.join("\n"),
    isDistinct: gen.isDistinct,
    isExported: true,
    oldType: gen.baseType.toNType(conf, cache),
    newType: gen.newAlias.toNType(conf, cache))

func hash*(spec: NimImportSpec): Hash =
  !$(hash(spec.importPath) !&
     hash(spec.isRelative) !&
     tern(spec.isRelative, hash(spec.relativeDepth), hash(true)))

proc addImport*(wrapped: var WrappedFile, imp: NimImportSpec) =
  if "include" in $imp:
    pprintStackTrace()

  wrapped.imports.incl imp


func toNNode*(names: NimImportSpec): PNode =
  var elements: seq[PNode]
  var imports = names.importPath
  if names.isRelative:
    var prefix: PNode
    if names.relativeDepth == 0:
      prefix = newPIdent("./")

    else:
      prefix = newPident(repeat("../", names.relativeDepth))

    elements.add nnkPrefix.newPTree(
      prefix,
      newPident(imports[0])
    )

    imports = imports[1 ..^ 1]

  for path in imports:
    elements.add newPident(path)

  nnkImportStmt.newPTree(
    foldl(elements, nnkInfix.newPTree(newPident("/"), a, b))
  )

func toNNode*(imports: HashSet[NimImportSpec]): PNode =
  nnkImportStmt.newPTree(imports.mapIt(it.toNNode()[0]))


proc toNNode*(gen: GenImport, conf: WrapConf): WrappedEntry =
  ## Convert import passthough wrapped entry.
  var decl = NimDecl[PNode](
    kind: nekPassthroughCode,
    passthrough: gen.importSpec.toNNode(),
    passIInfo: gen.iinfo
  )

  return newWrappedEntry(decl, wepBeforeAll, gen.iinfo)

proc toNNode*(
    gen: GenEntry, conf: WrapConf, cache: var WrapCache): seq[WrappedEntry] =
  case gen.kind:
    of gekEnum:
      result.add toNNode(gen.genEnum, conf, cache)

    of gekPass:
      result.add gen.genPass.passEntries

    of gekEmpty:
      discard

    of gekAlias:
      result.add toNNode(gen.genAlias, conf, cache).
        toNimDecl().
        newWrappedEntry(wepInTypes, gen.genAlias.iinfo, gen.cdecl)

    of gekObject:
      result.add toNNode(gen.genObject, conf, cache)

    of gekProc:
      result.add toNNode(gen.genProc, conf, cache).
        toNimDecl().
        newWrappedEntry(wepInProcs, gen.genProc.iinfo, gen.cdecl)

    of gekImport:
      result.add toNNode(gen.genImport, conf)

    of gekForward:
      raiseUnexpectedKindError(
        gen, "Forward declaration nodes should be converted to import/pass")

proc writeWrapped*(
    res: CodegenResult,
    outFile: FsFile,
    compile: seq[FsFile],
    conf: WrapConf
  ) =

  ##[

Write generated wrappers to single file

- @arg{res} :: Generated wrappers - just pass results of
 `wrapSingleFile`
- @arg{outFile} :: target file to write generated nim code to
- @arg{codegens} :: directory for saving codegen files
- @arg{compile} :: Additional list of files to add as `{.compile.}`
- @arg{conf} :: Wrap configuration state object

]##

  var filenames: HashSet[string]
  if res.codegen.len > 0 or res.decls.len > 0:
    withNewStreamFile(outFile):
      for gen in res.codegen:
        if gen.filename.hasExt("cpp") and $gen.filename notin filenames:
          let res = gen.filename.withBasePrefix("gen_")
          file.writeLine(&"{{.compile: \"{res}\".}}")
          filenames.incl $gen.filename

      for gen in compile:
        file.writeLine(&"{{.compile: \"{gen}\".}}")

      for entry in res.decls:
        file.write(entry)

  var resFiles: Table[string, File]

  if conf.codegenDir.isSome():
    let dir = conf.codegenDir.get()
    for gen in res.codegen:
      let target = dir /. gen.filename
      let res = target.withBasePrefix("gen_")
      # info "Writing generated code into", res
      if $target notin resFiles:
        resFiles[$target] = open(res, fmWrite)
        resFiles[$target].write(gen.header)

      resFiles[$target].write(gen.code)

    var content = newJArray()
    for (ident, loc) in res.cache.identRefidMap:
      let loc: JsonNode = %{
        "file": %loc.file.string,
        "line": %loc.line,
        "column": %loc.column
      }

      content.add %[conf.toHaxdocJson(ident), loc]

    writeFile(dir / conf.refidFile, pretty(content))

  for _, file in pairs(resFiles):
    file.close()
