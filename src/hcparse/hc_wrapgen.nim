import std/[strutils, sequtils, strformat, tables, lenientops,
            bitops, with, sets]

import hc_types, cxtypes, cxcommon, libclang_wrap, hc_typeconv

import hnimast, hnimast/pprint
import hmisc/macros/iflet
import hmisc/algo/[htemplates, hseq_distance]
import hmisc/helpers
import hmisc/other/[colorlogger, oswrap]
import hmisc/types/colorstring

import hc_visitors, hc_types

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
        case cursor[0].cxKind():
          of ckUnexposedExpr, ckCallExpr, ckFunctionalCastExpr:
            result = aux(cursor[0], ilist)

          of ckIntegerLiteral:
            result = cursor[0].aux(ilist)

          else:
            info cursor[0].cxKind()
            debug "\n" & cursor.treeRepr(conf.unit)
            raiseAssert("#[ IMPLEMENT ]#")


        let str = "init" & $cursor.cxType()
        if result.kind in nkTokenKinds:
          result = newPCall(str, result)

        elif result.kind == nkCall and
             result[0].getStrVal() != str:
          result = newPCall(str, result)


      of ckFunctionalCastExpr:
        result = aux(cursor[1], ilist)

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

      of ckIntegerLiteral, ckCharacterLiteral, ckFloatingLiteral:
        let tokens = cursor.tokenStrings(conf.unit)

        case cursor.cxKind():
          of ckIntegerLiteral:
            result = newPCall("cint", newPLit(parseInt(tokens[0])))

          of ckCharacterLiteral:
            result = newPLit(tokens[0][1])

          of ckFloatingLiteral:
            result = newPLit(parseFloat(tokens[0]))

          else:
            discard

      else:
        err "Implement for kind", cursor.cxKind()
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
    arg.default = some(toInitCall(cursor[1], conf))
    # debug arg.default

proc wrapOperator*(
    oper: CDecl,
    conf: WrapConfig
  ): tuple[decl: GenProc, addThis: bool] =

  var it = initGenProc(oper.cursor, currIInfo(), oper.ident)

  it.name = oper.getNimName()
  # it.genParams = genParams # FIXME: was it necessary to have generic
  # parameters conversion for operators?

  assert conf.isImportcpp
  let kind = oper.classifyOperator()
  it.kind = pkOperator

  if kind == cxoAsgnOp and it.name == "setFrom":
    it.icpp = &"(# = #)"
    it.kind = pkRegular

  else:
    case kind:
      of cxoNewOp, cxoDeleteOp:
        discard

      of cxoAsgnOp:
        it.icpp = &"(# {it.name} #)"

      of cxoArrayOp:
        let rtype = oper.cursor.retType()
        let (_, mutable) = rtype.toNType(conf)
        if mutable:
          it.name = "[]="
          # WARNING potential source of horrible c++ codegen errors
          it.icpp = &"#[#]= #"

        else:
          it.icpp = &"#[#]"

      of cxoInfixOp:
        it.icpp = &"(toCppImport(oper.ident)(#, #))"

        if oper.args.len == 1:
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

      of cxoDerefOp:
        it.name = "[]"
        it.icpp = &"(*#)"

      of cxoPrefixOp:
        it.icpp = &"({it.name}#)" # FIXME use scoped ident

      of cxoPostfixOp:
        it.icpp = &"(#{it.name})" # FIXME use scoped ident

      of cxoCommaOp:
        it.name = "commaOp"
        it.icpp = &"commaOp(@)"
        it.kind = pkRegular

      of cxoConvertOp:
        let restype = oper.cursor.retType().toNType(conf).ntype

        with it:
          name = "to" & capitalizeAscii(restype.head)
          icpp = &"@"
          retType = resType
          declType = ptkConverter
          kind = pkRegular

      of cxoUserLitOp:
        let restype = oper.cursor.retType().toNType(conf).ntype

        with it:
          name = "to" & capitalizeAscii(restype.head)
          icpp = &"({oper.cursor}(@))"
          retType = restype
          kind = pkRegular

  it.header = conf.makeHeader(oper.cursor, conf)
  result.decl = it
  result.addThis = result.addThis or
    (kind in {
      cxoAsgnOp, cxoArrayOp, cxoDerefOp, cxoArrowOp, cxoConvertOp
    })


proc wrapProcedure*(
    pr: CDecl,
    conf: WrapConfig,
    parent: Option[NType[PNode]],
    cache: var WrapCache,
    parentDecl: Option[CDecl],
    asNewConstructor: bool
  ): tuple[decl: WrappedEntry, canAdd: bool] =
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
  ## - TODO :: allow creating placement new constructors that generate
  ##   nim `ref` types and allow for nim-managed memory.



  var it = initGenProc(pr.cursor, currIInfo(), pr.ident)
  # WARNING `pr.ident` might contain incorrect full name

  var addThis = (
    pr.kind == cdkMethod and
    pr.cursor.cxKind notin {
      ckConstructor, ckDestructor, ckConversionFunction
    }
  )

  result.canAdd = true
  if pr.isOperator():
    # HACK temporary workaround for `new` and `delete` operator handing
    if classifyOperator(pr) notin {cxoNewOp, cxoDeleteOp}:
      let (decl, adt) = pr.wrapOperator(conf)
      it = decl
      it.iinfo = currIInfo()
      addThis = adt

    else:
      addThis = false
      result.canAdd = false

  else:
    it.name = pr.getNimName().fixIdentName()

    # FIXME
    # iflet (par = parent):
    #   it.genParams = par.nimname.genParams

    if parent.isSome():
      assert conf.isImportcpp,
        "Cannot wrap methods for non-cxx targets"

      it.iinfo = currIInfo()
      if pr.cursor.isStatic():
        addThis = false
        it.icpp = &"({toCppNamespace(pr.ident)}(@))"

      else:
        it.icpp = &"(#.{it.name}(@))"

      it.header = conf.makeHeader(pr.cursor, conf)

    else:
      if conf.isImportcpp:
        it.icpp = &"({toCppNamespace(pr.ident)}(@))"

      else:
        it.icpp = &"{it.name}"

      it.header = conf.makeHeader(pr.cursor, conf)


  if addThis:
    assert parent.isSome()
    it.args.add initCArg(
      "self", parent.get(),
      pr.cursor.isConstMethod.tern(nvdVar, nvdLet)
    )

  for arg in pr.args:
    var (vtype, mutable) = arg.cursor.cxType().toNType(conf)
    if arg.cursor.cxType().isEnum():
      vtype.head &= conf.isImportCpp.tern("Cxx", "C")

    if vtype.kind in {ntkIdent, ntkGenericSpec}:
      if vtype.head == "UNEXPOSED":
        # WARNING currently parameters which contain `tkUnexposed`
        # types are not handled but are skipped instead. I don't
        # know how to fix right now.
        result.canAdd = false

      if parentDecl.isSome() and
         arg.cursor.inheritsGenParamsOf(parentDecl.get().cursor) and
         parent.isSome() and
         (arg.cursor.cxType().kind notin {tkUnexposed})
        :
        # WARNING nested class definitions with additional template
        # parameters are not handled right now. It will break for
        # code like
        # `<Ta> struct A { <Tb> struct B {void func(); }; };`
        # and only add `Tb` as template parameter for `func()`.
        vtype.add parent.get().genParams
        # FAIL most likely broken with recent refactoring

    else:
      # FIXME determine and implement edge case handling for procvar
      # arguments

      # WARNING might cause duplication, for wrapping C++ functors better
      # handling should be implemented
      vtype.pragma.add newPident("cdecl")

    var newArg = initCArg(fixIdentName(arg.name), vtype, mutable)
    setDefaultForArg(newArg, arg.cursor, conf)
    it.args.add newArg

  if $it.cursor == "operator=":
    # FIXME check if first argument and parent declaration types are
    # identical. Right now it does not work because
    # `b3Matrix3x3` != `const b3Matrix3x3 &`
    if parentDecl.get().cursor.cxType() == pr.args[0].cursor.cxType():
      # By default `operator=` is converted to regular `setFrom` proc to
      # correctly handle multiple overloads (in C++ `operator=` can have
      # different types on RHS and LHS, but this is not the case in nim).
      # *if* assignmed is indeed done from two identical types, then it can
      # be wrapped as actual `=` proc.
      it.name = "="
      it.kind = pkOperator

  if pr.isOperator and pr.classifyOperator() == cxoAsgnOp:
    # HACK Force override return type for assignment operators
    it.retType = newPType("void")

  elif pr.cursor.kind in {ckConstructor, ckConversionFunction}:
    # Override handling of return types for constructors
    if not pr.isOperator():
      # But ignore implicit user-defined conversion functions like
      # `operator T()`
      assert parent.isSome(), "Cannot wrap constructor without parent object"

      it.iinfo = currIInfo()
      it.header = conf.makeHeader(pr.cursor, conf)
      if asNewConstructor:
        it.retType = newNType("ptr", @[parent.get()])
        it.icpp = &"new {toCppNamespace(parentDecl.get().ident)}(@)"

      else:
        it.retType = parent.get()
        it.icpp = &"{toCppNamespace(parentDecl.get().ident)}(@)"

  else:
    # Default handling of return types
    var (rtype, mutable) = toNType(pr.cursor.retType(), conf)
    if parentDecl.isSome() and
       parent.isSome() and
       pr.cursor.retType().
       getTypeDeclaration().
       inheritsGenParamsOf(parentDecl.get().cursor):

      rtype.genParams = parent.get().genParams
      # WARNING(refactor)

    it.retType = rtype

    if rtype.hasUnexposed():
      # WARNING dropping all methods that use `tkUnexposed` type
      # in return value. This must be fixed in future versions.
      result.canAdd = false

    if pr.cursor.cxkind == ckDestructor:
      # Implicitly calling destructor on object (if someone ever needs
      # something like that)
      it.icpp = &"~{it.name}()"
      it.header = conf.makeHeader(pr.cursor, conf)

  if pr.cursor.kind == ckDestructor:
    it.name = "destroy" & it.name

  elif pr.cursor.cxkind in {ckConstructor, ckConversionFunction}:
    if not pr.isOperator():
      it.name = tern(asNewConstructor, "new", "init") &
        it.name.capitalizeAscii()

  if pr.cursor.isVariadic() == 1:
    it.pragma.add newPIdent("varargs")

  result.decl = newWrappedEntry(it)


proc fixNames(
  ppd: var GenProc, conf: WrapConfig, parent: NType[PNode]) =

  var idx: int = 0
  for param in mitems(ppd.genParams):
    conf.fixTypeName(param, conf, 0)
    inc idx

  idx = 0
  for arg in mitems(ppd.args):
    conf.fixTypeName(arg.ntype, conf, idx)
    inc idx

  conf.fixTypeName(ppd.retType, conf, 0)

  ppd.name = ppd.name.fixIdentName()





proc wrapMethods*(
    cd: CDecl,
    conf: WrapConfig,
    parent: NType[PNode],
    cache: var WrapCache
  ): seq[WrappedEntry] =

  ## - @arg{cd} :: Class declaration to wrap methods for
  ## - @arg{parent} :: Nim name of class declaration
  ## - @arg{cname} :: C++ name of class declaration (with unconverted
  ##   namespaces etc.)

  assert cd.kind in {cdkClass, cdkStruct}
  for meth in cd.methods({
    ckMethod, ckDestructor, ckConstructor, ckConversionFunction
  }):
    if meth.cursor.cxKind() in {ckConstructor, ckConversionFunction}:
      block:
        # Wrap as `new` constructor
        let (decl, canAdd) = wrapProcedure(
          meth, conf, some parent, cache, some cd, true)

        if canAdd:
          result.add decl

      block:
        # Wrap as `init` constructor
        let (decl, canAdd) = wrapProcedure(
          meth, conf, some parent, cache, some cd, false)

        if canAdd:
          result.add decl
    else:
      let (decl, canAdd) = wrapProcedure(
        meth, conf, some parent, cache, some cd, true)

      if canAdd:
        result.add decl


  for decl in mitems(result):
    fixNames(decl.gproc, conf, parent)

  result = result.deduplicate()

proc wrapFunction*(
    cd: CDecl, conf: WrapConfig, cache: var WrapCache
  ): seq[WrappedEntry] =

  var (decl, canAdd) = wrapProcedure(
    cd, conf, none NType[PNode], cache, none CDecl, false)

  if canAdd:
    result.add decl


proc wrapTypeFromNamespace(
  ident: CScopedIdent, conf: WrapConfig, cursor: CXCursor): PObjectDecl =
  ## `cursor` points to type declaration being wrapped
  # WARNING for now I assume that 'UNEXPOSED' type only occurs in
  # situations like `std::move_iterator<'0>::pointer` where typedef
  # uses it's semantic parent (class or struct declaration) to get
  # template parameters. This might not be a valid assumption in
  # general case.k

  result = PObjectDecl(
    name: conf.typeNameForScoped(ident, conf),
    exported: true
  )

  result.annotation = some(newPPragma(
    newExprColonExpr(
      newPIdent(conf.importX()),
      ident.toCppNamespace().newRStrLit()
    ),
    newExprColonExpr(
      newPIdent "header",
      conf.makeHeader(cursor, conf).toNNode())))




proc wrapObject*(
    cd: CDecl, conf: WrapConfig, cache: var WrapCache
  ): tuple[
    obj: WrappedEntry, genAfter, genBefore: seq[WrappedEntry]
  ]

proc wrapAlias*(
    al: CDecl, parent: CScopedIdent, conf: WrapConfig, cache: var WrapCache,
  ): seq[WrappedEntry] =
  # NOTE returning multiple values because of
  # `typedef struct A {} A, *APtr` shit that can result in multple
  # declarations.

  # Get underlying type for alias
  let aliasof = al.cursor.cxType().getCanonicalType()
  # Create new identifier for aliased type
  var newAlias = conf.typeNameForScoped(al.ident, conf)
  # Identifier for old aliased type
  var baseType: NType[PNode]
  if getTypeDeclaration(aliasof).cxKind() == ckNodeclFound:
    let name = fromCxxTypeName($aliasof)
    if name.len > 0:
      baseType = newPType(name)

    else:
      baseType = newPType($aliasof)

  else:
    baseType = conf.typeNameForScoped(aliasof.toFullScopedIdent(), conf)
    # WARNING mismatched generic parameters between lhs and rhs parts of
    # alias might result in broken wrappers.


  if false: # TEMP need to find a real-world use-case to correctly handle
            # this, disabled for now.
    if aliasof.getNumTemplateArguments() > 0:
      let required =
        aliasof.getTypeDeclaration().
        getSpecializedCursorTemplate().
        requiredGenericParams()

      # WARNING HACK - ignore all template parameters that /might/ be
      # defaulted - i.e. only ones that *must* be specified (not defaulted in
      # declaration) are included. Better alias handling is necessary, for
      # now I just drop 'unnecessary' parts.
      baseType.genParams = baseType.genParams[
        0 ..< min(required.len(), baseType.genParams.len())]


  if baseType.hasUnexposed():
    raiseImplementError("Found unexposed type")
    # let namespace = parent & @[newPType($al.cursor)]
    # result = @[newWrappedEntry(
    #   toNimDecl(
    #     namespace.wrapTypeFromNamespace(conf, al.cursor)
    #   ),
    #   al
    # )]

  else:
    if al.cursor[0].cxKind() notin {ckStructDecl}:
      # NOTE ignore `typedef struct` in C
      result = @[newWrappedEntry(
        toNimDecl(newAliasDecl(
          newAlias, baseType, iinfo = currIInfo())), al
      )]

    else:
      if cache.canWrap(al.cursor[0]):
        # Multiple trailing typedefs result in several `ckTypedefDecl`
        # nodes

        cache.markWrap(al.cursor[0])
        let nested = visitClass(al.cursor[0], parent, conf)

        let (obj, pre, post) = wrapObject(nested, conf, cache)

        result.add pre & @[obj] & post

      if newAlias != baseType:
        # `typedef struct {} A;` has the same typedef name and type, and
        # should only be wrapped as type definition and not alias.
        result.add newWrappedEntry(
          toNimDecl(newAliasDecl(
            newAlias, baseType, iinfo = currIInfo(), isDistinct = false,
          )), al
        )

proc getParentFields*(
    inCursor: CXCursor, obj: PObjectDecl, wrapConf: WrapConfig
  ): seq[PProcDecl] =

  for class in inCUrsor.getClassBaseCursors():
    for entry in class:
      if entry.kind in {ckFieldDecl}:
        let
          (fldType, _) = entry.cxType().toNType(wrapConf)
          fldname = $entry


        result.add newPProcDecl(
          name = fldName,
          rtyp = some(fldType),
          args = { "self" : obj.name },
          iinfo = currIInfo(),
          pragma = newPPragma(newExprColonExpr(
            newPIdent(wrapConf.importX()), newRStrLit(&"(#.{fldName})")))
        )

        result[^1].genParams.add(obj.name.genParams)

        result[^1].addCodeComment(
          &"Parent field getter passtrough from {class}\n")

        if not entry.cxType().isConstQualifiedDeep():
          # FIXME replace with `GProc` declaration
          result.add newPProcDecl(
            name = fldName,
            iinfo = currIInfo(),
            args = { "self" : obj.name, "val" : fldType },
            pragma = newPPragma(newExprColonExpr(
              newPIdent(wrapConf.importX()), newRStrLit(&"(#.{fldName} = @)")))
          )

          result[^1].genParams.add(obj.name.genParams)
          result[^1].signature.arguments[0].kind = nvdVar
          result[^1].addCodeComment(
            &"Parent field assignment passtrough from {class}\n")

          result[^1].kind = pkAssgn


proc wrapEnum*(declEn: CDecl, conf: WrapConfig, cache: var WrapCache): seq[WrappedEntry]

proc getDefaultAccess*(cursor: CXCursor): CXAccessSpecifier =
  case cursor.cxKind():
    of ckClassDecl, ckClassTemplate:
      asPrivate

    of ckStructDecl:
      asPublic

    else:
      raiseAssert("Cannot get default visibility for cursor of kind " & $cursor.cxKind())

proc isAggregateInitable*(cd: CDecl, initArgs: var seq[CArg], conf: WrapConfig): bool =
  ## Determine if entry pointed to by `cd`'s cursor is subject to aggregate
  ## initalization. Add all fields for aggregate initalization into
  ## @arg{initArgs}. NOTE: fields will be added unconditionally, so first
  ## check return value.

  if cd.cursor.cxKind() in {ckUnionDecl, ckEnumDecl}:
    return false

  elif cd.cursor.cxKind() notin {ckClassDecl, ckStructDecl, ckClassTemplate}:
    assertionFail:
      "Invalid cursor kind of aggregate initalization check."
      "Expected type declaration (union/enum/struct/class),"
      "but found {toRed($cd.cursor.cxKind())}"

  # List of entries that immediately mean aggregate initalization is not
  # supported.
  const failKinds = {
    ckConstructor
  }

  const ignoreKinds = {
    ckTemplateTypeParameter,
    ckTypedefDecl,
    ckStructDecl,
    ckEnumDecl,
    ckUnionDecl,
    ckMethod,
    ckFunctionTemplate,
    ckDestructor,
    ckAlignedAttr
    # ckBaseClassSpecifier
  }

  proc aux(cursor: CXCursor): bool =
    ## Recursively determine if cursor points to type that is subject to
    ## aggregate initalization.
    # debug cursor.treeRepr(conf.unit)
    case cursor.cxType().cxKind():
      of tkPodKinds, tkPointer:
        return true

      of tkTypeRef, tkElaborated:
        for entry in cursor.cxType().getTypeDeclaration():
          if entry.cxKind() in failKinds or
             (entry.cxKind() notin ignoreKinds) and
             (not aux(entry)):
            return false

        return true

      of tkTypedef:
        return aux(cursor.cxType().getCanonicalType().getTypeDeclaration())

      of tkInvalid:
        return false

      of tkConstantArray:
        return aux(cursor[0])

      else:
        debug cursor.cxType().cxKind()
        debug cast[int](cursor.cxType().cxKind())
        debug cursor.getSpellingLocation()
        debug cursor.treeRepr(conf.unit)
        err cursor.cxType()
        raiseAssert("#[ IMPLEMENT ]#")

  result = true
  var access = cd.cursor.getDefaultAccess()
  for entry in cd.cursor:
    case entry.cxKind():
      of ckFieldDecl:
        # debug entry.treeRepr()
        if aux(entry):
          var arg = initCArg(
            fixIdentName($entry), entry.cxType().toNType(conf).ntype, false)

          setDefaultForArg(arg, entry, conf)
          initArgs.add arg

      of failKinds:
        return false

      of ignoreKinds:
        discard

      of ckAccessSpecifier:
        access = entry.getAccessSpecifier()

      of ckVarDecl:
        debug entry.treeRepr(conf.unit)
        discard

      of ckBaseSpecifier:
        if aux(entry[0]):
          discard

        else:
          return false

      else:
        debug entry.treeRepr(conf.unit)
        debug cast[int](entry.cxType().cxKind())
        debug cast[int](entry.cxKind())
        debug entry.getSpellingLocation()
        raiseAssert(&"#[ IMPLEMENT for kind {entry.kind} ]#")


proc wrapObject*(
    cd: CDecl, conf: WrapConfig, cache: var WrapCache
  ): tuple[
    obj: WrappedEntry, genAfter, genBefore: seq[WrappedEntry]
  ] =

  let tdecl = cd.cursor.cxType().getTypeDeclaration()

  assert cd.kind in {cdkClass, cdkStruct}
  var obj = PObjectDecl(
    name: conf.typeNameForScoped(cd.ident, conf),
    exported: true,
    iinfo: currIInfo()
  )

  block:
    var initArgs: seq[CArg]
    if isAggregateInitable(cd, initArgs, conf) and initArgs.len > 0:
      # info obj.name.head, "can be aggregate initialized"
      result.genAfter.add newWrappedEntry(
        # WARNING `cd.ident`
        initGenProc(CXCursor(), currIInfo(), cd.ident).withIt do:
          it.name = "init" & obj.name.head
          it.args = initArgs
          it.header = conf.makeHeader(cd.cursor, conf)
          it.icpp = &"{toCppNamespace(cd.ident)}({{@}})"
          it.retType = obj.name
          it.genParams = obj.name.genParams
      )

  for entry in cd.cursor:
    case entry.cxKind():
      of ckEnumDecl:
        let visited = visitEnum(entry, cd.ident, conf)
        result.genBefore.add wrapEnum(visited, conf, cache)

      of ckStructDecl, ckClassDecl, ckUnionDecl:
        let visited = visitClass(entry, cd.ident, conf)
        let (obj, pre, post) = wrapObject(visited, conf, cache)
        result.genBefore.add pre & @[obj] & post

      of ckFieldDecl, ckMethod, ckFriendDecl,
         ckFunctionTemplate, ckAccessSpecifier,
         ckConstructor, ckDestructor, ckTypedefDecl,
         ckBaseSpecifier:
        # Constructors, field access and other implementation parts have
        # already been added in `visitClass`, now we can ignore them
        # altogether.
        discard

      else:
        warn &"#[ IMPLEMENT for kind {entry.cxkind()} {instantiationInfo()} ]#"

  # WARNING might die on `<T<T<T<T<T<T>>>>>` things
  for fld in cd.pubFields:
    var resFld = PObjectField(
      isTuple: false,
      name: fixIdentName(fld.lastName()),
      exported: true,
      annotation: some newPPragma(
        newExprColonExpr(
          newPIdent("importc"), newRStrLit(fld.lastName()))),
    )

    if fld.cursor[0].cxKind() in {ckUnionDecl, ckStructDecl}:
      var nested = visitClass(fld.cursor[0], cd.ident, conf)

      # NOTE not sure what was going on here when I first wrote this, might
      # need more thorough fixup.

      # nested.name.head = resFld.name
      # nested.name = nested.inNamespace(@[])

      let (obj, pre, post) = wrapObject(nested, conf, cache)

      result.genBefore.add(pre & @[obj] & post)

      resFld.fldType = obj.wrapped.objectDecl.name

    else:
      resFld.fldType = fld.cursor.cxType().toNType(conf).ntype


    obj.flds.add resFld

  obj.annotation = some(newPPragma(
    newExprColonExpr(
      newPIdent (if conf.isImportcpp: "importcpp" else: "importc"),
      cd.ident.toCppNamespace().newRStrLit()
    ),
    newExprColonExpr(
      newPIdent "header",
      conf.makeHeader(cd.cursor, conf).toNNode()
    ),
  ))

  if cd.cursor.cxKind() == ckUnionDecl:
    obj.annotation.get().add newPIdent("union")

  result.genAfter.add wrapMethods(cd, conf, obj.name, cache)

  for mem in cd.members:
    case mem.kind:
      of cdkAlias:
        result.genBefore.add mem.wrapAlias(cd.ident, conf, cache)

      else:
        discard

  result.genAfter.add getParentFields(cd.cursor, obj, conf).mapIt(
    newWrappedEntry(toNimDecl(it)))

  result.obj = newWrappedEntry(toNimDecl(obj), cd)


proc getFields*(declEn: CDecl, conf: WrapConfig): tuple[
    namedvals: Table[string, BiggestInt],
    enfields: seq[tuple[name: CXCursor, value: Option[EnFieldVal]]]
  ] =

  for (name, value) in declEn.flds:
    let val = value.get()
    var resval: Option[EnFieldVal]
    case val.kind:
      of ckIntegerLiteral:
        resVal = some initEnFieldVal(
          val.tokenStrings(conf.unit)[0].parseInt())

      of ckBinaryOperator:
        # FIXME C++ allows to have arbitrary complex expressions for enum
        # values, so in the end I would have to implement simple math
        # expression AST interpreter in order to evaluate all of this, or
        # take some existing one (it is possible to convert expression back
        # to tokens and use something like
        # https://github.com/Yardanico/nim-mathexpr)
        let subn = val.children()
        let toks = val.tokenStrings(conf.unit)[1] # TEST for `(1 << 2) | (1 << 3)`
        # debug toks
        # debug subn.mapIt(it.tokenStrings(conf.unit))
        case toks:
          of "<<":
            resval = some initEnFieldVal(
              subn[0].tokenStrings(conf.unit)[0].parseInt() shl
              subn[1].tokenStrings(conf.unit)[0].parseInt(),
            )

          of "|":
            let toks = val.tokenStrings(conf.unit)
            # NOTE assuming `EnumField | OtherField` for now
            let
              lhs = toks[0]
              rhs = toks[2]
              lhsVal = result.namedVals[lhs]
              rhsVal = result.namedVals[rhs]

            resval = some initEnFieldVal(bitor(lhsVal, rhsVal))

          else:
            discard

      of ckUnaryOperator:
        let toks = val.tokenStrings(conf.unit)
        case toks[0]:
          of "-":
            resval = some initEnFieldVal(toks[1].parseInt())

          else:
            raiseAssert("#[ IMPLEMENT ]#")

      of ckUnexposedExpr:
        case val[0].kind:
          of ckIntegerLiteral:
            resVal = some initEnFieldVal(
              val.tokenStrings(conf.unit)[0].parseInt())

          else:
            raiseImplementError(&"Kind {val[0].kind}")

      elif $val.kind == "OverloadCandidate": # HACK
        resval = none EnFieldVal

      else:
        err val.treeRepr(conf.unit)
        raiseAssert(
          &"#[ IMPLEMENT for kind {val.kind} {instantiationInfo()} ]#")

    result.enfields.add (name: name, value: resval)

proc wrapEnum*(declEn: CDecl, conf: WrapConfig, cache: var WrapCache): seq[WrappedEntry] =
  ## Generate wrapper for enum declaration
  ##
  ## Generates wrapper for enum declaration, using wrap configuration.
  ## Wrapping is performed in two steps - underlying C enum is wrapped
  ## as-is, and additional nim `enum` is generated. Nim version does not
  ## have holes, which allows it to be used in `array`, iterated upon etc.
  ##
  ## In order to perform conversion between 'proxy' and underlying enum
  ## several helper procs are introduces, such as `toInt`.


  # Base wrapped enum
  var nt = conf.typeNameForScoped(declEn.ident, conf)
  var ennames: seq[string]

  proc cEnumName(str: string, cache: var WrapCache): string {.closure.} =
    # Generate name for C enum. QUESTION: enum names don't need to be
    # perfectly accurate as they are converted to integers and then casted
    # - I'm not completely sure if this correct.
    result = nt.head
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


  var vals: OrderedTable[string, tuple[
    resName: string,
    resVal: BiggestInt,
    stringif: string,
    cursor: CXCursor
  ]]

  let implName = nt.head & tern(conf.isImportCpp, "Cxx", "C")
  block:
    # Generate wrapper for default implementation of the enum
    var implEn = newPEnumDecl(name = implName, iinfo = currIInfo())
    implEn.addDocComment conf.docCommentFor(declEn.ident, declEn.cursor, cache)


    implEn.pragma.add newPIdentColonString(
      (if conf.isImportcpp: "importcpp" else: "importc"),
      toCppNamespace(declEn.ident)
    )

    implEn.pragma.add nnkExprColonExpr.newPTree(
      newPIdent("header"),
      conf.makeHeader(declEn.cursor, conf).toNNode()
    )

    implEn.exported = true


    # Get list of all enum fields with values, construct table of values
    # without filtering.
    let (namedvals, enfields) = getFields(declEn, conf)
    var fldVals: Table[BiggestInt, string]
    var repeated: Table[string, seq[string]]

    for (key, val) in enfields:
      if val.isSome():
        if val.get().isRefOther:
          repeated.mgetOrPut(val.get().othername, @[ $key ]).add $key

        else:
          let val = val.get().value
          if val notin fldVals:
            fldVals[val] = $key

          else:
            repeated.mgetOrPut(fldVals[val], @[ $key ]).add $key


    # List of field with respective values. Holes are filled with correct
    # values, and duplicated fields are dropped.
    var flds: seq[(CXCursor, BiggestInt)]

    var uniformEnum: bool = true
    block:
      var prev: BiggestInt = 0
      for (key, val) in enfields:
        let startPrev = prev
        if val.isSome():
          if val.get().isRefOther:
            discard

          else:
            prev = val.get().value
            flds.add (key, prev)

        else:
          # NOTE previously `inc prev` was /after/ field addition. Not sure
          # what other edge case was involved, or this is just plain
          # off-by-one error.
          inc prev
          flds.add (key, prev)

        if prev > startPrev + 1:
          uniformEnum = false

      # Sort fields based on value
      flds = flds.sorted(
        proc(f1, f2: (CXCursor, BiggestInt)): int {.closure.} =
          cmp(f1[1], f2[1])
      )

    block:
      # Generate wrapped for C enum. Each field has value assigned to it.
      var prev = BiggestInt(-120948783)
      for (name, val) in flds:
        let comment = conf.docCommentFor(declEn.ident & toCName(name), name, cache)
        if val != prev:
          prev = val
          implEn.addField(($name).cEnumName(cache), some newPLit(val), docComment = comment)

          vals[$name] = (
            resName: ($name).cEnumName(cache),
            resVal: val,
            stringif: toCppNamespace(declEn.ident) & "::" & $name,
            cursor: name
          )



    result.add newWrappedEntry(toNimDecl(implEn), declEn)

  block: # Nim proxy proc declaration.
    # Determine common prefix for C++ enum (if any)
    let pref = declEn.flds.mapIt($it.fldName).commonPrefix()

    # Get name of the enum type itsel by combining first letters of
    # `PascalCase` or `snake_case` name.
    let enumPref = conf.prefixForEnum(declEn.ident, conf, cache)

    var en = newPEnumDecl(name = nt.head, iinfo = currIInfo())
    en.addDocComment conf.docCommentFor(declEn.ident, declEn.cursor, cache)

    proc renameField(fld: string, cache: var WrapCache): string {.closure.} =
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

    # Metadata associated with proxy enum
    var arr = nnkBracket.newPTree()
    var strCase = nnkCaseStmt.newPtree(newPIdent "en")

    for name, wrap in vals:
      # Add fields to nim enum without values
      let comment = conf.docCommentFor(
        declEn.ident & toCName(name), wrap.cursor, cache)

      en.addField(name.renameField(cache), docComment = comment)
      strCase.add nnkOfBranch.newPTree(
        newPIdent(name.cEnumName(cache)),
        nnkAsgn.newPTree(newPIdent("result"), newPLit(wrap.stringif))
      )

      arr.add pquote do:
        (
          name: `newPLit(name)`, # Name of the original enum
          cEnum: `newPIdent(name.cEnumName(cache))`, # Original enum value
          cName: `newPLit(wrap.stringif)`, # Namespaced C++ enum name
          value: `newPLit(wrap.resVal)` # Integer value for field
        )

    let
      enName = newPIdent(en.name)
      arrName = newPIdent("arr" & en.name & "mapping")
      convName = newPident("to" & implName)
      implName = newPIdent(implName)


    let helpers = pquote do:
      const `arrName`: array[`enName`, tuple[
        name: string,
        cEnum: `implName`,
        cName: string,
        value: int
      ]] = `arr`

      # Convert proxy enum to integer value
      proc toInt*(en: `enName`): int {.inline.} =
        `arrName`[en].value

      # Convert set of enums to bitmasked integer
      proc toInt*(en: set[`enName`]): int {.inline.} =
        for val in en:
          result = bitor(result, `arrName`[val].value)

      # Return namespaced name of the original enum
      proc `$`*(en: `implName`): string {.inline.} =
        `strCase`

      converter `convName`*(en: `enName`): `implName` {.inline.} =
        `arrName`[en].cEnum



    result.add newWrappedEntry(toNimDecl(helpers), true)

    en.exported = true

    result.add newWrappedEntry(toNimDecl(en), declEn)

proc wrapMacros*(declMacros: seq[CDecl], conf: WrapConfig): seq[WrappedEntry] =
  return

  info "Wrapping macros"
  for decl in declMacros:
    debug decl.cursor
    logIndented:
      for (tok, kind) in decl.cursor.tokenKinds(conf.unit):
        debug tok.toGreen(), kind

      # debug decl.cursor.tokenStrings(conf.unit).join(", ", ("<\e[31m", "\e[39m>"))


proc wrapApiUnit*(
  api: CApiUnit, conf: WrapConfig,
  cache: var WrapCache, index: FileIndex): seq[WrappedEntry] =
  ## Generate wrapper for api unit.
  var macrolist: seq[CDecl]
  for decl in api.decls:
    if cache.canWrap(decl.cursor):
      cache.markWrap(decl.cursor)
    else:
      continue

    case decl.kind:
      of cdkClass:
        identLog()
        let spec = decl.cursor.getSpecializedCursorTemplate()

        if spec.cxKind() != ckFirstInvalid:
          discard

        else:
          let (obj, procs, other) = decl.wrapObject(conf, cache)

          result.add obj

          for it in other:
            result.add it

          for pr in procs:
            result.add pr

        dedentLog()

      of cdkAlias:
        result.add decl.wrapAlias(decl.ident, conf, cache)

      of cdkEnum:
        result.add decl.wrapEnum(conf, cache)

      of cdkFunction:
        for f in decl.wrapFunction(conf, cache):
          result.add f

      of cdkMacro:
        macrolist.add decl

      else:
        debug decl.kind

  result.add wrapMacros(macrolist, conf)

proc getNType*(carg: CArg): NType[PNode] =
  if carg.isRaw:
    raiseAssert("#[ IMPLEMENT ]#")

  else:
    return carg.ntype

proc toNNode*(gp: GenProc, wrapConf: WrapConfig): PProcDecl =
  result = newPProcDecl(
    name = gp.name,
    iinfo = gp.iinfo,
    exported = true,
    rtyp = some(gp.retType),
    genParams = gp.genParams,
    declType = gp.declType,
    kind = gp.kind
  )

  for arg in gp.args:
    result.signature.arguments.add newNIdentDefs(
      vname = arg.name,
      value = arg.default,
      vtype = arg.getNTYpe(),
      kind = arg.varkind
    )

  result.docComment = gp.docs.join("\n")

  result.signature.pragma = gp.pragma

  result.signature.pragma.add(
    newPIdentColonString(wrapConf.importX(), gp.icpp))

  result.signature.pragma.add(
    newExprColonExpr(newPIdent "header", gp.header.toNNode()))


proc writeWrapped*(
    res: tuple[decls: seq[NimDecl[PNode]], codegen: seq[CxxCodegen]],
    outFile: FsFile,
    codegens: Option[FsDir],
    compile: seq[FsFile],
    wrapConf: WrapConfig
  ) =

  ##[

Write generated wrappers to single file

- @arg{res} :: Generated wrappers - just pass results of
 `wrapSingleFile`
- @arg{outFile} :: target file to write generated nim code to
- @arg{codegens} :: directory for saving codegen files
- @arg{compile} :: Additional list of files to add as `{.compile.}`
- @arg{wrapConf} :: Wrap configuration state object

]##

  var filenames: HashSet[string]
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

  if codegens.isSome():
    for gen in res.codegen:
      let target = codegens.get() / gen.filename
      let res = target.withBasePrefix("gen_")
      # info "Writing generated code into", res
      if $target notin resFiles:
        resFiles[$target] = open($res, fmWrite)
        resFiles[$target].write(gen.header)

      resFiles[$target].write(gen.code)

  for _, file in pairs(resFiles):
    file.close()
      # writeFile(target, gen.code)
