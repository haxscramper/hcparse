import
  std/[
    strutils, sequtils, strformat, tables,
    lenientops, parseutils, bitops, with, sets
  ]

import
  ./hc_types, ./cxtypes, ./cxcommon, ./hc_visitors,
  ./libclang_wrap, ./hc_typeconv

import
  hnimast,
  hnimast/pprint,
  hmisc/macros/iflet,
  hmisc/algo/[htemplates, hseq_distance, namegen],
  hmisc/[helpers, hexceptions],
  hmisc/other/[colorlogger, oswrap, hjson],
  hmisc/types/colorstring

import htsparse/cpp/cpp
import fusion/matching except addPrefix

proc wrapEnum*(declEn: CDecl, conf: WrapConf, cache: var WrapCache):
  seq[GenEntry]


proc wrapOperator*(
    oper: CDecl,
    conf: WrapConf
  ): tuple[decl: GenProc, addThis: bool] =

  var it = initGenProc(oper, currIInfo())

  it.name = oper.getNimName()
  # it.genParams = genParams # FIXME: was it necessary to have generic
  # parameters conversion for operators?

  assert conf.isImportcpp
  let kind = oper.classifyOperator()
  it.kind = pkOperator

  case oper.operatorKind:
    of cxoCopyAsgnOp:
      it.name = "setFrom"
      it.icpp = &"(# = #)"
      it.kind = pkRegular

    of cxoNewOp, cxoDeleteOp:
      discard

    of cxoAsgnOp:
      it.icpp = &"(# {it.name} #)"

    of cxoArrayOp:
      let rtype = oper.cursor.retType()
      let nimReturn = rtype.toNimType(conf)
      if nimReturn.isMutable:
        it.name = "[]="
        # WARNING potential source of horrible c++ codegen errors
        it.icpp = &"#[#]= #"

      else:
        it.icpp = &"#[#]"

    of cxoInfixOp:
      it.icpp = &"({toCppNamespace(oper.ident)}(#, #))"

      if oper.arguments.len == 1:
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
      let restype = oper.cursor.retType().toNimType(conf)

      with it:
        name = "to" & capitalizeAscii(restype.nimName)
        icpp = &"@"
        returnType = resType
        declType = ptkConverter
        kind = pkRegular

    of cxoUserLitOp:
      let restype = oper.cursor.retType().toNimType(conf)

      with it:
        name = "to" & capitalizeAscii(restype.nimName)
        icpp = &"({oper.cursor}(@))"
        returnType = restype
        kind = pkRegular

  it.header = conf.makeHeader(oper.cursor, conf)
  result.decl = it
  result.addThis = result.addThis or
    (kind in {
      cxoAsgnOp, cxoArrayOp, cxoDerefOp, cxoArrowOp, cxoConvertOp
    })


proc wrapProcedure*(
    pr: CDecl,
    conf: WrapConf,
    parent: Option[NimType],
    cache: var WrapCache,
    parentDecl: Option[CDecl],
    specialProcKind: GenProcSpecialKind
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
  ## - TODO :: allow creating placement new constructors that generate
  ##   nim `ref` types and allow for nim-managed memory.

  var it = initGenProc(pr, currIInfo())

  var addThis = (
    pr.kind == cdkMethod and
    pr.cursor.cxKind notin {
      ckConstructor, ckDestructor, ckConversionFunction
    }
  )

  result.canAdd = true
  if pr.isOperator:
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

    let icppName = toCppNamespace(pr.ident)
    if parent.isSome():
      assert conf.isImportcpp,
        "Cannot wrap methods for non-cxx targets"

      if pr.cursor.isStatic():
        addThis = false
        it.iinfo = currIInfo()
        it.icpp = &"({icppName}(@))"

      else:
        it.iinfo = currIInfo()
        it.icpp = &"(#.{pr.getNimName()}(@))"

      it.header = conf.makeHeader(pr.cursor, conf)

    else:
      if conf.isImportcpp:
        it.iinfo = currIInfo()
        it.icpp = &"({icppName}(@))"

      else:
        it.iinfo = currIInfo()
        it.icpp = &"{icppName}"

      it.header = conf.makeHeader(pr.cursor, conf)


  if addThis:
    assert parent.isSome()
    it.arguments.add initCArg(
      "self", parent.get(),
      pr.cursor.isConstMethod.tern(nvdVar, nvdLet)
    )

  for arg in pr.arguments:
    var argType = arg.cursor.cxType().toNimType(conf)
    if arg.cursor.cxType().isEnum():
      argType.nimName &= conf.isImportCpp.tern("Cxx", "C")

    if argType.kind in {ctkIdent}:
      if argType.nimName == "UNEXPOSED":
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
        argType.add parent.get().genericParams
        # FAIL most likely broken with recent refactoring

    else:
      # FIXME determine and implement edge case handling for procvar
      # arguments
      discard



    var newArg = initCArg(fixIdentName(arg.name), argType)
    setDefaultForArg(newArg, arg.cursor, conf)
    it.arguments.add newArg

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

  if pr.isOperator and pr.classifyOperator() == cxoAsgnOp:
    # HACK Force override return type for assignment operators
    it.returnType = newNimType("void")

  elif pr.cursor.kind in {ckConstructor, ckConversionFunction}:
    # Override handling of return types for constructors
    if not pr.isOperator:
      # But ignore implicit user-defined conversion functions like
      # `operator T()`
      assert parent.isSome(), "Cannot wrap constructor without parent object"

      it.iinfo = currIInfo()
      it.header = conf.makeHeader(pr.cursor, conf)
      case specialProcKind:
        of gpskNewPtrConstructor:
          it.returnType = newNimType("ptr", @[parent.get()])
          it.icpp = &"new {toCppNamespace(parentDecl.get().ident)}(@)"

        of gpskNewRefConstructor:
          it.returnType = newNimType("ref", @[parent.get()])
          it.icpp = &"new {toCppNamespace(parentDecl.get().ident)}(@)"

        of gpskInitConstructor:
          it.returnType = parent.get()
          it.icpp = &"{toCppNamespace(parentDecl.get().ident)}(@)"

        of gpskDefault:
          discard

  else:
    # Default handling of return types
    var returnType = toNimType(pr.cursor.retType(), conf)
    if parentDecl.isSome() and
       parent.isSome() and
       pr.cursor.retType().
       getTypeDeclaration().
       inheritsGenParamsOf(parentDecl.get().cursor):

      returnType.genericParams = parent.get().genericParams
      # WARNING(refactor)

    it.returnType = returnType

    if returnType.hasUnexposed():
      # WARNING dropping all methods that use `tkUnexposed` type
      # in return value. This must be fixed in future versions.
      result.canAdd = false

    if pr.cursor.cxkind == ckDestructor:
      # Explicitly calling destructor on object (if someone ever needs
      # something like that)
      it.icpp = &"~{it.name}()"
      it.header = conf.makeHeader(pr.cursor, conf)

  if pr.cursor.kind == ckDestructor:
    it.name = "destroy" & it.name

  elif pr.cursor.cxkind in {ckConstructor, ckConversionFunction}:
    if not pr.isOperator:
      let suffix = it.name.capitalizeAscii()
      case specialProcKind:
        of gpskNewPtrConstructor: it.name = "cnew" & suffix
        of gpskNewRefConstructor: it.name = "new" & suffix
        of gpskInitConstructor: it.name = "init" & suffix
        of gpskDefault: discard

  if pr.cursor.isVariadic() == 1:
    it.pragma.add newPIdent("varargs")

  if specialProcKind == gpskNewRefConstructor:
    pprintStackTrace()
    let argType = newNType("ref", [parent.get().toNType()]).toNNode()
    let emitStr = &["`self`->~", parentDecl.get().lastName(), "();"]
    it.impl = some pquote do:
      # WARNING FAIL experimental
      new(self, proc(self: `argType`) = {.emit: `emitStr`.})

  let generated = newProcVisit(it, conf, cache)
  result.decl.add it
  result.decl.add GenPass(iinfo: currIInfo(), passEntries: generated)


proc fixNames(ppd: var GenProc, conf: WrapConf, parent: NimType) =
  var idx: int = 0
  for param in mitems(ppd.genParams):
    conf.fixTypeName(param, conf, 0)
    inc idx

  idx = 0
  for arg in mitems(ppd.arguments):
    conf.fixTypeName(arg.nimType, conf, idx)
    inc idx

  conf.fixTypeName(ppd.returnType, conf, 0)

  ppd.name = ppd.name.fixIdentName()





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


  for meth in cd.methods({ckMethod}):
    let (decl, canAdd) = wrapProcedure(
      meth, conf, some parent, cache, some cd, gpskDefault)

    if canAdd:
      result.methods.add decl[0].genProc
      result.extra.add decl[1..^1]

  var hasConstructor = false
  for meth in cd.methods({
    ckDestructor, ckConstructor, ckConversionFunction
  }):
    var constructorAddKind: set[GenProcSpecialKind]
    # QUESTION I'm not entirely sure why this has such weird arrangement
    # for choosing code generation options. Originally the choice was to
    # either generate `new T*` or `init T` procedures, but at the time when
    # I was adding `new ref T` I completely forgot why I can't just
    # genrated everything in the first place.
    if meth.cursor.cxKind() in {ckConstructor, ckConversionFunction}:
      hasConstructor = true
      constructorAddKind = {
        gpskNewRefConstructor,
        gpskNewPtrConstructor,
        gpskInitConstructor
      }

    else:
      constructorAddKind = {
        gpskNewRefConstructor,
        gpskNewPtrConstructor
      }

    for kind in constructorAddKind:
      logIndented:
        let (decl, canAdd) = wrapProcedure(
          meth, conf, some parent, cache, some cd, kind)

      if canAdd:
        result.methods.add decl[0].genProc
        result.extra.add decl[1..^1]

  if not hasConstructor:
    let className = cd.ident.toCppNamespace()
    result.extra.add GenProc(
      name: "cnew" & parent.nimName.capitalizeAscii(),
      returnType: newNimType("ptr", @[parent]),
      icpp: &"new {className}()",
      cdecl: cd,
      iinfo: currIInfo(),
      header: conf.makeHeader(cd.cursor, conf)
    )

    let emitStr = &"new ((void*)result) {className}(); /* Placement new */"

    let
      ntype = parent.toNTYpe().toNNode()
      destroyCall = "destroy" & parent.nimName.capitalizeAscii()

    result.extra.add GenProc(
      name: destroyCall,
      arguments: @[initCArg("obj", newNimType("ptr", @[parent]))],
      cdecl: cd,
      iinfo: currIInfo(),
      icpp: &"#.~{className}()",
      header: conf.makeHeader(cd.cursor, conf),
    )

    result.extra.add GenProc(
      name: "new" & parent.nimName.capitalizeAscii(),
      returnType: newNimType("ref", @[parent]),
      cdecl: cd,
      noPragmas: gpcNoPragma,
      iinfo: currIInfo(),
      impl: some (
        pquote do:
          newImportAux()
          new(
            result,
            proc(destr: ref `ntype`) =
              `newPIdent(destroyCall)`(addr destr[])
          )
          {.emit: `emitStr`.}
      )
    )


  for gproc in mitems(result.methods):
    fixNames(gproc, conf, parent)


proc wrapFunction*(cd: CDecl, conf: WrapConf, cache: var WrapCache):
  seq[GenEntry] =

  var (decl, canAdd) = wrapProcedure(
    cd, conf, none NimType, cache, none CDecl, gpskDefault)

  if canAdd:
    result = decl


proc wrapTypeFromNamespace(
  ident: CScopedIdent, conf: WrapConf, cursor: CXCursor): PObjectDecl =
  ## `cursor` points to type declaration being wrapped
  # WARNING for now I assume that 'UNEXPOSED' type only occurs in
  # situations like `std::move_iterator<'0>::pointer` where typedef
  # uses it's semantic parent (class or struct declaration) to get
  # template parameters. This might not be a valid assumption in
  # general case.k

  result = PObjectDecl(
    name: conf.typeNameForScoped(ident, conf).toNType(),
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
    al: CDecl, parent: CScopedIdent, conf: WrapConf, cache: var WrapCache):
  seq[GenEntry] =
  # NOTE returning multiple values because of
  # `typedef struct A {} A, *APtr` shit that can result in multple
  # declarations, nested types (that might recursively contain who-knows-what)


  if al.isNewType:
    info "typdef with new type declaration"
    var baseType: NimType

    if al.aliasNewType.kind in {cdkClass, cdkStruct}:
      let wrapBase = al.aliasNewType.wrapObject(conf, cache)
      result.add wrapBase

      baseType = wrapBase.name

    else:
      var wrapBase = al.aliasNewType.wrapEnum(conf, cache)
      baseType = newNimType(wrapBase[0].genEnum.name)
      debug al.aliasNewType.cursor.treeRepr()
      result.add wrapBase

    for newName in al.newTypes:
      debug "Alternative name", newName
      var newType = newNimType($newName)
      newType.genericParams = baseType.genericParams
      if newType.nimName != baseType.nimName:
        # Alias names might be the same for `typedef struct St {} St;`
        result.add GenAlias(
          iinfo: currIInfo(),
          cdecl: al,
          baseType: baseType,
          newAlias: newType
        )

  else:
    # Get underlying type for alias
    let aliasof = al.cursor.cxType().getCanonicalType()
    logIndented:
      # Create new identifier for aliased type
      var newAlias = conf.typeNameForScoped(al.ident, conf)
    # debug al.ident, " -> ", newAlias

    # Identifier for old aliased type
    var baseType: NimType
    if getTypeDeclaration(aliasof).cxKind() == ckNodeclFound:
      let name = fromCxxTypeName($aliasof)
      if name.len > 0:
        baseType = newNimType(name, aliasof)

      else:
        baseType = toNimType(aliasof, conf) # .ntype # newPType($aliasof)

    else:
      baseType = conf.typeNameForScoped(aliasof.toFullScopedIdent(), conf)
      # WARNING mismatched generic parameters between lhs and rhs parts of
      # alias might result in broken wrappers.

    if baseType.hasComplexParam():
      #[
      Type alias is declared in terms of sub-alias for some of the
      parameter types. For example std::string has following sub-alias declared:

      ```c++
        template<typename _CharT, typename _Traits, typename _Alloc>

        // ...

        typedef _Traits					traits_type; // Regular alias on the argument
        typedef typename _Traits::char_type		value_type;
        // type-name-0-1::char_type.
      ```

      Nim does not have a way to model type relations like this.

      ]#
      return @[]

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
        baseType.genericParams = baseType.genericParams[
          0 ..< min(required.len(), baseType.genericParams.len())]

    fixTypeParams(baseType, newAlias.genericParams)

    if baseType.hasUnexposed():
      debug al.cursor.treeRepr()
      debug aliasof.lispRepr()

      raiseImplementError("Found unexposed type")

    else:
      # NOTE ignore `typedef struct` in C
      result.add GenAlias(
        iinfo: currIINfo(),
        isDistinct: conf.isDistinct(al.ident, conf, cache),
        newAlias: newAlias,
        baseType: baseType,
        cdecl: al
      )

      # else:
      #   if cache.canWrap(al.cursor[0]):
      #     # Multiple trailing typedefs result in several `ckTypedefDecl`
      #     # nodes

      #     cache.markWrap(al.cursor[0])
      #     let nested = visitClass(al.cursor[0], parent, conf)

      #     let (obj, pre, post) = wrapObject(nested, conf, cache)

      #     result.add pre & @[obj] & post

      #   if newAlias != baseType:
      #     # `typedef struct {} A;` has the same typedef name and type, and
      #     # should only be wrapped as type definition and not alias.
      #     result.add newWrappedEntry(
      #       toNimDecl(newAliasDecl(
      #         newAlias, baseType, iinfo = currIInfo(), isDistinct = false,
      #       )), al
      #     )

proc getParentFields*(
    inCursor: CXCursor, obj: PObjectDecl, wrapConf: WrapConf):
  seq[PProcDecl] = # FIXME return `GenProc` declarations instead of nested cursors



  for class in inCUrsor.getClassBaseCursors():
    for entry in class:
      if entry.kind in {ckFieldDecl}:
        let
          fieldType = entry.cxType().toNimType(wrapConf)
          fldname = $entry

        result.add newPProcDecl(
          name = fldName,
          rtyp = some(fieldType.toNType()),
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
            args = { "self" : obj.name, "val" : fieldType.toNType() },
            pragma = newPPragma(newExprColonExpr(
              newPIdent(wrapConf.importX()), newRStrLit(&"(#.{fldName} = @)")))
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

    let pr = initGenProc(cd, currIInfo()).withIt do:
      it.name = "init" & gen.name.nimName
      it.arguments = cd.initArgs
      it.header = conf.makeHeader(cd.cursor, conf)
      it.icpp = &"{toCppNamespace(cd.ident)}({{@}})"
      it.returnType = gen.name
      it.genParams = gen.name.genericParams

    gen.nestedEntries.add pr


proc updateFieldExport*(
  cd: CDecl, conf: WrapConf, cache: var WrapCache, gen: var GenObject) =
  # Add getter/setter methods for *all* public fields that are accessible
  # from this object
  for fld in cd.publicFields():
    var res = GenField(
      # QUESTION `conf.identNameForScoped()?`
      name: fixIdentName(fld.lastName()),
      rawName: fld.lastName(),
      iinfo: currIInfo(),
      cdecl: fld,
      fieldType: fld.cursor.cxType().toNimType(conf),
      isConst: fld.isConst
    )

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
      # Proxy enum wrapper generator changes enum names, meaning all C/C++
      # enum fields should be renamed too.
      res.fieldType.nimName &= conf.isImportCpp.tern("Cxx", "C")

    gen.memberFields.add res


proc wrapObject*(cd: CDecl, conf: WrapConf, cache: var WrapCache): GenObject =
  let tdecl = cd.cursor.cxType().getTypeDeclaration()
  assert cd.kind in {cdkClass, cdkStruct, cdkUnion}, $cd.kind

  result = GenObject(
    rawName: $cd.cursor,
    iinfo: currIInfo(),
    name: conf.typeNameForScoped(cd.ident, conf),
    cdecl: cd
  )

  assert result.name.kind == ctkIdent

  updateAggregateInit(cd, conf, cache, result)

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

proc cEnumName(str: string, nt: NimType, cache: var WrapCache): string =
  # Generate name for C enum. QUESTION: enum names don't need to be
  # perfectly accurate as they are converted to integers and then casted
  # - I'm not completely sure if this correct.
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

  var nt = conf.typeNameForScoped(declEn.ident, conf)
  result = GenEnum(
    isMacroEnum: false,
    cdecl: declEn,
    iinfo: currIInfo(),
    rawName: nt.nimName & conf.importX(),
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
        cdecl: CDecl(cursor: name),
        iinfo: currIInfo(),
        baseName: $name,
        resCName: cEnumName($name, nt, cache),
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
    iinfo: currIInfo(),
    passEntries: @[newWrappedEntry(
      toNimDecl(helpers), true, currIInfo(),
      gen.cdecl
    )]
  )



proc wrapEnum*(declEn: CDecl, conf: WrapConf, cache: var WrapCache): seq[GenEntry] =
  ## Generate wrapper for enum declaration
  ##
  ## Generates wrapper for enum declaration, using wrap configuration.
  ## Wrapping is performed in two steps - underlying C enum is wrapped
  ## as-is, and additional nim `enum` is generated. Nim version does not
  ## have holes, which allows it to be used in `array`, iterated upon etc.
  ##
  ## In order to perform conversion between 'proxy' and underlying enum
  ## several helper procs are introduces, such as `toInt`.
  let gen = makeGenEnum(
    declEn,
    declEn.enumFields.sortedByIt(it[1]).deduplicate(isSorted = true),
    conf, cache
  )

  cache.genEnums.add gen
  result.add gen
  result.add makeEnumConverters(gen, conf, cache)


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
          let op = strip(str[invert])
          result = case op:
            of "<<": some lhs shl rhs
            of ">>": some lhs shr rhs
            of "+": some lhs + rhs
            of "-": some lhs - rhs
            of "/": some lhs div rhs
            of "*": some lhs * rhs
            else: raiseImplementError(op)

      of cppTranslationUnit, cppSyntaxError, cppExpressionStatement,
         cppParenthesizedExpression
           :
        return aux(node[0])

      of cppStringLiteral, cppCastExpression:
        discard

      else:
        raiseImplementError(node.treeRepr(str))



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
  # info "Wrapping", prefix, "as", enumPref, values.len
  var enumFields: seq[GenEnumValue]
  for val in values:
    let toks = val.cursor.tokenStrings(conf.unit)
    # FIXME range breaks on `#define func(arg)`
    let value = evalTokensInt(toks[1 ..^ 1])
    let name = enumPref & toks[0].splitCamel()[1..^1].capitalAscii().join("")
    # debug name, toks.join(", ", ("<\e[31m", "\e[39m>")), value

    if value.isSome():
      enumFields.add GenEnumValue(
        cdecl: val,
        iinfo: currIInfo(),
        resNimName: name,
        resVal: value.get(),
        stringif: toks[0]
      )

  if enumFields.len > 0:
    let name = capitalAscii(enumPref) & capitalAscii(prefix)
    var en = GenEnum(
      isMacroEnum: true,
      name: name,
      proxyName: name & conf.importX(),
      iinfo: currIINfo(),
      cdecl: nil,
      values: enumFields.sortedByIt(it.resVal)
    )

    # vals = vals
    # var en: PEnumDecl = newPEnumDecl(
    #   ,
    #   iinfo = currIInfo()
    # )

    # var cEnum: PEnumDecl = newPEnumDecl(, iinfo = currIInfo())

    # for value in vals:
    #   # cEnum.addField(name & "C", )
    #   en.addField(value.resNimName, some newPLit(value.resVal))

    # result.add newWrappedEntry(toNimDecl(en), values[0])

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
      iinfo: currIInfo(),
      passEntries: @[
        newWrappedEntry(toNimDecl(helpers), true, currIInfo())])



proc wrapMacros*(
  declMacros: seq[CDecl], conf: WrapConf, cache: var WrapCache): seq[GenEntry] =
  # info "Wrapping macros"
  var prefix: seq[string]
  var buf: seq[CDecl]
  var lastSplit: seq[string]
  logIndented:
    for decl in declMacros:
      # This implementation ties to find macro names with common prefixes,
      # group them together.
      let split = split($decl.cursor, "_")
      let pref = commonPrefix(@[lastSplit, split])
      # If has any common prefix with last split, add it
      # debug $split, $lastSplit, $pref
      if pref != prefix and lastSplit.len > 0:
        buf = @[]

      if prefix.len == 0 or pref == prefix:
        if pref.len > 0 # or lastSplit.len == 0
          :
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
        err "Cannot wrap macro collection to enum"
        debug e.msg



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
        identLog()
        let spec = decl.cursor.getSpecializedCursorTemplate()

        if spec.cxKind() != ckFirstInvalid:
          discard

        else:
          result.add decl.wrapObject(conf, cache)
          for i in result:
            echo i.kind

        dedentLog()

      of cdkAlias:
        logIndented:
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
        result.add GenForward(cdecl: decl, iinfo: currIInfo())


  result.add wrapMacros(macrolist, conf, cache)

proc getNType*(carg: CArg): NimType =
  if carg.isRaw:
    raiseAssert("#[ IMPLEMENT ]#")

  else:
    return carg.nimType

proc toNNode*(gen: GenEntry, conf: WrapConf, cache: var WrapCache): seq[WrappedEntry]

proc toNNode*(gen: GenProc, wrapConf: WrapConf): PProcDecl =
  result = newPProcDecl(
    name = gen.name,
    iinfo = gen.iinfo,
    exported = true,
    rtyp = some(gen.returnType.toNType()),
    genParams = gen.genParams.mapIt(it.toNType()),
    declType = gen.declType,
    kind = gen.kind
  )

  for arg in gen.arguments:
    result.signature.arguments.add newNIdentDefs(
      vname = arg.name,
      value = arg.default,
      vtype = arg.getNTYpe().toNType(),
      kind = arg.varkind
    )

  result.docComment = gen.docComment.join("\n")

  result.signature.pragma = gen.pragma

  if gen.noPragmas notin {gpcNoImportcpp, gpcNoPragma}:
    result.signature.pragma.add(
      newExprColonExpr(
        newPIdent(wrapConf.importX()),
        newRStrLit(gen.icpp)
    ))

  if gen.noPragmas notin {gpcNoHeader, gpcNoPragma}:
    result.signature.pragma.add(
      newExprColonExpr(newPIdent "header", gen.header.toNNode()))

  if gen.impl.isSome():
    result.impl = gen.impl.get()

proc toNNode*(gen: GenEnum, conf: WrapConf): (PEnumDecl, PEnumDecl) =
  block:
    var rawEnum = newPEnumDecl(gen.rawName, iinfo = currIInfo())
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
        value.resCName, some newPLit(value.resVal),
        docComment = value.docComment.join("\n")
      )


    result[0] = rawEnum

  block:
    var nimEnum = newPEnumDecl(gen.name, iinfo = currIInfo())
    nimEnum.addDocComment gen.docComment.join("\n")
    nimEnum.exported = true
    for value in gen.values:
      nimEnum.addField(
        value.resNimName, docComment = value.docComment.join("\n")
      )

    result[1] = nimEnum

proc toNNode*(
    gen: GenObject, conf: WrapConf; cache: var WrapCache
  ): seq[WrappedEntry] =
  var decl = PObjectDecl(
    iinfo: gen.iinfo,
    name: gen.name.toNType()
  )

  assert decl.name.kind == ntkIdent, $decl.name.kind

  decl.pragma = some newPPragma(
    newPIdent("bycopy"),
    nnkExprColonExpr.newPTree(
      newPIdent(conf.importX()),
      newPLit(gen.cdecl.ident.toCppNamespace())
    ),
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
          returnType = field.fieldType.toNType()
          iinfo = currIInfo()
          genParams = gen.name.genericParams.mapIt(it.toNType())
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

        getImpl.addArgument("self", gen.name.toNType())

        result.add newWrappedEntry(
          toNimDecl(getImpl), true, field.iinfo, field.cdecl)

      block setterImplementation:
        var setImpl = newPProcDecl(field.name)
        with setImpl:
          iinfo = currIInfo()
          pragma = newPPragma(
            newPIdentColonString(
              "error",
              &"Cannot assign to field {field.name} - declared `const` in {field.cdecl.ident}"
            )
          )

        setImpl.addArgument("self", gen.name.toNtype())
        setImpl.addArgument("value", field.fieldType.toNType())

        result.add newWrappedEntry(
          toNimDecl(setImpl), true, field.iinfo, field.cdecl)

    else:
      var newField = PObjectField(
        docComment: field.docComment.join("\n"),
        isTuple: false,
        isExported: true,
        name: field.name,
        pragma: some newPPragma(
          newPIdentColonString(conf.importX, field.cdecl.lastName())
        ),
        fldType: field.fieldType.toNType()
      )

      updateComments(
        newField,
        newWrappedEntry(toNimDecl(newField), false, field.iinfo, field.cdecl),
        conf, cache
      )

      decl.flds.add newField

  for nested in gen.nestedEntries:
    result.add nested.toNNode(conf, cache)

  var obj = newWrappedEntry(
    toNimDecl(decl), true, gen.iinfo, gen.cdecl)

  result.add obj

  for meth in gen.memberMethods:
    result.add toNNode(meth, conf).toNimDecl().newWrappedEntry(
      true, meth.iinfo, meth.cdecl)


proc toNNode*(gen: GenAlias, conf: WrapConf): AliasDecl[PNode] =
  result = AliasDecl[PNode](
    iinfo: gen.iinfo,
    docComment: gen.docComment.join("\n"),
    isDistinct: gen.isDistinct,
    isExported: true,
    oldType: gen.baseType.toNType(),
    newType: gen.newAlias.toNType()
  )

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



proc toNNode*(gen: GenImport, conf: WrapConf): WrappedEntry =
  ## Convert import passthough wrapped entry.
  var decl = NimDecl[PNode](
    kind: nekPassthroughCode,
    passthrough: gen.importSpec.toNNode(),
    passIInfo: gen.iinfo
  )

  return newWrappedEntry(decl, false, gen.iinfo)

proc toNNode*(gen: GenEntry, conf: WrapConf, cache: var WrapCache): seq[WrappedEntry] =
  case gen.kind:
    of gekEnum:
      let (e1, e2) = toNNode(gen.genEnum, conf)
      result.add toNimDecl(e1).newWrappedEntry(
        true, gen.genEnum.iinfo, gen.cdecl)

      result.add toNimDecl(e2).newWrappedEntry(
        true, gen.genEnum.iinfo, gen.cdecl)

    of gekPass:
      result.add gen.genPass.passEntries

    of gekAlias:
      result.add toNNode(gen.genAlias, conf).
        toNimDecl().
        newWrappedEntry(true, gen.genAlias.iinfo, gen.cdecl)

    of gekObject:
      result.add toNNode(gen.genObject, conf, cache)

    of gekProc:
      result.add toNNode(gen.genProc, conf).toNimDecl().newWrappedEntry(
        true, gen.genProc.iinfo, gen.cdecl)

    of gekImport:
      result.add toNNode(gen.genImport, conf)

    of gekForward:
      raiseUnexpectedKindError(
        gen, "Forward declaration nodes should be converted to import/pass")

proc writeWrapped*(
    res: CodegenResult,
    outFile: FsFile,
    compile: seq[FsFile],
    wrapConf: WrapConf
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

  if wrapConf.codegenDir.isSome():
    let dir = wrapConf.codegenDir.get()
    for gen in res.codegen:
      let target = dir /. gen.filename
      let res = target.withBasePrefix("gen_")
      # info "Writing generated code into", res
      if $target notin resFiles:
        resFiles[$target] = open(res, fmWrite)
        resFiles[$target].write(gen.header)

      resFiles[$target].write(gen.code)

    var content = newJArray()
    for (ident, dox) in res.cache.identRefidMap:
      content.add %[ident.toHaxdocJson(), newJString(dox)]

    writeFile(dir / wrapConf.refidFile, pretty(content))

  for _, file in pairs(resFiles):
    file.close()
      # writeFile(target, gen.code)
