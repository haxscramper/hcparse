import ./hc_types, ./cxtypes, ./cxvisitors

import ../hc_typeconv

import std/[sequtils, strutils]
import hmisc/other/[hlogger]
import hmisc/algo/hstring_algo
import hmisc/core/all
import hmisc/types/colorstring


proc visitMainFile*[T](
  cursor: CXCursor,
  callback: tuple[
    data: T,
    impl: proc(a, b: CXCursor,
               clientData: pointer): CXChildVisitResult {.cdecl.}
  ]) =
  ## Call visitor in each subnode if it is in main file
  var mainParent = cursor
  cursor.visitChildren do:
    makeVisitor [callback, mainParent]:
      if isFromMainFile(cursor):
        return callback.impl(cursor, mainParent, addr callback.data)

      else:
        return cvrRecurse

#===========  Splitting translation unit into logical chunks  ============#
proc getArguments*(cursor: CXCursor): seq[CArg] =
  for idx, subn in cursor.children():
    if subn.cxKind in {ckParmDecl}:
      var name = $subn
      if name.len == 0:
        name = "a" & $idx

      result.add initCArg(name, subn)


proc argsSignature*(
  args: seq[CArg], types: bool = true, names: bool = true,
  wrap: (bool, bool) = (false, false)): string =

  if args.len > 0 and wrap[0]:
    result = ","

  if types and names:
    result &= args.mapIt($it.cursor.cxType() & " " & it.name).join(", ")

  elif types:
    result &= args.mapIt($it.cursor.cxType()).join(", ")

  elif names:
    result &= args.mapIt(it.name).join(", ")

  else:
    raiseAssert("Either `types` or `arguments` must be enabled")

  if args.len > 0 and wrap[1]:
    result &= ","



proc argsSignature*(
  cursor: CXCursor, types: bool = true, names: bool = true,
  wrap: (bool, bool) = (false, false)): string =

  argsSignature(cursor.getArguments(), types, names, wrap)

proc visitCursor*(
    cursor: CXCursor, parent: CScopedIdent,
    conf: WrapConf, lastTypeDecl: var CDecl,
    cache: var WrapCache
  ): tuple[decls: seq[CDecl], recurse: bool]


proc visitClass*(
    cursor: CXCursor, parent: CScopedIdent,
    conf: WrapConf, typedef: Option[CXCursor],
    cache: var WrapCache,
  ): CDecl

proc visitMethod*(
    cursor: CXCursor, parent: CScopedIdent, accs: CXAccessSpecifier,
    conf: WrapConf
  ): CDecl =

  result = CDecl(
    kind: cdkMethod,
    cursor: cursor,
    ident: parent & toCName(cursor),
    access: accs,
    arguments: cursor.getArguments(),
    isOperator: isOperator(cursor)
  )

  if isOperator(result, conf):
    result.operatorKind = result.classifyOperator(conf)
    result.operatorName = result.lastName(conf).dropPrefix("operator")


proc visitField*(
    cursor: CXCursor, parent: CSCopedIdent, accs: CXAccessSpecifier,
    conf: WrapConf, cache: var WrapCache
  ): CDecl =

  result = CDecl(
    kind: cdkField, cursor: cursor,
    ident: parent & toCName(cursor),
    isConst: isConstQualified(cursor.cxType())
  )

  result.access = accs

  if cursor.len > 0:
    var decl: CDecl
    discard visitCursor(cursor[0], parent, conf, decl, cache)
    if not isNil(decl):
      result.fieldTypeDecl = some decl


var undefCnt: int = 0

proc visitFunction*(
  cursor: CXCursor, parent: CScopedIdent, conf: WrapConf): CDecl =
  result = CDecl(
    kind: cdkFunction,
    cursor: cursor,
    ident: parent & toCName(cursor),
    isOperator: cursor.isOperator()
  )

  if result.isOperator:
    result.operatorKind = result.classifyOperator(conf)
    result.operatorName = result.lastName(conf).dropPrefix("operator")

  result.arguments = cursor.getArguments()

  for subn in cursor:
    case subn.cxKind:
      of ckTemplateTypeParameter, ckNonTypeTemplateParameter,
         ckTemplateRef
           :
        discard

      of ckParmDecl, ckTypeRef, ckNamespaceRef,
         ckFirstExpr, ckCompoundStmt, ckMemberRef,
         ckFirstAttr, ckVisibilityAttr
           :
         # WARNING right now these things are just droppped. Maybe it
         # will cause some errors in the future, I don't really know.
        discard

      of ckCallExpr, ckBinaryOperator, ckUnaryOperator:
        # NOTE first encountered in `range_access.h:131`
        #```
        # template<typename _Container>
        #   inline constexpr auto
        #   cend(const _Container& __cont) noexcept(noexcept(std::end(__cont)))
        #     -> decltype(std::end(__cont))
        #   { return std::end(__cont); }
        #```
        #
        # HACK until I figure out how to correctly translate
        # `decltype(std::end(__const))` (call to templated method of the
        # class) to nim I'm going to mark this template as 'complex'.
        result.complexTemplate = true

      else:
        conf.warn "Unknown element", subn.cxKind, cast[int](subn.cxKind),
                          subn, $cursor

        conf.debug subn.getSpellingLocation()
        conf.debug cursor.treeRepr()


proc visitEnum*(
  cursor: CXcursor, parent: CScopedIdent, conf: WrapConf): CDecl =
  result = CDecl(
    kind: cdkEnum,
    cursor: cursor,
    ident: parent & toCName(cursor),
    isAnonymous: $cursor == ""
  )

  for elem in cursor:
    result.enumFields.add (elem, getEnumConstantDeclValue(elem))

  # info "Found enum", result.name

proc getDefaultAccess*(cursor: CXCursor): CXAccessSpecifier =
  if cursor.cxKind in { ckStructDecl, ckUnionDecl }:
    asPublic

  else:
    asPrivate


proc isAggregateInitable*(
    cd: CXCursor, initArgs: var seq[CArg],
    conf: WrapConf, cache: var WrapCache
  ): bool =
  ## Determine if entry pointed to by `cd`'s cursor is subject to aggregate
  ## initalization. Add all fields for aggregate initalization into
  ## @arg{initArgs}. NOTE: fields will be added unconditionally, so first
  ## check return value.

  when false:

    if cd.cxKind() in {
      ckUnionDecl, ckEnumDecl,

      # NOTE I think in some cases it /might/ be possible to use aggregate
      # initalization for template types, but at this point it is easier to
      # just fail check on this kind immediately.
      ckClassTemplatePartialSpecialization
    }:
      return false

    elif cd.cxKind() notin ckTypeDeclKinds:
      raise newLogicError(
        "Invalid cursor kind of aggregate initalization check.",
        "Expected type declaration (union/enum/struct/class),",
        "but found {toRed($cd.cxKind())} at {cd.getSpellingLocation()}\n",
        "{cd.treeRepr()}")

    # List of entries that immediately mean aggregate initalization is not
    # supported.
    const failKinds = {
      ckConstructor,
      ckConversionFunction
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
      ckAlignedAttr,
      ckTypeAliasDecl,
      ckNonTypeTemplateParameter,
      ckFriendDecl,
      ckFinalAttr,
      ckWarnUnusedAttr,
      ckWarnUnusedResultAttr,
      ckClassDecl,
      ckEnumDecl,
      ckNamespaceRef,
      ckStaticAssert,
      ckClassTemplate,
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

        of tkInvalid, tkUnexposed, tkEnum,
           tkLValueReference
             :
          return false

        of tkConstantArray:
          return aux(cursor[0])

        else:
          conf.debug cast[int](cursor.cxType().cxKind())
          conf.debug cursor.getSpellingLocation()
          conf.debug cursor.treeRepr(conf.unit)
          conf.err cursor.cxType().cxKind()
          conf.err cursor.cxType()
          raiseAssert("#[ IMPLEMENT ]#")

    result = true
    var access = cd.getDefaultAccess()
    for entry in cd:
      case entry.cxKind():
        of ckFieldDecl:
          if aux(entry):
            var arg = initCArg(
              $entry, entry.cxType().toNimType(conf, cache), nvdLet)

            setDefaultForArg(arg, entry, conf, cache)
            initArgs.add arg

        of failKinds: return false
        of ignoreKinds: discard
        of ckVarDecl: discard
        of ckTypeRef, ckTemplateRef: discard

        of ckAccessSpecifier:
          access = entry.getAccessSpecifier()

        of ckBaseSpecifier:
          if aux(entry[0]):
            discard

          else:
            return false

        else:
          return false


proc updateParentFields*(
    decl: var CDecl, conf: WrapConf, cache: var WrapCache) =

  for parent in decl.cursor.getClassBaseCursors():
    var buf = ParentDecl(cursor: parent)
    var accs = parent.getDefaultAccess()
    for entry in parent:
      if accs != asPublic:
        if entry.cxKind() == ckAccessSpecifier:
          accs = entry.getAccessSpecifier()

      else:
        case entry.cxKind():
          of ckFieldDecl:
            buf.derived.add visitField(entry, decl.ident, accs, conf, cache)

          of ckMethod:
            # FIXME not handling method overrides.
            buf.derived.add visitMethod(entry, decl.ident, accs, conf)

          of ckAccessSpecifier:
            accs = entry.getAccessSpecifier()

          else:
            # WARNING possibly missing on various edge cases like template
            # methods in parent classes.
            discard

proc visitAlias*(
    lastTypeDecl: var CDecl, parent: CSCopedIdent,
    cursor: CXCursor, conf: WrapConf, cache: var WrapCache
  ): Option[CDecl] =



  if cursor[0].cxKind() in ckTypeDeclKinds:
    # libclang represents grouped typedefs using *multiple* nodes, so
    # `typedef struct S1 {} S2, *S3` will appear *three times* in the clang
    # IR. It is not really convenient to work with, as it requires additional
    # layer of bookeeping to avoud duplication of identifiers.
    if lastTypeDecl.cursor == cursor[0]:
      # Second encounter of the typedefed struct
      if lastTypeDecl.isAnonymous:
        # FIXME use list of all type declaration kinds
        if cursor[0].cxKind() in {ckStructDecl}:
          # Type declaration itself is anonymous, but a part of larger
          # `typedef` statement that declares name that is used in other
          # code (to avoid `struct` namespacing in C I guess).

          # warn "First declaration was considered anonymous, second visitation"
          # logIndented:
            lastTypedecl = visitClass(cursor[0], parent, conf, some(cursor), cache)
            lastTypeDecl.isCTypedef = true


      conf.debug "Found typedef struct", cursor
      lastTypeDecl = CDecl(
        ident: lastTypeDecl.ident,
        cursor: cursor,
        kind: cdkAlias,
        newTypes: @[cursor],
        isNewType: true,
        aliasNewType: lastTypeDecl
      )

    elif lastTypeDecl.kind == cdkAlias and
         lastTypeDecl.isNewType and
         lastTypeDecl.aliasNewType.cursor == cursor[0]:
      # More trailing typedefs for existing declaration
      lastTypeDecl.newTypes.add cursor

    else:
      raiseImplementError(
        "New typedef without previously visited declaration")

  elif cursor[0].cxKind() in {ckTemplateRef}:
    result = some CDecl(
      isNewType: false,
      cursor: cursor,
      aliasBaseType: cursor[0],
      ident: parent & toCName(cursor),
      kind: cdkAlias
    )

  else:
    lastTypeDecl = CDecl(
      ident: parent & toCName(cursor),
      cursor: cursor,
      kind: cdkAlias,
      newTypes: @[cursor],
      isNewType: false,
      aliasBaseType: cursor
    )


proc visitClass*(
    cursor: CXCursor, parent: CScopedIdent,
    conf: WrapConf, typedef: Option[CXCursor],
    cache: var WrapCache
  ): CDecl =

  var kind =
    case cursor.cxKind():
      of ckClassDecl, ckClassTemplate, ckClassTemplatePartialSpecialization:
        cdkClass

      of ckUnionDecl:
        cdkUnion

      of ckStructDecl:
        cdkStruct

      else:
        raise newUnexpectedKindError(cursor)

  var initArgs: seq[CArg]
  var ident = parent
  case cursor[0].kind:
    of ckTypeRef:
      # `class locale::id` is parsed as, so I need to get semantic
      # namespaces directly. Maybe it would be better to switch for that
      # altogether?
      #
      # ```
      # kind: ClassDecl id:
      #     +-> type: std::locale::id
      #     +-> TypeRef:
      #     |   +-> type: std::locale
      #     |   +-> class std::locale
      # ```
      ident = conf.getSemanticNamespaces(cursor).mapIt(toCName(it))

    else:
      ident.add toCName(if typedef.isSome(): typedef.get() else: cursor)



  result = CDecl(
    isAnonymous: $cursor == "",
    kind: cdkClass,
    cursor: cursor,
    icpp: ident.toCppNamespace(),
    ident: ident,
    isAggregateInit: isAggregateInitable(
      cursor, initArgs, conf, cache))

  {.cast(uncheckedAssign).}:
    {.warning[CaseTransition]:off.}:
      result.kind = kind

  if not conf.isImportCpp:
    result.icpp = "struct " & result.icpp

  if result.isAggregateInit:
    result.initArgs = initArgs

  updateParentFields(result, conf, cache)

  var currentAccs = cursor.getDefaultAccess()

  var lastTypeDecl: CDecl

  var params: seq[CxCursor]
  for subn in cursor:
    if subn.cxKind == ckAccessSpecifier:
      currentAccs = subn.getAccessSpecifier()

    elif subn.cxKind in {
      ckTemplateTypeParameter, ckTemplateTemplateParameter
    }:
      params.add subn

    if currentAccs == asPublic and
       not conf.ignoreCursor(subn, conf):

      case subn.cxKind:
        of ckMethod, ckConversionFunction, ckConstructor, ckDestructor:
          result.members.add visitMethod(
            subn, result.ident, currentAccs, conf)

        of ckAccessSpecifier:
          currentAccs = subn.getAccessSpecifier()

        of ckFieldDecl, ckVarDecl:
           # WARNING static fields might need to be wrapped differently
          result.members.add visitField(
            subn, result.ident, currentAccs, conf, cache)

        of ckTemplateTypeParameter, ckTemplateTemplateParameter:
          params.add subn

        of ckFriendDecl, ckStaticAssert:
          discard

        of ckFunctionTemplate:
          result.members.add visitFunction(subn, result.ident, conf)

        of ckTypeAliasTemplateDecl, ckTypeAliasDecl,
           ckTypedefDecl, ckUsingDeclaration:

          let alias = visitAlias(
            lastTypeDecl, result.ident, subn, conf, cache)

          if alias.isSome():
            result.members.add alias.get()

        of ckStructDecl, ckClassDecl, ckUnionDecl, ckClassTemplate:
          if not isNil(lastTypeDecl): result.members.add lastTypeDecl

          lastTypeDecl = visitClass(
            subn, result.ident, conf, none(CXCursor), cache)

        of ckEnumDecl:
          if not isNil(lastTypeDecl): result.members.add lastTypeDecl

          lastTypeDecl = visitEnum(subn, result.ident, conf)

        of ckBaseSpecifier:
          # Base class specifier is ignored. All additional wrapping
          # operations should happend afterwards.
          discard

        of ckVisibilityAttr:
          # Visibility attributes are ignored for now
          discard

        of ckTypeRef, ckTemplateRef:
          # First found in std strign, need to find better location
          discard

        else:
          # inc undefCnt
          if undefCnt > 20:
            conf.debug subn.getSpellingLocation()
            raiseAssert("Reached unknown class element limit")

          else:
            discard
            conf.warn "IMPLEMENT class element:", ($subn.cxKind).toRed(), subn
            # debug subn.treeRepr()

  cache.setParamsForType(conf, ident, params)


proc visitNamespace*(
    cursor: CXCursor, parent: CScopedIdent, conf: WrapConf,
    cache: var WrapCache
  ): seq[CDecl] =
  ## Convert all elements in namespace into sequence of `CDecl`
  ## elements.

  var parent = parent
  if not (cursor.isInlineNamespace() == 1):
    parent &= toCName(cursor)

  var lastTypeDecl: CDecl
  if not conf.ignoreCursor(cursor, conf):
    for subn in cursor:
      result.add visitCursor(
        subn, parent, conf, lastTypeDecl, cache).decls

  if not isNil(lastTypeDecl):
    result.add lastTypeDecl

proc visitMacrodef*(
  cursor: CXCursor, parent: CScopedIdent, conf: WrapConf): CDecl =
  CDecl(
    cursor: cursor,
    kind: cdkMacro,
    ident: @[toCName(cursor)]
  )


proc visitCursor*(
    cursor: CXCursor, parent: CScopedIdent,
    conf: WrapConf, lastTypeDecl: var CDecl,
    cache: var WrapCache
  ): tuple[decls: seq[CDecl], recurse: bool] =

  const classDeclKinds = {
    ckClassDecl, ckClassTemplate, ckUnionDecl,
    ckClassTemplatePartialSpecialization,
    ckStructDecl
  }

  if cursor.isForward() and
     cursor.cxKind() in (classDeclKinds + {ckEnumDecl}):
    # Early return for method forward declarations.
    result.decls.add CDecl(
      kind: cdkForward,
      cursor: cursor,
      ident: parent & toCName(cursor))

  if conf.ignoreCursor(cursor, conf):
    if cursor.cxKind() in {ckNamespace}:
      conf.info "Ignoring namespace", cursor, "with elements"
      conf.logger.indented:
        for entry in cursor:
          conf.debug entry

  else:
    case cursor.cxKind:
      of ckNamespace:
        result.decls.add visitNamespace(cursor, parent, conf, cache)

      of classDeclKinds:
        if not isNil(lastTypeDecl): result.decls.add lastTypeDecl
        lastTypeDecl = visitClass(cursor, parent, conf, none(CXCursor), cache)

      of ckFunctionDecl, ckFunctionTemplate:
        result.decls.add visitFunction(cursor, parent, conf)

      of ckMethod:
        # Method definition in toplevel
        discard

      of ckTypedefDecl, ckTypeAliasDecl:
        let alias = visitAlias(
          lastTypeDecl, parent, cursor, conf, cache)

        if alias.isSome():
          result.decls.add alias.get()

      of ckEnumDecl:
        if not isNil(lastTypeDecl): result.decls.add lastTypeDecl

        lastTypeDecl = visitEnum(cursor, parent, conf)

      of ckInclusionDirective:
        result.recurse = false

      of ckMacroDefinition:
        result.decls.add visitMacrodef(cursor, parent, conf)

      else:
        result.recurse = true

proc splitDeclarations*(
  tu: CXTranslationUnit, conf: WrapConf, cache: var WrapCache): CApiUnit =
  assert not tu.isNil
  let tuCursor = tu.getTranslationUnitCursor()
  var res: CApiUnit
  var lastTypeDecl: CDecl
  tuCursor.visitChildren do:
    makeVisitor [tu, res, conf, tuCursor, lastTypeDecl, cache]:
      let resolve = conf.depResolver(cursor, tuCursor)
      if resolve == drkWrapDirectly:
        let (decls, rec) = visitCursor(cursor, @[], conf, lastTypeDecl, cache)

        if rec:
          return cvrRecurse

        else:
          res.decls.add decls
          for decl in decls:
            assert not isNil(decl)

          return cvrContinue

      else:
        return cvrRecurse

  if not isNil(lastTypeDecl):
    res.decls.add lastTypeDecl

  return res
