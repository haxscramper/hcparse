import ./hc_types, ./cxtypes, ./cxvisitors, ./hc_typeconv, ./cxcommon

import hnimast

import std/[sequtils, strutils]
import hmisc/other/[colorlogger, oswrap]
import hmisc/algo/hstring_algo
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
      # if getDepResolutionKind(cursor) == drkWrapDirectly:
        return callback.impl(cursor, mainParent, addr callback.data)
      else:
        # debug cursor
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
    conf: WrapConfig, lastTypeDecl: var CDecl
  ): tuple[decls: seq[CDecl], recurse: bool, includes: seq[IncludeDep]]


proc visitClass*(
    cursor: CXCursor, parent: CScopedIdent,
    conf: WrapConfig, typedef: Option[CXCursor]
  ): CDecl

proc visitMethod*(
    cursor: CXCursor, parent: CScopedIdent, accs: CXAccessSpecifier
  ): CDecl =

  result = CDecl(
    kind: cdkMethod,
    cursor: cursor,
    ident: parent & toCName(cursor),
    access: accs,
    arguments: cursor.getArguments(),
    isOperator: isOperator(cursor)
  )

  # result.access = accs
  # result.arguments = cursor.getArguments()

  if isOperator(result):
    result.operatorKind = result.classifyOperator()
    result.operatorName = result.lastName().dropPrefix("operator")


proc visitField*(
    cursor: CXCursor, parent: CSCopedIdent, accs: CXAccessSpecifier,
    conf: WrapConfig
  ): CDecl =

  result = CDecl(
    kind: cdkField, cursor: cursor, ident: parent & toCName(cursor))
  result.access = accs

  if cursor.len > 0:
    var decl: CDecl
    let visit = visitCursor(cursor[0], parent, conf, decl)
    result.fieldTypeDecl = some decl


var undefCnt: int = 0

# proc visitAlias*(
#   cursor: CXCursor, parent: CScopedIdent, conf: WrapConfig): CDecl =
#   result = CDecl(
#     kind: cdkAlias, cursor: cursor, ident: parent & toCName(cursor))

proc visitFunction*(
  cursor: CXCursor, parent: CScopedIdent, conf: WrapConfig): CDecl =
  result = CDecl(
    kind: cdkFunction,
    cursor: cursor,
    ident: parent & toCName(cursor),
    isOperator: cursor.isOperator()
  )

  if result.isOperator:
    result.operatorKind = result.classifyOperator()
    result.operatorName = result.lastName().dropPrefix("operator")

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
      else:
        warn "Unknown element", subn.cxKind, cast[int](subn.cxKind), subn, $cursor
        debug subn.getSpellingLocation()
        debug cursor.treeRepr()


proc visitEnum*(
  cursor: CXcursor, parent: CScopedIdent, conf: WrapConfig): CDecl =
  result = CDecl(
    kind: cdkEnum,
    cursor: cursor,
    ident: parent & toCName(cursor),
    isAnonymous: $cursor == ""
  )

  for elem in cursor:
    result.enumFields.add (elem, some(elem[0]))

  # info "Found enum", result.name

proc getDefaultAccess*(cursor: CXCursor): CXAccessSpecifier =
  if cursor.cxKind in {ckStructDecl, ckUnionDecl}:
    asPublic
  else:
    asPrivate


proc isAggregateInitable*(
  cd: CXCursor, initArgs: var seq[CArg], conf: WrapConfig): bool =
  ## Determine if entry pointed to by `cd`'s cursor is subject to aggregate
  ## initalization. Add all fields for aggregate initalization into
  ## @arg{initArgs}. NOTE: fields will be added unconditionally, so first
  ## check return value.

  if cd.cxKind() in {ckUnionDecl, ckEnumDecl}:
    return false

  elif cd.cxKind() notin {ckClassDecl, ckStructDecl, ckClassTemplate}:
    assertionFail:
      "Invalid cursor kind of aggregate initalization check."
      "Expected type declaration (union/enum/struct/class),"
      "but found {toRed($cd.cxKind())}"

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
  var access = cd.getDefaultAccess()
  for entry in cd:
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
        raiseImplementKindError(entry)


proc updateParentFields*(decl: var CDecl, conf: WrapConfig) =
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
            buf.derived.add visitField(entry, decl.ident, accs, conf)

          of ckMethod:
            buf.derived.add visitMethod(entry, decl.ident, accs)

          else:
            raiseImplementKindError(entry)

proc visitAlias*(
    lastTypeDecl: var CDecl, parent: CSCopedIdent,
    subn: CXCursor, conf: WrapConfig
  ): Option[CDecl] =

  if subn[0].cxKind() in {ckEnumDecl, ckStructDecl, ckUnionDecl, ckClassDecl}:
    # libclang represents grouped typedefs using *multiple* nodes, so
    # `typedef struct S1 {} S2, *S3` will appear *three times* in the clang
    # IR. It is not really convenient to work with, as it requires additional
    # layer of bookeeping to avoud duplication of identifiers.
    if lastTypeDecl.cursor == subn[0]:
      # Second encounter of the typedefed struct
      if lastTypeDecl.isAnonymous:
        # FIXME use list of all type declaration kinds
        if subn[0].cxKind() in {ckStructDecl}:
          # Type declaration itself is anonymous, but a part of larger
          # `typedef` statement that declares name that is used in other
          # code (to avoid `struct` namespacing in C I guess).

          # warn "First declaration was considered anonymous, second visitation"
          # logIndented:
            lastTypedecl = visitClass(subn[0], parent, conf, some(subn))
            lastTypeDecl.isCTypedef = true


      debug "Found typedef struct", subn
      lastTypeDecl = CDecl(
        ident: lastTypeDecl.ident,
        cursor: subn,
        kind: cdkAlias,
        newTypes: @[subn],
        isNewType: true,
        aliasNewType: lastTypeDecl
      )

      # if $lastTypeDecl.aliasNewType.cursor == "":
      #   lastTypeDecl.aliasNewType.isCTypedef = true
      #   # lastTypeDecl.aliasNewType.ident[^1] = toCName(subn)
      #   # lastTypeDecl.ident[^1] = toCName(subn)

    elif lastTypeDecl.kind == cdkAlias and
         lastTypeDecl.isNewType and
         lastTypeDecl.aliasNewType.cursor == subn[0]:
      # More trailing typedefs for existing declaration
      lastTypeDecl.newTypes.add subn

    else:
      raiseImplementError(
        "New typedef without previously visited declaration")

  else:
    raiseImplementError("")
    # result = some CDecl(
    # )


  # if subn[0] == lastTypeDecl:


proc visitClass*(
    cursor: CXCursor, parent: CScopedIdent,
    conf: WrapConfig, typedef: Option[CXCursor]
  ): CDecl =

  ## Convert class under cursor to `CDecl`
  let name =
    if cursor.cxKind == ckStructDecl and len($cursor) == 0:
      $cursor.cxType()
    else:
      $cursor

  var kind =
    case cursor.cxKind():
      of ckClassDecl, ckClassTemplate, ckClassTemplatePartialSpecialization:
        cdkClass

      of ckUnionDecl:
        cdkUnion

      of ckStructDecl:
        cdkStruct

      else:
        raiseUnexpectedKindError(cursor)

  var initArgs: seq[CArg]
  result = CDecl(
    isAnonymous: $cursor == "",
    kind: cdkClass,
    cursor: cursor,
    ident: parent & toCName(
      if typedef.isSome(): typedef.get() else: cursor),
    isAggregateInit: isAggregateInitable(cursor, initArgs, conf)
  )

  debug "Visited class declaration. Ident: ", result.ident
  debug "Cursor: ", cursor

  result.kind = kind
  if result.isAggregateInit:
    result.initArgs = initArgs

  updateParentFields(result, conf)

  var currentAccs = cursor.getDefaultAccess()

  var lastTypeDecl: CDecl

  for subn in cursor:
    if subn.cxKind == ckAccessSpecifier:
      currentAccs = subn.getAccessSpecifier()

    if currentAccs == asPublic and
       not conf.ignoreCursor(subn, conf):

      case subn.cxKind:
        of ckMethod, ckConversionFunction, ckConstructor, ckDestructor:
          result.members.add visitMethod(subn, result.ident, currentAccs)

        of ckAccessSpecifier:
          currentAccs = subn.getAccessSpecifier()

        of ckFieldDecl, ckVarDecl:
           # WARNING static fields might need to be wrapped differently
          result.members.add visitField(
            subn, result.ident, currentAccs, conf)

        of ckTemplateTypeParameter, ckFriendDecl,
           ckStaticAssert, ckTemplateTemplateParameter:
          discard

        of ckFunctionTemplate:
          result.members.add visitFunction(subn, result.ident, conf)

        of ckTypeAliasTemplateDecl, ckTypeAliasDecl,
           ckTypedefDecl, ckUsingDeclaration:

          let alias = visitAlias(lastTypeDecl, result.ident, subn, conf)
          if alias.isSome():
            result.members.add alias.get()
          # result.members.add visitAlias(subn, result.ident, conf)

        of ckStructDecl, ckClassDecl, ckUnionDecl, ckClassTemplate:
          if not isNil(lastTypeDecl): result.members.add lastTypeDecl

          lastTypeDecl = visitClass(subn, result.ident, conf, none(CXCursor))

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

        else:
          inc undefCnt
          if undefCnt > 20:
            raiseAssert("Reached unknown class element limit")
          else:
            discard
            warn "IMPLEMENT class element:", ($subn.cxKind).toRed(), subn
            # debug subn.treeRepr()



proc visitNamespace*(
  cursor: CXCursor, parent: CScopedIdent, conf: WrapConfig): seq[CDecl] =
  ## Convert all elements in namespace into sequence of `CDecl`
  ## elements.

  var parent = parent
  if not (cursor.isInlineNamespace() == 1):
    parent &= toCName(cursor)

  var lastTypeDecl: CDecl
  if not conf.ignoreCursor(cursor, conf):
    for subn in cursor:
      result.add visitCursor(subn, parent, conf, lastTypeDecl).decls

proc visitMacrodef*(
  cursor: CXCursor, parent: CScopedIdent, conf: WrapConfig): CDecl =
  CDecl(cursor: cursor, kind: cdkMacro)



proc visitCursor*(
    cursor: CXCursor, parent: CScopedIdent,
    conf: WrapConfig, lastTypeDecl: var CDecl
  ): tuple[decls: seq[CDecl], recurse: bool, includes: seq[IncludeDep]] =

  if conf.ignoreCursor(cursor, conf):
    info "Ignoring cursor ", cursor

  else:
    case cursor.cxKind:
      of ckNamespace:
        result.decls.add visitNamespace(cursor, parent, conf)

      of ckClassDecl, ckClassTemplate, ckUnionDecl,
         ckClassTemplatePartialSpecialization,
         ckStructDecl:
        if not isNil(lastTypeDecl): result.decls.add lastTypeDecl

        lastTypeDecl = visitClass(cursor, parent, conf, none(CXCursor))

      of ckFunctionDecl, ckFunctionTemplate:
        result.decls.add visitFunction(cursor, parent, conf)

      of ckTypedefDecl, ckTypeAliasDecl:
        let alias = visitAlias(lastTypeDecl, parent, cursor, conf)
        if alias.isSome():
          result.decls.add alias.get()

      of ckEnumDecl:
        if not isNil(lastTypeDecl): result.decls.add lastTypeDecl

        lastTypeDecl = visitEnum(cursor, parent, conf)

      of ckInclusionDirective:
        let loc = cursor.getSpellingLocation().get()
        result.includes.add IncludeDep(
          includedAs: $cursor,
          includedFrom: loc.file,
          fromLine: loc.line,
          fromColumn: loc.column,
          fromOffset: loc.offset,
          includedPath: AbsFile($cursor.getIncludedFile()).realpath
        )

      of ckMacroDefinition:
        result.decls.add visitMacrodef(cursor, parent, conf)

      else:
        # warn "Recursing on", cursor, "of kind", cursor.cxKind()
        result.recurse = true

proc getPublicAPI*(cd: CDecl): seq[CXCursor] =
  ## Get list of cursors referring to parts of the public API for a
  ## declaration: return and argument types for functions and methods,
  ## public fields for objects.
  case cd.kind:
    of cdkClass, cdkStruct, cdkUnion:
      for member in cd.members:
        result.add member.getPublicAPI()

    of cdkField:
      if cd.access == asPublic:
        return @[ cd.cursor ]

      else:
        return @[]

    of cdkFunction, cdkMethod:
      let exportd: bool = (cd.kind == cdkFunction) or
        (cd.access == asPublic)

      if exportd:
        result.add cd.cursor
        for arg in cd.arguments:
          result.add arg.cursor

    of cdkAlias:
      return @[cd.cursor]

    of cdkEnum:
      return @[]

    of cdkMacro:
      return @[]



proc splitDeclarations*(
  tu: CXTranslationUnit, conf: WrapConfig): CApiUnit =
  ## Convert main file of translation unit into flattened sequence of
  ## high-level declarations. All cursors for objects/structs are
  ## retained. Public API elements are stored in `publicAPI` field
  assert not tu.isNil
  let tuCursor = tu.getTranslationUnitCursor()
  var res: CApiUnit
  var lastTypeDecl: CDecl
  tuCursor.visitChildren do:
    makeVisitor [tu, res, conf, tuCursor, lastTypeDecl]:
      let resolve = conf.depResolver(cursor, tuCursor)
      if resolve == drkWrapDirectly:
        let (decls, rec, incls) = visitCursor(
          cursor, @[], conf, lastTypeDecl)
        res.includes.add incls
        if rec:
          return cvrRecurse

        else:
          res.decls.add decls
          for decl in decls:
            res.publicAPI.add decl.getPublicAPI()

          return cvrContinue

      else:
        return cvrRecurse

  res.decls.add lastTypeDecl

  return res
