import hc_types, cxtypes, cxvisitors

import hnimast

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
    makeVisitor [callback, mainParent, getDepResolutionKind]:
      if getDepResolutionKind(cursor) == drkWrapDirectly:
        return callback.impl(cursor, mainParent, addr callback.data)
      else:
        debug cursor
        return cvrRecurse

#===========  Splitting translation unit into logical chunks  ============#
proc getArguments*(cursor: CXCursor): seq[CArg] =
  for idx, subn in cursor.children():
    if subn.cxKind in {ckParmDecl}:
      var name = $subn
      if name.len == 0:
        name = "a" & $idx

      result.add CArg(name: name, cursor: subn)

proc visitMethod*(cursor: CXCursor, accs: CX_AccessSpecifier): CDecl =
  result = CDecl(kind: cdkMethod, cursor: cursor, name: newPType $cursor)
  result.accs = accs
  result.args = cursor.getArguments()


proc visitField*(cursor: CXCursor, accs: CX_AccessSpecifier): CDecl =
  result = CDecl(kind: cdkField, cursor: cursor, name: newPType $cursor)
  result.accs = accs

var undefCnt: int = 0

proc visitAlias*(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  result = CDecl(kind: cdkAlias, cursor: cursor, namespace: parent)
  result.name = newPType $cursor

proc visitFunction*(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  result = CDecl(kind: cdkFunction, cursor: cursor, namespace: parent)
  result.name = newPType $cursor
  result.args = cursor.getArguments()

  for subn in cursor:
    case subn.cxKind:
      of ckTemplateTypeParameter:
        result.name.add newPType($subn)
      of ckNonTypeTemplateParameter, ckTemplateRef:
        result.genConstraints.add subn
      of ckParmDecl, ckTypeRef, ckNamespaceRef,
         ckFirstExpr, ckCompoundStmt, ckMemberRef,
         ckFirstAttr, ckVisibilityAttr
           :
         # WARNING right now these things are just droppped. Maybe it
         # will cause some errors in the future, I don't really know.
        discard
      else:
        warn "Unknown element", subn.cxKind, cast[int](subn.cxKind),
                subn, "in\n" & $cursor


proc visitEnum*(
  cursor: CXcursor, parent: CNamespace, conf: WrapConfig): CDecl =
  result = CDecl(kind: cdkEnum, cursor: cursor,
                 namespace: parent,
                 name: newPType(($cursor).dropPrefix("enum ")))

  for elem in cursor:
    result.flds.add ($elem, some(elem[0]))

  # info "Found enum", result.name


proc requiredGenericParams*(cursor: CXCursor): seq[NType[PNode]] =
  ## Get list of required generic parameters from cursor pointing to
  ## class or struct declaration
  for subn in cursor:
    if subn.cxKind in {ckTemplateTemplateParameter,
                        ckTemplateTypeParameter}:
      if subn.len > 0:
        # WARNING Just drop all template parameters that are not
        # simply `T`.
        discard
      else:
        result.add newPType $subn # WARNING blow up on `a<b>`



proc visitClass*(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  ## Convert class under cursor to `CDecl`
  cursor.expectKind({
    ckClassDecl, ckClassTemplate, ckClassTemplatePartialSpecialization,
    ckStructDecl, ckUnionDecl
  })

  let name =
    if cursor.cxKind == ckStructDecl and len($cursor) == 0:
      $cursor.cxType()
    else:
      $cursor

  result = CDecl(kind: cdkClass, cursor: cursor, name: newPType(name))
  result.namespace = parent
  result.name.add cursor.requiredGenericParams()

  identLog()
  defer: dedentLog()

  var currentAccs =
    if cursor.cxKind in {ckStructDecl, ckUnionDecl}:
      asPublic
    else:
      asPrivate


  # debug cursor.treeRepr()

  for subn in cursor:
    if subn.cxKind == ckAccessSpecifier:
      currentAccs = subn.getAccessSpecifier()

    if currentAccs == asPublic:
      case subn.cxKind:
        of ckMethod, ckConversionFunction:
          result.members.add visitMethod(subn, currentAccs)
        of ckConstructor:
          var res = visitMethod(subn, currentAccs)
          res.name.head = result.name.head
          result.members.add res
        of ckAccessSpecifier:
          currentAccs = subn.getAccessSpecifier()
        of ckFieldDecl,
           ckVarDecl # WARNING static fields might need to be wrapped differently
             :

          if not conf.ignoreCursor(subn, conf):
            # debug subn.cxType().getTypeDeclaration()
            result.members.add visitField(subn, currentAccs)

        of ckTemplateTypeParameter, ckFriendDecl,
           ckStaticAssert, ckTemplateTemplateParameter:
          discard
        of ckDestructor:
          # QUESTION WARNING I think using `destroy=` hook from nim side is
          # unnecessary, but still need to verify this later.
          var res = visitMethod(subn, currentAccs)
          res.name.head = result.name.head
          result.members.add res
        of ckFunctionTemplate:
          result.members.add visitFunction(
            subn, parent & @[ result.name ], conf)
        of ckTypeAliasTemplateDecl, ckTypeAliasDecl,
           ckTypedefDecl, ckUsingDeclaration:
          if not conf.ignoreCursor(subn, conf):
            result.members.add visitAlias(
              subn, parent & @[result.name], conf)
        of ckStructDecl, ckClassDecl, ckUnionDecl, ckEnumDecl,
           ckClassTemplate
             :
          # NOTE nested structs will be handled during object wrapping and
          # moved into separate type declarations
          discard
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



proc visitCursor*(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): tuple[
  decls: seq[CDecl], recurse: bool, includes: seq[IncludeDep]]

proc visitNamespace*(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): seq[CDecl] =
  ## Convert all elements in namespace into sequence of `CDecl`
  ## elements.

  let namespace =
    if not (cursor.isInlineNamespace() == 1):
      @[ newPType($cursor) ]
    else:
      @[]

  if not conf.ignoreCursor(cursor, conf):
    for subn in cursor:
      result.add visitCursor(subn, parent & namespace, conf).decls

proc visitMacrodef*(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): CDecl =
  CDecl(cursor: cursor, kind: cdkMacro)

proc visitCursor*(
  cursor: CXCursor, parent: CNamespace, conf: WrapConfig): tuple[
  decls: seq[CDecl], recurse: bool, includes: seq[IncludeDep]] =

  # debug cursor.cxKind()
  # debug $cursor.cxType()
  # debug cursor.getCursorSemanticParent().cxKind()
  # debug cursor.getCursorLexicalParent().cxKind()
  if not conf.ignoreCursor(cursor, conf):
    case cursor.cxKind:
      of ckNamespace:
        result.decls.add visitNamespace(cursor, parent, conf)

      of ckClassDecl, ckClassTemplate,
         ckClassTemplatePartialSpecialization,
         ckStructDecl:
        result.decls.add visitClass(cursor, parent, conf)

      of ckFunctionDecl, ckFunctionTemplate:
        result.decls.add visitFunction(cursor, parent, conf)

      of ckTypedefDecl, ckTypeAliasDecl:
        # debug cursor.treeRepr()
        result.decls.add visitAlias(cursor, parent, conf)

      of ckEnumDecl:
        result.decls.add visitEnum(cursor, parent, conf)

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
    of cdkClass, cdkStruct:
      for member in cd.members:
        result.add member.getPublicAPI()
    of cdkField:
      if cd.accs == asPublic:
        return @[ cd.cursor ]
      else:
        return @[]
    of cdkFunction, cdkMethod:
      let exportd: bool = (cd.kind == cdkFunction) or
        (cd.accs() == asPublic)

      if exportd:
        result.add cd.cursor
        for arg in cd.args:
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
  tuCursor.visitChildren do:
    makeVisitor [tu, res, conf, tuCursor]:
      # debug cursor.cxKind()
      let resolve = conf.depResolver(cursor, tuCursor)
      if resolve == drkWrapDirectly:
        let (decls, rec, incls) = visitCursor(cursor, @[], conf)
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

  return res
