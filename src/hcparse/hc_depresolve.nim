import std/[tables, sequtils, strutils, options]
import hmisc/helpers
import hmisc/other/[oswrap]
import cxtypes, cxcommon, hc_types

proc getDepFiles*(deps: seq[CXCursor], conf: WrapConf): seq[AbsFile]

proc isDependency*(cursor: CXCursor, conf: WrapConf): bool =
  return conf.depResolver(
    cursor, conf.unit.getTranslationUnitCursor()) == drkImportUses




proc getDepFiles*(cxtype: CXType, conf: WrapConf): seq[AbsFile] =
  let decl = cxtype.getTypeDeclaration()
  for parm in cxtype.genParams():
    if parm.cxKind != tkInvalid:
      for file in getDepFiles(parm, conf):
        result.add file

  ignorePathErrors {pekInvalidEntry}:
    if decl.kind notin {ckNoDeclFound}:
      if decl.isDependency(conf):
        let (file, _, _, _) = decl.getSpellingLocation().get()
        result.add file

  for file in result:
    assertExists(file)

proc isInternalImpl*(
  dep: AbsFile, conf: WrapConf, index: FileIndex): bool =
  return not index.index[dep].isExplicitlyAdded

proc getDepFiles*(deps: seq[CXCursor], conf: WrapConf): seq[AbsFile] =
  ## Generate list of files that have to be wrapped
  # assert conf.unit.getTranslationUnitCursor().cxKind == ckTranslationUnit

  for dep in deps:
    var decl: (CXCursor, bool)
    case dep.cxKind:
      of ckFunctionDecl, ckMethod, ckConversionFunction:
        result.add getDepFiles(dep.params(), conf).withIt do:
          for file in it:
            assertExists file

        decl = (dep.retType().getTypeDeclaration(), true)

      of ckFunctionTemplate:
        result.add dep.retType().getDepFiles(conf).withIt do:
          for file in it:
            assertExists file

      of ckTypeAliasTemplateDecl, ckTypeAliasDecl,
         ckTypedefDecl, ckUsingDeclaration:

        decl = (
          dep.cxType().getCanonicalType().getTypeDeclaration(),
          true
        )

      of ckParmDecl:
        var cxt = dep.cxType()

        let subn = dep.children()
        var typeRef = false

        for sub in subn:
          case sub.cxKind:
            of ckNamespaceRef:
              discard
            of ckTypeRef:
              cxt = sub.cxType()
              typeRef = true
            else:
              break


        if not typeRef and (cxt.cxKind() notin {tkInt}):
          for parm in cxt.genParams():
            if parm.cxKind != tkInvalid:
              result.add getDepFiles(parm, conf).withIt do:
                for file in it:
                  assertExists file


        decl = (cxt.getTypeDeclaration(), true)
      else:
        # warn "dep for:", dep.cxKind(), dep, dep.cxType()
        decl = (dep.cxType.getTypeDeclaration(), true)

    if decl[1]:
      ignorePathErrors {pekInvalidEntry}:
        # WARNING ignore invalid `#include`
        if decl[0].cxKind() notin {ckNoDeclFound}:
          if decl[0].isDependency(conf):
            let (file, line, column, _) = decl[0].getSpellingLocation().get()
            assertExists(file)
            result.add file


  result = result.deduplicate().
    filterIt(it.len > 0 and it.hasExt()).
    mapIt(it.realpath())
