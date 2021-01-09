import cxtypes, cxcommon, hc_types, hc_visitors
import hnimast
import hmisc/helpers
import hmisc/other/oswrap
import hmisc/other/colorlogger

import std/[decls, strutils, sequtils, tables, sugar, strformat]
import hc_typeconv, cxtypes

proc nimifyInfixOperators*(
    we: var WrappedEntry,
    conf: WrapConfig,
    codegen: var seq[CxxCodegen]
  ): seq[WrappedEntry] {.nimcall.} =

  if we.kind != wekNimDecl:
    return

  var we = we
  we.mwrapped.iinfo = currIInfo()

  if we.wrapped.kind == nekProcDecl and
     we.wrapped.procdecl.kind == pkOperator:
    var opd {.byaddr.} = we.mwrapped.procdecl
    case opd.name:
      of "<<":
        opd.name = "shl"
        result.add we

      of ">>":
        opd.name = "shr"
        result.add we

      of "%":
        opd.name = "mod"
        result.add we

      else:
        discard

proc nep1Idents*(
    we: var WrappedEntry,
    conf: WrapConfig,
    codegen: var seq[CxxCodegen]
  ): seq[WrappedEntry] {.nimcall.} =

  if we.kind == wekNimDecl:
    if we.wrapped.kind == nekProcDecl and
       we.wrapped.procdecl.kind == pkRegular:
      we.mwrapped.procdecl.name[0] = toLowerAscii(we.wrapped.procdecl.name[0])

proc enumOverloads*(
    we: var WrappedEntry,
    conf: WrapConfig,
    codegen: var seq[CxxCodegen]
  ): seq[WrappedEntry] {.nimcall.} =

  if we.kind != wekNimDecl:
    return

  if we.wrapped.kind == nekProcDecl:
    let enArgs = toTable: collect(newSeq):
      for idx, arg in we.cursor.getArguments():
        if arg.cursor.cxType().getTypeDeclaration().cxKind() in {ckEnumDecl}:
          (idx, arg)

    var pr {.byaddr.} = we.mwrapped.procdecl
    if enArgs.len > 0:
      var reproc = we.wrapped.procdecl
      reproc.iinfo = currIInfo()
      reproc.signature.pragma.clear()

      var impl = nnkStmtList.newPTree()
      var subcall = nnkCall.newPTree(newPIdent reproc.name)

      for idx, arg in mpairs(pr.signature.arguments):
        let argname = newPIdent(arg.varname)
        let arrname = newPIdent("arr" & arg.vtype.head & "mapping")

        if idx in enArgs:
          arg.vtype.head &= "_Impl"
          let argImpl = newPIdent(arg.varname & "_main")

          impl = pquote do:
            let `argImpl` = `arrname`[`argname`].cEnum

          subcall.add nnkExprEqExpr.newPTree(argname, argImpl)

        else:
          subcall.add nnkExprEqExpr.newPTree(argname, argname)

      reproc.impl = pquote do:
        `impl`
        `subcall`

      result.add newWrappedEntry(toNimDecl(reproc), we.original, we.cursor)

import post/closure_override

var defaultPostprocessSteps*: seq[Postprocess]

defaultPostprocessSteps.add @[
  newPostprocess(nep1Idents),
  newPostprocess(nimifyInfixOperators),
  newPostprocess(enumOverloads),
  # newPostprocess(callbackOverride)
]
