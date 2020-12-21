import cxtypes, cxcommon, hc_types, hc_visitors
import hnimast
import hmisc/helpers
import hmisc/other/oswrap
import std/[decls, strutils, sequtils, tables, sugar, strformat]
import hmisc/other/colorlogger
import hc_typeconv, cxtypes

proc nimifyInfixOperators*(
  we: var WrappedEntry, conf: WrapConfig): seq[WrappedEntry] {.nimcall.} =
  if we.isMultitype:
    return

  var we = we
  we.wrapped.iinfo = currIInfo()

  if we.wrapped.kind == nekProcDecl and
     we.wrapped.procdecl.kind == pkOperator:
    var opd {.byaddr.} = we.wrapped.procdecl
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
  we: var WrappedEntry, conf: WrapConfig): seq[WrappedEntry] {.nimcall.} =

  if not we.isMultitype:
    if we.wrapped.kind == nekProcDecl and
       we.wrapped.procdecl.kind == pkRegular:
      we.wrapped.procdecl.name[0] = toLowerAscii(we.wrapped.procdecl.name[0])

proc enumOverloads*(
  we: var WrappedEntry, conf: WrapConfig): seq[WrappedEntry] {.nimcall.} =

  if we.isMultitype:
    return

  if we.wrapped.kind == nekProcDecl:
    let enArgs = toTable: collect(newSeq):
      for idx, arg in we.cursor.getArguments():
        if arg.cursor.cxType().getTypeDeclaration().cxKind() in {ckEnumDecl}:
          (idx, arg)

    # info enArgs

    var pr {.byaddr.} = we.wrapped.procdecl
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

var codegen*: seq[CxxCodegen]

proc callbackOverride*(we: var WrappedEntry, conf: WrapConfig): seq[WrappedEntry] {.nimcall.} =
  ## Generate additional derivation for each encountered class.
  if we.isMultitype:
    for entry in we.decls:
      if entry.wrapped.kind == nekObjectDecl:
        info "Object decl", entry.cursor
        let eName = $entry.cursor
        let headerFile = entry.cursor.relSpellingFile().withExt("hpp")
        let inclSpec = conf.makeHeader(entry.cursor, conf)

        block:
          var obj = newPObjectDecl(
            $entry.cursor & "NimRaw",
            exported = true,
            annotate = newPPragma(
                newPIdent("importcpp"),
                newPIdentColonString("header", headerFile.string)
            ),
            iinfo = currIInfo()
          )

          obj.addField("derivedImpl", newPType("pointer"))

          for meth in items(entry.cursor, {ckMethod}):
            var args: seq[(string, NType[PNode])]
            args.add ("derivedImpl", newPType("pointer"))

            args.add meth.getArguments.mapIt(
              (it.name, it.cursor.cxType.toNType(conf).ntype))

            args.add ("closureEnv", newPType("pointer"))
            args.add ("closureProc", newPType("pointer"))


            obj.addField($meth & "Wrap", newProcNType[PNode](
              args,
              meth.retType().toNType(conf).ntype,
              newPPragma("cdecl")
            ))

            obj.addField($meth & "Proc", newPType("pointer"))
            obj.addField($meth & "Env", newPType("pointer"))

          result.add newWrappedEntry(toNimDecl(obj), entry.original, entry.cursor)

        block:
          var wrap = newPObjectDecl(
            $entry.cursor & "Nim",
            iinfo = currIInfo(),
            genParams = @[newPType("T")]
          )

          wrap.addField("impl", newPType("ptr", @[$entry.cursor & "NimRaw"]))
          wrap.addField("derivedImpl", newPType("T"))

          result.add newWrappedEntry(toNimDecl(wrap), entry.original, entry.cursor)

        block:
          var pr = newPProcDecl(
            name = "new" & eName & "NimRaw",
            rtyp = some(newPType("ptr", [eName & "NimRaw"])),
            pragma = newPPragma(
              newPIdent("constructor"),
              newExprColonExpr(newPident("header"), inclSpec.toNNode()),
              newPIdentColonString("importcpp", &"new {eName}NimRaw(@)")
            )
          )

          result.add newWrappedEntry(toNimDecl(pr), entry.original, entry.cursor)

        block:
          let
            n1 = newPIdent(eName & "Nim")
            n2 = newPIdent("new" & eName & "NimRaw")

          var pr = newPProcDecl(
            name = "new" & eName & "Nim",
            genParams = @[newPType("T")],
            rtyp = some(newPType(eName & "Nim", ["T"])),
            iinfo = currIInfo(),
            impl = (
              pquote do:
                `n1`[T](impl: `n2`())
            )
          )

          # NOTE I have no fucking idea about `entry.original`
          result.add newWrappedEntry(toNimDecl(pr), entry.original, entry.cursor)


        for meth in items(entry.cursor, {ckMethod}):
          var call = nnkCall.newPTree(newPIdent($meth & "Wrap"))
          var nimArgs: seq[(string, NType[PNode])]

          call.add nnkDotExpr.newPTree(newPIdent("derived"),
                                       newPIdent("impl"))

          for arg in meth.getArguments():
            call.add newPIdent(arg.name)
            nimArgs.add (arg.name, arg.cursor.cxType().toNType(conf).ntype)

          var subImpl = newPProcDecl(
            name = $meth & "Wrap",
            exported = false,
            args = (
              "raw",
              newPType("ptr", [eName & "NimRaw"])
            ) & nimArgs,
            pragma = newPPragma(
              newExprColonExpr(newPIdent("header"), inclSpec.toNNode()),
              newPidentColonString("importcpp", &"#.{meth}(@)")
            )
          )


          var pr = newPProcDecl(
            name = $meth,
            genParams = @[newPType("T")],
            args = (
              "derived",
              newNType("var", [newPType(eName & "Nim", ["T"])])

            ) & nimArgs,
            rtyp = some meth.retType().toNType(conf).ntype,
            impl = (
              pquote do:
                `subImpl.toNNode()`
                `call`
            )
          )


          result.add newWrappedEntry(
            toNimDecl(pr), entry.original, entry.cursor)

        for meth in items(entry.cursor, {ckMethod}):
          let
            mn1 = capitalizeAscii($meth)
            mn2 = $meth
            prIdent = newPIdent("set" & mn1)
            nArgList: seq[PNode] = meth.getArguments().mapIt(
              nnkExprColonExpr.newPTree(
                newPIdent(it.name),
                it.cursor.cxType().toNType(conf).ntype.toNNode()
              )
            )


          let rawid = newPIdent(eName & "Nim")
          let impl = pquote do:
            proc `prIdent`*[T](
                self: var `rawId`[T],
                cb: proc(this: var `rawId`[T],
                         arg: @@@^nArgList
                ) {.closure.}
              ) =

              type
                ClosImplType = typeof(closureToCdecl(cb))
                SelfType = typeof(self)

              # `{.cdecl.}` implementation callback that will be passed back to
              # raw derived class
              let wrap = proc(
                derivedImpl: pointer, arg: @@@^nArgList,
                cbEnv, cbImpl: pointer): void {.cdecl.} =

                # Uncast pointer to derived class
                var derived = cast[ptr SelfType](derivedImpl)

                # Call closure implementation, arguments and closure environment.
                cast[ClosImplType](cbImpl)(
                  derived[],
                  arg: @@@^(meth.getArguments().mapIt(newPIdent(it.name))),
                  cbEnv
                )


              self.impl.@@@!(newPIdent(mn2 & "Wrap")) = wrap
              self.impl.derivedImpl = addr self
              self.impl.@@@!(newPIdent(mn2 & "Env")) = cb.rawEnv()
              self.impl.@@@!(newPIdent(mn2 & "Proc")) = cb.rawProc()

          result.add newWrappedEntry(toNimDecl(impl))



        let decls = collect(newSeq):
          for meth in items(entry.cursor, {ckMethod}):
            let args = (
              "void* derivedImpl, " &
                meth.argsSignature(wrap = (false, true)) & " " &
                "void* closureEnv, " &
                "void* closureProc"
            )

            # let overrideArgs = "void* derivedImpl, " &
            #   meth.argsSignature(names = false, wrap = (false, true))

            let name = meth.getSemanticNamespaces().join("::")
            &"""
    // Override wrapper for `{name}`
    {meth.retType()} (*{meth}Wrap)({args}) = 0;
    void* {meth}Proc = 0;
    void* {meth}Env = 0;

    {meth.retType()} {meth}({meth.argsSignature()}) override;
"""


        let inclFile = tern(inclSpec.kind == nhskGlobal,
                            &"<{inclSpec.global}>",
                            &"\"{inclSpec.file}\"")


        let res = &"""
#pragma once
#include {inclFile}

// Final overide struct
struct {entry.cursor}NimRaw : public {entry.cursor} {{
    // Pointer to wrapper object
    void* derivedImpl = 0;

{decls.joinl()}

}};
"""
        codegen.add CxxCodegen(
          cursor: entry.cursor,
          code: res,
          filename: headerFile
        )


        let impls = collect(newSeq):
          for meth in items(entry.cursor, {ckMethod}):
            let args = meth.getArguments()

            &"""
#include "{headerFile}"

{meth.retType()} {entry.cursor}NimRaw::{meth}({meth.argsSignature()}) {{
    if (this->{meth}Wrap == 0) {{
        {entry.cursor}::{meth}({meth.argsSignature(types = false)});

    }} else {{
        this->{meth}Wrap(
            this->derivedImpl,
            {meth.argsSignature(types = false, wrap = (false, true))}
            this->{meth}Env,
            this->{meth}Proc
        );
    }}
}}

"""

        codegen.add CxxCodegen(
          cursor: entry.cursor,
          code: impls.join("\n"),
          filename: entry.cursor.relSpellingFile().withExt("cpp")
        )



var defaultPostprocessSteps*: seq[Postprocess]

defaultPostprocessSteps.add @[
  newPostprocess(nep1Idents),
  newPostprocess(nimifyInfixOperators),
  newPostprocess(enumOverloads),
  newPostprocess(callbackOverride)
]
