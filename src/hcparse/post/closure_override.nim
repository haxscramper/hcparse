import hcparse/[
  cxtypes,
  cxcommon,
  hc_types,
  hc_wrapgen,
  hc_typeconv,
  hc_visitors
]

## .. include:: closure_override.rst



import hnimast

import hmisc/helpers
import hmisc/other/oswrap
import hmisc/other/colorlogger

import std/[decls, strutils, sequtils, tables, sugar, strformat]

proc callbackOverride*(
    we: var WrappedEntry, conf: WrapConfig, codegen: var seq[CxxCodegen]
  ): seq[WrappedEntry] {.nimcall.} =

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

          obj.addField("userData", newPType("pointer"))

          for meth in items(entry.cursor, {ckMethod}):
            var args: seq[(string, NType[PNode])]
            args.add ("userData", newPType("pointer"))

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

          wrap.addField("rawImpl", newPType("ptr", @[$entry.cursor & "NimRaw"]))
          wrap.addField("userData", newPType("T"))

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
                `n1`[T](rawImpl: `n2`())
            )
          )

          # NOTE I have no fucking idea about `entry.original`
          result.add newWrappedEntry(toNimDecl(pr), entry.original, entry.cursor)


        for meth in items(entry.cursor, {ckMethod}):
          var call = nnkCall.newPTree(newPIdent($meth & "Wrap"))
          var nimArgs: seq[(string, NType[PNode])]

          call.add nnkDotExpr.newPTree(newPIdent("derived"),
                                       newPIdent("rawImpl"))

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
                userData: pointer, arg: @@@^nArgList,
                cbEnv, cbImpl: pointer): void {.cdecl.} =

                # Uncast pointer to derived class
                var derived = cast[ptr SelfType](userData)

                # Call closure implementation, arguments and closure environment.
                cast[ClosImplType](cbImpl)(
                  derived[],
                  arg: @@@^(meth.getArguments().mapIt(newPIdent(it.name))),
                  cbEnv
                )


              self.rawImpl.@@@!(newPIdent(mn2 & "Wrap")) = wrap
              self.rawImpl.userData = addr self
              self.rawImpl.@@@!(newPIdent(mn2 & "Env")) = cb.rawEnv()
              self.rawImpl.@@@!(newPIdent(mn2 & "Proc")) = cb.rawProc()

          result.add newWrappedEntry(toNimDecl(impl))



        let decls = collect(newSeq):
          for meth in items(entry.cursor, {ckMethod}):
            let args = (
              "void* userData, " &
                meth.argsSignature(wrap = (false, true)) & " " &
                "void* closureEnv, " &
                "void* closureProc"
            )

            # let overrideArgs = "void* userData, " &
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
    void* userData = 0;

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
            this->userData,
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
