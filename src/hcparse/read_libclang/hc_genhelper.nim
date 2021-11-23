when false:
  ## The API is exactly as I want it, but due to IR breakages I temporarily
  ## commented things out, will fix later.
  type
    CErrorCodeKind = enum
      eckBadInteger
      eckErrorEnum

    SetLit = range[0 .. 65535]

    CErrorCode = object
      message*: string
      printArgs*: set[SetLit]
      case kind*: CErrorCodeKind
        of eckErrorEnum:
          enumIdent*: CScopedIdent

        of eckBadInteger:
          validRange*: Slice[cint]

  func negativeError*(message: string, printIdx: set[SetLit] = {}): CErrorCode =
    CErrorCode(
      message: message,
      kind: eckBadInteger,
      validRange: (cint(0) .. high(cint)),
      printArgs: printIdx
    )

  func errorEnum*(path: CScopedIdent): CErrorCode =
    CErrorCode(kind: eckErrorEnum, enumIdent: path)


  proc errorCodesToException*(
      genProc: var GenProc, conf: WrapConf, cache: var WrapCache,
      errorMap: seq[(CSCopedIdent, CErrorCode)]
    ): seq[WrappedEntry] =

    for (ident, code) in errorMap:
      if sameNoGeneric(genProc.cdecl.ident, ident):
        var gen2 = deepCopy(genProc)
        genProc.name &= "Raw"
        gen2.noPragmas = gpcNoPragma

        var call = newPCall(genProc.name)
        for arg in genProc.arguments:
          call.add newPIdent(arg.name)

        let validRange = nnkInfix.newPTree(
          newPIdent(".."),
          newPCall("cint", newPLit(code.validRange.a)),
          newPCall("cint", newPLit(code.validRange.b))
        )

        case code.kind:
          of eckBadInteger:
            var msg = &"Return value of the {genProc.cdecl.cursor}"
            msg &= " is not in valid range - expected ["
            msg &= tern(code.validRange.a == low(cint), "low(cint)", $code.validRange.a)
            msg &= " .. "
            msg &= tern(code.validRange.b == high(cint), "high(cint)", $code.validRange.b)
            msg &= "], but got "

            var msg2 = ". "
            if code.message.len > 0:
              msg2 &= code.message & ". "

            var argList = newPStmtList()
            if code.printArgs.len > 0:
              msg2 &= tern(code.printArgs.len > 0, "Arguments were '", "Argument was '")

              var cnt = 0
              for idx, arg in gen2.arguments:
                if SetLit(idx) in code.printArgs:
                  if cnt > 0:
                    argList.add pquote(errMsg &= "', '")

                  argList.add pquote(errMsg &= $(`newPIdent(arg.name)`))
                  inc cnt

              if cnt > 0:
                argList.add pquote(errMsg &= "'.")

            gen2.impl = some pquote do:
              result = `call`
              if result notin `validRange`:
                var errMsg = `msg` & $result & `msg2`
                `argList`
                raise newException(ValueError, errMsg)

          else:
            raiseImplementError("")

        return @[newWrappedEntry(
          gen2.toNNode(conf, cache).toNimDecl(), wepInProcs, currLInfo(),
          genProc.cdecl
        )]
