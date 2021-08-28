import hmisc/wrappers/wraphelp

import ./boost_wave_tokens

const hdr = "wave_c_api.h"
const so = "./libboost_wave.so"


{.pragma: apiPtr, header: hdr, requiresinit, bycopy.}
{.pragma: apiProc, dynlib: so, cdecl.}


type
  MethodImpl {.header: hdr, importc.} = object
    impl: pointer
    payload: pointer


type
  WaveToken {.apiPtr.} = object
  PWaveToken = ptr WaveToken

proc getValue(tok: PWaveToken): cstring {.apiProc, importc: "wave_tokGetValue".}
proc getIdRaw(tok: PWaveToken): cint {.apiProc, importc: "wave_tokGetId".}

proc getId(tok: PWaveToken): WaveTokenId = 
  tok.getIdRaw().WaveTokenId()

type
  WaveIterator {.apiPtr.} = object
  PWaveIterator = ptr WaveIterator

proc getTok(iter: PWaveIterator): PWaveToken {.apiProc, importc: "wave_iterGetTok".}
proc advance(iter: PWaveIterator) {.apiProc, importc: "wave_advanceIterator".}
proc `!=`(iter1, iter2: PWaveIterator): bool {.apiProc, importc: "wave_neqIterator".}
proc `==`(iter1, iter2: PWaveIterator): bool {.error.}

type
  WaveContext {.apiPtr.} = object
  PWaveContext = ptr WaveContext


proc newWaveContext(str, filename: cstring): PWaveContext {.apiProc, importc: "wave_newWaveContext".}
proc destroyContext(ctx: PWaveContext) {.apiProc, importc: "wave_destroyContext".}

proc first(ctx: PWaveContext): PWaveIterator {.apiProc, importc: "wave_beginIterator".}
proc last(ctx: PWaveContext): PWaveIterator {.apiProc, importc: "wave_endIterator".}


type
  WaveProcessingHooks {.apiPtr.} = object
  PWaveProcessingHooks = ptr WaveProcessingHooks






proc main() =
  let ctx = newWaveContext(allocCStringArray(["int main() {}"])[0], "<Unknown>".cstring)

  var first: PWaveIterator = ctx.first()
  while first != ctx.last():
    # echo "TOK: ", first.getTok().getValue(), " ", first.getTok().getId()
    first.advance()

when isMainModule:
  main()

