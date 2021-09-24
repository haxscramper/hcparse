import hmisc/wrappers/wraphelp

import ./boost_wave_tokens

const hdr = "wave_c_api.h"
const so = "./libboost_wave.so"


{.pragma: apiPtr, header: hdr, requiresinit, bycopy.}
{.pragma: apiProc, dynlib: so, cdecl.}


type
  MethodImpl* {.header: hdr, importc.} = object
    impl: pointer
    payload: pointer


type
  CWaveToken* {.apiPtr.} = object
  PWaveToken* = ptr CWaveToken

proc getValue*(tok: PWaveToken): cstring {.apiProc, importc: "wave_tokGetValue".}
proc getIdRaw*(tok: PWaveToken): cint {.apiProc, importc: "wave_tokGetId".}

proc getId*(tok: PWaveToken): WaveTokId =
  tok.getIdRaw().WaveTokId()

type
  CWaveIterator* {.apiPtr.} = object
  PWaveIterator* = ptr CWaveIterator

proc getTok*(iter: PWaveIterator): PWaveToken {.apiProc, importc: "wave_iterGetTok".}
proc advance*(iter: PWaveIterator) {.apiProc, importc: "wave_advanceIterator".}
proc `!=`*(iter1, iter2: PWaveIterator): bool {.apiProc, importc: "wave_neqIterator".}
proc `==`*(iter1, iter2: PWaveIterator): bool {.error.}

type
  CWaveContext {.apiPtr.} = object
  PWaveContext = ptr CWaveContext


proc newWaveContext*(str, filename: cstring): PWaveContext {.apiProc, importc: "wave_newWaveContext".}
proc destroyContext*(ctx: PWaveContext) {.apiProc, importc: "wave_destroyContext".}

proc first*(ctx: PWaveContext): PWaveIterator {.apiProc, importc: "wave_beginIterator".}
proc last*(ctx: PWaveContext): PWaveIterator {.apiProc, importc: "wave_endIterator".}


type
  WaveProcessingHooks {.apiPtr.} = object
  PWaveProcessingHooks = ptr WaveProcessingHooks






