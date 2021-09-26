type
  CWaveTokId* = enum
  ErrorCode* = enum
  SeverityLevel* = enum
  EntryHandling* = enum
  Position* {.bycopy, dynlib: "libbost_wave.so", importc: "WavePosition".} = object
  
  EntryHandling* = distinct CFoundWarningDirectiveCbType
  EntryHandling* = distinct CFoundUnknownDirectiveCbType
  EntryHandling* = distinct CFoundDirectiveCbType
  Diagnostics* {.bycopy, dynlib: "libbost_wave.so", importc: "WaveDiagnostics".} = object
    line*: cint
    column*: cint
    code*: ErrorCode
    level*: SeverityLevel
    filename*: ptr char
    errorText*: ptr char


proc deleteWaveTokenVector*(vec: TokenVectorHandle): void {.
    dynlib: "libbost_wave.so", importc: "wave_deleteWaveTokenVector".}

proc newProcessingHooks*(): ptr WaveProcessingHooksHandle {.
    dynlib: "libbost_wave.so", importc: "wave_newProcessingHooks".}

proc destroyProcessingHooks*(hooks: ptr ProcessingHooksHandle): void {.
    dynlib: "libbost_wave.so", importc: "wave_destroyProcessingHooks".}

proc newWaveContext*(instring: ptr char; filename: ptr char): ptr WaveContextHandle {.
    dynlib: "libbost_wave.so", importc: "wave_newWaveContext".}

proc processAll*(context: ptr ContextHandle): void {.dynlib: "libbost_wave.so",
    importc: "wave_processAll".}

proc setFoundWarningDirective*(context: ptr ContextHandle;
                               impl: CFoundWarningDirectiveCbType; env: ptr void): void {.
    dynlib: "libbost_wave.so", importc: "wave_setFoundWarningDirective".}

proc setFoundUnknownDirective*(context: ptr ContextHandle;
                               impl: CFoundUnknownDirectiveCbType): void {.
    dynlib: "libbost_wave.so", importc: "wave_setFoundUnknownDirective".}

proc setFoundDirective*(context: ptr ContextHandle; impl: CFoundDirectiveCbType): void {.
    dynlib: "libbost_wave.so", importc: "wave_setFoundDirective".}

proc destroyContext*(context: ptr ContextHandle): void {.
    dynlib: "libbost_wave.so", importc: "wave_destroyContext".}

proc contextSetData*(context: ptr ContextHandle; data: ptr void): void {.
    dynlib: "libbost_wave.so", importc: "wave_contextSetData".}

proc contextGetData*(context: ptr ContextHandle): ptr void {.
    dynlib: "libbost_wave.so", importc: "wave_contextGetData".}

proc contextHasError*(context: ptr ContextHandle): bool {.
    dynlib: "libbost_wave.so", importc: "wave_contextHasError".}

proc contextHasWarnings*(context: ptr ContextHandle): bool {.
    dynlib: "libbost_wave.so", importc: "wave_contextHasWarnings".}

proc deleteDiagnostics*(diag: ptr Diagnostics): void {.
    dynlib: "libbost_wave.so", importc: "wave_deleteDiagnostics".}

proc contextPopWarning*(context: ptr ContextHandle): WaveDiagnostics {.
    dynlib: "libbost_wave.so", importc: "wave_contextPopWarning".}

proc addMacroDefinition*(context: ptr ContextHandle; macrostring: ptr char;
                         is_predefined: bool): void {.dynlib: "libbost_wave.so",
    importc: "wave_addMacroDefinition".}

proc removeMacroDefinition*(context: ptr ContextHandle; macrostring: ptr char;
                            is_predefined: bool): bool {.
    dynlib: "libbost_wave.so", importc: "wave_removeMacroDefinition".}

proc isDefinedMacro*(context: ptr ContextHandle; name: ptr char): bool {.
    dynlib: "libbost_wave.so", importc: "wave_isDefinedMacro".}

proc getMacroDefinition*(context: ptr ContextHandle; name: ptr char;
                         is_function_style: ptr bool; is_predefined: ptr bool;
                         pos: ptr Position;
                         parameters: ptr ptr TokenVectorHandle;
                         definition: ptr ptr TokenVectorHandle): bool {.
    dynlib: "libbost_wave.so", importc: "wave_getMacroDefinition".}

proc beginIterator*(context: ptr ContextHandle): ptr WaveIteratorHandle {.
    dynlib: "libbost_wave.so", importc: "wave_beginIterator".}

proc endIterator*(context: ptr ContextHandle): ptr WaveIteratorHandle {.
    dynlib: "libbost_wave.so", importc: "wave_endIterator".}

proc advanceIterator*(iter: ptr IteratorHandle): void {.
    dynlib: "libbost_wave.so", importc: "wave_advanceIterator".}

proc neqIterator*(iter1: ptr IteratorHandle; iter2: ptr IteratorHandle): bool {.
    dynlib: "libbost_wave.so", importc: "wave_neqIterator".}

proc iterGetTok*(iter: ptr IteratorHandle): ptr WaveTokenHandle {.
    dynlib: "libbost_wave.so", importc: "wave_iterGetTok".}

proc deleteTok*(tok: ptr TokenHandle): void {.dynlib: "libbost_wave.so",
    importc: "wave_deleteTok".}

proc tokGetId*(tok: ptr TokenHandle): CWaveTokId {.dynlib: "libbost_wave.so",
    importc: "wave_tokGetId".}

proc tokGetValue*(tok: ptr TokenHandle): ptr char {.dynlib: "libbost_wave.so",
    importc: "wave_tokGetValue".}