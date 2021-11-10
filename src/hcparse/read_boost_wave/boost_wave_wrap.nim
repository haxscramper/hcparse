import std/os
const boostWaveLibDir = currentSourcePath().splitFile().dir / "../../../lib"
const cwaveDl* = boostWaveLibDir / "libboost_cwave.so"


type
  WaveTokId* = enum
    tokId_UNKNOWN = 0, tokId_FIRST_TOKEN = 1, tokId_AND = 2, tokId_AND_ALT = 3,
    tokId_ANDAND = 4, tokId_ANDAND_ALT = 5, tokId_ASSIGN = 6,
    tokId_ANDASSIGN = 7, tokId_ANDASSIGN_ALT = 8, tokId_OR = 9,
    tokId_OR_ALT = 10, tokId_OR_TRIGRAPH = 11, tokId_ORASSIGN = 12,
    tokId_ORASSIGN_ALT = 13, tokId_ORASSIGN_TRIGRAPH = 14, tokId_XOR = 15,
    tokId_XOR_ALT = 16, tokId_XOR_TRIGRAPH = 17, tokId_XORASSIGN = 18,
    tokId_XORASSIGN_ALT = 19, tokId_XORASSIGN_TRIGRAPH = 20, tokId_COMMA = 21,
    tokId_COLON = 22, tokId_DIVIDE = 23, tokId_DIVIDEASSIGN = 24,
    tokId_DOT = 25, tokId_DOTSTAR = 26, tokId_ELLIPSIS = 27, tokId_EQUAL = 28,
    tokId_GREATER = 29, tokId_GREATEREQUAL = 30, tokId_LEFTBRACE = 31,
    tokId_LEFTBRACE_ALT = 32, tokId_LEFTBRACE_TRIGRAPH = 33, tokId_LESS = 34,
    tokId_LESSEQUAL = 35, tokId_LEFTPAREN = 36, tokId_LEFTBRACKET = 37,
    tokId_LEFTBRACKET_ALT = 38, tokId_LEFTBRACKET_TRIGRAPH = 39,
    tokId_MINUS = 40, tokId_MINUSASSIGN = 41, tokId_MINUSMINUS = 42,
    tokId_PERCENT = 43, tokId_PERCENTASSIGN = 44, tokId_NOT = 45,
    tokId_NOT_ALT = 46, tokId_NOTEQUAL = 47, tokId_NOTEQUAL_ALT = 48,
    tokId_OROR = 49, tokId_OROR_ALT = 50, tokId_OROR_TRIGRAPH = 51,
    tokId_PLUS = 52, tokId_PLUSASSIGN = 53, tokId_PLUSPLUS = 54,
    tokId_ARROW = 55, tokId_ARROWSTAR = 56, tokId_QUESTION_MARK = 57,
    tokId_RIGHTBRACE = 58, tokId_RIGHTBRACE_ALT = 59,
    tokId_RIGHTBRACE_TRIGRAPH = 60, tokId_RIGHTPAREN = 61,
    tokId_RIGHTBRACKET = 62, tokId_RIGHTBRACKET_ALT = 63,
    tokId_RIGHTBRACKET_TRIGRAPH = 64, tokId_COLON_COLON = 65,
    tokId_SEMICOLON = 66, tokId_SHIFTLEFT = 67, tokId_SHIFTLEFTASSIGN = 68,
    tokId_SHIFTRIGHT = 69, tokId_SHIFTRIGHTASSIGN = 70, tokId_STAR = 71,
    tokId_COMPL = 72, tokId_COMPL_ALT = 73, tokId_COMPL_TRIGRAPH = 74,
    tokId_STARASSIGN = 75, tokId_ASM = 76, tokId_AUTO = 77, tokId_BOOL = 78,
    tokId_FALSE = 79, tokId_TRUE = 80, tokId_BREAK = 81, tokId_CASE = 82,
    tokId_CATCH = 83, tokId_CHAR = 84, tokId_CLASS = 85, tokId_CONST = 86,
    tokId_CONSTCAST = 87, tokId_CONTINUE = 88, tokId_DEFAULT = 89,
    tokId_DELETE = 90, tokId_DO = 91, tokId_DOUBLE = 92, tokId_DYNAMICCAST = 93,
    tokId_ELSE = 94, tokId_ENUM = 95, tokId_EXPLICIT = 96, tokId_EXPORT = 97,
    tokId_EXTERN = 98, tokId_FLOAT = 99, tokId_FOR = 100, tokId_FRIEND = 101,
    tokId_GOTO = 102, tokId_IF = 103, tokId_INLINE = 104, tokId_INT = 105,
    tokId_LONG = 106, tokId_MUTABLE = 107, tokId_NAMESPACE = 108,
    tokId_NEW = 109, tokId_OPERATOR = 110, tokId_PRIVATE = 111,
    tokId_PROTECTED = 112, tokId_PUBLIC = 113, tokId_REGISTER = 114,
    tokId_REINTERPRETCAST = 115, tokId_RETURN = 116, tokId_SHORT = 117,
    tokId_SIGNED = 118, tokId_SIZEOF = 119, tokId_STATIC = 120,
    tokId_STATICCAST = 121, tokId_STRUCT = 122, tokId_SWITCH = 123,
    tokId_TEMPLATE = 124, tokId_THIS = 125, tokId_THROW = 126, tokId_TRY = 127,
    tokId_TYPEDEF = 128, tokId_TYPEID = 129, tokId_TYPENAME = 130,
    tokId_UNION = 131, tokId_UNSIGNED = 132, tokId_USING = 133,
    tokId_VIRTUAL = 134, tokId_VOID = 135, tokId_VOLATILE = 136,
    tokId_WCHART = 137, tokId_WHILE = 138, tokId_PP_DEFINE = 139,
    tokId_PP_IF = 140, tokId_PP_IFDEF = 141, tokId_PP_IFNDEF = 142,
    tokId_PP_ELSE = 143, tokId_PP_ELIF = 144, tokId_PP_ENDIF = 145,
    tokId_PP_ERROR = 146, tokId_PP_LINE = 147, tokId_PP_PRAGMA = 148,
    tokId_PP_UNDEF = 149, tokId_PP_WARNING = 150, tokId_IDENTIFIER = 151,
    tokId_OCTALINT = 152, tokId_DECIMALINT = 153, tokId_HEXAINT = 154,
    tokId_INTLIT = 155, tokId_LONGINTLIT = 156, tokId_FLOATLIT = 157,
    tokId_FIXEDPOINTLIT = 158, tokId_CCOMMENT = 159, tokId_CPPCOMMENT = 160,
    tokId_CHARLIT = 161, tokId_STRINGLIT = 162, tokId_CONTLINE = 163,
    tokId_SPACE = 164, tokId_SPACE2 = 165, tokId_NEWLINE = 166,
    tokId_GENERATEDNEWLINE = 167, tokId_POUND_POUND = 168,
    tokId_POUND_POUND_ALT = 169, tokId_POUND_POUND_TRIGRAPH = 170,
    tokId_POUND = 171, tokId_POUND_ALT = 172, tokId_POUND_TRIGRAPH = 173,
    tokId_ANY = 174, tokId_ANY_TRIGRAPH = 175, tokId_PP_INCLUDE = 176,
    tokId_PP_QHEADER = 177, tokId_PP_HHEADER = 178, tokId_PP_INCLUDE_NEXT = 179,
    tokId_PP_QHEADER_NEXT = 180, tokId_PP_HHEADER_NEXT = 181, tokId_EOF = 182,
    tokId_EOI = 183, tokId_PP_NUMBER = 184, tokId_MSEXT_INT8 = 185,
    tokId_MSEXT_INT16 = 186, tokId_MSEXT_INT32 = 187, tokId_MSEXT_INT64 = 188,
    tokId_MSEXT_BASED = 189, tokId_MSEXT_DECLSPEC = 190,
    tokId_MSEXT_CDECL = 191, tokId_MSEXT_FASTCALL = 192,
    tokId_MSEXT_STDCALL = 193, tokId_MSEXT_TRY = 194, tokId_MSEXT_EXCEPT = 195,
    tokId_MSEXT_FINALLY = 196, tokId_MSEXT_LEAVE = 197,
    tokId_MSEXT_INLINE = 198, tokId_MSEXT_ASM = 199,
    tokId_MSEXT_PP_REGION = 200, tokId_MSEXT_PP_ENDREGION = 201,
    tokId_IMPORT = 202, tokId_ALIGNAS = 203, tokId_ALIGNOF = 204,
    tokId_CHAR16_T = 205, tokId_CHAR32_T = 206, tokId_CONSTEXPR = 207,
    tokId_DECLTYPE = 208, tokId_NOEXCEPT = 209, tokId_NULLPTR = 210,
    tokId_STATICASSERT = 211, tokId_THREADLOCAL = 212, tokId_RAWSTRINGLIT = 213,
    tokId_CHAR8_T = 214, tokId_CONCEPT = 215, tokId_CONSTEVAL = 216,
    tokId_CONSTINIT = 217, tokId_CO_AWAIT = 218, tokId_CO_RETURN = 219,
    tokId_CO_YIELD = 220, tokId_REQUIRES = 221, tokId_SPACESHIP = 222,
    tokId_LAST_TOKEN_ID = 223, tokId_LAST_TOKEN = 224,
    tokId_UNKNOWN_UNIVERSALCHAR = 225, tokId_NONREPLACABLE_IDENTIFIER = 226,
    tokId_PLACEHOLDER = 227, tokId_PLACEMARKER = 228, tokId_PARAMETERBASE = 229,
    tokId_EXTPARAMETERBASE = 230, tokId_OPTPARAMETERBASE = 231
  WaveErrorCode* = enum
    wekNoError = 0, wekUnexpectedError = 1, wekMacroRedefinition = 2,
    wekMacroInsertionError = 3, wekBadIncludeFile = 4,
    wekBadIncludeStatement = 5, wekBadHasIncludeExpression = 6,
    wekIllFormedDirective = 7, wekErrorDirective = 8, wekWarningDirective = 9,
    wekIllFormedExpression = 10, wekMissingMatchingIf = 11,
    wekMissingMatchingEndif = 12, wekIllFormedOperator = 13,
    wekBadDefineStatement = 14, wekBadDefineStatementVaArgs = 15,
    wekBadDefineStatementVaOpt = 16, wekBadDefineStatementVaOptParens = 17,
    wekBadDefineStatementVaOptRecurse = 18, wekTooFewMacroarguments = 19,
    wekTooManyMacroarguments = 20, wekEmptyMacroarguments = 21,
    wekImproperlyTerminatedMacro = 22, wekBadLineStatement = 23,
    wekBadLineNumber = 24, wekBadLineFilename = 25,
    wekBadUndefineStatement = 26, wekBadMacroDefinition = 27,
    wekIllegalRedefinition = 28, wekDuplicateParameterName = 29,
    wekInvalidConcat = 30, wekLastLineNotTerminated = 31,
    wekIllFormedPragmaOption = 32, wekIncludeNestingTooDeep = 33,
    wekMisplacedOperator = 34, wekAlreadydefinedName = 35,
    wekUndefinedMacroname = 36, wekInvalidMacroname = 37,
    wekUnexpectedQualifiedName = 38, wekDivisionByZero = 39,
    wekIntegerOverflow = 40, wekIllegalOperatorRedefinition = 41,
    wekIllFormedIntegerLiteral = 42, wekIllFormedCharacterLiteral = 43,
    wekUnbalancedIfEndif = 44, wekCharacterLiteralOutOfRange = 45,
    wekCouldNotOpenOutputFile = 46, wekIncompatibleConfig = 47,
    wekIllFormedPragmaMessage = 48, wekPragmaMessageDirective = 49,
    wekLexerErrorBegin = 50, wekLexerUnexpectedError = 51,
    wekLexerUniversalCharInvalid = 52, wekLexerUniversalCharBaseCharset = 53,
    wekLexerUniversalCharNotAllowed = 54, wekLexerInvalidLongLongLiteral = 55,
    wekLexerGenericLexingError = 56, wekLexerGenericLexingWarning = 57
  WaveSeverityLevel* = enum
    wslRemark = 0, wslWarning = 1, wslError = 2, wslFatal = 3,
    wslCommandlineError = 4
  WaveLanguageModeImpl* = enum
    iwlmSupportNormal = 0, iwlmLongLong = 1, iwlmVariadics = 2,
    iwlmNoNewlineAtEndOfFIle = 3, iwlmHasInclude = 4, iwlmVaOpt = 5,
    iwlmEmitContline = 6, iwlmInsertWhitespace = 7, iwlmPreserveComments = 8,
    iwlmNoCharacterValidation = 9, iwlmConvertTrigraphs = 10,
    iwlmSingleLine = 11, iwlmPreferPpNumbers = 12, iwlmEmitLineDirectives = 13,
    iwlmIncludeGuardDetection = 14, iwlmEmitPragmaDirectives = 15, iwlmC99 = 16,
    iwlmCpp11 = 17, iwlmCpp17 = 18, iwlmCpp20 = 19
  EntryHandling* = enum
    EntryHandlingSkip = 0, EntryHandlingProcess = 1, EntryHandlingRaise = 2
  WaveProcessingHooksHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h".} = object
  
  WaveContextHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h".} = object
  
  WaveIteratorHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h".} = object
  
  WaveTokenHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h".} = object
  
  WaveContextImplHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h".} = object
  
  WaveTokenListHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h".} = object
  
  WaveTokenVectorHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h".} = object
  
  WaveMacroIteratorHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h".} = object
  
  WaveTokenListIteratorHandle* {.bycopy, incompleteStruct,
                                 header: "wave_c_api.h".} = object
  
  WavePosition* {.bycopy, header: "wave_c_api.h", importc: "WavePosition".} = object
    ## TYPE struct WaveTokenHandle {
                                                                                     ## char fake
                                                                                     ## } TYPE_NAME(WaveTokenHandle);
  
  FoundWarningDirectiveImplType* = proc (ctx: ptr WaveContextImplHandle;
      message: ptr WaveTokenListHandle; env: pointer): EntryHandling {.cdecl.}
  FoundWarningDirectiveImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
      message: ptr WaveTokenListHandle): EntryHandling
  EvaluatedConditionalExpressionImplType* = proc (
      ctx: ptr WaveContextImplHandle; directive: ptr WaveTokenHandle;
      expression: ptr WaveTokenListHandle; expression_value: bool; env: pointer): bool {.
      cdecl.}
  EvaluatedConditionalExpressionImplTypeNim* = proc (
      ctx: ptr WaveContextImplHandle; directive: ptr WaveTokenHandle;
      expression: ptr WaveTokenListHandle; expression_value: bool): bool
  FoundUnknownDirectiveImplType* = proc (ctx: ptr WaveContextImplHandle;
      line: ptr WaveTokenListHandle; pending: ptr WaveTokenListHandle;
      env: pointer): EntryHandling {.cdecl.}
  FoundUnknownDirectiveImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
      line: ptr WaveTokenListHandle; pending: ptr WaveTokenListHandle): EntryHandling
  ExpandingFunctionLikeMacroImplType* = proc (ctx: ptr WaveContextImplHandle;
      macrodef: ptr WaveTokenHandle; formal_args: ptr WaveTokenVectorHandle;
      definition: ptr WaveTokenListHandle; macrocall: ptr WaveTokenHandle;
      arguments: ptr WaveTokenVectorHandle; seqstart: pointer; seqend: pointer;
      env: pointer): bool {.cdecl.}
  ExpandingFunctionLikeMacroImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
      macrodef: ptr WaveTokenHandle; formal_args: ptr WaveTokenVectorHandle;
      definition: ptr WaveTokenListHandle; macrocall: ptr WaveTokenHandle;
      arguments: ptr WaveTokenVectorHandle; seqstart: pointer; seqend: pointer): bool
  ExpandingObjectLikeMacroImplType* = proc (ctx: ptr WaveContextImplHandle;
      argmacro: ptr WaveTokenHandle; definition: ptr WaveTokenListHandle;
      macrocall: ptr WaveTokenHandle; env: pointer): EntryHandling {.cdecl.}
  ExpandingObjectLikeMacroImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
      argmacro: ptr WaveTokenHandle; definition: ptr WaveTokenListHandle;
      macrocall: ptr WaveTokenHandle): EntryHandling
  ExpandedMacroImplType* = proc (ctx: ptr WaveContextImplHandle;
                                 result: ptr WaveTokenListHandle; env: pointer): void {.
      cdecl.}
  ExpandedMacroImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
                                    result: ptr WaveTokenListHandle): void
  RescannedMacroImplType* = proc (ctx: ptr WaveContextImplHandle;
                                  result: ptr WaveTokenListHandle; env: pointer): void {.
      cdecl.}
  RescannedMacroImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
                                     result: ptr WaveTokenListHandle): void
  FoundDirectiveImplType* = proc (ctx: ptr WaveContextImplHandle;
                                  tok: ptr WaveTokenHandle; env: pointer): EntryHandling {.
      cdecl.}
  FoundDirectiveImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
                                     tok: ptr WaveTokenHandle): EntryHandling
  FoundIncludeDirectiveImplType* = proc (context: ptr WaveContextImplHandle;
      impl: cstring; include_next: bool; env: pointer): EntryHandling {.cdecl.}
  FoundIncludeDirectiveImplTypeNim* = proc (context: ptr WaveContextImplHandle;
      impl: cstring; include_next: bool): EntryHandling
  SkippedTokenImplType* = proc (context: ptr WaveContextImplHandle;
                                token: ptr WaveTokenHandle; env: pointer): void {.
      cdecl.}
  SkippedTokenImplTypeNim* = proc (context: ptr WaveContextImplHandle;
                                   token: ptr WaveTokenHandle): void
  EmitLineDirectiveImplType* = proc (ctx: ptr WaveContextImplHandle;
                                     pending: ptr WaveTokenListHandle;
                                     act_token: ptr WaveTokenHandle;
                                     env: pointer): bool {.cdecl.}
  EmitLineDirectiveImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
                                        pending: ptr WaveTokenListHandle;
                                        act_token: ptr WaveTokenHandle): bool
  FoundLineDirectiveImplType* = proc (ctx: ptr WaveContextImplHandle;
                                      arguments: ptr WaveTokenListHandle;
                                      line: cuint; filename: cstring;
                                      env: pointer): EntryHandling {.cdecl.}
  FoundLineDirectiveImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
      arguments: ptr WaveTokenListHandle; line: cuint; filename: cstring): EntryHandling
  FoundErrorDirectiveImplType* = proc (ctx: ptr WaveContextImplHandle;
                                       message: ptr WaveTokenListHandle;
                                       env: pointer): EntryHandling {.cdecl.}
  FoundErrorDirectiveImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
      message: ptr WaveTokenListHandle): EntryHandling
  DefinedMacroImplType* = proc (ctx: ptr WaveContextImplHandle;
                                name: ptr WaveTokenHandle;
                                is_functionlike: bool;
                                parameters: ptr WaveTokenVectorHandle;
                                definition: ptr WaveTokenListHandle;
                                is_predefined: bool; env: pointer): void {.cdecl.}
  DefinedMacroImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
                                   name: ptr WaveTokenHandle;
                                   is_functionlike: bool;
                                   parameters: ptr WaveTokenVectorHandle;
                                   definition: ptr WaveTokenListHandle;
                                   is_predefined: bool): void
  LocateIncludeFileImplType* = proc (ctx: ptr WaveContextImplHandle;
                                     file_path: cstring; is_system: bool;
                                     current_name: cstring; dir_path: cstring;
                                     native_name: cstring; env: pointer): EntryHandling {.
      cdecl.}
  LocateIncludeFileImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
                                        file_path: cstring; is_system: bool;
                                        current_name: cstring;
                                        dir_path: cstring; native_name: cstring): EntryHandling
  OpenedIncludeFileImplType* = proc (ctx: ptr WaveContextImplHandle;
                                     rel_filename: cstring;
                                     abs_filename: cstring;
                                     is_system_include: bool; env: pointer): void {.
      cdecl.}
  OpenedIncludeFileImplTypeNim* = proc (ctx: ptr WaveContextImplHandle;
                                        rel_filename: cstring;
                                        abs_filename: cstring;
                                        is_system_include: bool): void
  ReturningFromIncludeFileImplType* = proc (arg0: ptr WaveContextImplHandle;
      env: pointer): void {.cdecl.}
  ReturningFromIncludeFileImplTypeNim* = proc (arg0: ptr WaveContextImplHandle): void
  WaveDiagnostics* {.bycopy, header: "wave_c_api.h", importc: "WaveDiagnostics".} = object
    ## Expanding function like macro
                                                                                           ## Expanding object like macro
                                                                                           ## Expanded macro
                                                                                           ## Rescanned macro
                                                                                           ## Found directive
                                                                                           ## Found include directive
                                                                                           ## Skipped token
                                                                                           ## Emit line directive
                                                                                           ## Found line directive
                                                                                           ## Found error directive
                                                                                           ## Defined macro
                                                                                           ## Locate include file
                                                                                           ## Opened include file
                                                                                           ## Returning from include file
    line*: cint
    column*: cint
    code*: WaveErrorCode
    level*: WaveSeverityLevel
    filename*: cstring
    errorText*: cstring

proc tokenVectorLen*(vec: ptr WaveTokenVectorHandle): cint {.dynlib: cwaveDl,
    importc: "wave_tokenVectorLen".}
proc tokenVectorGetAt*(vec: ptr WaveTokenVectorHandle; idx: cint): ptr WaveTokenHandle {.
    dynlib: cwaveDl, importc: "wave_tokenVectorGetAt".}
proc deleteWaveTokenVector*(vec: ptr WaveTokenVectorHandle): void {.
    dynlib: cwaveDl, importc: "wave_deleteWaveTokenVector".}
proc tokenListLen*(list: ptr WaveTokenListHandle): cint {.dynlib: cwaveDl,
    importc: "wave_tokenListLen".}
proc tokenListToStr*(list: ptr WaveTokenListHandle): cstring {.dynlib: cwaveDl,
    importc: "wave_tokenListToStr".}
proc tokenListBeginIterator*(l: ptr WaveTokenListHandle): ptr WaveTokenListIteratorHandle {.
    dynlib: cwaveDl, importc: "wave_tokenListBeginIterator".}
proc tokenListEndIterator*(l: ptr WaveTokenListHandle): ptr WaveTokenListIteratorHandle {.
    dynlib: cwaveDl, importc: "wave_tokenListEndIterator".}
proc neqListIterator*(i1: ptr WaveTokenListIteratorHandle;
                      i2: ptr WaveTokenListIteratorHandle): bool {.
    dynlib: cwaveDl, importc: "wave_neqListIterator".}
proc listIterDeref*(i: ptr WaveTokenListIteratorHandle): ptr WaveTokenHandle {.
    dynlib: cwaveDl, importc: "wave_listIterDeref".}
proc listIterAdvance*(i: ptr WaveTokenListIteratorHandle): void {.
    dynlib: cwaveDl, importc: "wave_listIterAdvance".}
proc newProcessingHooks*(): ptr WaveProcessingHooksHandle {.dynlib: cwaveDl,
    importc: "wave_newProcessingHooks".}
proc destroyProcessingHooks*(hooks: ptr WaveProcessingHooksHandle): void {.
    dynlib: cwaveDl, importc: "wave_destroyProcessingHooks".}
proc newWaveContext*(instring: cstring; filename: cstring): ptr WaveContextHandle {.
    dynlib: cwaveDl, importc: "wave_newWaveContext".}
proc setLanguageMode*(ctx: ptr WaveContextHandle; mode: WaveLanguageModeImpl): void {.
    dynlib: cwaveDl, importc: "wave_setLanguageMode".}
proc processAll*(context: ptr WaveContextHandle): void {.dynlib: cwaveDl,
    importc: "wave_processAll".}
proc setFoundWarningDirective*(context: ptr WaveContextHandle;
                               impl: FoundWarningDirectiveImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setFoundWarningDirective".}
proc setEvaluatedConditionalExpression*(context: ptr WaveContextHandle; impl: EvaluatedConditionalExpressionImplType;
                                        env: pointer): void {.dynlib: cwaveDl,
    importc: "wave_setEvaluatedConditionalExpression".}
proc setFoundUnknownDirectiveImplType*(context: ptr WaveContextHandle;
                                       impl: FoundUnknownDirectiveImplType;
                                       env: pointer): void {.dynlib: cwaveDl,
    importc: "wave_setFoundUnknownDirectiveImplType".}
proc setExpandingFunctionLikeMacro*(context: ptr WaveContextHandle;
                                    impl: ExpandingFunctionLikeMacroImplType;
                                    env: pointer): void {.dynlib: cwaveDl,
    importc: "wave_setExpandingFunctionLikeMacro".}
proc setExpandingObjectLikeMacro*(context: ptr WaveContextHandle;
                                  impl: ExpandingObjectLikeMacroImplType;
                                  env: pointer): void {.dynlib: cwaveDl,
    importc: "wave_setExpandingObjectLikeMacro".}
proc setExpandedMacro*(context: ptr WaveContextHandle;
                       impl: ExpandedMacroImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setExpandedMacro".}
proc setRescannedMacro*(context: ptr WaveContextHandle;
                        impl: RescannedMacroImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setRescannedMacro".}
proc setFoundDirective*(ctx: ptr WaveContextHandle;
                        filename: FoundDirectiveImplType; include_next: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setFoundDirective".}
proc setFoundIncludeDirective*(context: ptr WaveContextHandle;
                               impl: FoundIncludeDirectiveImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setFoundIncludeDirective".}
proc setSkippedToken*(context: ptr WaveContextHandle;
                      impl: SkippedTokenImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setSkippedToken".}
proc setEmitLineDirective*(context: ptr WaveContextHandle;
                           impl: EmitLineDirectiveImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setEmitLineDirective".}
proc setFoundLineDirective*(context: ptr WaveContextHandle;
                            impl: FoundLineDirectiveImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setFoundLineDirective".}
proc setFoundErrorDirective*(context: ptr WaveContextHandle;
                             impl: FoundErrorDirectiveImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setFoundErrorDirective".}
proc setDefinedMacro*(context: ptr WaveContextHandle;
                      impl: DefinedMacroImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setDefinedMacro".}
proc setLocateIncludeFile*(context: ptr WaveContextHandle;
                           impl: LocateIncludeFileImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setLocateIncludeFile".}
proc setOpenedIncludeFile*(context: ptr WaveContextHandle;
                           impl: OpenedIncludeFileImplType; env: pointer): void {.
    dynlib: cwaveDl, importc: "wave_setOpenedIncludeFile".}
proc setReturningFromIncludeFile*(context: ptr WaveContextHandle;
                                  impl: ReturningFromIncludeFileImplType;
                                  env: pointer): void {.dynlib: cwaveDl,
    importc: "wave_setReturningFromIncludeFile".}
proc destroyContext*(context: ptr WaveContextHandle): void {.dynlib: cwaveDl,
    importc: "wave_destroyContext".}
proc contextSetData*(context: ptr WaveContextHandle; data: pointer): void {.
    dynlib: cwaveDl, importc: "wave_contextSetData".}
proc contextGetData*(context: ptr WaveContextHandle): pointer {.dynlib: cwaveDl,
    importc: "wave_contextGetData".}
proc contextHasErrors*(context: ptr WaveContextHandle): bool {.dynlib: cwaveDl,
    importc: "wave_contextHasErrors".}
proc contextHasWarnings*(context: ptr WaveContextHandle): bool {.
    dynlib: cwaveDl, importc: "wave_contextHasWarnings".}
proc deleteDiagnostics*(diag: ptr WaveDiagnostics): void {.dynlib: cwaveDl,
    importc: "wave_deleteDiagnostics".}
proc contextPopDiagnostics*(context: ptr WaveContextHandle): WaveDiagnostics {.
    dynlib: cwaveDl, importc: "wave_contextPopDiagnostics".}
proc addMacroDefinition*(context: ptr WaveContextHandle; macrostring: cstring;
                         is_predefined: bool): bool {.dynlib: cwaveDl,
    importc: "wave_addMacroDefinition".}
proc removeMacroDefinition*(context: ptr WaveContextHandle;
                            macrostring: cstring; is_predefined: bool): bool {.
    dynlib: cwaveDl, importc: "wave_removeMacroDefinition".}
proc isDefinedMacro*(context: ptr WaveContextHandle; name: cstring): bool {.
    dynlib: cwaveDl, importc: "wave_isDefinedMacro".}
proc getMacroDefinition*(context: ptr WaveContextHandle; name: cstring;
                         is_function_style: ptr bool; is_predefined: ptr bool;
                         pos: ptr WavePosition;
                         parameters: ptr ptr WaveTokenVectorHandle;
                         definition: ptr ptr WaveTokenListHandle): bool {.
    dynlib: cwaveDl, importc: "wave_getMacroDefinition".}
proc macroBeginIterator*(context: ptr WaveContextHandle): ptr WaveMacroIteratorHandle {.
    dynlib: cwaveDl, importc: "wave_macroBeginIterator".}
proc macroEndIterator*(context: ptr WaveContextHandle): ptr WaveMacroIteratorHandle {.
    dynlib: cwaveDl, importc: "wave_macroEndIterator".}
proc neqMacroIterator*(i1: ptr WaveMacroIteratorHandle;
                       i2: ptr WaveMacroIteratorHandle): bool {.dynlib: cwaveDl,
    importc: "wave_neqMacroIterator".}
proc macroIteratorAdvance*(i: ptr WaveMacroIteratorHandle): void {.
    dynlib: cwaveDl, importc: "wave_macroIteratorAdvance".}
proc macroIteratorDeref*(i: ptr WaveMacroIteratorHandle): cstring {.
    dynlib: cwaveDl, importc: "wave_macroIteratorDeref".}
proc addSysincludePath*(context: ptr WaveContextHandle; path: cstring): bool {.
    dynlib: cwaveDl, importc: "wave_addSysincludePath".}
proc addIncludePath*(context: ptr WaveContextHandle; path: cstring): bool {.
    dynlib: cwaveDl, importc: "wave_addIncludePath".}
proc setSysincludeDelimiter*(context: ptr WaveContextHandle): void {.
    dynlib: cwaveDl, importc: "wave_setSysincludeDelimiter".}
proc setCurrentFilename*(context: ptr WaveContextHandle; name: cstring): void {.
    dynlib: cwaveDl, importc: "wave_setCurrentFilename".}
proc findIncludeFile*(ctx: ptr WaveContextHandle; str: ptr cstring;
                      dir: ptr cstring; is_system: bool; current_file: cstring): bool {.
    dynlib: cwaveDl, importc: "wave_findIncludeFile".}
proc getCurrentFilename*(context: ptr WaveContextHandle): cstring {.
    dynlib: cwaveDl, importc: "wave_getCurrentFilename".}
proc getCurrentDirectory*(context: ptr WaveContextHandle): cstring {.
    dynlib: cwaveDl, importc: "wave_getCurrentDirectory".}
proc getIterationDepth*(context: ptr WaveContextHandle): cint {.dynlib: cwaveDl,
    importc: "getIterationDepth".}
proc beginIterator*(context: ptr WaveContextHandle): ptr WaveIteratorHandle {.
    dynlib: cwaveDl, importc: "wave_beginIterator".}
proc endIterator*(context: ptr WaveContextHandle): ptr WaveIteratorHandle {.
    dynlib: cwaveDl, importc: "wave_endIterator".}
proc advanceIterator*(iter: ptr WaveIteratorHandle): void {.dynlib: cwaveDl,
    importc: "wave_advanceIterator".}
proc neqIterator*(iter1: ptr WaveIteratorHandle; iter2: ptr WaveIteratorHandle): bool {.
    dynlib: cwaveDl, importc: "wave_neqIterator".}
proc iterGetTok*(iter: ptr WaveIteratorHandle): ptr WaveTokenHandle {.
    dynlib: cwaveDl, importc: "wave_iterGetTok".}
proc deleteTok*(tok: ptr WaveTokenHandle): void {.dynlib: cwaveDl,
    importc: "wave_deleteTok".}
proc tokGetId*(tok: ptr WaveTokenHandle): WaveTokId {.dynlib: cwaveDl,
    importc: "wave_tokGetId".}
proc tokGetValue*(tok: ptr WaveTokenHandle): cstring {.dynlib: cwaveDl,
    importc: "wave_tokGetValue".}
proc unescapeIncludeToken*(s: cstring): cstring {.dynlib: cwaveDl,
    importc: "wave_unescapeIncludeToken".}