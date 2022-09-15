import std/os
const boostWaveLibDir = currentSourcePath().splitFile().dir / "../../../lib"
const cwaveDl* = boostWaveLibDir / "libboost_cwave.so"

type
  CEntryHandling* = enum
    cEntryHandlingSkip    = 0 shl 0
    cEntryHandlingProcess = 1 shl 0
    cEntryHandlingRaise   = 1 shl 1

  CWaveErrorCode* = enum
    cwekNoError                        = 0
    cwekUnexpectedError                = 1
    cwekMacroRedefinition              = 2
    cwekMacroInsertionError            = 3
    cwekBadIncludeFile                 = 4
    cwekBadIncludeStatement            = 5
    cwekBadHasIncludeExpression        = 6
    cwekIllFormedDirective             = 7
    cwekErrorDirective                 = 8
    cwekWarningDirective               = 9
    cwekIllFormedExpression            = 10
    cwekMissingMatchingIf              = 11
    cwekMissingMatchingEndif           = 12
    cwekIllFormedOperator              = 13
    cwekBadDefineStatement             = 14
    cwekBadDefineStatementVaArgs       = 15
    cwekBadDefineStatementVaOpt        = 16
    cwekBadDefineStatementVaOptParens  = 17
    cwekBadDefineStatementVaOptRecurse = 18
    cwekTooFewMacroarguments           = 19
    cwekTooManyMacroarguments          = 20
    cwekEmptyMacroarguments            = 21
    cwekImproperlyTerminatedMacro      = 22
    cwekBadLineStatement               = 23
    cwekBadLineNumber                  = 24
    cwekBadLineFilename                = 25
    cwekBadUndefineStatement           = 26
    cwekBadMacroDefinition             = 27
    cwekIllegalRedefinition            = 28
    cwekDuplicateParameterName         = 29
    cwekInvalidConcat                  = 30
    cwekLastLineNotTerminated          = 31
    cwekIllFormedPragmaOption          = 32
    cwekIncludeNestingTooDeep          = 33
    cwekMisplacedOperator              = 34
    cwekAlreadydefinedName             = 35
    cwekUndefinedMacroname             = 36
    cwekInvalidMacroname               = 37
    cwekUnexpectedQualifiedName        = 38
    cwekDivisionByZero                 = 39
    cwekIntegerOverflow                = 40
    cwekIllegalOperatorRedefinition    = 41
    cwekIllFormedIntegerLiteral        = 42
    cwekIllFormedCharacterLiteral      = 43
    cwekUnbalancedIfEndif              = 44
    cwekCharacterLiteralOutOfRange     = 45
    cwekCouldNotOpenOutputFile         = 46
    cwekIncompatibleConfig             = 47
    cwekIllFormedPragmaMessage         = 48
    cwekPragmaMessageDirective         = 49
    cwekLexerErrorBegin                = 50
    cwekLexerUnexpectedError           = 51
    cwekLexerUniversalCharInvalid      = 52
    cwekLexerUniversalCharBaseCharset  = 53
    cwekLexerUniversalCharNotAllowed   = 54
    cwekLexerInvalidLongLongLiteral    = 55
    cwekLexerGenericLexingError        = 56
    cwekLexerGenericLexingWarning      = 57

  CWaveLanguageModeImpl* = enum
    ciwlmSupportNormal         = 0
    ciwlmLongLong              = 1
    ciwlmVariadics             = 2
    ciwlmNoNewlineAtEndOfFIle  = 3
    ciwlmHasInclude            = 4
    ciwlmVaOpt                 = 5
    ciwlmEmitContline          = 6
    ciwlmInsertWhitespace      = 7
    ciwlmPreserveComments      = 8
    ciwlmNoCharacterValidation = 9
    ciwlmConvertTrigraphs      = 10
    ciwlmSingleLine            = 11
    ciwlmPreferPpNumbers       = 12
    ciwlmEmitLineDirectives    = 13
    ciwlmIncludeGuardDetection = 14
    ciwlmEmitPragmaDirectives  = 15
    ciwlmC99                   = 16
    ciwlmCpp11                 = 17
    ciwlmCpp17                 = 18
    ciwlmCpp20                 = 19

  CWaveSeverityLevel* = enum
    cwslRemark           = 0
    cwslWarning          = 1
    cwslError            = 2
    cwslFatal            = 3
    cwslCommandlineError = 4

  CWaveTokId* = enum
    ctokId_UNKNOWN                  = 0
    ctokId_FIRST_TOKEN              = 1
    ctokId_AND                      = 2
    ctokId_AND_ALT                  = 3
    ctokId_ANDAND                   = 4
    ctokId_ANDAND_ALT               = 5
    ctokId_ASSIGN                   = 6
    ctokId_ANDASSIGN                = 7
    ctokId_ANDASSIGN_ALT            = 8
    ctokId_OR                       = 9
    ctokId_OR_ALT                   = 10
    ctokId_OR_TRIGRAPH              = 11
    ctokId_ORASSIGN                 = 12
    ctokId_ORASSIGN_ALT             = 13
    ctokId_ORASSIGN_TRIGRAPH        = 14
    ctokId_XOR                      = 15
    ctokId_XOR_ALT                  = 16
    ctokId_XOR_TRIGRAPH             = 17
    ctokId_XORASSIGN                = 18
    ctokId_XORASSIGN_ALT            = 19
    ctokId_XORASSIGN_TRIGRAPH       = 20
    ctokId_COMMA                    = 21
    ctokId_COLON                    = 22
    ctokId_DIVIDE                   = 23
    ctokId_DIVIDEASSIGN             = 24
    ctokId_DOT                      = 25
    ctokId_DOTSTAR                  = 26
    ctokId_ELLIPSIS                 = 27
    ctokId_EQUAL                    = 28
    ctokId_GREATER                  = 29
    ctokId_GREATEREQUAL             = 30
    ctokId_LEFTBRACE                = 31
    ctokId_LEFTBRACE_ALT            = 32
    ctokId_LEFTBRACE_TRIGRAPH       = 33
    ctokId_LESS                     = 34
    ctokId_LESSEQUAL                = 35
    ctokId_LEFTPAREN                = 36
    ctokId_LEFTBRACKET              = 37
    ctokId_LEFTBRACKET_ALT          = 38
    ctokId_LEFTBRACKET_TRIGRAPH     = 39
    ctokId_MINUS                    = 40
    ctokId_MINUSASSIGN              = 41
    ctokId_MINUSMINUS               = 42
    ctokId_PERCENT                  = 43
    ctokId_PERCENTASSIGN            = 44
    ctokId_NOT                      = 45
    ctokId_NOT_ALT                  = 46
    ctokId_NOTEQUAL                 = 47
    ctokId_NOTEQUAL_ALT             = 48
    ctokId_OROR                     = 49
    ctokId_OROR_ALT                 = 50
    ctokId_OROR_TRIGRAPH            = 51
    ctokId_PLUS                     = 52
    ctokId_PLUSASSIGN               = 53
    ctokId_PLUSPLUS                 = 54
    ctokId_ARROW                    = 55
    ctokId_ARROWSTAR                = 56
    ctokId_QUESTION_MARK            = 57
    ctokId_RIGHTBRACE               = 58
    ctokId_RIGHTBRACE_ALT           = 59
    ctokId_RIGHTBRACE_TRIGRAPH      = 60
    ctokId_RIGHTPAREN               = 61
    ctokId_RIGHTBRACKET             = 62
    ctokId_RIGHTBRACKET_ALT         = 63
    ctokId_RIGHTBRACKET_TRIGRAPH    = 64
    ctokId_COLON_COLON              = 65
    ctokId_SEMICOLON                = 66
    ctokId_SHIFTLEFT                = 67
    ctokId_SHIFTLEFTASSIGN          = 68
    ctokId_SHIFTRIGHT               = 69
    ctokId_SHIFTRIGHTASSIGN         = 70
    ctokId_STAR                     = 71
    ctokId_COMPL                    = 72
    ctokId_COMPL_ALT                = 73
    ctokId_COMPL_TRIGRAPH           = 74
    ctokId_STARASSIGN               = 75
    ctokId_ASM                      = 76
    ctokId_AUTO                     = 77
    ctokId_BOOL                     = 78
    ctokId_FALSE                    = 79
    ctokId_TRUE                     = 80
    ctokId_BREAK                    = 81
    ctokId_CASE                     = 82
    ctokId_CATCH                    = 83
    ctokId_CHAR                     = 84
    ctokId_CLASS                    = 85
    ctokId_CONST                    = 86
    ctokId_CONSTCAST                = 87
    ctokId_CONTINUE                 = 88
    ctokId_DEFAULT                  = 89
    ctokId_DELETE                   = 90
    ctokId_DO                       = 91
    ctokId_DOUBLE                   = 92
    ctokId_DYNAMICCAST              = 93
    ctokId_ELSE                     = 94
    ctokId_ENUM                     = 95
    ctokId_EXPLICIT                 = 96
    ctokId_EXPORT                   = 97
    ctokId_EXTERN                   = 98
    ctokId_FLOAT                    = 99
    ctokId_FOR                      = 100
    ctokId_FRIEND                   = 101
    ctokId_GOTO                     = 102
    ctokId_IF                       = 103
    ctokId_INLINE                   = 104
    ctokId_INT                      = 105
    ctokId_LONG                     = 106
    ctokId_MUTABLE                  = 107
    ctokId_NAMESPACE                = 108
    ctokId_NEW                      = 109
    ctokId_OPERATOR                 = 110
    ctokId_PRIVATE                  = 111
    ctokId_PROTECTED                = 112
    ctokId_PUBLIC                   = 113
    ctokId_REGISTER                 = 114
    ctokId_REINTERPRETCAST          = 115
    ctokId_RETURN                   = 116
    ctokId_SHORT                    = 117
    ctokId_SIGNED                   = 118
    ctokId_SIZEOF                   = 119
    ctokId_STATIC                   = 120
    ctokId_STATICCAST               = 121
    ctokId_STRUCT                   = 122
    ctokId_SWITCH                   = 123
    ctokId_TEMPLATE                 = 124
    ctokId_THIS                     = 125
    ctokId_THROW                    = 126
    ctokId_TRY                      = 127
    ctokId_TYPEDEF                  = 128
    ctokId_TYPEID                   = 129
    ctokId_TYPENAME                 = 130
    ctokId_UNION                    = 131
    ctokId_UNSIGNED                 = 132
    ctokId_USING                    = 133
    ctokId_VIRTUAL                  = 134
    ctokId_VOID                     = 135
    ctokId_VOLATILE                 = 136
    ctokId_WCHART                   = 137
    ctokId_WHILE                    = 138
    ctokId_PP_DEFINE                = 139
    ctokId_PP_IF                    = 140
    ctokId_PP_IFDEF                 = 141
    ctokId_PP_IFNDEF                = 142
    ctokId_PP_ELSE                  = 143
    ctokId_PP_ELIF                  = 144
    ctokId_PP_ENDIF                 = 145
    ctokId_PP_ERROR                 = 146
    ctokId_PP_LINE                  = 147
    ctokId_PP_PRAGMA                = 148
    ctokId_PP_UNDEF                 = 149
    ctokId_PP_WARNING               = 150
    ctokId_IDENTIFIER               = 151
    ctokId_OCTALINT                 = 152
    ctokId_DECIMALINT               = 153
    ctokId_HEXAINT                  = 154
    ctokId_INTLIT                   = 155
    ctokId_LONGINTLIT               = 156
    ctokId_FLOATLIT                 = 157
    ctokId_FIXEDPOINTLIT            = 158
    ctokId_CCOMMENT                 = 159
    ctokId_CPPCOMMENT               = 160
    ctokId_CHARLIT                  = 161
    ctokId_STRINGLIT                = 162
    ctokId_CONTLINE                 = 163
    ctokId_SPACE                    = 164
    ctokId_SPACE2                   = 165
    ctokId_NEWLINE                  = 166
    ctokId_GENERATEDNEWLINE         = 167
    ctokId_POUND_POUND              = 168
    ctokId_POUND_POUND_ALT          = 169
    ctokId_POUND_POUND_TRIGRAPH     = 170
    ctokId_POUND                    = 171
    ctokId_POUND_ALT                = 172
    ctokId_POUND_TRIGRAPH           = 173
    ctokId_ANY                      = 174
    ctokId_ANY_TRIGRAPH             = 175
    ctokId_PP_INCLUDE               = 176
    ctokId_PP_QHEADER               = 177
    ctokId_PP_HHEADER               = 178
    ctokId_PP_INCLUDE_NEXT          = 179
    ctokId_PP_QHEADER_NEXT          = 180
    ctokId_PP_HHEADER_NEXT          = 181
    ctokId_EOF                      = 182
    ctokId_EOI                      = 183
    ctokId_PP_NUMBER                = 184
    ctokId_MSEXT_INT8               = 185
    ctokId_MSEXT_INT16              = 186
    ctokId_MSEXT_INT32              = 187
    ctokId_MSEXT_INT64              = 188
    ctokId_MSEXT_BASED              = 189
    ctokId_MSEXT_DECLSPEC           = 190
    ctokId_MSEXT_CDECL              = 191
    ctokId_MSEXT_FASTCALL           = 192
    ctokId_MSEXT_STDCALL            = 193
    ctokId_MSEXT_TRY                = 194
    ctokId_MSEXT_EXCEPT             = 195
    ctokId_MSEXT_FINALLY            = 196
    ctokId_MSEXT_LEAVE              = 197
    ctokId_MSEXT_INLINE             = 198
    ctokId_MSEXT_ASM                = 199
    ctokId_MSEXT_PP_REGION          = 200
    ctokId_MSEXT_PP_ENDREGION       = 201
    ctokId_IMPORT                   = 202
    ctokId_ALIGNAS                  = 203
    ctokId_ALIGNOF                  = 204
    ctokId_CHAR16_T                 = 205
    ctokId_CHAR32_T                 = 206
    ctokId_CONSTEXPR                = 207
    ctokId_DECLTYPE                 = 208
    ctokId_NOEXCEPT                 = 209
    ctokId_NULLPTR                  = 210
    ctokId_STATICASSERT             = 211
    ctokId_THREADLOCAL              = 212
    ctokId_RAWSTRINGLIT             = 213
    ctokId_CHAR8_T                  = 214
    ctokId_CONCEPT                  = 215
    ctokId_CONSTEVAL                = 216
    ctokId_CONSTINIT                = 217
    ctokId_CO_AWAIT                 = 218
    ctokId_CO_RETURN                = 219
    ctokId_CO_YIELD                 = 220
    ctokId_REQUIRES                 = 221
    ctokId_SPACESHIP                = 222
    ctokId_LAST_TOKEN_ID            = 223
    ctokId_LAST_TOKEN               = 224
    ctokId_UNKNOWN_UNIVERSALCHAR    = 225
    ctokId_NONREPLACABLE_IDENTIFIER = 226
    ctokId_PLACEHOLDER              = 227
    ctokId_PLACEMARKER              = 228
    ctokId_PARAMETERBASE            = 229
    ctokId_EXTPARAMETERBASE         = 230
    ctokId_OPTPARAMETERBASE         = 231

  DefinedMacroImplType* =
    proc(ctx:             ptr WaveContextImplHandle,
         name:            ptr WaveTokenHandle,
         is_functionlike: bool,
         parameters:      ptr WaveTokenVectorHandle,
         definition:      ptr WaveTokenListHandle,
         is_predefined:   bool,
         env:             pointer): void {.cdecl.}

  DefinedMacroImplTypeNim* =
    proc(ctx:             ptr WaveContextImplHandle,
         name:            ptr WaveTokenHandle,
         is_functionlike: bool,
         parameters:      ptr WaveTokenVectorHandle,
         definition:      ptr WaveTokenListHandle,
         is_predefined:   bool): void

  EmitLineDirectiveImplType* =
    proc(ctx:       ptr WaveContextImplHandle,
         pending:   ptr WaveTokenListHandle,
         act_token: ptr WaveTokenHandle,
         env:       pointer): bool {.cdecl.}

  EmitLineDirectiveImplTypeNim* =
    proc(ctx:       ptr WaveContextImplHandle,
         pending:   ptr WaveTokenListHandle,
         act_token: ptr WaveTokenHandle): bool

  EntryHandling* {.size: sizeof(cint).} = enum
    EntryHandlingSkip
    EntryHandlingProcess
    EntryHandlingRaise

  EvaluatedConditionalExpressionImplType* =
    proc(ctx:              ptr WaveContextImplHandle,
         directive:        ptr WaveTokenHandle,
         expression:       ptr WaveTokenListHandle,
         expression_value: bool,
         env:              pointer): bool {.cdecl.}

  EvaluatedConditionalExpressionImplTypeNim* =
    proc(ctx:              ptr WaveContextImplHandle,
         directive:        ptr WaveTokenHandle,
         expression:       ptr WaveTokenListHandle,
         expression_value: bool): bool

  ExpandedMacroImplType* =
    proc(ctx:    ptr WaveContextImplHandle,
         result: ptr WaveTokenListHandle,
         env:    pointer): void {.cdecl.}

  ExpandedMacroImplTypeNim* =
    proc(ctx:    ptr WaveContextImplHandle,
         result: ptr WaveTokenListHandle): void

  ExpandingFunctionLikeMacroImplType* =
    proc(ctx:         ptr WaveContextImplHandle,
         macrodef:    ptr WaveTokenHandle,
         formal_args: ptr WaveTokenVectorHandle,
         definition:  ptr WaveTokenListHandle,
         macrocall:   ptr WaveTokenHandle,
         arguments:   ptr WaveTokenVectorHandle,
         seqstart:    pointer,
         seqend:      pointer,
         env:         pointer): bool {.cdecl.}

  ExpandingFunctionLikeMacroImplTypeNim* =
    proc(ctx:         ptr WaveContextImplHandle,
         macrodef:    ptr WaveTokenHandle,
         formal_args: ptr WaveTokenVectorHandle,
         definition:  ptr WaveTokenListHandle,
         macrocall:   ptr WaveTokenHandle,
         arguments:   ptr WaveTokenVectorHandle,
         seqstart:    pointer,
         seqend:      pointer): bool

  ExpandingObjectLikeMacroImplType* =
    proc(ctx:        ptr WaveContextImplHandle,
         argmacro:   ptr WaveTokenHandle,
         definition: ptr WaveTokenListHandle,
         macrocall:  ptr WaveTokenHandle,
         env:        pointer): CEntryHandling {.cdecl.}

  ExpandingObjectLikeMacroImplTypeNim* =
    proc(ctx:        ptr WaveContextImplHandle,
         argmacro:   ptr WaveTokenHandle,
         definition: ptr WaveTokenListHandle,
         macrocall:  ptr WaveTokenHandle): CEntryHandling

  FoundDirectiveImplType* =
    proc(ctx: ptr WaveContextImplHandle,
         tok: ptr WaveTokenHandle,
         env: pointer): CEntryHandling {.cdecl.}

  FoundDirectiveImplTypeNim* =
    proc(ctx: ptr WaveContextImplHandle,
         tok: ptr WaveTokenHandle): CEntryHandling

  FoundErrorDirectiveImplType* =
    proc(ctx:     ptr WaveContextImplHandle,
         message: ptr WaveTokenListHandle,
         env:     pointer): CEntryHandling {.cdecl.}

  FoundErrorDirectiveImplTypeNim* =
    proc(ctx:     ptr WaveContextImplHandle,
         message: ptr WaveTokenListHandle): CEntryHandling

  FoundIncludeDirectiveImplType* =
    proc(context:      ptr WaveContextImplHandle,
         impl:         cstring,
         include_next: bool,
         env:          pointer): CEntryHandling {.cdecl.}

  FoundIncludeDirectiveImplTypeNim* =
    proc(context:      ptr WaveContextImplHandle,
         impl:         cstring,
         include_next: bool): CEntryHandling

  FoundLineDirectiveImplType* =
    proc(ctx:       ptr WaveContextImplHandle,
         arguments: ptr WaveTokenListHandle,
         line:      cuint,
         filename:  cstring,
         env:       pointer): CEntryHandling {.cdecl.}

  FoundLineDirectiveImplTypeNim* =
    proc(ctx:       ptr WaveContextImplHandle,
         arguments: ptr WaveTokenListHandle,
         line:      cuint,
         filename:  cstring): CEntryHandling

  FoundUnknownDirectiveImplType* =
    proc(ctx:     ptr WaveContextImplHandle,
         line:    ptr WaveTokenListHandle,
         pending: ptr WaveTokenListHandle,
         env:     pointer): CEntryHandling {.cdecl.}

  FoundUnknownDirectiveImplTypeNim* =
    proc(ctx:     ptr WaveContextImplHandle,
         line:    ptr WaveTokenListHandle,
         pending: ptr WaveTokenListHandle): CEntryHandling

  FoundWarningDirectiveImplType* =
    proc(ctx:     ptr WaveContextImplHandle,
         message: ptr WaveTokenListHandle,
         env:     pointer): CEntryHandling {.cdecl.}

  FoundWarningDirectiveImplTypeNim* =
    proc(ctx:     ptr WaveContextImplHandle,
         message: ptr WaveTokenListHandle): CEntryHandling

  LocateIncludeFileImplType* =
    proc(ctx:          ptr WaveContextImplHandle,
         file_path:    cstring,
         is_system:    bool,
         current_name: cstring,
         dir_path:     cstring,
         native_name:  cstring,
         env:          pointer): CEntryHandling {.cdecl.}

  LocateIncludeFileImplTypeNim* =
    proc(ctx:          ptr WaveContextImplHandle,
         file_path:    cstring,
         is_system:    bool,
         current_name: cstring,
         dir_path:     cstring,
         native_name:  cstring): CEntryHandling

  OpenedIncludeFileImplType* =
    proc(ctx:               ptr WaveContextImplHandle,
         rel_filename:      cstring,
         abs_filename:      cstring,
         is_system_include: bool,
         env:               pointer): void {.cdecl.}

  OpenedIncludeFileImplTypeNim* =
    proc(ctx:               ptr WaveContextImplHandle,
         rel_filename:      cstring,
         abs_filename:      cstring,
         is_system_include: bool): void

  RescannedMacroImplType* =
    proc(ctx:    ptr WaveContextImplHandle,
         result: ptr WaveTokenListHandle,
         env:    pointer): void {.cdecl.}

  RescannedMacroImplTypeNim* =
    proc(ctx:    ptr WaveContextImplHandle,
         result: ptr WaveTokenListHandle): void

  ReturningFromIncludeFileImplType* =
    proc(arg0: ptr WaveContextImplHandle,
         env:  pointer): void {.cdecl.}

  ReturningFromIncludeFileImplTypeNim* =
    proc(arg0: ptr WaveContextImplHandle): void

  SkippedTokenImplType* =
    proc(context: ptr WaveContextImplHandle,
         token:   ptr WaveTokenHandle,
         env:     pointer): void {.cdecl.}

  SkippedTokenImplTypeNim* =
    proc(context: ptr WaveContextImplHandle,
         token:   ptr WaveTokenHandle): void

  WaveContextHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h", importc.} = object


  WaveContextImplHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h", importc.} = object


  WaveDiagnostics* {.bycopy, header: "wave_c_api.h", importc.} = object
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
    line*:      cint
    column*:    cint
    code*:      CWaveErrorCode
    level*:     CWaveSeverityLevel
    filename*:  cstring
    errorText*: cstring

  WaveErrorCode* = enum
    wekNoError
    wekUnexpectedError
    wekMacroRedefinition
    wekMacroInsertionError
    wekBadIncludeFile
    wekBadIncludeStatement
    wekBadHasIncludeExpression
    wekIllFormedDirective
    wekErrorDirective
    wekWarningDirective
    wekIllFormedExpression
    wekMissingMatchingIf
    wekMissingMatchingEndif
    wekIllFormedOperator
    wekBadDefineStatement
    wekBadDefineStatementVaArgs
    wekBadDefineStatementVaOpt
    wekBadDefineStatementVaOptParens
    wekBadDefineStatementVaOptRecurse
    wekTooFewMacroarguments
    wekTooManyMacroarguments
    wekEmptyMacroarguments
    wekImproperlyTerminatedMacro
    wekBadLineStatement
    wekBadLineNumber
    wekBadLineFilename
    wekBadUndefineStatement
    wekBadMacroDefinition
    wekIllegalRedefinition
    wekDuplicateParameterName
    wekInvalidConcat
    wekLastLineNotTerminated
    wekIllFormedPragmaOption
    wekIncludeNestingTooDeep
    wekMisplacedOperator
    wekAlreadydefinedName
    wekUndefinedMacroname
    wekInvalidMacroname
    wekUnexpectedQualifiedName
    wekDivisionByZero
    wekIntegerOverflow
    wekIllegalOperatorRedefinition
    wekIllFormedIntegerLiteral
    wekIllFormedCharacterLiteral
    wekUnbalancedIfEndif
    wekCharacterLiteralOutOfRange
    wekCouldNotOpenOutputFile
    wekIncompatibleConfig
    wekIllFormedPragmaMessage
    wekPragmaMessageDirective
    wekLexerErrorBegin
    wekLexerUnexpectedError
    wekLexerUniversalCharInvalid
    wekLexerUniversalCharBaseCharset
    wekLexerUniversalCharNotAllowed
    wekLexerInvalidLongLongLiteral
    wekLexerGenericLexingError
    wekLexerGenericLexingWarning

  WaveIteratorHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h", importc.} = object


  WaveLanguageModeImpl* = enum
    iwlmSupportNormal
    iwlmLongLong
    iwlmVariadics
    iwlmNoNewlineAtEndOfFIle
    iwlmHasInclude
    iwlmVaOpt
    iwlmEmitContline
    iwlmInsertWhitespace
    iwlmPreserveComments
    iwlmNoCharacterValidation
    iwlmConvertTrigraphs
    iwlmSingleLine
    iwlmPreferPpNumbers
    iwlmEmitLineDirectives
    iwlmIncludeGuardDetection
    iwlmEmitPragmaDirectives
    iwlmC99
    iwlmCpp11
    iwlmCpp17
    iwlmCpp20

  WaveMacroIteratorHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h", importc.} = object


  WavePosition* {.bycopy, header: "wave_c_api.h", importc.} = object
    ## TYPE struct WaveTokenHandle {
    ## char fake
    ## } TYPE_NAME(WaveTokenHandle);

  WaveProcessingHooksHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h", importc.} = object


  WaveSeverityLevel* = enum
    wslRemark
    wslWarning
    wslError
    wslFatal
    wslCommandlineError

  WaveTokId* = enum
    tokId_UNKNOWN
    tokId_FIRST_TOKEN
    tokId_AND
    tokId_AND_ALT
    tokId_ANDAND
    tokId_ANDAND_ALT
    tokId_ASSIGN
    tokId_ANDASSIGN
    tokId_ANDASSIGN_ALT
    tokId_OR
    tokId_OR_ALT
    tokId_OR_TRIGRAPH
    tokId_ORASSIGN
    tokId_ORASSIGN_ALT
    tokId_ORASSIGN_TRIGRAPH
    tokId_XOR
    tokId_XOR_ALT
    tokId_XOR_TRIGRAPH
    tokId_XORASSIGN
    tokId_XORASSIGN_ALT
    tokId_XORASSIGN_TRIGRAPH
    tokId_COMMA
    tokId_COLON
    tokId_DIVIDE
    tokId_DIVIDEASSIGN
    tokId_DOT
    tokId_DOTSTAR
    tokId_ELLIPSIS
    tokId_EQUAL
    tokId_GREATER
    tokId_GREATEREQUAL
    tokId_LEFTBRACE
    tokId_LEFTBRACE_ALT
    tokId_LEFTBRACE_TRIGRAPH
    tokId_LESS
    tokId_LESSEQUAL
    tokId_LEFTPAREN
    tokId_LEFTBRACKET
    tokId_LEFTBRACKET_ALT
    tokId_LEFTBRACKET_TRIGRAPH
    tokId_MINUS
    tokId_MINUSASSIGN
    tokId_MINUSMINUS
    tokId_PERCENT
    tokId_PERCENTASSIGN
    tokId_NOT
    tokId_NOT_ALT
    tokId_NOTEQUAL
    tokId_NOTEQUAL_ALT
    tokId_OROR
    tokId_OROR_ALT
    tokId_OROR_TRIGRAPH
    tokId_PLUS
    tokId_PLUSASSIGN
    tokId_PLUSPLUS
    tokId_ARROW
    tokId_ARROWSTAR
    tokId_QUESTION_MARK
    tokId_RIGHTBRACE
    tokId_RIGHTBRACE_ALT
    tokId_RIGHTBRACE_TRIGRAPH
    tokId_RIGHTPAREN
    tokId_RIGHTBRACKET
    tokId_RIGHTBRACKET_ALT
    tokId_RIGHTBRACKET_TRIGRAPH
    tokId_COLON_COLON
    tokId_SEMICOLON
    tokId_SHIFTLEFT
    tokId_SHIFTLEFTASSIGN
    tokId_SHIFTRIGHT
    tokId_SHIFTRIGHTASSIGN
    tokId_STAR
    tokId_COMPL
    tokId_COMPL_ALT
    tokId_COMPL_TRIGRAPH
    tokId_STARASSIGN
    tokId_ASM
    tokId_AUTO
    tokId_BOOL
    tokId_FALSE
    tokId_TRUE
    tokId_BREAK
    tokId_CASE
    tokId_CATCH
    tokId_CHAR
    tokId_CLASS
    tokId_CONST
    tokId_CONSTCAST
    tokId_CONTINUE
    tokId_DEFAULT
    tokId_DELETE
    tokId_DO
    tokId_DOUBLE
    tokId_DYNAMICCAST
    tokId_ELSE
    tokId_ENUM
    tokId_EXPLICIT
    tokId_EXPORT
    tokId_EXTERN
    tokId_FLOAT
    tokId_FOR
    tokId_FRIEND
    tokId_GOTO
    tokId_IF
    tokId_INLINE
    tokId_INT
    tokId_LONG
    tokId_MUTABLE
    tokId_NAMESPACE
    tokId_NEW
    tokId_OPERATOR
    tokId_PRIVATE
    tokId_PROTECTED
    tokId_PUBLIC
    tokId_REGISTER
    tokId_REINTERPRETCAST
    tokId_RETURN
    tokId_SHORT
    tokId_SIGNED
    tokId_SIZEOF
    tokId_STATIC
    tokId_STATICCAST
    tokId_STRUCT
    tokId_SWITCH
    tokId_TEMPLATE
    tokId_THIS
    tokId_THROW
    tokId_TRY
    tokId_TYPEDEF
    tokId_TYPEID
    tokId_TYPENAME
    tokId_UNION
    tokId_UNSIGNED
    tokId_USING
    tokId_VIRTUAL
    tokId_VOID
    tokId_VOLATILE
    tokId_WCHART
    tokId_WHILE
    tokId_PP_DEFINE
    tokId_PP_IF
    tokId_PP_IFDEF
    tokId_PP_IFNDEF
    tokId_PP_ELSE
    tokId_PP_ELIF
    tokId_PP_ENDIF
    tokId_PP_ERROR
    tokId_PP_LINE
    tokId_PP_PRAGMA
    tokId_PP_UNDEF
    tokId_PP_WARNING
    tokId_IDENTIFIER
    tokId_OCTALINT
    tokId_DECIMALINT
    tokId_HEXAINT
    tokId_INTLIT
    tokId_LONGINTLIT
    tokId_FLOATLIT
    tokId_FIXEDPOINTLIT
    tokId_CCOMMENT
    tokId_CPPCOMMENT
    tokId_CHARLIT
    tokId_STRINGLIT
    tokId_CONTLINE
    tokId_SPACE
    tokId_SPACE2
    tokId_NEWLINE
    tokId_GENERATEDNEWLINE
    tokId_POUND_POUND
    tokId_POUND_POUND_ALT
    tokId_POUND_POUND_TRIGRAPH
    tokId_POUND
    tokId_POUND_ALT
    tokId_POUND_TRIGRAPH
    tokId_ANY
    tokId_ANY_TRIGRAPH
    tokId_PP_INCLUDE
    tokId_PP_QHEADER
    tokId_PP_HHEADER
    tokId_PP_INCLUDE_NEXT
    tokId_PP_QHEADER_NEXT
    tokId_PP_HHEADER_NEXT
    tokId_EOF
    tokId_EOI
    tokId_PP_NUMBER
    tokId_MSEXT_INT8
    tokId_MSEXT_INT16
    tokId_MSEXT_INT32
    tokId_MSEXT_INT64
    tokId_MSEXT_BASED
    tokId_MSEXT_DECLSPEC
    tokId_MSEXT_CDECL
    tokId_MSEXT_FASTCALL
    tokId_MSEXT_STDCALL
    tokId_MSEXT_TRY
    tokId_MSEXT_EXCEPT
    tokId_MSEXT_FINALLY
    tokId_MSEXT_LEAVE
    tokId_MSEXT_INLINE
    tokId_MSEXT_ASM
    tokId_MSEXT_PP_REGION
    tokId_MSEXT_PP_ENDREGION
    tokId_IMPORT
    tokId_ALIGNAS
    tokId_ALIGNOF
    tokId_CHAR16_T
    tokId_CHAR32_T
    tokId_CONSTEXPR
    tokId_DECLTYPE
    tokId_NOEXCEPT
    tokId_NULLPTR
    tokId_STATICASSERT
    tokId_THREADLOCAL
    tokId_RAWSTRINGLIT
    tokId_CHAR8_T
    tokId_CONCEPT
    tokId_CONSTEVAL
    tokId_CONSTINIT
    tokId_CO_AWAIT
    tokId_CO_RETURN
    tokId_CO_YIELD
    tokId_REQUIRES
    tokId_SPACESHIP
    tokId_LAST_TOKEN_ID
    tokId_LAST_TOKEN
    tokId_UNKNOWN_UNIVERSALCHAR
    tokId_NONREPLACABLE_IDENTIFIER
    tokId_PLACEHOLDER
    tokId_PLACEMARKER
    tokId_PARAMETERBASE
    tokId_EXTPARAMETERBASE
    tokId_OPTPARAMETERBASE

  WaveTokenHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h", importc.} = object


  WaveTokenListHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h", importc.} = object


  WaveTokenListIteratorHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h", importc.} = object


  WaveTokenVectorHandle* {.bycopy, incompleteStruct, header: "wave_c_api.h", importc.} = object


converter toCWaveTokId*(arg: WaveTokId): CWaveTokId =
  case arg:
    of tokId_UNKNOWN:                  ctokId_UNKNOWN
    of tokId_FIRST_TOKEN:              ctokId_FIRST_TOKEN
    of tokId_AND:                      ctokId_AND
    of tokId_AND_ALT:                  ctokId_AND_ALT
    of tokId_ANDAND:                   ctokId_ANDAND
    of tokId_ANDAND_ALT:               ctokId_ANDAND_ALT
    of tokId_ASSIGN:                   ctokId_ASSIGN
    of tokId_ANDASSIGN:                ctokId_ANDASSIGN
    of tokId_ANDASSIGN_ALT:            ctokId_ANDASSIGN_ALT
    of tokId_OR:                       ctokId_OR
    of tokId_OR_ALT:                   ctokId_OR_ALT
    of tokId_OR_TRIGRAPH:              ctokId_OR_TRIGRAPH
    of tokId_ORASSIGN:                 ctokId_ORASSIGN
    of tokId_ORASSIGN_ALT:             ctokId_ORASSIGN_ALT
    of tokId_ORASSIGN_TRIGRAPH:        ctokId_ORASSIGN_TRIGRAPH
    of tokId_XOR:                      ctokId_XOR
    of tokId_XOR_ALT:                  ctokId_XOR_ALT
    of tokId_XOR_TRIGRAPH:             ctokId_XOR_TRIGRAPH
    of tokId_XORASSIGN:                ctokId_XORASSIGN
    of tokId_XORASSIGN_ALT:            ctokId_XORASSIGN_ALT
    of tokId_XORASSIGN_TRIGRAPH:       ctokId_XORASSIGN_TRIGRAPH
    of tokId_COMMA:                    ctokId_COMMA
    of tokId_COLON:                    ctokId_COLON
    of tokId_DIVIDE:                   ctokId_DIVIDE
    of tokId_DIVIDEASSIGN:             ctokId_DIVIDEASSIGN
    of tokId_DOT:                      ctokId_DOT
    of tokId_DOTSTAR:                  ctokId_DOTSTAR
    of tokId_ELLIPSIS:                 ctokId_ELLIPSIS
    of tokId_EQUAL:                    ctokId_EQUAL
    of tokId_GREATER:                  ctokId_GREATER
    of tokId_GREATEREQUAL:             ctokId_GREATEREQUAL
    of tokId_LEFTBRACE:                ctokId_LEFTBRACE
    of tokId_LEFTBRACE_ALT:            ctokId_LEFTBRACE_ALT
    of tokId_LEFTBRACE_TRIGRAPH:       ctokId_LEFTBRACE_TRIGRAPH
    of tokId_LESS:                     ctokId_LESS
    of tokId_LESSEQUAL:                ctokId_LESSEQUAL
    of tokId_LEFTPAREN:                ctokId_LEFTPAREN
    of tokId_LEFTBRACKET:              ctokId_LEFTBRACKET
    of tokId_LEFTBRACKET_ALT:          ctokId_LEFTBRACKET_ALT
    of tokId_LEFTBRACKET_TRIGRAPH:     ctokId_LEFTBRACKET_TRIGRAPH
    of tokId_MINUS:                    ctokId_MINUS
    of tokId_MINUSASSIGN:              ctokId_MINUSASSIGN
    of tokId_MINUSMINUS:               ctokId_MINUSMINUS
    of tokId_PERCENT:                  ctokId_PERCENT
    of tokId_PERCENTASSIGN:            ctokId_PERCENTASSIGN
    of tokId_NOT:                      ctokId_NOT
    of tokId_NOT_ALT:                  ctokId_NOT_ALT
    of tokId_NOTEQUAL:                 ctokId_NOTEQUAL
    of tokId_NOTEQUAL_ALT:             ctokId_NOTEQUAL_ALT
    of tokId_OROR:                     ctokId_OROR
    of tokId_OROR_ALT:                 ctokId_OROR_ALT
    of tokId_OROR_TRIGRAPH:            ctokId_OROR_TRIGRAPH
    of tokId_PLUS:                     ctokId_PLUS
    of tokId_PLUSASSIGN:               ctokId_PLUSASSIGN
    of tokId_PLUSPLUS:                 ctokId_PLUSPLUS
    of tokId_ARROW:                    ctokId_ARROW
    of tokId_ARROWSTAR:                ctokId_ARROWSTAR
    of tokId_QUESTION_MARK:            ctokId_QUESTION_MARK
    of tokId_RIGHTBRACE:               ctokId_RIGHTBRACE
    of tokId_RIGHTBRACE_ALT:           ctokId_RIGHTBRACE_ALT
    of tokId_RIGHTBRACE_TRIGRAPH:      ctokId_RIGHTBRACE_TRIGRAPH
    of tokId_RIGHTPAREN:               ctokId_RIGHTPAREN
    of tokId_RIGHTBRACKET:             ctokId_RIGHTBRACKET
    of tokId_RIGHTBRACKET_ALT:         ctokId_RIGHTBRACKET_ALT
    of tokId_RIGHTBRACKET_TRIGRAPH:    ctokId_RIGHTBRACKET_TRIGRAPH
    of tokId_COLON_COLON:              ctokId_COLON_COLON
    of tokId_SEMICOLON:                ctokId_SEMICOLON
    of tokId_SHIFTLEFT:                ctokId_SHIFTLEFT
    of tokId_SHIFTLEFTASSIGN:          ctokId_SHIFTLEFTASSIGN
    of tokId_SHIFTRIGHT:               ctokId_SHIFTRIGHT
    of tokId_SHIFTRIGHTASSIGN:         ctokId_SHIFTRIGHTASSIGN
    of tokId_STAR:                     ctokId_STAR
    of tokId_COMPL:                    ctokId_COMPL
    of tokId_COMPL_ALT:                ctokId_COMPL_ALT
    of tokId_COMPL_TRIGRAPH:           ctokId_COMPL_TRIGRAPH
    of tokId_STARASSIGN:               ctokId_STARASSIGN
    of tokId_ASM:                      ctokId_ASM
    of tokId_AUTO:                     ctokId_AUTO
    of tokId_BOOL:                     ctokId_BOOL
    of tokId_FALSE:                    ctokId_FALSE
    of tokId_TRUE:                     ctokId_TRUE
    of tokId_BREAK:                    ctokId_BREAK
    of tokId_CASE:                     ctokId_CASE
    of tokId_CATCH:                    ctokId_CATCH
    of tokId_CHAR:                     ctokId_CHAR
    of tokId_CLASS:                    ctokId_CLASS
    of tokId_CONST:                    ctokId_CONST
    of tokId_CONSTCAST:                ctokId_CONSTCAST
    of tokId_CONTINUE:                 ctokId_CONTINUE
    of tokId_DEFAULT:                  ctokId_DEFAULT
    of tokId_DELETE:                   ctokId_DELETE
    of tokId_DO:                       ctokId_DO
    of tokId_DOUBLE:                   ctokId_DOUBLE
    of tokId_DYNAMICCAST:              ctokId_DYNAMICCAST
    of tokId_ELSE:                     ctokId_ELSE
    of tokId_ENUM:                     ctokId_ENUM
    of tokId_EXPLICIT:                 ctokId_EXPLICIT
    of tokId_EXPORT:                   ctokId_EXPORT
    of tokId_EXTERN:                   ctokId_EXTERN
    of tokId_FLOAT:                    ctokId_FLOAT
    of tokId_FOR:                      ctokId_FOR
    of tokId_FRIEND:                   ctokId_FRIEND
    of tokId_GOTO:                     ctokId_GOTO
    of tokId_IF:                       ctokId_IF
    of tokId_INLINE:                   ctokId_INLINE
    of tokId_INT:                      ctokId_INT
    of tokId_LONG:                     ctokId_LONG
    of tokId_MUTABLE:                  ctokId_MUTABLE
    of tokId_NAMESPACE:                ctokId_NAMESPACE
    of tokId_NEW:                      ctokId_NEW
    of tokId_OPERATOR:                 ctokId_OPERATOR
    of tokId_PRIVATE:                  ctokId_PRIVATE
    of tokId_PROTECTED:                ctokId_PROTECTED
    of tokId_PUBLIC:                   ctokId_PUBLIC
    of tokId_REGISTER:                 ctokId_REGISTER
    of tokId_REINTERPRETCAST:          ctokId_REINTERPRETCAST
    of tokId_RETURN:                   ctokId_RETURN
    of tokId_SHORT:                    ctokId_SHORT
    of tokId_SIGNED:                   ctokId_SIGNED
    of tokId_SIZEOF:                   ctokId_SIZEOF
    of tokId_STATIC:                   ctokId_STATIC
    of tokId_STATICCAST:               ctokId_STATICCAST
    of tokId_STRUCT:                   ctokId_STRUCT
    of tokId_SWITCH:                   ctokId_SWITCH
    of tokId_TEMPLATE:                 ctokId_TEMPLATE
    of tokId_THIS:                     ctokId_THIS
    of tokId_THROW:                    ctokId_THROW
    of tokId_TRY:                      ctokId_TRY
    of tokId_TYPEDEF:                  ctokId_TYPEDEF
    of tokId_TYPEID:                   ctokId_TYPEID
    of tokId_TYPENAME:                 ctokId_TYPENAME
    of tokId_UNION:                    ctokId_UNION
    of tokId_UNSIGNED:                 ctokId_UNSIGNED
    of tokId_USING:                    ctokId_USING
    of tokId_VIRTUAL:                  ctokId_VIRTUAL
    of tokId_VOID:                     ctokId_VOID
    of tokId_VOLATILE:                 ctokId_VOLATILE
    of tokId_WCHART:                   ctokId_WCHART
    of tokId_WHILE:                    ctokId_WHILE
    of tokId_PP_DEFINE:                ctokId_PP_DEFINE
    of tokId_PP_IF:                    ctokId_PP_IF
    of tokId_PP_IFDEF:                 ctokId_PP_IFDEF
    of tokId_PP_IFNDEF:                ctokId_PP_IFNDEF
    of tokId_PP_ELSE:                  ctokId_PP_ELSE
    of tokId_PP_ELIF:                  ctokId_PP_ELIF
    of tokId_PP_ENDIF:                 ctokId_PP_ENDIF
    of tokId_PP_ERROR:                 ctokId_PP_ERROR
    of tokId_PP_LINE:                  ctokId_PP_LINE
    of tokId_PP_PRAGMA:                ctokId_PP_PRAGMA
    of tokId_PP_UNDEF:                 ctokId_PP_UNDEF
    of tokId_PP_WARNING:               ctokId_PP_WARNING
    of tokId_IDENTIFIER:               ctokId_IDENTIFIER
    of tokId_OCTALINT:                 ctokId_OCTALINT
    of tokId_DECIMALINT:               ctokId_DECIMALINT
    of tokId_HEXAINT:                  ctokId_HEXAINT
    of tokId_INTLIT:                   ctokId_INTLIT
    of tokId_LONGINTLIT:               ctokId_LONGINTLIT
    of tokId_FLOATLIT:                 ctokId_FLOATLIT
    of tokId_FIXEDPOINTLIT:            ctokId_FIXEDPOINTLIT
    of tokId_CCOMMENT:                 ctokId_CCOMMENT
    of tokId_CPPCOMMENT:               ctokId_CPPCOMMENT
    of tokId_CHARLIT:                  ctokId_CHARLIT
    of tokId_STRINGLIT:                ctokId_STRINGLIT
    of tokId_CONTLINE:                 ctokId_CONTLINE
    of tokId_SPACE:                    ctokId_SPACE
    of tokId_SPACE2:                   ctokId_SPACE2
    of tokId_NEWLINE:                  ctokId_NEWLINE
    of tokId_GENERATEDNEWLINE:         ctokId_GENERATEDNEWLINE
    of tokId_POUND_POUND:              ctokId_POUND_POUND
    of tokId_POUND_POUND_ALT:          ctokId_POUND_POUND_ALT
    of tokId_POUND_POUND_TRIGRAPH:     ctokId_POUND_POUND_TRIGRAPH
    of tokId_POUND:                    ctokId_POUND
    of tokId_POUND_ALT:                ctokId_POUND_ALT
    of tokId_POUND_TRIGRAPH:           ctokId_POUND_TRIGRAPH
    of tokId_ANY:                      ctokId_ANY
    of tokId_ANY_TRIGRAPH:             ctokId_ANY_TRIGRAPH
    of tokId_PP_INCLUDE:               ctokId_PP_INCLUDE
    of tokId_PP_QHEADER:               ctokId_PP_QHEADER
    of tokId_PP_HHEADER:               ctokId_PP_HHEADER
    of tokId_PP_INCLUDE_NEXT:          ctokId_PP_INCLUDE_NEXT
    of tokId_PP_QHEADER_NEXT:          ctokId_PP_QHEADER_NEXT
    of tokId_PP_HHEADER_NEXT:          ctokId_PP_HHEADER_NEXT
    of tokId_EOF:                      ctokId_EOF
    of tokId_EOI:                      ctokId_EOI
    of tokId_PP_NUMBER:                ctokId_PP_NUMBER
    of tokId_MSEXT_INT8:               ctokId_MSEXT_INT8
    of tokId_MSEXT_INT16:              ctokId_MSEXT_INT16
    of tokId_MSEXT_INT32:              ctokId_MSEXT_INT32
    of tokId_MSEXT_INT64:              ctokId_MSEXT_INT64
    of tokId_MSEXT_BASED:              ctokId_MSEXT_BASED
    of tokId_MSEXT_DECLSPEC:           ctokId_MSEXT_DECLSPEC
    of tokId_MSEXT_CDECL:              ctokId_MSEXT_CDECL
    of tokId_MSEXT_FASTCALL:           ctokId_MSEXT_FASTCALL
    of tokId_MSEXT_STDCALL:            ctokId_MSEXT_STDCALL
    of tokId_MSEXT_TRY:                ctokId_MSEXT_TRY
    of tokId_MSEXT_EXCEPT:             ctokId_MSEXT_EXCEPT
    of tokId_MSEXT_FINALLY:            ctokId_MSEXT_FINALLY
    of tokId_MSEXT_LEAVE:              ctokId_MSEXT_LEAVE
    of tokId_MSEXT_INLINE:             ctokId_MSEXT_INLINE
    of tokId_MSEXT_ASM:                ctokId_MSEXT_ASM
    of tokId_MSEXT_PP_REGION:          ctokId_MSEXT_PP_REGION
    of tokId_MSEXT_PP_ENDREGION:       ctokId_MSEXT_PP_ENDREGION
    of tokId_IMPORT:                   ctokId_IMPORT
    of tokId_ALIGNAS:                  ctokId_ALIGNAS
    of tokId_ALIGNOF:                  ctokId_ALIGNOF
    of tokId_CHAR16_T:                 ctokId_CHAR16_T
    of tokId_CHAR32_T:                 ctokId_CHAR32_T
    of tokId_CONSTEXPR:                ctokId_CONSTEXPR
    of tokId_DECLTYPE:                 ctokId_DECLTYPE
    of tokId_NOEXCEPT:                 ctokId_NOEXCEPT
    of tokId_NULLPTR:                  ctokId_NULLPTR
    of tokId_STATICASSERT:             ctokId_STATICASSERT
    of tokId_THREADLOCAL:              ctokId_THREADLOCAL
    of tokId_RAWSTRINGLIT:             ctokId_RAWSTRINGLIT
    of tokId_CHAR8_T:                  ctokId_CHAR8_T
    of tokId_CONCEPT:                  ctokId_CONCEPT
    of tokId_CONSTEVAL:                ctokId_CONSTEVAL
    of tokId_CONSTINIT:                ctokId_CONSTINIT
    of tokId_CO_AWAIT:                 ctokId_CO_AWAIT
    of tokId_CO_RETURN:                ctokId_CO_RETURN
    of tokId_CO_YIELD:                 ctokId_CO_YIELD
    of tokId_REQUIRES:                 ctokId_REQUIRES
    of tokId_SPACESHIP:                ctokId_SPACESHIP
    of tokId_LAST_TOKEN_ID:            ctokId_LAST_TOKEN_ID
    of tokId_LAST_TOKEN:               ctokId_LAST_TOKEN
    of tokId_UNKNOWN_UNIVERSALCHAR:    ctokId_UNKNOWN_UNIVERSALCHAR
    of tokId_NONREPLACABLE_IDENTIFIER: ctokId_NONREPLACABLE_IDENTIFIER
    of tokId_PLACEHOLDER:              ctokId_PLACEHOLDER
    of tokId_PLACEMARKER:              ctokId_PLACEMARKER
    of tokId_PARAMETERBASE:            ctokId_PARAMETERBASE
    of tokId_EXTPARAMETERBASE:         ctokId_EXTPARAMETERBASE
    of tokId_OPTPARAMETERBASE:         ctokId_OPTPARAMETERBASE

converter toWaveTokId*(arg: CWaveTokId): WaveTokId =
  case arg:
    of ctokId_UNKNOWN:                  tokId_UNKNOWN
    of ctokId_FIRST_TOKEN:              tokId_FIRST_TOKEN
    of ctokId_AND:                      tokId_AND
    of ctokId_AND_ALT:                  tokId_AND_ALT
    of ctokId_ANDAND:                   tokId_ANDAND
    of ctokId_ANDAND_ALT:               tokId_ANDAND_ALT
    of ctokId_ASSIGN:                   tokId_ASSIGN
    of ctokId_ANDASSIGN:                tokId_ANDASSIGN
    of ctokId_ANDASSIGN_ALT:            tokId_ANDASSIGN_ALT
    of ctokId_OR:                       tokId_OR
    of ctokId_OR_ALT:                   tokId_OR_ALT
    of ctokId_OR_TRIGRAPH:              tokId_OR_TRIGRAPH
    of ctokId_ORASSIGN:                 tokId_ORASSIGN
    of ctokId_ORASSIGN_ALT:             tokId_ORASSIGN_ALT
    of ctokId_ORASSIGN_TRIGRAPH:        tokId_ORASSIGN_TRIGRAPH
    of ctokId_XOR:                      tokId_XOR
    of ctokId_XOR_ALT:                  tokId_XOR_ALT
    of ctokId_XOR_TRIGRAPH:             tokId_XOR_TRIGRAPH
    of ctokId_XORASSIGN:                tokId_XORASSIGN
    of ctokId_XORASSIGN_ALT:            tokId_XORASSIGN_ALT
    of ctokId_XORASSIGN_TRIGRAPH:       tokId_XORASSIGN_TRIGRAPH
    of ctokId_COMMA:                    tokId_COMMA
    of ctokId_COLON:                    tokId_COLON
    of ctokId_DIVIDE:                   tokId_DIVIDE
    of ctokId_DIVIDEASSIGN:             tokId_DIVIDEASSIGN
    of ctokId_DOT:                      tokId_DOT
    of ctokId_DOTSTAR:                  tokId_DOTSTAR
    of ctokId_ELLIPSIS:                 tokId_ELLIPSIS
    of ctokId_EQUAL:                    tokId_EQUAL
    of ctokId_GREATER:                  tokId_GREATER
    of ctokId_GREATEREQUAL:             tokId_GREATEREQUAL
    of ctokId_LEFTBRACE:                tokId_LEFTBRACE
    of ctokId_LEFTBRACE_ALT:            tokId_LEFTBRACE_ALT
    of ctokId_LEFTBRACE_TRIGRAPH:       tokId_LEFTBRACE_TRIGRAPH
    of ctokId_LESS:                     tokId_LESS
    of ctokId_LESSEQUAL:                tokId_LESSEQUAL
    of ctokId_LEFTPAREN:                tokId_LEFTPAREN
    of ctokId_LEFTBRACKET:              tokId_LEFTBRACKET
    of ctokId_LEFTBRACKET_ALT:          tokId_LEFTBRACKET_ALT
    of ctokId_LEFTBRACKET_TRIGRAPH:     tokId_LEFTBRACKET_TRIGRAPH
    of ctokId_MINUS:                    tokId_MINUS
    of ctokId_MINUSASSIGN:              tokId_MINUSASSIGN
    of ctokId_MINUSMINUS:               tokId_MINUSMINUS
    of ctokId_PERCENT:                  tokId_PERCENT
    of ctokId_PERCENTASSIGN:            tokId_PERCENTASSIGN
    of ctokId_NOT:                      tokId_NOT
    of ctokId_NOT_ALT:                  tokId_NOT_ALT
    of ctokId_NOTEQUAL:                 tokId_NOTEQUAL
    of ctokId_NOTEQUAL_ALT:             tokId_NOTEQUAL_ALT
    of ctokId_OROR:                     tokId_OROR
    of ctokId_OROR_ALT:                 tokId_OROR_ALT
    of ctokId_OROR_TRIGRAPH:            tokId_OROR_TRIGRAPH
    of ctokId_PLUS:                     tokId_PLUS
    of ctokId_PLUSASSIGN:               tokId_PLUSASSIGN
    of ctokId_PLUSPLUS:                 tokId_PLUSPLUS
    of ctokId_ARROW:                    tokId_ARROW
    of ctokId_ARROWSTAR:                tokId_ARROWSTAR
    of ctokId_QUESTION_MARK:            tokId_QUESTION_MARK
    of ctokId_RIGHTBRACE:               tokId_RIGHTBRACE
    of ctokId_RIGHTBRACE_ALT:           tokId_RIGHTBRACE_ALT
    of ctokId_RIGHTBRACE_TRIGRAPH:      tokId_RIGHTBRACE_TRIGRAPH
    of ctokId_RIGHTPAREN:               tokId_RIGHTPAREN
    of ctokId_RIGHTBRACKET:             tokId_RIGHTBRACKET
    of ctokId_RIGHTBRACKET_ALT:         tokId_RIGHTBRACKET_ALT
    of ctokId_RIGHTBRACKET_TRIGRAPH:    tokId_RIGHTBRACKET_TRIGRAPH
    of ctokId_COLON_COLON:              tokId_COLON_COLON
    of ctokId_SEMICOLON:                tokId_SEMICOLON
    of ctokId_SHIFTLEFT:                tokId_SHIFTLEFT
    of ctokId_SHIFTLEFTASSIGN:          tokId_SHIFTLEFTASSIGN
    of ctokId_SHIFTRIGHT:               tokId_SHIFTRIGHT
    of ctokId_SHIFTRIGHTASSIGN:         tokId_SHIFTRIGHTASSIGN
    of ctokId_STAR:                     tokId_STAR
    of ctokId_COMPL:                    tokId_COMPL
    of ctokId_COMPL_ALT:                tokId_COMPL_ALT
    of ctokId_COMPL_TRIGRAPH:           tokId_COMPL_TRIGRAPH
    of ctokId_STARASSIGN:               tokId_STARASSIGN
    of ctokId_ASM:                      tokId_ASM
    of ctokId_AUTO:                     tokId_AUTO
    of ctokId_BOOL:                     tokId_BOOL
    of ctokId_FALSE:                    tokId_FALSE
    of ctokId_TRUE:                     tokId_TRUE
    of ctokId_BREAK:                    tokId_BREAK
    of ctokId_CASE:                     tokId_CASE
    of ctokId_CATCH:                    tokId_CATCH
    of ctokId_CHAR:                     tokId_CHAR
    of ctokId_CLASS:                    tokId_CLASS
    of ctokId_CONST:                    tokId_CONST
    of ctokId_CONSTCAST:                tokId_CONSTCAST
    of ctokId_CONTINUE:                 tokId_CONTINUE
    of ctokId_DEFAULT:                  tokId_DEFAULT
    of ctokId_DELETE:                   tokId_DELETE
    of ctokId_DO:                       tokId_DO
    of ctokId_DOUBLE:                   tokId_DOUBLE
    of ctokId_DYNAMICCAST:              tokId_DYNAMICCAST
    of ctokId_ELSE:                     tokId_ELSE
    of ctokId_ENUM:                     tokId_ENUM
    of ctokId_EXPLICIT:                 tokId_EXPLICIT
    of ctokId_EXPORT:                   tokId_EXPORT
    of ctokId_EXTERN:                   tokId_EXTERN
    of ctokId_FLOAT:                    tokId_FLOAT
    of ctokId_FOR:                      tokId_FOR
    of ctokId_FRIEND:                   tokId_FRIEND
    of ctokId_GOTO:                     tokId_GOTO
    of ctokId_IF:                       tokId_IF
    of ctokId_INLINE:                   tokId_INLINE
    of ctokId_INT:                      tokId_INT
    of ctokId_LONG:                     tokId_LONG
    of ctokId_MUTABLE:                  tokId_MUTABLE
    of ctokId_NAMESPACE:                tokId_NAMESPACE
    of ctokId_NEW:                      tokId_NEW
    of ctokId_OPERATOR:                 tokId_OPERATOR
    of ctokId_PRIVATE:                  tokId_PRIVATE
    of ctokId_PROTECTED:                tokId_PROTECTED
    of ctokId_PUBLIC:                   tokId_PUBLIC
    of ctokId_REGISTER:                 tokId_REGISTER
    of ctokId_REINTERPRETCAST:          tokId_REINTERPRETCAST
    of ctokId_RETURN:                   tokId_RETURN
    of ctokId_SHORT:                    tokId_SHORT
    of ctokId_SIGNED:                   tokId_SIGNED
    of ctokId_SIZEOF:                   tokId_SIZEOF
    of ctokId_STATIC:                   tokId_STATIC
    of ctokId_STATICCAST:               tokId_STATICCAST
    of ctokId_STRUCT:                   tokId_STRUCT
    of ctokId_SWITCH:                   tokId_SWITCH
    of ctokId_TEMPLATE:                 tokId_TEMPLATE
    of ctokId_THIS:                     tokId_THIS
    of ctokId_THROW:                    tokId_THROW
    of ctokId_TRY:                      tokId_TRY
    of ctokId_TYPEDEF:                  tokId_TYPEDEF
    of ctokId_TYPEID:                   tokId_TYPEID
    of ctokId_TYPENAME:                 tokId_TYPENAME
    of ctokId_UNION:                    tokId_UNION
    of ctokId_UNSIGNED:                 tokId_UNSIGNED
    of ctokId_USING:                    tokId_USING
    of ctokId_VIRTUAL:                  tokId_VIRTUAL
    of ctokId_VOID:                     tokId_VOID
    of ctokId_VOLATILE:                 tokId_VOLATILE
    of ctokId_WCHART:                   tokId_WCHART
    of ctokId_WHILE:                    tokId_WHILE
    of ctokId_PP_DEFINE:                tokId_PP_DEFINE
    of ctokId_PP_IF:                    tokId_PP_IF
    of ctokId_PP_IFDEF:                 tokId_PP_IFDEF
    of ctokId_PP_IFNDEF:                tokId_PP_IFNDEF
    of ctokId_PP_ELSE:                  tokId_PP_ELSE
    of ctokId_PP_ELIF:                  tokId_PP_ELIF
    of ctokId_PP_ENDIF:                 tokId_PP_ENDIF
    of ctokId_PP_ERROR:                 tokId_PP_ERROR
    of ctokId_PP_LINE:                  tokId_PP_LINE
    of ctokId_PP_PRAGMA:                tokId_PP_PRAGMA
    of ctokId_PP_UNDEF:                 tokId_PP_UNDEF
    of ctokId_PP_WARNING:               tokId_PP_WARNING
    of ctokId_IDENTIFIER:               tokId_IDENTIFIER
    of ctokId_OCTALINT:                 tokId_OCTALINT
    of ctokId_DECIMALINT:               tokId_DECIMALINT
    of ctokId_HEXAINT:                  tokId_HEXAINT
    of ctokId_INTLIT:                   tokId_INTLIT
    of ctokId_LONGINTLIT:               tokId_LONGINTLIT
    of ctokId_FLOATLIT:                 tokId_FLOATLIT
    of ctokId_FIXEDPOINTLIT:            tokId_FIXEDPOINTLIT
    of ctokId_CCOMMENT:                 tokId_CCOMMENT
    of ctokId_CPPCOMMENT:               tokId_CPPCOMMENT
    of ctokId_CHARLIT:                  tokId_CHARLIT
    of ctokId_STRINGLIT:                tokId_STRINGLIT
    of ctokId_CONTLINE:                 tokId_CONTLINE
    of ctokId_SPACE:                    tokId_SPACE
    of ctokId_SPACE2:                   tokId_SPACE2
    of ctokId_NEWLINE:                  tokId_NEWLINE
    of ctokId_GENERATEDNEWLINE:         tokId_GENERATEDNEWLINE
    of ctokId_POUND_POUND:              tokId_POUND_POUND
    of ctokId_POUND_POUND_ALT:          tokId_POUND_POUND_ALT
    of ctokId_POUND_POUND_TRIGRAPH:     tokId_POUND_POUND_TRIGRAPH
    of ctokId_POUND:                    tokId_POUND
    of ctokId_POUND_ALT:                tokId_POUND_ALT
    of ctokId_POUND_TRIGRAPH:           tokId_POUND_TRIGRAPH
    of ctokId_ANY:                      tokId_ANY
    of ctokId_ANY_TRIGRAPH:             tokId_ANY_TRIGRAPH
    of ctokId_PP_INCLUDE:               tokId_PP_INCLUDE
    of ctokId_PP_QHEADER:               tokId_PP_QHEADER
    of ctokId_PP_HHEADER:               tokId_PP_HHEADER
    of ctokId_PP_INCLUDE_NEXT:          tokId_PP_INCLUDE_NEXT
    of ctokId_PP_QHEADER_NEXT:          tokId_PP_QHEADER_NEXT
    of ctokId_PP_HHEADER_NEXT:          tokId_PP_HHEADER_NEXT
    of ctokId_EOF:                      tokId_EOF
    of ctokId_EOI:                      tokId_EOI
    of ctokId_PP_NUMBER:                tokId_PP_NUMBER
    of ctokId_MSEXT_INT8:               tokId_MSEXT_INT8
    of ctokId_MSEXT_INT16:              tokId_MSEXT_INT16
    of ctokId_MSEXT_INT32:              tokId_MSEXT_INT32
    of ctokId_MSEXT_INT64:              tokId_MSEXT_INT64
    of ctokId_MSEXT_BASED:              tokId_MSEXT_BASED
    of ctokId_MSEXT_DECLSPEC:           tokId_MSEXT_DECLSPEC
    of ctokId_MSEXT_CDECL:              tokId_MSEXT_CDECL
    of ctokId_MSEXT_FASTCALL:           tokId_MSEXT_FASTCALL
    of ctokId_MSEXT_STDCALL:            tokId_MSEXT_STDCALL
    of ctokId_MSEXT_TRY:                tokId_MSEXT_TRY
    of ctokId_MSEXT_EXCEPT:             tokId_MSEXT_EXCEPT
    of ctokId_MSEXT_FINALLY:            tokId_MSEXT_FINALLY
    of ctokId_MSEXT_LEAVE:              tokId_MSEXT_LEAVE
    of ctokId_MSEXT_INLINE:             tokId_MSEXT_INLINE
    of ctokId_MSEXT_ASM:                tokId_MSEXT_ASM
    of ctokId_MSEXT_PP_REGION:          tokId_MSEXT_PP_REGION
    of ctokId_MSEXT_PP_ENDREGION:       tokId_MSEXT_PP_ENDREGION
    of ctokId_IMPORT:                   tokId_IMPORT
    of ctokId_ALIGNAS:                  tokId_ALIGNAS
    of ctokId_ALIGNOF:                  tokId_ALIGNOF
    of ctokId_CHAR16_T:                 tokId_CHAR16_T
    of ctokId_CHAR32_T:                 tokId_CHAR32_T
    of ctokId_CONSTEXPR:                tokId_CONSTEXPR
    of ctokId_DECLTYPE:                 tokId_DECLTYPE
    of ctokId_NOEXCEPT:                 tokId_NOEXCEPT
    of ctokId_NULLPTR:                  tokId_NULLPTR
    of ctokId_STATICASSERT:             tokId_STATICASSERT
    of ctokId_THREADLOCAL:              tokId_THREADLOCAL
    of ctokId_RAWSTRINGLIT:             tokId_RAWSTRINGLIT
    of ctokId_CHAR8_T:                  tokId_CHAR8_T
    of ctokId_CONCEPT:                  tokId_CONCEPT
    of ctokId_CONSTEVAL:                tokId_CONSTEVAL
    of ctokId_CONSTINIT:                tokId_CONSTINIT
    of ctokId_CO_AWAIT:                 tokId_CO_AWAIT
    of ctokId_CO_RETURN:                tokId_CO_RETURN
    of ctokId_CO_YIELD:                 tokId_CO_YIELD
    of ctokId_REQUIRES:                 tokId_REQUIRES
    of ctokId_SPACESHIP:                tokId_SPACESHIP
    of ctokId_LAST_TOKEN_ID:            tokId_LAST_TOKEN_ID
    of ctokId_LAST_TOKEN:               tokId_LAST_TOKEN
    of ctokId_UNKNOWN_UNIVERSALCHAR:    tokId_UNKNOWN_UNIVERSALCHAR
    of ctokId_NONREPLACABLE_IDENTIFIER: tokId_NONREPLACABLE_IDENTIFIER
    of ctokId_PLACEHOLDER:              tokId_PLACEHOLDER
    of ctokId_PLACEMARKER:              tokId_PLACEMARKER
    of ctokId_PARAMETERBASE:            tokId_PARAMETERBASE
    of ctokId_EXTPARAMETERBASE:         tokId_EXTPARAMETERBASE
    of ctokId_OPTPARAMETERBASE:         tokId_OPTPARAMETERBASE

converter toCint*(arg: CWaveTokId): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(arg))

converter toCint*(arg: WaveTokId): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(toCWaveTokId(arg)))

func `+`*(arg: CWaveTokId, offset: int): CWaveTokId =
  cast[CWaveTokId](ord(arg) + offset)

func `+`*(offset: int, arg: CWaveTokId): CWaveTokId =
  cast[CWaveTokId](ord(arg) + offset)

func `-`*(arg: CWaveTokId, offset: int): CWaveTokId =
  cast[CWaveTokId](ord(arg) - offset)

func `-`*(offset: int, arg: CWaveTokId): CWaveTokId =
  cast[CWaveTokId](ord(arg) - offset)

converter toCWaveErrorCode*(arg: WaveErrorCode): CWaveErrorCode =
  case arg:
    of wekNoError:                        cwekNoError
    of wekUnexpectedError:                cwekUnexpectedError
    of wekMacroRedefinition:              cwekMacroRedefinition
    of wekMacroInsertionError:            cwekMacroInsertionError
    of wekBadIncludeFile:                 cwekBadIncludeFile
    of wekBadIncludeStatement:            cwekBadIncludeStatement
    of wekBadHasIncludeExpression:        cwekBadHasIncludeExpression
    of wekIllFormedDirective:             cwekIllFormedDirective
    of wekErrorDirective:                 cwekErrorDirective
    of wekWarningDirective:               cwekWarningDirective
    of wekIllFormedExpression:            cwekIllFormedExpression
    of wekMissingMatchingIf:              cwekMissingMatchingIf
    of wekMissingMatchingEndif:           cwekMissingMatchingEndif
    of wekIllFormedOperator:              cwekIllFormedOperator
    of wekBadDefineStatement:             cwekBadDefineStatement
    of wekBadDefineStatementVaArgs:       cwekBadDefineStatementVaArgs
    of wekBadDefineStatementVaOpt:        cwekBadDefineStatementVaOpt
    of wekBadDefineStatementVaOptParens:  cwekBadDefineStatementVaOptParens
    of wekBadDefineStatementVaOptRecurse: cwekBadDefineStatementVaOptRecurse
    of wekTooFewMacroarguments:           cwekTooFewMacroarguments
    of wekTooManyMacroarguments:          cwekTooManyMacroarguments
    of wekEmptyMacroarguments:            cwekEmptyMacroarguments
    of wekImproperlyTerminatedMacro:      cwekImproperlyTerminatedMacro
    of wekBadLineStatement:               cwekBadLineStatement
    of wekBadLineNumber:                  cwekBadLineNumber
    of wekBadLineFilename:                cwekBadLineFilename
    of wekBadUndefineStatement:           cwekBadUndefineStatement
    of wekBadMacroDefinition:             cwekBadMacroDefinition
    of wekIllegalRedefinition:            cwekIllegalRedefinition
    of wekDuplicateParameterName:         cwekDuplicateParameterName
    of wekInvalidConcat:                  cwekInvalidConcat
    of wekLastLineNotTerminated:          cwekLastLineNotTerminated
    of wekIllFormedPragmaOption:          cwekIllFormedPragmaOption
    of wekIncludeNestingTooDeep:          cwekIncludeNestingTooDeep
    of wekMisplacedOperator:              cwekMisplacedOperator
    of wekAlreadydefinedName:             cwekAlreadydefinedName
    of wekUndefinedMacroname:             cwekUndefinedMacroname
    of wekInvalidMacroname:               cwekInvalidMacroname
    of wekUnexpectedQualifiedName:        cwekUnexpectedQualifiedName
    of wekDivisionByZero:                 cwekDivisionByZero
    of wekIntegerOverflow:                cwekIntegerOverflow
    of wekIllegalOperatorRedefinition:    cwekIllegalOperatorRedefinition
    of wekIllFormedIntegerLiteral:        cwekIllFormedIntegerLiteral
    of wekIllFormedCharacterLiteral:      cwekIllFormedCharacterLiteral
    of wekUnbalancedIfEndif:              cwekUnbalancedIfEndif
    of wekCharacterLiteralOutOfRange:     cwekCharacterLiteralOutOfRange
    of wekCouldNotOpenOutputFile:         cwekCouldNotOpenOutputFile
    of wekIncompatibleConfig:             cwekIncompatibleConfig
    of wekIllFormedPragmaMessage:         cwekIllFormedPragmaMessage
    of wekPragmaMessageDirective:         cwekPragmaMessageDirective
    of wekLexerErrorBegin:                cwekLexerErrorBegin
    of wekLexerUnexpectedError:           cwekLexerUnexpectedError
    of wekLexerUniversalCharInvalid:      cwekLexerUniversalCharInvalid
    of wekLexerUniversalCharBaseCharset:  cwekLexerUniversalCharBaseCharset
    of wekLexerUniversalCharNotAllowed:   cwekLexerUniversalCharNotAllowed
    of wekLexerInvalidLongLongLiteral:    cwekLexerInvalidLongLongLiteral
    of wekLexerGenericLexingError:        cwekLexerGenericLexingError
    of wekLexerGenericLexingWarning:      cwekLexerGenericLexingWarning

converter toWaveErrorCode*(arg: CWaveErrorCode): WaveErrorCode =
  case arg:
    of cwekNoError:                        wekNoError
    of cwekUnexpectedError:                wekUnexpectedError
    of cwekMacroRedefinition:              wekMacroRedefinition
    of cwekMacroInsertionError:            wekMacroInsertionError
    of cwekBadIncludeFile:                 wekBadIncludeFile
    of cwekBadIncludeStatement:            wekBadIncludeStatement
    of cwekBadHasIncludeExpression:        wekBadHasIncludeExpression
    of cwekIllFormedDirective:             wekIllFormedDirective
    of cwekErrorDirective:                 wekErrorDirective
    of cwekWarningDirective:               wekWarningDirective
    of cwekIllFormedExpression:            wekIllFormedExpression
    of cwekMissingMatchingIf:              wekMissingMatchingIf
    of cwekMissingMatchingEndif:           wekMissingMatchingEndif
    of cwekIllFormedOperator:              wekIllFormedOperator
    of cwekBadDefineStatement:             wekBadDefineStatement
    of cwekBadDefineStatementVaArgs:       wekBadDefineStatementVaArgs
    of cwekBadDefineStatementVaOpt:        wekBadDefineStatementVaOpt
    of cwekBadDefineStatementVaOptParens:  wekBadDefineStatementVaOptParens
    of cwekBadDefineStatementVaOptRecurse: wekBadDefineStatementVaOptRecurse
    of cwekTooFewMacroarguments:           wekTooFewMacroarguments
    of cwekTooManyMacroarguments:          wekTooManyMacroarguments
    of cwekEmptyMacroarguments:            wekEmptyMacroarguments
    of cwekImproperlyTerminatedMacro:      wekImproperlyTerminatedMacro
    of cwekBadLineStatement:               wekBadLineStatement
    of cwekBadLineNumber:                  wekBadLineNumber
    of cwekBadLineFilename:                wekBadLineFilename
    of cwekBadUndefineStatement:           wekBadUndefineStatement
    of cwekBadMacroDefinition:             wekBadMacroDefinition
    of cwekIllegalRedefinition:            wekIllegalRedefinition
    of cwekDuplicateParameterName:         wekDuplicateParameterName
    of cwekInvalidConcat:                  wekInvalidConcat
    of cwekLastLineNotTerminated:          wekLastLineNotTerminated
    of cwekIllFormedPragmaOption:          wekIllFormedPragmaOption
    of cwekIncludeNestingTooDeep:          wekIncludeNestingTooDeep
    of cwekMisplacedOperator:              wekMisplacedOperator
    of cwekAlreadydefinedName:             wekAlreadydefinedName
    of cwekUndefinedMacroname:             wekUndefinedMacroname
    of cwekInvalidMacroname:               wekInvalidMacroname
    of cwekUnexpectedQualifiedName:        wekUnexpectedQualifiedName
    of cwekDivisionByZero:                 wekDivisionByZero
    of cwekIntegerOverflow:                wekIntegerOverflow
    of cwekIllegalOperatorRedefinition:    wekIllegalOperatorRedefinition
    of cwekIllFormedIntegerLiteral:        wekIllFormedIntegerLiteral
    of cwekIllFormedCharacterLiteral:      wekIllFormedCharacterLiteral
    of cwekUnbalancedIfEndif:              wekUnbalancedIfEndif
    of cwekCharacterLiteralOutOfRange:     wekCharacterLiteralOutOfRange
    of cwekCouldNotOpenOutputFile:         wekCouldNotOpenOutputFile
    of cwekIncompatibleConfig:             wekIncompatibleConfig
    of cwekIllFormedPragmaMessage:         wekIllFormedPragmaMessage
    of cwekPragmaMessageDirective:         wekPragmaMessageDirective
    of cwekLexerErrorBegin:                wekLexerErrorBegin
    of cwekLexerUnexpectedError:           wekLexerUnexpectedError
    of cwekLexerUniversalCharInvalid:      wekLexerUniversalCharInvalid
    of cwekLexerUniversalCharBaseCharset:  wekLexerUniversalCharBaseCharset
    of cwekLexerUniversalCharNotAllowed:   wekLexerUniversalCharNotAllowed
    of cwekLexerInvalidLongLongLiteral:    wekLexerInvalidLongLongLiteral
    of cwekLexerGenericLexingError:        wekLexerGenericLexingError
    of cwekLexerGenericLexingWarning:      wekLexerGenericLexingWarning

converter toCint*(arg: CWaveErrorCode): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(arg))

converter toCint*(arg: WaveErrorCode): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(toCWaveErrorCode(arg)))

func `+`*(arg: CWaveErrorCode, offset: int): CWaveErrorCode =
  cast[CWaveErrorCode](ord(arg) + offset)

func `+`*(offset: int, arg: CWaveErrorCode): CWaveErrorCode =
  cast[CWaveErrorCode](ord(arg) + offset)

func `-`*(arg: CWaveErrorCode, offset: int): CWaveErrorCode =
  cast[CWaveErrorCode](ord(arg) - offset)

func `-`*(offset: int, arg: CWaveErrorCode): CWaveErrorCode =
  cast[CWaveErrorCode](ord(arg) - offset)

converter toCWaveSeverityLevel*(arg: WaveSeverityLevel): CWaveSeverityLevel =
  case arg:
    of wslRemark:           cwslRemark
    of wslWarning:          cwslWarning
    of wslError:            cwslError
    of wslFatal:            cwslFatal
    of wslCommandlineError: cwslCommandlineError

converter toWaveSeverityLevel*(arg: CWaveSeverityLevel): WaveSeverityLevel =
  case arg:
    of cwslRemark:           wslRemark
    of cwslWarning:          wslWarning
    of cwslError:            wslError
    of cwslFatal:            wslFatal
    of cwslCommandlineError: wslCommandlineError

converter toCint*(arg: CWaveSeverityLevel): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(arg))

converter toCint*(arg: WaveSeverityLevel): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(toCWaveSeverityLevel(arg)))

func `+`*(arg: CWaveSeverityLevel, offset: int): CWaveSeverityLevel =
  cast[CWaveSeverityLevel](ord(arg) + offset)

func `+`*(offset: int, arg: CWaveSeverityLevel): CWaveSeverityLevel =
  cast[CWaveSeverityLevel](ord(arg) + offset)

func `-`*(arg: CWaveSeverityLevel, offset: int): CWaveSeverityLevel =
  cast[CWaveSeverityLevel](ord(arg) - offset)

func `-`*(offset: int, arg: CWaveSeverityLevel): CWaveSeverityLevel =
  cast[CWaveSeverityLevel](ord(arg) - offset)

converter toCWaveLanguageModeImpl*(
    arg: WaveLanguageModeImpl
  ): CWaveLanguageModeImpl =
  case arg:
    of iwlmSupportNormal:         ciwlmSupportNormal
    of iwlmLongLong:              ciwlmLongLong
    of iwlmVariadics:             ciwlmVariadics
    of iwlmNoNewlineAtEndOfFIle:  ciwlmNoNewlineAtEndOfFIle
    of iwlmHasInclude:            ciwlmHasInclude
    of iwlmVaOpt:                 ciwlmVaOpt
    of iwlmEmitContline:          ciwlmEmitContline
    of iwlmInsertWhitespace:      ciwlmInsertWhitespace
    of iwlmPreserveComments:      ciwlmPreserveComments
    of iwlmNoCharacterValidation: ciwlmNoCharacterValidation
    of iwlmConvertTrigraphs:      ciwlmConvertTrigraphs
    of iwlmSingleLine:            ciwlmSingleLine
    of iwlmPreferPpNumbers:       ciwlmPreferPpNumbers
    of iwlmEmitLineDirectives:    ciwlmEmitLineDirectives
    of iwlmIncludeGuardDetection: ciwlmIncludeGuardDetection
    of iwlmEmitPragmaDirectives:  ciwlmEmitPragmaDirectives
    of iwlmC99:                   ciwlmC99
    of iwlmCpp11:                 ciwlmCpp11
    of iwlmCpp17:                 ciwlmCpp17
    of iwlmCpp20:                 ciwlmCpp20

converter toWaveLanguageModeImpl*(
    arg: CWaveLanguageModeImpl
  ): WaveLanguageModeImpl =
  case arg:
    of ciwlmSupportNormal:         iwlmSupportNormal
    of ciwlmLongLong:              iwlmLongLong
    of ciwlmVariadics:             iwlmVariadics
    of ciwlmNoNewlineAtEndOfFIle:  iwlmNoNewlineAtEndOfFIle
    of ciwlmHasInclude:            iwlmHasInclude
    of ciwlmVaOpt:                 iwlmVaOpt
    of ciwlmEmitContline:          iwlmEmitContline
    of ciwlmInsertWhitespace:      iwlmInsertWhitespace
    of ciwlmPreserveComments:      iwlmPreserveComments
    of ciwlmNoCharacterValidation: iwlmNoCharacterValidation
    of ciwlmConvertTrigraphs:      iwlmConvertTrigraphs
    of ciwlmSingleLine:            iwlmSingleLine
    of ciwlmPreferPpNumbers:       iwlmPreferPpNumbers
    of ciwlmEmitLineDirectives:    iwlmEmitLineDirectives
    of ciwlmIncludeGuardDetection: iwlmIncludeGuardDetection
    of ciwlmEmitPragmaDirectives:  iwlmEmitPragmaDirectives
    of ciwlmC99:                   iwlmC99
    of ciwlmCpp11:                 iwlmCpp11
    of ciwlmCpp17:                 iwlmCpp17
    of ciwlmCpp20:                 iwlmCpp20

converter toCint*(arg: CWaveLanguageModeImpl): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(arg))

converter toCint*(arg: WaveLanguageModeImpl): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(toCWaveLanguageModeImpl(arg)))

func `+`*(arg: CWaveLanguageModeImpl, offset: int): CWaveLanguageModeImpl =
  cast[CWaveLanguageModeImpl](ord(arg) + offset)

func `+`*(offset: int, arg: CWaveLanguageModeImpl): CWaveLanguageModeImpl =
  cast[CWaveLanguageModeImpl](ord(arg) + offset)

func `-`*(arg: CWaveLanguageModeImpl, offset: int): CWaveLanguageModeImpl =
  cast[CWaveLanguageModeImpl](ord(arg) - offset)

func `-`*(offset: int, arg: CWaveLanguageModeImpl): CWaveLanguageModeImpl =
  cast[CWaveLanguageModeImpl](ord(arg) - offset)

converter toCEntryHandling*(arg: EntryHandling): CEntryHandling =
  case arg:
    of EntryHandlingSkip:    cEntryHandlingSkip
    of EntryHandlingProcess: cEntryHandlingProcess
    of EntryHandlingRaise:   cEntryHandlingRaise

converter toEntryHandling*(arg: CEntryHandling): EntryHandling =
  case arg:
    of cEntryHandlingSkip:    EntryHandlingSkip
    of cEntryHandlingProcess: EntryHandlingProcess
    of cEntryHandlingRaise:   EntryHandlingRaise

converter toCint*(arg: CEntryHandling): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(arg))

converter toCint*(arg: EntryHandling): cint =
  ## Convert nim enum value into cint that can be passed to wrapped C
  ## procs.
  cint(ord(toCEntryHandling(arg)))

func `+`*(arg: CEntryHandling, offset: int): CEntryHandling =
  cast[CEntryHandling](ord(arg) + offset)

func `+`*(offset: int, arg: CEntryHandling): CEntryHandling =
  cast[CEntryHandling](ord(arg) + offset)

func `-`*(arg: CEntryHandling, offset: int): CEntryHandling =
  cast[CEntryHandling](ord(arg) - offset)

func `-`*(offset: int, arg: CEntryHandling): CEntryHandling =
  cast[CEntryHandling](ord(arg) - offset)

converter toCint*(args: set[EntryHandling]): cint =
  ## Convert set of nim enum values into cint that can be passed
  ## to wrapped C procs.
  for value in items(args):
    case value:
      of EntryHandlingSkip:    result = cint(result or (0 shl 0))
      of EntryHandlingProcess: result = cint(result or (1 shl 0))
      of EntryHandlingRaise:   result = cint(result or (1 shl 1))

proc tokenVectorLen*(
    vec: ptr WaveTokenVectorHandle
  ): cint {.dynlib: cwaveDl, importc: "wave_tokenVectorLen".}


proc tokenVectorGetAt*(
    vec: ptr WaveTokenVectorHandle,
    idx: cint
  ): ptr WaveTokenHandle {.dynlib: cwaveDl, importc: "wave_tokenVectorGetAt".}


proc deleteWaveTokenVector*(
    vec: ptr WaveTokenVectorHandle
  ): void {.dynlib: cwaveDl, importc: "wave_deleteWaveTokenVector".}


proc tokenListLen*(
    list: ptr WaveTokenListHandle
  ): cint {.dynlib: cwaveDl, importc: "wave_tokenListLen".}


proc tokenListToStr*(
    list: ptr WaveTokenListHandle
  ): cstring {.dynlib: cwaveDl, importc: "wave_tokenListToStr".}


proc tokenListBeginIterator*(
    l: ptr WaveTokenListHandle
  ): ptr WaveTokenListIteratorHandle {.dynlib: cwaveDl, importc: "wave_tokenListBeginIterator".}


proc tokenListEndIterator*(
    l: ptr WaveTokenListHandle
  ): ptr WaveTokenListIteratorHandle {.dynlib: cwaveDl, importc: "wave_tokenListEndIterator".}


proc neqListIterator*(
    i1: ptr WaveTokenListIteratorHandle,
    i2: ptr WaveTokenListIteratorHandle
  ): bool {.dynlib: cwaveDl, importc: "wave_neqListIterator".}


proc listIterDeref*(
    i: ptr WaveTokenListIteratorHandle
  ): ptr WaveTokenHandle {.dynlib: cwaveDl, importc: "wave_listIterDeref".}


proc listIterAdvance*(
    i: ptr WaveTokenListIteratorHandle
  ): void {.dynlib: cwaveDl, importc: "wave_listIterAdvance".}


proc newProcessingHooks*(): ptr WaveProcessingHooksHandle {.dynlib: cwaveDl, importc: "wave_newProcessingHooks".}


proc destroyProcessingHooks*(
    hooks: ptr WaveProcessingHooksHandle
  ): void {.dynlib: cwaveDl, importc: "wave_destroyProcessingHooks".}


proc newWaveContext*(
    instring: cstring,
    filename: cstring
  ): ptr WaveContextHandle {.dynlib: cwaveDl, importc: "wave_newWaveContext".}


proc setLanguageMode*(
    ctx:  ptr WaveContextHandle,
    mode: CWaveLanguageModeImpl
  ): void {.dynlib: cwaveDl, importc: "wave_setLanguageMode".}


proc processAll*(
    context: ptr WaveContextHandle
  ): void {.dynlib: cwaveDl, importc: "wave_processAll".}


proc setFoundWarningDirective*(
    context: ptr WaveContextHandle,
    impl:    FoundWarningDirectiveImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setFoundWarningDirective".}


proc setEvaluatedConditionalExpression*(
    context: ptr WaveContextHandle,
    impl:    EvaluatedConditionalExpressionImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setEvaluatedConditionalExpression".}


proc setFoundUnknownDirectiveImplType*(
    context: ptr WaveContextHandle,
    impl:    FoundUnknownDirectiveImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setFoundUnknownDirectiveImplType".}


proc setExpandingFunctionLikeMacro*(
    context: ptr WaveContextHandle,
    impl:    ExpandingFunctionLikeMacroImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setExpandingFunctionLikeMacro".}


proc setExpandingObjectLikeMacro*(
    context: ptr WaveContextHandle,
    impl:    ExpandingObjectLikeMacroImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setExpandingObjectLikeMacro".}


proc setExpandedMacro*(
    context: ptr WaveContextHandle,
    impl:    ExpandedMacroImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setExpandedMacro".}


proc setRescannedMacro*(
    context: ptr WaveContextHandle,
    impl:    RescannedMacroImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setRescannedMacro".}


proc setFoundDirective*(
    ctx:          ptr WaveContextHandle,
    filename:     FoundDirectiveImplType,
    include_next: pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setFoundDirective".}


proc setFoundIncludeDirective*(
    context: ptr WaveContextHandle,
    impl:    FoundIncludeDirectiveImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setFoundIncludeDirective".}


proc setSkippedToken*(
    context: ptr WaveContextHandle,
    impl:    SkippedTokenImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setSkippedToken".}


proc setEmitLineDirective*(
    context: ptr WaveContextHandle,
    impl:    EmitLineDirectiveImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setEmitLineDirective".}


proc setFoundLineDirective*(
    context: ptr WaveContextHandle,
    impl:    FoundLineDirectiveImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setFoundLineDirective".}


proc setFoundErrorDirective*(
    context: ptr WaveContextHandle,
    impl:    FoundErrorDirectiveImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setFoundErrorDirective".}


proc setDefinedMacro*(
    context: ptr WaveContextHandle,
    impl:    DefinedMacroImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setDefinedMacro".}


proc setLocateIncludeFile*(
    context: ptr WaveContextHandle,
    impl:    LocateIncludeFileImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setLocateIncludeFile".}


proc setOpenedIncludeFile*(
    context: ptr WaveContextHandle,
    impl:    OpenedIncludeFileImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setOpenedIncludeFile".}


proc setReturningFromIncludeFile*(
    context: ptr WaveContextHandle,
    impl:    ReturningFromIncludeFileImplType,
    env:     pointer
  ): void {.dynlib: cwaveDl, importc: "wave_setReturningFromIncludeFile".}


proc destroyContext*(
    context: ptr WaveContextHandle
  ): void {.dynlib: cwaveDl, importc: "wave_destroyContext".}


proc contextSetData*(
    context: ptr WaveContextHandle,
    data:    pointer
  ): void {.dynlib: cwaveDl, importc: "wave_contextSetData".}


proc contextGetData*(
    context: ptr WaveContextHandle
  ): pointer {.dynlib: cwaveDl, importc: "wave_contextGetData".}


proc contextHasErrors*(
    context: ptr WaveContextHandle
  ): bool {.dynlib: cwaveDl, importc: "wave_contextHasErrors".}


proc contextHasWarnings*(
    context: ptr WaveContextHandle
  ): bool {.dynlib: cwaveDl, importc: "wave_contextHasWarnings".}


proc deleteDiagnostics*(
    diag: ptr WaveDiagnostics
  ): void {.dynlib: cwaveDl, importc: "wave_deleteDiagnostics".}


proc contextPopDiagnostics*(
    context: ptr WaveContextHandle
  ): WaveDiagnostics {.dynlib: cwaveDl, importc: "wave_contextPopDiagnostics".}


proc addMacroDefinition*(
    context:       ptr WaveContextHandle,
    macrostring:   cstring,
    is_predefined: bool
  ): bool {.dynlib: cwaveDl, importc: "wave_addMacroDefinition".}


proc removeMacroDefinition*(
    context:       ptr WaveContextHandle,
    macrostring:   cstring,
    is_predefined: bool
  ): bool {.dynlib: cwaveDl, importc: "wave_removeMacroDefinition".}


proc isDefinedMacro*(
    context: ptr WaveContextHandle,
    name:    cstring
  ): bool {.dynlib: cwaveDl, importc: "wave_isDefinedMacro".}


proc getMacroDefinition*(
    context:           ptr WaveContextHandle,
    name:              cstring,
    is_function_style: ptr bool,
    is_predefined:     ptr bool,
    pos:               ptr WavePosition,
    parameters:        ptr ptr WaveTokenVectorHandle,
    definition:        ptr ptr WaveTokenListHandle
  ): bool {.dynlib: cwaveDl, importc: "wave_getMacroDefinition".}


proc macroBeginIterator*(
    context: ptr WaveContextHandle
  ): ptr WaveMacroIteratorHandle {.dynlib: cwaveDl, importc: "wave_macroBeginIterator".}


proc macroEndIterator*(
    context: ptr WaveContextHandle
  ): ptr WaveMacroIteratorHandle {.dynlib: cwaveDl, importc: "wave_macroEndIterator".}


proc neqMacroIterator*(
    i1: ptr WaveMacroIteratorHandle,
    i2: ptr WaveMacroIteratorHandle
  ): bool {.dynlib: cwaveDl, importc: "wave_neqMacroIterator".}


proc macroIteratorAdvance*(
    i: ptr WaveMacroIteratorHandle
  ): void {.dynlib: cwaveDl, importc: "wave_macroIteratorAdvance".}


proc macroIteratorDeref*(
    i: ptr WaveMacroIteratorHandle
  ): cstring {.dynlib: cwaveDl, importc: "wave_macroIteratorDeref".}


proc addSysincludePath*(
    context: ptr WaveContextHandle,
    path:    cstring
  ): bool {.dynlib: cwaveDl, importc: "wave_addSysincludePath".}


proc addIncludePath*(
    context: ptr WaveContextHandle,
    path:    cstring
  ): bool {.dynlib: cwaveDl, importc: "wave_addIncludePath".}


proc setSysincludeDelimiter*(
    context: ptr WaveContextHandle
  ): void {.dynlib: cwaveDl, importc: "wave_setSysincludeDelimiter".}


proc setCurrentFilename*(
    context: ptr WaveContextHandle,
    name:    cstring
  ): void {.dynlib: cwaveDl, importc: "wave_setCurrentFilename".}


proc findIncludeFile*(
    ctx:          ptr WaveContextHandle,
    str:          ptr cstring,
    dir:          ptr cstring,
    is_system:    bool,
    current_file: cstring
  ): bool {.dynlib: cwaveDl, importc: "wave_findIncludeFile".}


proc getCurrentFilename*(
    context: ptr WaveContextHandle
  ): cstring {.dynlib: cwaveDl, importc: "wave_getCurrentFilename".}


proc getCurrentDirectory*(
    context: ptr WaveContextHandle
  ): cstring {.dynlib: cwaveDl, importc: "wave_getCurrentDirectory".}


proc getIterationDepth*(
    context: ptr WaveContextHandle
  ): cint {.dynlib: cwaveDl, importc.}


proc beginIterator*(
    context: ptr WaveContextHandle
  ): ptr WaveIteratorHandle {.dynlib: cwaveDl, importc: "wave_beginIterator".}


proc endIterator*(
    context: ptr WaveContextHandle
  ): ptr WaveIteratorHandle {.dynlib: cwaveDl, importc: "wave_endIterator".}


proc advanceIterator*(
    iter: ptr WaveIteratorHandle
  ): void {.dynlib: cwaveDl, importc: "wave_advanceIterator".}


proc neqIterator*(
    iter1: ptr WaveIteratorHandle,
    iter2: ptr WaveIteratorHandle
  ): bool {.dynlib: cwaveDl, importc: "wave_neqIterator".}


proc iterGetTok*(
    iter: ptr WaveIteratorHandle
  ): ptr WaveTokenHandle {.dynlib: cwaveDl, importc: "wave_iterGetTok".}


proc deleteTok*(
    tok: ptr WaveTokenHandle
  ): void {.dynlib: cwaveDl, importc: "wave_deleteTok".}


proc tokGetId*(
    tok: ptr WaveTokenHandle
  ): CWaveTokId {.dynlib: cwaveDl, importc: "wave_tokGetId".}


proc tokGetValue*(
    tok: ptr WaveTokenHandle
  ): cstring {.dynlib: cwaveDl, importc: "wave_tokGetValue".}


proc unescapeIncludeToken*(
    s: cstring
  ): cstring {.dynlib: cwaveDl, importc: "wave_unescapeIncludeToken".}


proc currentFile*(
    ctx: ptr WaveContextHandle
  ): cstring {.dynlib: cwaveDl, importc: "wave_currentFile".}


proc currentLine*(
    ctx: ptr WaveContextHandle
  ): cint {.dynlib: cwaveDl, importc: "wave_currentLine".}


proc currentColumn*(
    ctx: ptr WaveContextHandle
  ): cint {.dynlib: cwaveDl, importc: "wave_currentColumn".}
