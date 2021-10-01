#ifndef WAVE_C_API_HPP
#define WAVE_C_API_HPP

#include "boost_wave_global.hpp"

#ifdef __cplusplus
extern "C" {

#    define TYPE
#    define TYPE_NAME(x)

#else

#    define TYPE typedef
#    define TYPE_NAME(x) x

#endif

TYPE enum WaveTokId {
    tokId_UNKNOWN,
    tokId_FIRST_TOKEN,
    tokId_AND,
    tokId_AND_ALT,
    tokId_ANDAND,
    tokId_ANDAND_ALT,
    tokId_ASSIGN,
    tokId_ANDASSIGN,
    tokId_ANDASSIGN_ALT,
    tokId_OR,
    tokId_OR_ALT,
    tokId_OR_TRIGRAPH,
    tokId_ORASSIGN,
    tokId_ORASSIGN_ALT,
    tokId_ORASSIGN_TRIGRAPH,
    tokId_XOR,
    tokId_XOR_ALT,
    tokId_XOR_TRIGRAPH,
    tokId_XORASSIGN,
    tokId_XORASSIGN_ALT,
    tokId_XORASSIGN_TRIGRAPH,
    tokId_COMMA,
    tokId_COLON,
    tokId_DIVIDE,
    tokId_DIVIDEASSIGN,
    tokId_DOT,
    tokId_DOTSTAR,
    tokId_ELLIPSIS,
    tokId_EQUAL,
    tokId_GREATER,
    tokId_GREATEREQUAL,
    tokId_LEFTBRACE,
    tokId_LEFTBRACE_ALT,
    tokId_LEFTBRACE_TRIGRAPH,
    tokId_LESS,
    tokId_LESSEQUAL,
    tokId_LEFTPAREN,
    tokId_LEFTBRACKET,
    tokId_LEFTBRACKET_ALT,
    tokId_LEFTBRACKET_TRIGRAPH,
    tokId_MINUS,
    tokId_MINUSASSIGN,
    tokId_MINUSMINUS,
    tokId_PERCENT,
    tokId_PERCENTASSIGN,
    tokId_NOT,
    tokId_NOT_ALT,
    tokId_NOTEQUAL,
    tokId_NOTEQUAL_ALT,
    tokId_OROR,
    tokId_OROR_ALT,
    tokId_OROR_TRIGRAPH,
    tokId_PLUS,
    tokId_PLUSASSIGN,
    tokId_PLUSPLUS,
    tokId_ARROW,
    tokId_ARROWSTAR,
    tokId_QUESTION_MARK,
    tokId_RIGHTBRACE,
    tokId_RIGHTBRACE_ALT,
    tokId_RIGHTBRACE_TRIGRAPH,
    tokId_RIGHTPAREN,
    tokId_RIGHTBRACKET,
    tokId_RIGHTBRACKET_ALT,
    tokId_RIGHTBRACKET_TRIGRAPH,
    tokId_COLON_COLON,
    tokId_SEMICOLON,
    tokId_SHIFTLEFT,
    tokId_SHIFTLEFTASSIGN,
    tokId_SHIFTRIGHT,
    tokId_SHIFTRIGHTASSIGN,
    tokId_STAR,
    tokId_COMPL,
    tokId_COMPL_ALT,
    tokId_COMPL_TRIGRAPH,
    tokId_STARASSIGN,
    tokId_ASM,
    tokId_AUTO,
    tokId_BOOL,
    tokId_FALSE,
    tokId_TRUE,
    tokId_BREAK,
    tokId_CASE,
    tokId_CATCH,
    tokId_CHAR,
    tokId_CLASS,
    tokId_CONST,
    tokId_CONSTCAST,
    tokId_CONTINUE,
    tokId_DEFAULT,
    tokId_DELETE,
    tokId_DO,
    tokId_DOUBLE,
    tokId_DYNAMICCAST,
    tokId_ELSE,
    tokId_ENUM,
    tokId_EXPLICIT,
    tokId_EXPORT,
    tokId_EXTERN,
    tokId_FLOAT,
    tokId_FOR,
    tokId_FRIEND,
    tokId_GOTO,
    tokId_IF,
    tokId_INLINE,
    tokId_INT,
    tokId_LONG,
    tokId_MUTABLE,
    tokId_NAMESPACE,
    tokId_NEW,
    tokId_OPERATOR,
    tokId_PRIVATE,
    tokId_PROTECTED,
    tokId_PUBLIC,
    tokId_REGISTER,
    tokId_REINTERPRETCAST,
    tokId_RETURN,
    tokId_SHORT,
    tokId_SIGNED,
    tokId_SIZEOF,
    tokId_STATIC,
    tokId_STATICCAST,
    tokId_STRUCT,
    tokId_SWITCH,
    tokId_TEMPLATE,
    tokId_THIS,
    tokId_THROW,
    tokId_TRY,
    tokId_TYPEDEF,
    tokId_TYPEID,
    tokId_TYPENAME,
    tokId_UNION,
    tokId_UNSIGNED,
    tokId_USING,
    tokId_VIRTUAL,
    tokId_VOID,
    tokId_VOLATILE,
    tokId_WCHART,
    tokId_WHILE,
    tokId_PP_DEFINE,
    tokId_PP_IF,
    tokId_PP_IFDEF,
    tokId_PP_IFNDEF,
    tokId_PP_ELSE,
    tokId_PP_ELIF,
    tokId_PP_ENDIF,
    tokId_PP_ERROR,
    tokId_PP_LINE,
    tokId_PP_PRAGMA,
    tokId_PP_UNDEF,
    tokId_PP_WARNING,
    tokId_IDENTIFIER,
    tokId_OCTALINT,
    tokId_DECIMALINT,
    tokId_HEXAINT,
    tokId_INTLIT,
    tokId_LONGINTLIT,
    tokId_FLOATLIT,
    tokId_FIXEDPOINTLIT,
    tokId_CCOMMENT,
    tokId_CPPCOMMENT,
    tokId_CHARLIT,
    tokId_STRINGLIT,
    tokId_CONTLINE,
    tokId_SPACE,
    tokId_SPACE2,
    tokId_NEWLINE,
    tokId_GENERATEDNEWLINE,
    tokId_POUND_POUND,
    tokId_POUND_POUND_ALT,
    tokId_POUND_POUND_TRIGRAPH,
    tokId_POUND,
    tokId_POUND_ALT,
    tokId_POUND_TRIGRAPH,
    tokId_ANY,
    tokId_ANY_TRIGRAPH,
    tokId_PP_INCLUDE,
    tokId_PP_QHEADER,
    tokId_PP_HHEADER,
    tokId_PP_INCLUDE_NEXT,
    tokId_PP_QHEADER_NEXT,
    tokId_PP_HHEADER_NEXT,
    tokId_EOF,
    tokId_EOI,
    tokId_PP_NUMBER,
    tokId_MSEXT_INT8,
    tokId_MSEXT_INT16,
    tokId_MSEXT_INT32,
    tokId_MSEXT_INT64,
    tokId_MSEXT_BASED,
    tokId_MSEXT_DECLSPEC,
    tokId_MSEXT_CDECL,
    tokId_MSEXT_FASTCALL,
    tokId_MSEXT_STDCALL,
    tokId_MSEXT_TRY,
    tokId_MSEXT_EXCEPT,
    tokId_MSEXT_FINALLY,
    tokId_MSEXT_LEAVE,
    tokId_MSEXT_INLINE,
    tokId_MSEXT_ASM,

    tokId_MSEXT_PP_REGION,
    tokId_MSEXT_PP_ENDREGION,
    tokId_IMPORT,
    tokId_ALIGNAS,
    tokId_ALIGNOF,
    tokId_CHAR16_T,
    tokId_CHAR32_T,
    tokId_CONSTEXPR,
    tokId_DECLTYPE,
    tokId_NOEXCEPT,
    tokId_NULLPTR,
    tokId_STATICASSERT,
    tokId_THREADLOCAL,
    tokId_RAWSTRINGLIT,
    tokId_CHAR8_T,
    tokId_CONCEPT,
    tokId_CONSTEVAL,
    tokId_CONSTINIT,
    tokId_CO_AWAIT,
    tokId_CO_RETURN,
    tokId_CO_YIELD,
    tokId_REQUIRES,
    tokId_SPACESHIP,

    tokId_LAST_TOKEN_ID,
    tokId_LAST_TOKEN,

    tokId_UNKNOWN_UNIVERSALCHAR,
    tokId_NONREPLACABLE_IDENTIFIER,
    tokId_PLACEHOLDER,
    tokId_PLACEMARKER,
    tokId_PARAMETERBASE,
    tokId_EXTPARAMETERBASE,
    tokId_OPTPARAMETERBASE,
} TYPE_NAME(WaveTokId);

TYPE enum WaveErrorCode {
    wekNoError = 0,
    wekUnexpectedError,
    wekMacroRedefinition,
    wekMacroInsertionError,
    wekBadIncludeFile,
    wekBadIncludeStatement,
    wekBadHasIncludeExpression,
    wekIllFormedDirective,
    wekErrorDirective,
    wekWarningDirective,
    wekIllFormedExpression,
    wekMissingMatchingIf,
    wekMissingMatchingEndif,
    wekIllFormedOperator,
    wekBadDefineStatement,
    wekBadDefineStatementVaArgs,
    wekBadDefineStatementVaOpt,
    wekBadDefineStatementVaOptParens,
    wekBadDefineStatementVaOptRecurse,
    wekTooFewMacroarguments,
    wekTooManyMacroarguments,
    wekEmptyMacroarguments,
    wekImproperlyTerminatedMacro,
    wekBadLineStatement,
    wekBadLineNumber,
    wekBadLineFilename,
    wekBadUndefineStatement,
    wekBadMacroDefinition,
    wekIllegalRedefinition,
    wekDuplicateParameterName,
    wekInvalidConcat,
    wekLastLineNotTerminated,
    wekIllFormedPragmaOption,
    wekIncludeNestingTooDeep,
    wekMisplacedOperator,
    wekAlreadydefinedName,
    wekUndefinedMacroname,
    wekInvalidMacroname,
    wekUnexpectedQualifiedName,
    wekDivisionByZero,
    wekIntegerOverflow,
    wekIllegalOperatorRedefinition,
    wekIllFormedIntegerLiteral,
    wekIllFormedCharacterLiteral,
    wekUnbalancedIfEndif,
    wekCharacterLiteralOutOfRange,
    wekCouldNotOpenOutputFile,
    wekIncompatibleConfig,
    wekIllFormedPragmaMessage,
    wekPragmaMessageDirective,
    wekLastErrorNumber = wekPragmaMessageDirective
} TYPE_NAME(WaveErrorCode);


TYPE enum WaveSeverityLevel {
    wslRemark,
    wslWarning,
    wslError,
    wslFatal,
    wslCommandlineError,
    wslLastCode = wslCommandlineError
} TYPE_NAME(WaveSeverityLevel);


TYPE enum EntryHandling {
    EntryHandlingSkip,
    EntryHandlingProcess,
    EntryHandlingRaise
} TYPE_NAME(EntryHandling);


#define DECL_STRUCT(name) TYPE struct name TYPE_NAME(name);

DECL_STRUCT(WaveProcessingHooksHandle);
DECL_STRUCT(WaveContextHandle);
DECL_STRUCT(WaveIteratorHandle);
DECL_STRUCT(WaveTokenHandle);
DECL_STRUCT(WaveContextImplHandle);
DECL_STRUCT(WaveTokenListHandle);
DECL_STRUCT(WaveTokenVectorHandle);
DECL_STRUCT(WaveTokenListHandle);
DECL_STRUCT(WaveMacroIteratorHandle);
DECL_STRUCT(WaveTokenListIteratorHandle);


// TYPE struct WaveTokenHandle {
//    char fake
//} TYPE_NAME(WaveTokenHandle);

TYPE struct WavePosition {
} TYPE_NAME(WavePosition);


BOOST_WAVE_EXPORT int wave_tokenVectorLen(WaveTokenVectorHandle* vec);
BOOST_WAVE_EXPORT WaveTokenHandle* wave_tokenVectorGetAt(
    WaveTokenVectorHandle* vec,
    int                    idx);
BOOST_WAVE_EXPORT void wave_deleteWaveTokenVector(
    WaveTokenVectorHandle* vec);

BOOST_WAVE_EXPORT int         wave_tokenListLen(WaveTokenListHandle* list);
BOOST_WAVE_EXPORT const char* wave_tokenListToStr(
    WaveTokenListHandle* list);

BOOST_WAVE_EXPORT WaveTokenListIteratorHandle* wave_tokenListBeginIterator(
    WaveTokenListHandle* l);
BOOST_WAVE_EXPORT WaveTokenListIteratorHandle* wave_tokenListEndIterator(
    WaveTokenListHandle* l);
BOOST_WAVE_EXPORT bool wave_neqListIterator(
    WaveTokenListIteratorHandle* i1,
    WaveTokenListIteratorHandle* i2);
BOOST_WAVE_EXPORT WaveTokenHandle* wave_listIterDeref(
    WaveTokenListIteratorHandle* i);
BOOST_WAVE_EXPORT void wave_listIterAdvance(
    WaveTokenListIteratorHandle* i);

BOOST_WAVE_EXPORT WaveProcessingHooksHandle* wave_newProcessingHooks();
BOOST_WAVE_EXPORT void                       wave_destroyProcessingHooks(
                          WaveProcessingHooksHandle* hooks);


BOOST_WAVE_EXPORT WaveContextHandle* wave_newWaveContext(
    const char* instring,
    const char* filename);

BOOST_WAVE_EXPORT void wave_processAll(WaveContextHandle* context);


typedef EntryHandling (*FoundWarningDirectiveImplType)(
    const WaveContextImplHandle* ctx,
    const WaveTokenListHandle*   message,
    void*                        env);

BOOST_WAVE_EXPORT void wave_setFoundWarningDirective(
    WaveContextHandle*            context,
    FoundWarningDirectiveImplType impl,
    void*                         env);


typedef bool (*EvaluatedConditionalExpressionImplType)(
    const WaveContextImplHandle* ctx,
    const WaveTokenHandle*       directive,
    const WaveTokenListHandle*   expression,
    bool                         expression_value,
    void*                        env);

BOOST_WAVE_EXPORT void wave_setEvaluatedConditionalExpression(
    WaveContextHandle*                     context,
    EvaluatedConditionalExpressionImplType impl,
    void*                                  env);


typedef EntryHandling (*FoundUnknownDirectiveImplType)(
    WaveContextImplHandle* ctx,
    WaveTokenListHandle*   line,
    WaveTokenListHandle*   pending,
    void*                  env);

BOOST_WAVE_EXPORT void wave_setFoundUnknownDirectiveImplType(
    WaveContextHandle*            context,
    FoundUnknownDirectiveImplType impl,
    void*                         env);

// Expanding function like macro

typedef bool (*ExpandingFunctionLikeMacroImplType)(
    const WaveContextImplHandle* ctx,
    const WaveTokenHandle*       macrodef,
    const WaveTokenVectorHandle* formal_args,
    const WaveTokenListHandle*   definition,
    const WaveTokenHandle*       macrocall,
    WaveTokenVectorHandle*       arguments,
    void*                        seqstart,
    void*                        seqend,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setExpandingFunctionLikeMacro(
    WaveContextHandle*                 context,
    ExpandingFunctionLikeMacroImplType impl,
    void*                              env);

// Expanding object like macro

typedef EntryHandling (*ExpandingObjectLikeMacroImplType)(
    const WaveContextImplHandle* ctx,
    const WaveTokenHandle*       macro,
    const WaveTokenListHandle*   definition,
    const WaveTokenHandle*       macrocall,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setExpandingObjectLikeMacro(
    WaveContextHandle*               context,
    ExpandingObjectLikeMacroImplType impl,
    void*                            env);

// Expanded macro

typedef void (*ExpandedMacroImplType)(
    const WaveContextImplHandle* ctx,
    const WaveTokenListHandle*   result,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setExpandedMacro(
    WaveContextHandle*    context,
    ExpandedMacroImplType impl,
    void*                 env);


// Rescanned macro

typedef void (*RescannedMacroImplType)(
    const WaveContextImplHandle* ctx,
    const WaveTokenListHandle*   result,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setRescannedMacro(
    WaveContextHandle*     context,
    RescannedMacroImplType impl,
    void*                  env);


// Found directive

typedef EntryHandling (*FoundDirectiveImplType)(
    WaveContextImplHandle* ctx,
    WaveTokenHandle*       tok,
    void*                  env);


BOOST_WAVE_EXPORT void wave_setFoundDirective(
    WaveContextHandle*     ctx,
    FoundDirectiveImplType filename,
    void*                  include_next);

// Found include directive

typedef EntryHandling (*FoundIncludeDirectiveImplType)(
    const WaveContextImplHandle* context,
    const char*                  impl,
    bool                         include_next,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setFoundIncludeDirective(
    WaveContextHandle*            context,
    FoundIncludeDirectiveImplType impl,
    void*                         env);


// Skipped token

typedef void (*SkippedTokenImplType)(
    const WaveContextImplHandle* context,
    const WaveTokenHandle*       token,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setSkippedToken(
    WaveContextHandle*   context,
    SkippedTokenImplType impl,
    void*                env);

// Emit line directive

typedef bool (*EmitLineDirectiveImplType)(
    const WaveContextImplHandle* ctx,
    WaveTokenListHandle*         pending,
    const WaveTokenHandle*       act_token,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setEmitLineDirective(
    WaveContextHandle*        context,
    EmitLineDirectiveImplType impl,
    void*                     env);

// Found line directive

typedef EntryHandling (*FoundLineDirectiveImplType)(
    const WaveContextImplHandle* ctx,
    const WaveTokenListHandle*   arguments,
    unsigned int                 line,
    const char*                  filename,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setFoundLineDirective(
    WaveContextHandle*         context,
    FoundLineDirectiveImplType impl,
    void*                      env);

// Found error directive

typedef EntryHandling (*FoundErrorDirectiveImplType)(
    const WaveContextImplHandle* ctx,
    const WaveTokenListHandle*   message,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setFoundErrorDirective(
    WaveContextHandle*          context,
    FoundErrorDirectiveImplType impl,
    void*                       env);

// Defined macro

typedef void (*DefinedMacroImplType)(
    const WaveContextImplHandle* ctx,
    const WaveTokenHandle*       name,
    bool                         is_functionlike,
    const WaveTokenVectorHandle* parameters,
    const WaveTokenListHandle*   definition,
    bool                         is_predefined,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setDefinedMacro(
    WaveContextHandle*   context,
    DefinedMacroImplType impl,
    void*                env);

// Locate include file

typedef EntryHandling (*LocateIncludeFileImplType)(
    WaveContextImplHandle* ctx,
    char*                  file_path,
    bool                   is_system,
    char const*            current_name,
    char*                  dir_path,
    char*                  native_name,
    void*                  env);


BOOST_WAVE_EXPORT void wave_setLocateIncludeFile(
    WaveContextHandle*        context,
    LocateIncludeFileImplType impl,
    void*                     env);

// Opened include file

typedef void (*OpenedIncludeFileImplType)(
    const WaveContextImplHandle* ctx,
    const char*                  rel_filename,
    const char*                  abs_filename,
    bool                         is_system_include,
    void*                        env);


BOOST_WAVE_EXPORT void wave_setOpenedIncludeFile(
    WaveContextHandle*        context,
    OpenedIncludeFileImplType impl,
    void*                     env);

// Returning from include file

typedef void (*ReturningFromIncludeFileImplType)(
    const WaveContextImplHandle*,
    void* env);


BOOST_WAVE_EXPORT void wave_setReturningFromIncludeFile(
    WaveContextHandle*               context,
    ReturningFromIncludeFileImplType impl,
    void*                            env);


BOOST_WAVE_EXPORT void wave_destroyContext(WaveContextHandle* context);

TYPE struct WaveDiagnostics {
    int               line;
    int               column;
    WaveErrorCode     code;
    WaveSeverityLevel level;

    char* filename;
    char* errorText;
} TYPE_NAME(WaveDiagnostics);


BOOST_WAVE_EXPORT void wave_contextSetData(
    WaveContextHandle* context,
    void*              data);
BOOST_WAVE_EXPORT void* wave_contextGetData(WaveContextHandle* context);
BOOST_WAVE_EXPORT bool  wave_contextHasErrors(WaveContextHandle* context);
BOOST_WAVE_EXPORT bool wave_contextHasWarnings(WaveContextHandle* context);
BOOST_WAVE_EXPORT void wave_deleteDiagnostics(WaveDiagnostics* diag);
BOOST_WAVE_EXPORT WaveDiagnostics
    wave_contextPopDiagnostics(WaveContextHandle* context);
BOOST_WAVE_EXPORT void wave_addMacroDefinition(
    WaveContextHandle* context,
    const char*        macrostring,
    bool               is_predefined);
BOOST_WAVE_EXPORT bool wave_removeMacroDefinition(
    WaveContextHandle* context,
    const char*        macrostring,
    bool               is_predefined);
BOOST_WAVE_EXPORT bool wave_isDefinedMacro(
    WaveContextHandle* context,
    const char*        name);
BOOST_WAVE_EXPORT bool wave_getMacroDefinition(
    WaveContextHandle*      context,
    const char*             name,
    bool*                   is_function_style,
    bool*                   is_predefined,
    WavePosition*           pos,
    WaveTokenVectorHandle** parameters,
    WaveTokenVectorHandle** definition);

BOOST_WAVE_EXPORT WaveMacroIteratorHandle* wave_macroBeginIterator(
    WaveContextHandle* context);
BOOST_WAVE_EXPORT WaveMacroIteratorHandle* wave_macroEndIterator(
    WaveContextHandle* context);
BOOST_WAVE_EXPORT bool wave_neqMacroIterator(
    WaveMacroIteratorHandle* i1,
    WaveMacroIteratorHandle* i2);
BOOST_WAVE_EXPORT void wave_macroIteratorAdvance(
    WaveMacroIteratorHandle* i);
BOOST_WAVE_EXPORT const char* wave_macroIteratorDeref(
    WaveMacroIteratorHandle* i);

BOOST_WAVE_EXPORT bool wave_addSysincludePath(
    WaveContextHandle* context,
    char const*        path);

BOOST_WAVE_EXPORT bool wave_addIncludePath(
    WaveContextHandle* context,
    char const*        path);

BOOST_WAVE_EXPORT void wave_setSysincludeDelimiter(
    WaveContextHandle* context);

BOOST_WAVE_EXPORT void wave_setCurrentFilename(
    WaveContextHandle* context,
    const char*        name);

BOOST_WAVE_EXPORT bool wave_findIncludeFile(
    WaveContextHandle* ctx,
    char**             str,
    char**             dir,
    bool               is_system,
    char const*        current_file);


BOOST_WAVE_EXPORT const char* wave_getCurrentFilename(
    WaveContextHandle* context);
BOOST_WAVE_EXPORT const char* wave_getCurrentDirectory(
    WaveContextHandle* context);

BOOST_WAVE_EXPORT int getIterationDepth(WaveContextHandle* context);

BOOST_WAVE_EXPORT WaveIteratorHandle* wave_beginIterator(
    WaveContextHandle* context);
BOOST_WAVE_EXPORT WaveIteratorHandle* wave_endIterator(
    WaveContextHandle* context);
BOOST_WAVE_EXPORT void wave_advanceIterator(WaveIteratorHandle* iter);
BOOST_WAVE_EXPORT bool wave_neqIterator(
    WaveIteratorHandle* iter1,
    WaveIteratorHandle* iter2);

BOOST_WAVE_EXPORT WaveTokenHandle* wave_iterGetTok(
    WaveIteratorHandle* iter);
BOOST_WAVE_EXPORT void wave_deleteTok(WaveTokenHandle* tok);

BOOST_WAVE_EXPORT WaveTokId   wave_tokGetId(WaveTokenHandle* tok);
BOOST_WAVE_EXPORT const char* wave_tokGetValue(WaveTokenHandle* tok);
BOOST_WAVE_EXPORT const char* wave_unescapeIncludeToken(const char* s);

#ifdef __cplusplus
}
#endif

#endif // WAVE_C_API_HPP
