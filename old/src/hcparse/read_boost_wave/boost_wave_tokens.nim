import std/bitops

func `or`[A, B](a: A, b: B): int = a.int or b.int
func `and`[A, B](a: A, b: B): int = a.int and b.int

template fromId(id, cat: untyped): untyped = bitor(id.int, cat.int)
# template fromTok(tok: )
# template fromId(id, cat: untyped): untyped = id  or  cat

# #define ID_FROM_TOKEN(tok)       ((tok) & ~TokenTypeMask)
# #define BASEID_FROM_TOKEN(tok)   ((tok) & ~ExtTokenTypeMask)


type
  TokenCategory = enum
    PPTokenFlag                 = 0x00040000
    TokenValueMask              = 0x0007FFFF
    AltTokenType                = 0x00080000
    TriGraphTokenType           = 0x00100000
    AltExtTokenType             = 0x00280000
    ExtTokenOnlyMask            = 0x00780000


    IdentifierTokenType         = 0x08040000
    ParameterTokenType          = 0x08840000
    ExtParameterTokenType       = 0x088C0000
    OptParameterTokenType       = 0x08940000
    KeywordTokenType            = 0x10040000
    OperatorTokenType           = 0x18040000
    LiteralTokenType            = 0x20040000
    IntegerLiteralTokenType     = 0x20840000
    FloatingLiteralTokenType    = 0x21040000
    StringLiteralTokenType      = 0x21840000
    CharacterLiteralTokenType   = 0x22040000
    BoolLiteralTokenType        = 0x22840000
    PPTokenType                 = 0x28040000
    PPConditionalTokenType      = 0x28440000

    UnknownTokenType            = 0x50000000
    EOLTokenType                = 0x58000000
    EOFTokenType                = 0x60000000
    WhiteSpaceTokenType         = 0x68000000
    InternalTokenType           = 0x70040000

    TokenTypeMask               = 0x7F800000


    MainTokenMask               = 0x7F87FFFF
    ExtTokenTypeMask            = 0x7FF80000

import std/macros

type
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

    # # T_LAST_TOKEN_ID = 1879310739 + 1

    # T_LAST_TOKEN               = (((1879310739 + 1) and not PPTokenFlag.uint) and not TokenTypeMask.uint)
    # T_UNKNOWN_UNIVERSALCHAR    = (('\\'.int) or (UnknownTokenType))
    # T_NONREPLACABLE_IDENTIFIER = ((T_LAST_TOKEN + 1) or (IdentifierTokenType))
    # T_PLACEHOLDER              = ((T_LAST_TOKEN + 2) or (WhiteSpaceTokenType))
    # T_PLACEMARKER              = ((T_LAST_TOKEN + 3) or (InternalTokenType))
    # T_PARAMETERBASE            = ((T_LAST_TOKEN + 4) or (ParameterTokenType))
    # T_EXTPARAMETERBASE         = ((T_LAST_TOKEN + 4) or (ExtParameterTokenType))
    # T_OPTPARAMETERBASE         = ((T_LAST_TOKEN + 4) or (OptParameterTokenType))
