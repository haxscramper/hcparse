import externc
import index
from times import Time
{.deadCodeElim: on.}
{.push callconv: cdecl.}
import opaque_impls

when defined(windows):
  const
    libclang = "libclang.dll"
elif defined(macosx):
  const
    libclang = "libclang.dylib"
else:
  const
    libclang = "libclang.so"


type
  CXComment* {.pure, bycopy.} = object
    aSTNode*: pointer # `const void *`
    translationUnit*: CXTranslationUnit # `CXTranslationUnit`

proc clang_Cursor_getParsedComment*(
  c: CXCursor, # `CXCursor`
): CXComment {.
    cdecl,
    importc: "clang_Cursor_getParsedComment",
    dynlib: libclang
  .}

proc clang_Comment_getKind*(
  comment: CXComment, # `CXComment`
): CXCommentKind {.
    cdecl,
    importc: "clang_Comment_getKind",
    dynlib: libclang
  .}

proc clang_Comment_getNumChildren*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_Comment_getNumChildren",
    dynlib: libclang
  .}

proc clang_Comment_getChild*(
  comment: CXComment, # `CXComment`
  childIdx: cuint, # `unsigned int`
): CXComment {.
    cdecl,
    importc: "clang_Comment_getChild",
    dynlib: libclang
  .}

proc clang_Comment_isWhitespace*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_Comment_isWhitespace",
    dynlib: libclang
  .}

proc clang_InlineContentComment_hasTrailingNewline*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_InlineContentComment_hasTrailingNewline",
    dynlib: libclang
  .}

proc clang_TextComment_getText*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_TextComment_getText",
    dynlib: libclang
  .}

proc clang_InlineCommandComment_getCommandName*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_InlineCommandComment_getCommandName",
    dynlib: libclang
  .}

proc clang_InlineCommandComment_getRenderKind*(
  comment: CXComment, # `CXComment`
): CXCommentInlineCommandRenderKind {.
    cdecl,
    importc: "clang_InlineCommandComment_getRenderKind",
    dynlib: libclang
  .}

proc clang_InlineCommandComment_getNumArgs*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_InlineCommandComment_getNumArgs",
    dynlib: libclang
  .}

proc clang_InlineCommandComment_getArgText*(
  comment: CXComment, # `CXComment`
  argIdx: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_InlineCommandComment_getArgText",
    dynlib: libclang
  .}

proc clang_HTMLTagComment_getTagName*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_HTMLTagComment_getTagName",
    dynlib: libclang
  .}

proc clang_HTMLStartTagComment_isSelfClosing*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_HTMLStartTagComment_isSelfClosing",
    dynlib: libclang
  .}

proc clang_HTMLStartTag_getNumAttrs*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_HTMLStartTag_getNumAttrs",
    dynlib: libclang
  .}

proc clang_HTMLStartTag_getAttrName*(
  comment: CXComment, # `CXComment`
  attrIdx: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_HTMLStartTag_getAttrName",
    dynlib: libclang
  .}

proc clang_HTMLStartTag_getAttrValue*(
  comment: CXComment, # `CXComment`
  attrIdx: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_HTMLStartTag_getAttrValue",
    dynlib: libclang
  .}

proc clang_BlockCommandComment_getCommandName*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_BlockCommandComment_getCommandName",
    dynlib: libclang
  .}

proc clang_BlockCommandComment_getNumArgs*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_BlockCommandComment_getNumArgs",
    dynlib: libclang
  .}

proc clang_BlockCommandComment_getArgText*(
  comment: CXComment, # `CXComment`
  argIdx: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_BlockCommandComment_getArgText",
    dynlib: libclang
  .}

proc clang_BlockCommandComment_getParagraph*(
  comment: CXComment, # `CXComment`
): CXComment {.
    cdecl,
    importc: "clang_BlockCommandComment_getParagraph",
    dynlib: libclang
  .}

proc clang_ParamCommandComment_getParamName*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_ParamCommandComment_getParamName",
    dynlib: libclang
  .}

proc clang_ParamCommandComment_isParamIndexValid*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_ParamCommandComment_isParamIndexValid",
    dynlib: libclang
  .}

proc clang_ParamCommandComment_getParamIndex*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_ParamCommandComment_getParamIndex",
    dynlib: libclang
  .}

proc clang_ParamCommandComment_isDirectionExplicit*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_ParamCommandComment_isDirectionExplicit",
    dynlib: libclang
  .}

proc clang_ParamCommandComment_getDirection*(
  comment: CXComment, # `CXComment`
): CXCommentParamPassDirection {.
    cdecl,
    importc: "clang_ParamCommandComment_getDirection",
    dynlib: libclang
  .}

proc clang_TParamCommandComment_getParamName*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_TParamCommandComment_getParamName",
    dynlib: libclang
  .}

proc clang_TParamCommandComment_isParamPositionValid*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_TParamCommandComment_isParamPositionValid",
    dynlib: libclang
  .}

proc clang_TParamCommandComment_getDepth*(
  comment: CXComment, # `CXComment`
): cuint {.
    cdecl,
    importc: "clang_TParamCommandComment_getDepth",
    dynlib: libclang
  .}

proc clang_TParamCommandComment_getIndex*(
  comment: CXComment, # `CXComment`
  depth: cuint, # `unsigned int`
): cuint {.
    cdecl,
    importc: "clang_TParamCommandComment_getIndex",
    dynlib: libclang
  .}

proc clang_VerbatimBlockLineComment_getText*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_VerbatimBlockLineComment_getText",
    dynlib: libclang
  .}

proc clang_VerbatimLineComment_getText*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_VerbatimLineComment_getText",
    dynlib: libclang
  .}

proc clang_HTMLTagComment_getAsString*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_HTMLTagComment_getAsString",
    dynlib: libclang
  .}

proc clang_FullComment_getAsHTML*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_FullComment_getAsHTML",
    dynlib: libclang
  .}

proc clang_FullComment_getAsXML*(
  comment: CXComment, # `CXComment`
): CXString {.
    cdecl,
    importc: "clang_FullComment_getAsXML",
    dynlib: libclang
  .}

