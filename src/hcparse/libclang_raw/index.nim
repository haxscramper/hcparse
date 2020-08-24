import build_system
import cxcompilation_database
import cxerror_code
import cxstring
import externc
import fatal_error_handler
import opaque_impls
import platform
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


type CXIndex* = distinct pointer # CXIndex

type CXTargetInfo* = distinct ptr[CXTargetInfoImpl] # CXTargetInfo

type CXTranslationUnit* = distinct ptr[CXTranslationUnitImpl] # CXTranslationUnit

type CXClientData* = distinct pointer # CXClientData

type
  CXVersion* {.pure, bycopy.} = object
    major*: cint # `int`
    minor*: cint # `int`
    subminor*: cint # `int`

proc clang_createIndex*(
  excludeDeclarationsFromPCH: cint, # `int`
  displayDiagnostics: cint, # `int`
): CXIndex {.
    cdecl,
    importc: "clang_createIndex",
    dynlib: libclang
  .}

proc clang_disposeIndex*(
  index: CXIndex, # `CXIndex`
): void {.
    cdecl,
    importc: "clang_disposeIndex",
    dynlib: libclang
  .}

type
  CXGlobalOptFlags* {.pure, size: sizeof(cint).} = enum
    CXGlobalOpt_None = 0
    CXGlobalOpt_ThreadBackgroundPriorityForIndexing = 1
    CXGlobalOpt_ThreadBackgroundPriorityForEditing = 2
    CXGlobalOpt_ThreadBackgroundPriorityForAll = 4

proc clang_CXIndex_setGlobalOptions*(
  arg_1: CXIndex, # `CXIndex`
  options: cuint, # `unsigned int`
): void {.
    cdecl,
    importc: "clang_CXIndex_setGlobalOptions",
    dynlib: libclang
  .}

proc clang_CXIndex_getGlobalOptions*(
  arg_1: CXIndex, # `CXIndex`
): cuint {.
    cdecl,
    importc: "clang_CXIndex_getGlobalOptions",
    dynlib: libclang
  .}

proc clang_CXIndex_setInvocationEmissionPathOption*(
  arg_1: CXIndex, # `CXIndex`
  path: ptr[cstring], # `const char *`
): void {.
    cdecl,
    importc: "clang_CXIndex_setInvocationEmissionPathOption",
    dynlib: libclang
  .}

type CXFile* = distinct pointer # CXFile

proc clang_getFileName*(
  sFile: CXFile, # `CXFile`
): CXString {.
    cdecl,
    importc: "clang_getFileName",
    dynlib: libclang
  .}

proc clang_getFileTime*(
  sFile: CXFile, # `CXFile`
): time_t {.
    cdecl,
    importc: "clang_getFileTime",
    dynlib: libclang
  .}

type
  CXFileUniqueID* {.pure, bycopy.} = object
    data*: array[3, unsigned long long] # `unsigned long long [3]`

proc clang_getFileUniqueID*(
  file: CXFile, # `CXFile`
  outID: ptr[CXFileUniqueID], # `CXFileUniqueID *`
): cint {.
    cdecl,
    importc: "clang_getFileUniqueID",
    dynlib: libclang
  .}

proc clang_isFileMultipleIncludeGuarded*(
  tu: CXTranslationUnit, # `CXTranslationUnit`
  file: CXFile, # `CXFile`
): cuint {.
    cdecl,
    importc: "clang_isFileMultipleIncludeGuarded",
    dynlib: libclang
  .}

proc clang_getFile*(
  tu: CXTranslationUnit, # `CXTranslationUnit`
  file_name: ptr[cstring], # `const char *`
): CXFile {.
    cdecl,
    importc: "clang_getFile",
    dynlib: libclang
  .}

proc clang_getFileContents*(
  tu: CXTranslationUnit, # `CXTranslationUnit`
  file: CXFile, # `CXFile`
  size: ptr[cint], # `int *`
): ptr[cstring] {.
    cdecl,
    importc: "clang_getFileContents",
    dynlib: libclang
  .}

proc clang_File_isEqual*(
  file1: CXFile, # `CXFile`
  file2: CXFile, # `CXFile`
): cint {.
    cdecl,
    importc: "clang_File_isEqual",
    dynlib: libclang
  .}

proc clang_File_tryGetRealPathName*(
  file: CXFile, # `CXFile`
): CXString {.
    cdecl,
    importc: "clang_File_tryGetRealPathName",
    dynlib: libclang
  .}

type
  CXSourceLocation* {.pure, bycopy.} = object
    ptr_data*: array[2, const void *] # `const void *[2]`
    int_data*: cuint # `unsigned int`

type
  CXSourceRange* {.pure, bycopy.} = object
    ptr_data*: array[2, const void *] # `const void *[2]`
    begin_int_data*: cuint # `unsigned int`
    end_int_data*: cuint # `unsigned int`

proc clang_getNullLocation*(): CXSourceLocation {.
    cdecl,
    importc: "clang_getNullLocation",
    dynlib: libclang
  .}

proc clang_equalLocations*(
  loc1: CXSourceLocation, # `CXSourceLocation`
  loc2: CXSourceLocation, # `CXSourceLocation`
): cuint {.
    cdecl,
    importc: "clang_equalLocations",
    dynlib: libclang
  .}

proc clang_getLocation*(
  tu: CXTranslationUnit, # `CXTranslationUnit`
  file: CXFile, # `CXFile`
  line: cuint, # `unsigned int`
  column: cuint, # `unsigned int`
): CXSourceLocation {.
    cdecl,
    importc: "clang_getLocation",
    dynlib: libclang
  .}

proc clang_getLocationForOffset*(
  tu: CXTranslationUnit, # `CXTranslationUnit`
  file: CXFile, # `CXFile`
  offset: cuint, # `unsigned int`
): CXSourceLocation {.
    cdecl,
    importc: "clang_getLocationForOffset",
    dynlib: libclang
  .}

proc clang_Location_isInSystemHeader*(
  location: CXSourceLocation, # `CXSourceLocation`
): cint {.
    cdecl,
    importc: "clang_Location_isInSystemHeader",
    dynlib: libclang
  .}

proc clang_Location_isFromMainFile*(
  location: CXSourceLocation, # `CXSourceLocation`
): cint {.
    cdecl,
    importc: "clang_Location_isFromMainFile",
    dynlib: libclang
  .}

proc clang_getNullRange*(): CXSourceRange {.
    cdecl,
    importc: "clang_getNullRange",
    dynlib: libclang
  .}

proc clang_getRange*(
  begin: CXSourceLocation, # `CXSourceLocation`
  cend: CXSourceLocation, # `CXSourceLocation`
): CXSourceRange {.
    cdecl,
    importc: "clang_getRange",
    dynlib: libclang
  .}

proc clang_equalRanges*(
  range1: CXSourceRange, # `CXSourceRange`
  range2: CXSourceRange, # `CXSourceRange`
): cuint {.
    cdecl,
    importc: "clang_equalRanges",
    dynlib: libclang
  .}

proc clang_Range_isNull*(
  crange: CXSourceRange, # `CXSourceRange`
): cint {.
    cdecl,
    importc: "clang_Range_isNull",
    dynlib: libclang
  .}

proc clang_getExpansionLocation*(
  location: CXSourceLocation, # `CXSourceLocation`
  file: ptr[CXFile], # `CXFile *`
  line: ptr[cuint], # `unsigned int *`
  column: ptr[cuint], # `unsigned int *`
  offset: ptr[cuint], # `unsigned int *`
): void {.
    cdecl,
    importc: "clang_getExpansionLocation",
    dynlib: libclang
  .}

proc clang_getPresumedLocation*(
  location: CXSourceLocation, # `CXSourceLocation`
  filename: ptr[CXString], # `CXString *`
  line: ptr[cuint], # `unsigned int *`
  column: ptr[cuint], # `unsigned int *`
): void {.
    cdecl,
    importc: "clang_getPresumedLocation",
    dynlib: libclang
  .}

proc clang_getInstantiationLocation*(
  location: CXSourceLocation, # `CXSourceLocation`
  file: ptr[CXFile], # `CXFile *`
  line: ptr[cuint], # `unsigned int *`
  column: ptr[cuint], # `unsigned int *`
  offset: ptr[cuint], # `unsigned int *`
): void {.
    cdecl,
    importc: "clang_getInstantiationLocation",
    dynlib: libclang
  .}

proc clang_getSpellingLocation*(
  location: CXSourceLocation, # `CXSourceLocation`
  file: ptr[CXFile], # `CXFile *`
  line: ptr[cuint], # `unsigned int *`
  column: ptr[cuint], # `unsigned int *`
  offset: ptr[cuint], # `unsigned int *`
): void {.
    cdecl,
    importc: "clang_getSpellingLocation",
    dynlib: libclang
  .}

proc clang_getFileLocation*(
  location: CXSourceLocation, # `CXSourceLocation`
  file: ptr[CXFile], # `CXFile *`
  line: ptr[cuint], # `unsigned int *`
  column: ptr[cuint], # `unsigned int *`
  offset: ptr[cuint], # `unsigned int *`
): void {.
    cdecl,
    importc: "clang_getFileLocation",
    dynlib: libclang
  .}

proc clang_getRangeStart*(
  crange: CXSourceRange, # `CXSourceRange`
): CXSourceLocation {.
    cdecl,
    importc: "clang_getRangeStart",
    dynlib: libclang
  .}

proc clang_getRangeEnd*(
  crange: CXSourceRange, # `CXSourceRange`
): CXSourceLocation {.
    cdecl,
    importc: "clang_getRangeEnd",
    dynlib: libclang
  .}

type
  CXSourceRangeList* {.pure, bycopy.} = object
    count*: cuint # `unsigned int`
    ranges*: ptr[CXSourceRange] # `CXSourceRange *`

proc clang_getSkippedRanges*(
  tu: CXTranslationUnit, # `CXTranslationUnit`
  file: CXFile, # `CXFile`
): ptr[CXSourceRangeList] {.
    cdecl,
    importc: "clang_getSkippedRanges",
    dynlib: libclang
  .}

proc clang_getAllSkippedRanges*(
  tu: CXTranslationUnit, # `CXTranslationUnit`
): ptr[CXSourceRangeList] {.
    cdecl,
    importc: "clang_getAllSkippedRanges",
    dynlib: libclang
  .}

proc clang_disposeSourceRangeList*(
  ranges: ptr[CXSourceRangeList], # `CXSourceRangeList *`
): void {.
    cdecl,
    importc: "clang_disposeSourceRangeList",
    dynlib: libclang
  .}

type CXDiagnostic* = distinct pointer # CXDiagnostic

type CXDiagnosticSet* = distinct pointer # CXDiagnosticSet

proc clang_getNumDiagnosticsInSet*(
  diags: CXDiagnosticSet, # `CXDiagnosticSet`
): cuint {.
    cdecl,
    importc: "clang_getNumDiagnosticsInSet",
    dynlib: libclang
  .}

proc clang_getDiagnosticInSet*(
  diags: CXDiagnosticSet, # `CXDiagnosticSet`
  index: cuint, # `unsigned int`
): CXDiagnostic {.
    cdecl,
    importc: "clang_getDiagnosticInSet",
    dynlib: libclang
  .}

proc clang_loadDiagnostics*(
  file: ptr[cstring], # `const char *`
  error: ptr[CXLoadDiag_Error], # `enum CXLoadDiag_Error *`
  errorString: ptr[CXString], # `CXString *`
): CXDiagnosticSet {.
    cdecl,
    importc: "clang_loadDiagnostics",
    dynlib: libclang
  .}

proc clang_disposeDiagnosticSet*(
  diags: CXDiagnosticSet, # `CXDiagnosticSet`
): void {.
    cdecl,
    importc: "clang_disposeDiagnosticSet",
    dynlib: libclang
  .}

proc clang_getChildDiagnostics*(
  d: CXDiagnostic, # `CXDiagnostic`
): CXDiagnosticSet {.
    cdecl,
    importc: "clang_getChildDiagnostics",
    dynlib: libclang
  .}

proc clang_getNumDiagnostics*(
  unit: CXTranslationUnit, # `CXTranslationUnit`
): cuint {.
    cdecl,
    importc: "clang_getNumDiagnostics",
    dynlib: libclang
  .}

proc clang_getDiagnostic*(
  unit: CXTranslationUnit, # `CXTranslationUnit`
  index: cuint, # `unsigned int`
): CXDiagnostic {.
    cdecl,
    importc: "clang_getDiagnostic",
    dynlib: libclang
  .}

proc clang_getDiagnosticSetFromTU*(
  unit: CXTranslationUnit, # `CXTranslationUnit`
): CXDiagnosticSet {.
    cdecl,
    importc: "clang_getDiagnosticSetFromTU",
    dynlib: libclang
  .}

proc clang_disposeDiagnostic*(
  diagnostic: CXDiagnostic, # `CXDiagnostic`
): void {.
    cdecl,
    importc: "clang_disposeDiagnostic",
    dynlib: libclang
  .}

proc clang_formatDiagnostic*(
  diagnostic: CXDiagnostic, # `CXDiagnostic`
  options: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_formatDiagnostic",
    dynlib: libclang
  .}

proc clang_defaultDiagnosticDisplayOptions*(): cuint {.
    cdecl,
    importc: "clang_defaultDiagnosticDisplayOptions",
    dynlib: libclang
  .}

proc clang_getDiagnosticSeverity*(
  arg_1: CXDiagnostic, # `CXDiagnostic`
): CXDiagnosticSeverity {.
    cdecl,
    importc: "clang_getDiagnosticSeverity",
    dynlib: libclang
  .}

proc clang_getDiagnosticLocation*(
  arg_1: CXDiagnostic, # `CXDiagnostic`
): CXSourceLocation {.
    cdecl,
    importc: "clang_getDiagnosticLocation",
    dynlib: libclang
  .}

proc clang_getDiagnosticSpelling*(
  arg_1: CXDiagnostic, # `CXDiagnostic`
): CXString {.
    cdecl,
    importc: "clang_getDiagnosticSpelling",
    dynlib: libclang
  .}

proc clang_getDiagnosticOption*(
  diag: CXDiagnostic, # `CXDiagnostic`
  disable: ptr[CXString], # `CXString *`
): CXString {.
    cdecl,
    importc: "clang_getDiagnosticOption",
    dynlib: libclang
  .}

proc clang_getDiagnosticCategory*(
  arg_1: CXDiagnostic, # `CXDiagnostic`
): cuint {.
    cdecl,
    importc: "clang_getDiagnosticCategory",
    dynlib: libclang
  .}

proc clang_getDiagnosticCategoryName*(
  category: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_getDiagnosticCategoryName",
    dynlib: libclang
  .}

proc clang_getDiagnosticCategoryText*(
  arg_1: CXDiagnostic, # `CXDiagnostic`
): CXString {.
    cdecl,
    importc: "clang_getDiagnosticCategoryText",
    dynlib: libclang
  .}

proc clang_getDiagnosticNumRanges*(
  arg_1: CXDiagnostic, # `CXDiagnostic`
): cuint {.
    cdecl,
    importc: "clang_getDiagnosticNumRanges",
    dynlib: libclang
  .}

proc clang_getDiagnosticRange*(
  diagnostic: CXDiagnostic, # `CXDiagnostic`
  range: cuint, # `unsigned int`
): CXSourceRange {.
    cdecl,
    importc: "clang_getDiagnosticRange",
    dynlib: libclang
  .}

proc clang_getDiagnosticNumFixIts*(
  diagnostic: CXDiagnostic, # `CXDiagnostic`
): cuint {.
    cdecl,
    importc: "clang_getDiagnosticNumFixIts",
    dynlib: libclang
  .}

proc clang_getDiagnosticFixIt*(
  diagnostic: CXDiagnostic, # `CXDiagnostic`
  fixIt: cuint, # `unsigned int`
  replacementRange: ptr[CXSourceRange], # `CXSourceRange *`
): CXString {.
    cdecl,
    importc: "clang_getDiagnosticFixIt",
    dynlib: libclang
  .}

proc clang_getTranslationUnitSpelling*(
  cTUnit: CXTranslationUnit, # `CXTranslationUnit`
): CXString {.
    cdecl,
    importc: "clang_getTranslationUnitSpelling",
    dynlib: libclang
  .}

proc clang_createTranslationUnitFromSourceFile*(
  cIdx: CXIndex, # `CXIndex`
  source_filename: ptr[cstring], # `const char *`
  num_clang_command_line_args: cint, # `int`
  clang_command_line_args: ptr[ptr[cstring]], # `const char *const *`
  num_unsaved_files: cuint, # `unsigned int`
  unsaved_files: ptr[CXUnsavedFile], # `struct CXUnsavedFile *`
): CXTranslationUnit {.
    cdecl,
    importc: "clang_createTranslationUnitFromSourceFile",
    dynlib: libclang
  .}

proc clang_createTranslationUnit*(
  cIdx: CXIndex, # `CXIndex`
  ast_filename: ptr[cstring], # `const char *`
): CXTranslationUnit {.
    cdecl,
    importc: "clang_createTranslationUnit",
    dynlib: libclang
  .}

proc clang_createTranslationUnit2*(
  cIdx: CXIndex, # `CXIndex`
  ast_filename: ptr[cstring], # `const char *`
  out_TU: ptr[CXTranslationUnit], # `CXTranslationUnit *`
): CXErrorCode {.
    cdecl,
    importc: "clang_createTranslationUnit2",
    dynlib: libclang
  .}

proc clang_defaultEditingTranslationUnitOptions*(): cuint {.
    cdecl,
    importc: "clang_defaultEditingTranslationUnitOptions",
    dynlib: libclang
  .}

proc clang_parseTranslationUnit*(
  cIdx: CXIndex, # `CXIndex`
  source_filename: ptr[cstring], # `const char *`
  command_line_args: ptr[ptr[cstring]], # `const char *const *`
  num_command_line_args: cint, # `int`
  unsaved_files: ptr[CXUnsavedFile], # `struct CXUnsavedFile *`
  num_unsaved_files: cuint, # `unsigned int`
  options: cuint, # `unsigned int`
): CXTranslationUnit {.
    cdecl,
    importc: "clang_parseTranslationUnit",
    dynlib: libclang
  .}

proc clang_parseTranslationUnit2*(
  cIdx: CXIndex, # `CXIndex`
  source_filename: ptr[cstring], # `const char *`
  command_line_args: ptr[ptr[cstring]], # `const char *const *`
  num_command_line_args: cint, # `int`
  unsaved_files: ptr[CXUnsavedFile], # `struct CXUnsavedFile *`
  num_unsaved_files: cuint, # `unsigned int`
  options: cuint, # `unsigned int`
  out_TU: ptr[CXTranslationUnit], # `CXTranslationUnit *`
): CXErrorCode {.
    cdecl,
    importc: "clang_parseTranslationUnit2",
    dynlib: libclang
  .}

proc clang_parseTranslationUnit2FullArgv*(
  cIdx: CXIndex, # `CXIndex`
  source_filename: ptr[cstring], # `const char *`
  command_line_args: ptr[ptr[cstring]], # `const char *const *`
  num_command_line_args: cint, # `int`
  unsaved_files: ptr[CXUnsavedFile], # `struct CXUnsavedFile *`
  num_unsaved_files: cuint, # `unsigned int`
  options: cuint, # `unsigned int`
  out_TU: ptr[CXTranslationUnit], # `CXTranslationUnit *`
): CXErrorCode {.
    cdecl,
    importc: "clang_parseTranslationUnit2FullArgv",
    dynlib: libclang
  .}

proc clang_defaultSaveOptions*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
): cuint {.
    cdecl,
    importc: "clang_defaultSaveOptions",
    dynlib: libclang
  .}

proc clang_saveTranslationUnit*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
  fileName: ptr[cstring], # `const char *`
  options: cuint, # `unsigned int`
): cint {.
    cdecl,
    importc: "clang_saveTranslationUnit",
    dynlib: libclang
  .}

proc clang_suspendTranslationUnit*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
): cuint {.
    cdecl,
    importc: "clang_suspendTranslationUnit",
    dynlib: libclang
  .}

proc clang_disposeTranslationUnit*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
): void {.
    cdecl,
    importc: "clang_disposeTranslationUnit",
    dynlib: libclang
  .}

proc clang_defaultReparseOptions*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
): cuint {.
    cdecl,
    importc: "clang_defaultReparseOptions",
    dynlib: libclang
  .}

proc clang_reparseTranslationUnit*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
  num_unsaved_files: cuint, # `unsigned int`
  unsaved_files: ptr[CXUnsavedFile], # `struct CXUnsavedFile *`
  options: cuint, # `unsigned int`
): cint {.
    cdecl,
    importc: "clang_reparseTranslationUnit",
    dynlib: libclang
  .}

proc clang_getTUResourceUsageName*(
  kind: CXTUResourceUsageKind, # `enum CXTUResourceUsageKind`
): ptr[cstring] {.
    cdecl,
    importc: "clang_getTUResourceUsageName",
    dynlib: libclang
  .}

type
  CXTUResourceUsageEntry* {.pure, bycopy.} = object
    kind*: CXTUResourceUsageKind # `enum CXTUResourceUsageKind`
    amount*: culong # `unsigned long`

type
  CXTUResourceUsage* {.pure, bycopy.} = object
    data*: pointer # `void *`
    numEntries*: cuint # `unsigned int`
    entries*: ptr[CXTUResourceUsageEntry] # `CXTUResourceUsageEntry *`

proc clang_getCXTUResourceUsage*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
): CXTUResourceUsage {.
    cdecl,
    importc: "clang_getCXTUResourceUsage",
    dynlib: libclang
  .}

proc clang_disposeCXTUResourceUsage*(
  usage: CXTUResourceUsage, # `CXTUResourceUsage`
): void {.
    cdecl,
    importc: "clang_disposeCXTUResourceUsage",
    dynlib: libclang
  .}

proc clang_getTranslationUnitTargetInfo*(
  cTUnit: CXTranslationUnit, # `CXTranslationUnit`
): CXTargetInfo {.
    cdecl,
    importc: "clang_getTranslationUnitTargetInfo",
    dynlib: libclang
  .}

proc clang_TargetInfo_dispose*(
  info: CXTargetInfo, # `CXTargetInfo`
): void {.
    cdecl,
    importc: "clang_TargetInfo_dispose",
    dynlib: libclang
  .}

proc clang_TargetInfo_getTriple*(
  info: CXTargetInfo, # `CXTargetInfo`
): CXString {.
    cdecl,
    importc: "clang_TargetInfo_getTriple",
    dynlib: libclang
  .}

proc clang_TargetInfo_getPointerWidth*(
  info: CXTargetInfo, # `CXTargetInfo`
): cint {.
    cdecl,
    importc: "clang_TargetInfo_getPointerWidth",
    dynlib: libclang
  .}

type
  CXCursor* {.pure, bycopy.} = object
    kind*: CXCursorKind # `enum CXCursorKind`
    xdata*: cint # `int`
    data*: array[3, const void *] # `const void *[3]`

proc clang_getNullCursor*(): CXCursor {.
    cdecl,
    importc: "clang_getNullCursor",
    dynlib: libclang
  .}

proc clang_getTranslationUnitCursor*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
): CXCursor {.
    cdecl,
    importc: "clang_getTranslationUnitCursor",
    dynlib: libclang
  .}

proc clang_equalCursors*(
  arg_1: CXCursor, # `CXCursor`
  arg_2: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_equalCursors",
    dynlib: libclang
  .}

proc clang_Cursor_isNull*(
  cursor: CXCursor, # `CXCursor`
): cint {.
    cdecl,
    importc: "clang_Cursor_isNull",
    dynlib: libclang
  .}

proc clang_hashCursor*(
  arg_1: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_hashCursor",
    dynlib: libclang
  .}

proc clang_getCursorKind*(
  arg_1: CXCursor, # `CXCursor`
): CXCursorKind {.
    cdecl,
    importc: "clang_getCursorKind",
    dynlib: libclang
  .}

proc clang_isDeclaration*(
  arg_1: CXCursorKind, # `enum CXCursorKind`
): cuint {.
    cdecl,
    importc: "clang_isDeclaration",
    dynlib: libclang
  .}

proc clang_isInvalidDeclaration*(
  arg_1: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_isInvalidDeclaration",
    dynlib: libclang
  .}

proc clang_isReference*(
  arg_1: CXCursorKind, # `enum CXCursorKind`
): cuint {.
    cdecl,
    importc: "clang_isReference",
    dynlib: libclang
  .}

proc clang_isExpression*(
  arg_1: CXCursorKind, # `enum CXCursorKind`
): cuint {.
    cdecl,
    importc: "clang_isExpression",
    dynlib: libclang
  .}

proc clang_isStatement*(
  arg_1: CXCursorKind, # `enum CXCursorKind`
): cuint {.
    cdecl,
    importc: "clang_isStatement",
    dynlib: libclang
  .}

proc clang_isAttribute*(
  arg_1: CXCursorKind, # `enum CXCursorKind`
): cuint {.
    cdecl,
    importc: "clang_isAttribute",
    dynlib: libclang
  .}

proc clang_Cursor_hasAttrs*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_hasAttrs",
    dynlib: libclang
  .}

proc clang_isInvalid*(
  arg_1: CXCursorKind, # `enum CXCursorKind`
): cuint {.
    cdecl,
    importc: "clang_isInvalid",
    dynlib: libclang
  .}

proc clang_isTranslationUnit*(
  arg_1: CXCursorKind, # `enum CXCursorKind`
): cuint {.
    cdecl,
    importc: "clang_isTranslationUnit",
    dynlib: libclang
  .}

proc clang_isPreprocessing*(
  arg_1: CXCursorKind, # `enum CXCursorKind`
): cuint {.
    cdecl,
    importc: "clang_isPreprocessing",
    dynlib: libclang
  .}

proc clang_isUnexposed*(
  arg_1: CXCursorKind, # `enum CXCursorKind`
): cuint {.
    cdecl,
    importc: "clang_isUnexposed",
    dynlib: libclang
  .}

proc clang_getCursorLinkage*(
  cursor: CXCursor, # `CXCursor`
): CXLinkageKind {.
    cdecl,
    importc: "clang_getCursorLinkage",
    dynlib: libclang
  .}

proc clang_getCursorVisibility*(
  cursor: CXCursor, # `CXCursor`
): CXVisibilityKind {.
    cdecl,
    importc: "clang_getCursorVisibility",
    dynlib: libclang
  .}

proc clang_getCursorAvailability*(
  cursor: CXCursor, # `CXCursor`
): CXAvailabilityKind {.
    cdecl,
    importc: "clang_getCursorAvailability",
    dynlib: libclang
  .}

type
  CXPlatformAvailability* {.pure, bycopy.} = object
    platform*: CXString # `CXString`
    introduced*: CXVersion # `CXVersion`
    deprecated*: CXVersion # `CXVersion`
    obsoleted*: CXVersion # `CXVersion`
    unavailable*: cint # `int`
    message*: CXString # `CXString`

proc clang_getCursorPlatformAvailability*(
  cursor: CXCursor, # `CXCursor`
  always_deprecated: ptr[cint], # `int *`
  deprecated_message: ptr[CXString], # `CXString *`
  always_unavailable: ptr[cint], # `int *`
  unavailable_message: ptr[CXString], # `CXString *`
  availability: ptr[CXPlatformAvailability], # `CXPlatformAvailability *`
  availability_size: cint, # `int`
): cint {.
    cdecl,
    importc: "clang_getCursorPlatformAvailability",
    dynlib: libclang
  .}

proc clang_disposeCXPlatformAvailability*(
  availability: ptr[CXPlatformAvailability], # `CXPlatformAvailability *`
): void {.
    cdecl,
    importc: "clang_disposeCXPlatformAvailability",
    dynlib: libclang
  .}

proc clang_getCursorLanguage*(
  cursor: CXCursor, # `CXCursor`
): CXLanguageKind {.
    cdecl,
    importc: "clang_getCursorLanguage",
    dynlib: libclang
  .}

proc clang_getCursorTLSKind*(
  cursor: CXCursor, # `CXCursor`
): CXTLSKind {.
    cdecl,
    importc: "clang_getCursorTLSKind",
    dynlib: libclang
  .}

proc clang_Cursor_getTranslationUnit*(
  arg_1: CXCursor, # `CXCursor`
): CXTranslationUnit {.
    cdecl,
    importc: "clang_Cursor_getTranslationUnit",
    dynlib: libclang
  .}

type CXCursorSet* = distinct ptr[CXCursorSetImpl] # CXCursorSet

proc clang_createCXCursorSet*(): CXCursorSet {.
    cdecl,
    importc: "clang_createCXCursorSet",
    dynlib: libclang
  .}

proc clang_disposeCXCursorSet*(
  cset: CXCursorSet, # `CXCursorSet`
): void {.
    cdecl,
    importc: "clang_disposeCXCursorSet",
    dynlib: libclang
  .}

proc clang_CXCursorSet_contains*(
  cset: CXCursorSet, # `CXCursorSet`
  cursor: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXCursorSet_contains",
    dynlib: libclang
  .}

proc clang_CXCursorSet_insert*(
  cset: CXCursorSet, # `CXCursorSet`
  cursor: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXCursorSet_insert",
    dynlib: libclang
  .}

proc clang_getCursorSemanticParent*(
  cursor: CXCursor, # `CXCursor`
): CXCursor {.
    cdecl,
    importc: "clang_getCursorSemanticParent",
    dynlib: libclang
  .}

proc clang_getCursorLexicalParent*(
  cursor: CXCursor, # `CXCursor`
): CXCursor {.
    cdecl,
    importc: "clang_getCursorLexicalParent",
    dynlib: libclang
  .}

proc clang_getOverriddenCursors*(
  cursor: CXCursor, # `CXCursor`
  overridden: ptr[ptr[CXCursor]], # `CXCursor **`
  num_overridden: ptr[cuint], # `unsigned int *`
): void {.
    cdecl,
    importc: "clang_getOverriddenCursors",
    dynlib: libclang
  .}

proc clang_disposeOverriddenCursors*(
  overridden: ptr[CXCursor], # `CXCursor *`
): void {.
    cdecl,
    importc: "clang_disposeOverriddenCursors",
    dynlib: libclang
  .}

proc clang_getIncludedFile*(
  cursor: CXCursor, # `CXCursor`
): CXFile {.
    cdecl,
    importc: "clang_getIncludedFile",
    dynlib: libclang
  .}

proc clang_getCursor*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
  arg_2: CXSourceLocation, # `CXSourceLocation`
): CXCursor {.
    cdecl,
    importc: "clang_getCursor",
    dynlib: libclang
  .}

proc clang_getCursorLocation*(
  arg_1: CXCursor, # `CXCursor`
): CXSourceLocation {.
    cdecl,
    importc: "clang_getCursorLocation",
    dynlib: libclang
  .}

proc clang_getCursorExtent*(
  arg_1: CXCursor, # `CXCursor`
): CXSourceRange {.
    cdecl,
    importc: "clang_getCursorExtent",
    dynlib: libclang
  .}

type
  CXType* {.pure, bycopy.} = object
    kind*: CXTypeKind # `enum CXTypeKind`
    data*: array[2, void *] # `void *[2]`

proc clang_getCursorType*(
  c: CXCursor, # `CXCursor`
): CXType {.
    cdecl,
    importc: "clang_getCursorType",
    dynlib: libclang
  .}

proc clang_getTypeSpelling*(
  cT: CXType, # `CXType`
): CXString {.
    cdecl,
    importc: "clang_getTypeSpelling",
    dynlib: libclang
  .}

proc clang_getTypedefDeclUnderlyingType*(
  c: CXCursor, # `CXCursor`
): CXType {.
    cdecl,
    importc: "clang_getTypedefDeclUnderlyingType",
    dynlib: libclang
  .}

proc clang_getEnumDeclIntegerType*(
  c: CXCursor, # `CXCursor`
): CXType {.
    cdecl,
    importc: "clang_getEnumDeclIntegerType",
    dynlib: libclang
  .}

proc clang_getEnumConstantDeclValue*(
  c: CXCursor, # `CXCursor`
): clonglong {.
    cdecl,
    importc: "clang_getEnumConstantDeclValue",
    dynlib: libclang
  .}

proc clang_getEnumConstantDeclUnsignedValue*(
  c: CXCursor, # `CXCursor`
): culonglong {.
    cdecl,
    importc: "clang_getEnumConstantDeclUnsignedValue",
    dynlib: libclang
  .}

proc clang_getFieldDeclBitWidth*(
  c: CXCursor, # `CXCursor`
): cint {.
    cdecl,
    importc: "clang_getFieldDeclBitWidth",
    dynlib: libclang
  .}

proc clang_Cursor_getNumArguments*(
  c: CXCursor, # `CXCursor`
): cint {.
    cdecl,
    importc: "clang_Cursor_getNumArguments",
    dynlib: libclang
  .}

proc clang_Cursor_getArgument*(
  c: CXCursor, # `CXCursor`
  i: cuint, # `unsigned int`
): CXCursor {.
    cdecl,
    importc: "clang_Cursor_getArgument",
    dynlib: libclang
  .}

proc clang_Cursor_getNumTemplateArguments*(
  c: CXCursor, # `CXCursor`
): cint {.
    cdecl,
    importc: "clang_Cursor_getNumTemplateArguments",
    dynlib: libclang
  .}

proc clang_Cursor_getTemplateArgumentKind*(
  c: CXCursor, # `CXCursor`
  i: cuint, # `unsigned int`
): CXTemplateArgumentKind {.
    cdecl,
    importc: "clang_Cursor_getTemplateArgumentKind",
    dynlib: libclang
  .}

proc clang_Cursor_getTemplateArgumentType*(
  c: CXCursor, # `CXCursor`
  i: cuint, # `unsigned int`
): CXType {.
    cdecl,
    importc: "clang_Cursor_getTemplateArgumentType",
    dynlib: libclang
  .}

proc clang_Cursor_getTemplateArgumentValue*(
  c: CXCursor, # `CXCursor`
  i: cuint, # `unsigned int`
): clonglong {.
    cdecl,
    importc: "clang_Cursor_getTemplateArgumentValue",
    dynlib: libclang
  .}

proc clang_Cursor_getTemplateArgumentUnsignedValue*(
  c: CXCursor, # `CXCursor`
  i: cuint, # `unsigned int`
): culonglong {.
    cdecl,
    importc: "clang_Cursor_getTemplateArgumentUnsignedValue",
    dynlib: libclang
  .}

proc clang_equalTypes*(
  a: CXType, # `CXType`
  b: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_equalTypes",
    dynlib: libclang
  .}

proc clang_getCanonicalType*(
  t: CXType, # `CXType`
): CXType {.
    cdecl,
    importc: "clang_getCanonicalType",
    dynlib: libclang
  .}

proc clang_isConstQualifiedType*(
  t: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_isConstQualifiedType",
    dynlib: libclang
  .}

proc clang_Cursor_isMacroFunctionLike*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isMacroFunctionLike",
    dynlib: libclang
  .}

proc clang_Cursor_isMacroBuiltin*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isMacroBuiltin",
    dynlib: libclang
  .}

proc clang_Cursor_isFunctionInlined*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isFunctionInlined",
    dynlib: libclang
  .}

proc clang_isVolatileQualifiedType*(
  t: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_isVolatileQualifiedType",
    dynlib: libclang
  .}

proc clang_isRestrictQualifiedType*(
  t: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_isRestrictQualifiedType",
    dynlib: libclang
  .}

proc clang_getAddressSpace*(
  t: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_getAddressSpace",
    dynlib: libclang
  .}

proc clang_getTypedefName*(
  cT: CXType, # `CXType`
): CXString {.
    cdecl,
    importc: "clang_getTypedefName",
    dynlib: libclang
  .}

proc clang_getPointeeType*(
  t: CXType, # `CXType`
): CXType {.
    cdecl,
    importc: "clang_getPointeeType",
    dynlib: libclang
  .}

proc clang_getTypeDeclaration*(
  t: CXType, # `CXType`
): CXCursor {.
    cdecl,
    importc: "clang_getTypeDeclaration",
    dynlib: libclang
  .}

proc clang_getDeclObjCTypeEncoding*(
  c: CXCursor, # `CXCursor`
): CXString {.
    cdecl,
    importc: "clang_getDeclObjCTypeEncoding",
    dynlib: libclang
  .}

proc clang_Type_getObjCEncoding*(
  type: CXType, # `CXType`
): CXString {.
    cdecl,
    importc: "clang_Type_getObjCEncoding",
    dynlib: libclang
  .}

proc clang_getTypeKindSpelling*(
  k: CXTypeKind, # `enum CXTypeKind`
): CXString {.
    cdecl,
    importc: "clang_getTypeKindSpelling",
    dynlib: libclang
  .}

proc clang_getFunctionTypeCallingConv*(
  t: CXType, # `CXType`
): CXCallingConv {.
    cdecl,
    importc: "clang_getFunctionTypeCallingConv",
    dynlib: libclang
  .}

proc clang_getResultType*(
  t: CXType, # `CXType`
): CXType {.
    cdecl,
    importc: "clang_getResultType",
    dynlib: libclang
  .}

proc clang_getExceptionSpecificationType*(
  t: CXType, # `CXType`
): cint {.
    cdecl,
    importc: "clang_getExceptionSpecificationType",
    dynlib: libclang
  .}

proc clang_getNumArgTypes*(
  t: CXType, # `CXType`
): cint {.
    cdecl,
    importc: "clang_getNumArgTypes",
    dynlib: libclang
  .}

proc clang_getArgType*(
  t: CXType, # `CXType`
  i: cuint, # `unsigned int`
): CXType {.
    cdecl,
    importc: "clang_getArgType",
    dynlib: libclang
  .}

proc clang_Type_getObjCObjectBaseType*(
  t: CXType, # `CXType`
): CXType {.
    cdecl,
    importc: "clang_Type_getObjCObjectBaseType",
    dynlib: libclang
  .}

proc clang_Type_getNumObjCProtocolRefs*(
  t: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_Type_getNumObjCProtocolRefs",
    dynlib: libclang
  .}

proc clang_Type_getObjCProtocolDecl*(
  t: CXType, # `CXType`
  i: cuint, # `unsigned int`
): CXCursor {.
    cdecl,
    importc: "clang_Type_getObjCProtocolDecl",
    dynlib: libclang
  .}

proc clang_Type_getNumObjCTypeArgs*(
  t: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_Type_getNumObjCTypeArgs",
    dynlib: libclang
  .}

proc clang_Type_getObjCTypeArg*(
  t: CXType, # `CXType`
  i: cuint, # `unsigned int`
): CXType {.
    cdecl,
    importc: "clang_Type_getObjCTypeArg",
    dynlib: libclang
  .}

proc clang_isFunctionTypeVariadic*(
  t: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_isFunctionTypeVariadic",
    dynlib: libclang
  .}

proc clang_getCursorResultType*(
  c: CXCursor, # `CXCursor`
): CXType {.
    cdecl,
    importc: "clang_getCursorResultType",
    dynlib: libclang
  .}

proc clang_getCursorExceptionSpecificationType*(
  c: CXCursor, # `CXCursor`
): cint {.
    cdecl,
    importc: "clang_getCursorExceptionSpecificationType",
    dynlib: libclang
  .}

proc clang_isPODType*(
  t: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_isPODType",
    dynlib: libclang
  .}

proc clang_getElementType*(
  t: CXType, # `CXType`
): CXType {.
    cdecl,
    importc: "clang_getElementType",
    dynlib: libclang
  .}

proc clang_getNumElements*(
  t: CXType, # `CXType`
): clonglong {.
    cdecl,
    importc: "clang_getNumElements",
    dynlib: libclang
  .}

proc clang_getArrayElementType*(
  t: CXType, # `CXType`
): CXType {.
    cdecl,
    importc: "clang_getArrayElementType",
    dynlib: libclang
  .}

proc clang_getArraySize*(
  t: CXType, # `CXType`
): clonglong {.
    cdecl,
    importc: "clang_getArraySize",
    dynlib: libclang
  .}

proc clang_Type_getNamedType*(
  t: CXType, # `CXType`
): CXType {.
    cdecl,
    importc: "clang_Type_getNamedType",
    dynlib: libclang
  .}

proc clang_Type_isTransparentTagTypedef*(
  t: CXType, # `CXType`
): cuint {.
    cdecl,
    importc: "clang_Type_isTransparentTagTypedef",
    dynlib: libclang
  .}

proc clang_Type_getNullability*(
  t: CXType, # `CXType`
): CXTypeNullabilityKind {.
    cdecl,
    importc: "clang_Type_getNullability",
    dynlib: libclang
  .}

proc clang_Type_getAlignOf*(
  t: CXType, # `CXType`
): clonglong {.
    cdecl,
    importc: "clang_Type_getAlignOf",
    dynlib: libclang
  .}

proc clang_Type_getClassType*(
  t: CXType, # `CXType`
): CXType {.
    cdecl,
    importc: "clang_Type_getClassType",
    dynlib: libclang
  .}

proc clang_Type_getSizeOf*(
  t: CXType, # `CXType`
): clonglong {.
    cdecl,
    importc: "clang_Type_getSizeOf",
    dynlib: libclang
  .}

proc clang_Type_getOffsetOf*(
  t: CXType, # `CXType`
  s: ptr[cstring], # `const char *`
): clonglong {.
    cdecl,
    importc: "clang_Type_getOffsetOf",
    dynlib: libclang
  .}

proc clang_Type_getModifiedType*(
  t: CXType, # `CXType`
): CXType {.
    cdecl,
    importc: "clang_Type_getModifiedType",
    dynlib: libclang
  .}

proc clang_Cursor_getOffsetOfField*(
  c: CXCursor, # `CXCursor`
): clonglong {.
    cdecl,
    importc: "clang_Cursor_getOffsetOfField",
    dynlib: libclang
  .}

proc clang_Cursor_isAnonymous*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isAnonymous",
    dynlib: libclang
  .}

proc clang_Cursor_isAnonymousRecordDecl*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isAnonymousRecordDecl",
    dynlib: libclang
  .}

proc clang_Cursor_isInlineNamespace*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isInlineNamespace",
    dynlib: libclang
  .}

proc clang_Type_getNumTemplateArguments*(
  t: CXType, # `CXType`
): cint {.
    cdecl,
    importc: "clang_Type_getNumTemplateArguments",
    dynlib: libclang
  .}

proc clang_Type_getTemplateArgumentAsType*(
  t: CXType, # `CXType`
  i: cuint, # `unsigned int`
): CXType {.
    cdecl,
    importc: "clang_Type_getTemplateArgumentAsType",
    dynlib: libclang
  .}

proc clang_Type_getCXXRefQualifier*(
  t: CXType, # `CXType`
): CXRefQualifierKind {.
    cdecl,
    importc: "clang_Type_getCXXRefQualifier",
    dynlib: libclang
  .}

proc clang_Cursor_isBitField*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isBitField",
    dynlib: libclang
  .}

proc clang_isVirtualBase*(
  arg_1: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_isVirtualBase",
    dynlib: libclang
  .}

proc clang_getCXXAccessSpecifier*(
  arg_1: CXCursor, # `CXCursor`
): CX_CXXAccessSpecifier {.
    cdecl,
    importc: "clang_getCXXAccessSpecifier",
    dynlib: libclang
  .}

proc clang_Cursor_getStorageClass*(
  arg_1: CXCursor, # `CXCursor`
): CX_StorageClass {.
    cdecl,
    importc: "clang_Cursor_getStorageClass",
    dynlib: libclang
  .}

proc clang_getNumOverloadedDecls*(
  cursor: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_getNumOverloadedDecls",
    dynlib: libclang
  .}

proc clang_getOverloadedDecl*(
  cursor: CXCursor, # `CXCursor`
  index: cuint, # `unsigned int`
): CXCursor {.
    cdecl,
    importc: "clang_getOverloadedDecl",
    dynlib: libclang
  .}

proc clang_getIBOutletCollectionType*(
  arg_1: CXCursor, # `CXCursor`
): CXType {.
    cdecl,
    importc: "clang_getIBOutletCollectionType",
    dynlib: libclang
  .}

type CXCursorVisitor* = distinct proc(a0: CXCursor, a1: CXCursor, a2: pointer): enum CXChildVisitResult {.cdecl.} # CXCursorVisitor

proc clang_visitChildren*(
  parent: CXCursor, # `CXCursor`
  visitor: CXCursorVisitor, # `CXCursorVisitor`
  client_data: CXClientData, # `CXClientData`
): cuint {.
    cdecl,
    importc: "clang_visitChildren",
    dynlib: libclang
  .}

proc clang_getCursorUSR*(
  arg_1: CXCursor, # `CXCursor`
): CXString {.
    cdecl,
    importc: "clang_getCursorUSR",
    dynlib: libclang
  .}

proc clang_constructUSR_ObjCClass*(
  class_name: ptr[cstring], # `const char *`
): CXString {.
    cdecl,
    importc: "clang_constructUSR_ObjCClass",
    dynlib: libclang
  .}

proc clang_constructUSR_ObjCCategory*(
  class_name: ptr[cstring], # `const char *`
  category_name: ptr[cstring], # `const char *`
): CXString {.
    cdecl,
    importc: "clang_constructUSR_ObjCCategory",
    dynlib: libclang
  .}

proc clang_constructUSR_ObjCProtocol*(
  protocol_name: ptr[cstring], # `const char *`
): CXString {.
    cdecl,
    importc: "clang_constructUSR_ObjCProtocol",
    dynlib: libclang
  .}

proc clang_constructUSR_ObjCIvar*(
  name: ptr[cstring], # `const char *`
  classUSR: CXString, # `CXString`
): CXString {.
    cdecl,
    importc: "clang_constructUSR_ObjCIvar",
    dynlib: libclang
  .}

proc clang_constructUSR_ObjCMethod*(
  name: ptr[cstring], # `const char *`
  isInstanceMethod: cuint, # `unsigned int`
  classUSR: CXString, # `CXString`
): CXString {.
    cdecl,
    importc: "clang_constructUSR_ObjCMethod",
    dynlib: libclang
  .}

proc clang_constructUSR_ObjCProperty*(
  property: ptr[cstring], # `const char *`
  classUSR: CXString, # `CXString`
): CXString {.
    cdecl,
    importc: "clang_constructUSR_ObjCProperty",
    dynlib: libclang
  .}

proc clang_getCursorSpelling*(
  arg_1: CXCursor, # `CXCursor`
): CXString {.
    cdecl,
    importc: "clang_getCursorSpelling",
    dynlib: libclang
  .}

proc clang_Cursor_getSpellingNameRange*(
  arg_1: CXCursor, # `CXCursor`
  pieceIndex: cuint, # `unsigned int`
  options: cuint, # `unsigned int`
): CXSourceRange {.
    cdecl,
    importc: "clang_Cursor_getSpellingNameRange",
    dynlib: libclang
  .}

type CXPrintingPolicy* = distinct pointer # CXPrintingPolicy

proc clang_PrintingPolicy_getProperty*(
  policy: CXPrintingPolicy, # `CXPrintingPolicy`
  property: CXPrintingPolicyProperty, # `enum CXPrintingPolicyProperty`
): cuint {.
    cdecl,
    importc: "clang_PrintingPolicy_getProperty",
    dynlib: libclang
  .}

proc clang_PrintingPolicy_setProperty*(
  policy: CXPrintingPolicy, # `CXPrintingPolicy`
  property: CXPrintingPolicyProperty, # `enum CXPrintingPolicyProperty`
  value: cuint, # `unsigned int`
): void {.
    cdecl,
    importc: "clang_PrintingPolicy_setProperty",
    dynlib: libclang
  .}

proc clang_getCursorPrintingPolicy*(
  arg_1: CXCursor, # `CXCursor`
): CXPrintingPolicy {.
    cdecl,
    importc: "clang_getCursorPrintingPolicy",
    dynlib: libclang
  .}

proc clang_PrintingPolicy_dispose*(
  policy: CXPrintingPolicy, # `CXPrintingPolicy`
): void {.
    cdecl,
    importc: "clang_PrintingPolicy_dispose",
    dynlib: libclang
  .}

proc clang_getCursorPrettyPrinted*(
  cursor: CXCursor, # `CXCursor`
  policy: CXPrintingPolicy, # `CXPrintingPolicy`
): CXString {.
    cdecl,
    importc: "clang_getCursorPrettyPrinted",
    dynlib: libclang
  .}

proc clang_getCursorDisplayName*(
  arg_1: CXCursor, # `CXCursor`
): CXString {.
    cdecl,
    importc: "clang_getCursorDisplayName",
    dynlib: libclang
  .}

proc clang_getCursorReferenced*(
  arg_1: CXCursor, # `CXCursor`
): CXCursor {.
    cdecl,
    importc: "clang_getCursorReferenced",
    dynlib: libclang
  .}

proc clang_getCursorDefinition*(
  arg_1: CXCursor, # `CXCursor`
): CXCursor {.
    cdecl,
    importc: "clang_getCursorDefinition",
    dynlib: libclang
  .}

proc clang_isCursorDefinition*(
  arg_1: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_isCursorDefinition",
    dynlib: libclang
  .}

proc clang_getCanonicalCursor*(
  arg_1: CXCursor, # `CXCursor`
): CXCursor {.
    cdecl,
    importc: "clang_getCanonicalCursor",
    dynlib: libclang
  .}

proc clang_Cursor_getObjCSelectorIndex*(
  arg_1: CXCursor, # `CXCursor`
): cint {.
    cdecl,
    importc: "clang_Cursor_getObjCSelectorIndex",
    dynlib: libclang
  .}

proc clang_Cursor_isDynamicCall*(
  c: CXCursor, # `CXCursor`
): cint {.
    cdecl,
    importc: "clang_Cursor_isDynamicCall",
    dynlib: libclang
  .}

proc clang_Cursor_getReceiverType*(
  c: CXCursor, # `CXCursor`
): CXType {.
    cdecl,
    importc: "clang_Cursor_getReceiverType",
    dynlib: libclang
  .}

type
  CXObjCPropertyAttrKind* {.pure, size: sizeof(cint).} = enum
    CXObjCPropertyAttr_noattr = 0
    CXObjCPropertyAttr_readonly = 1
    CXObjCPropertyAttr_getter = 2
    CXObjCPropertyAttr_assign = 4
    CXObjCPropertyAttr_readwrite = 8
    CXObjCPropertyAttr_retain = 16
    CXObjCPropertyAttr_copy = 32
    CXObjCPropertyAttr_nonatomic = 64
    CXObjCPropertyAttr_setter = 128
    CXObjCPropertyAttr_atomic = 256
    CXObjCPropertyAttr_weak = 512
    CXObjCPropertyAttr_strong = 1024
    CXObjCPropertyAttr_unsafe_unretained = 2048
    CXObjCPropertyAttr_class = 4096

proc clang_Cursor_getObjCPropertyAttributes*(
  c: CXCursor, # `CXCursor`
  reserved: cuint, # `unsigned int`
): cuint {.
    cdecl,
    importc: "clang_Cursor_getObjCPropertyAttributes",
    dynlib: libclang
  .}

proc clang_Cursor_getObjCPropertyGetterName*(
  c: CXCursor, # `CXCursor`
): CXString {.
    cdecl,
    importc: "clang_Cursor_getObjCPropertyGetterName",
    dynlib: libclang
  .}

proc clang_Cursor_getObjCPropertySetterName*(
  c: CXCursor, # `CXCursor`
): CXString {.
    cdecl,
    importc: "clang_Cursor_getObjCPropertySetterName",
    dynlib: libclang
  .}

type
  CXObjCDeclQualifierKind* {.pure, size: sizeof(cint).} = enum
    CXObjCDeclQualifier_None = 0
    CXObjCDeclQualifier_In = 1
    CXObjCDeclQualifier_Inout = 2
    CXObjCDeclQualifier_Out = 4
    CXObjCDeclQualifier_Bycopy = 8
    CXObjCDeclQualifier_Byref = 16
    CXObjCDeclQualifier_Oneway = 32

proc clang_Cursor_getObjCDeclQualifiers*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_getObjCDeclQualifiers",
    dynlib: libclang
  .}

proc clang_Cursor_isObjCOptional*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isObjCOptional",
    dynlib: libclang
  .}

proc clang_Cursor_isVariadic*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isVariadic",
    dynlib: libclang
  .}

proc clang_Cursor_isExternalSymbol*(
  c: CXCursor, # `CXCursor`
  language: ptr[CXString], # `CXString *`
  definedIn: ptr[CXString], # `CXString *`
  isGenerated: ptr[cuint], # `unsigned int *`
): cuint {.
    cdecl,
    importc: "clang_Cursor_isExternalSymbol",
    dynlib: libclang
  .}

proc clang_Cursor_getCommentRange*(
  c: CXCursor, # `CXCursor`
): CXSourceRange {.
    cdecl,
    importc: "clang_Cursor_getCommentRange",
    dynlib: libclang
  .}

proc clang_Cursor_getRawCommentText*(
  c: CXCursor, # `CXCursor`
): CXString {.
    cdecl,
    importc: "clang_Cursor_getRawCommentText",
    dynlib: libclang
  .}

proc clang_Cursor_getBriefCommentText*(
  c: CXCursor, # `CXCursor`
): CXString {.
    cdecl,
    importc: "clang_Cursor_getBriefCommentText",
    dynlib: libclang
  .}

proc clang_Cursor_getMangling*(
  arg_1: CXCursor, # `CXCursor`
): CXString {.
    cdecl,
    importc: "clang_Cursor_getMangling",
    dynlib: libclang
  .}

proc clang_Cursor_getCXXManglings*(
  arg_1: CXCursor, # `CXCursor`
): ptr[CXStringSet] {.
    cdecl,
    importc: "clang_Cursor_getCXXManglings",
    dynlib: libclang
  .}

proc clang_Cursor_getObjCManglings*(
  arg_1: CXCursor, # `CXCursor`
): ptr[CXStringSet] {.
    cdecl,
    importc: "clang_Cursor_getObjCManglings",
    dynlib: libclang
  .}

type CXModule* = distinct pointer # CXModule

proc clang_Cursor_getModule*(
  c: CXCursor, # `CXCursor`
): CXModule {.
    cdecl,
    importc: "clang_Cursor_getModule",
    dynlib: libclang
  .}

proc clang_getModuleForFile*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
  arg_2: CXFile, # `CXFile`
): CXModule {.
    cdecl,
    importc: "clang_getModuleForFile",
    dynlib: libclang
  .}

proc clang_Module_getASTFile*(
  module: CXModule, # `CXModule`
): CXFile {.
    cdecl,
    importc: "clang_Module_getASTFile",
    dynlib: libclang
  .}

proc clang_Module_getParent*(
  module: CXModule, # `CXModule`
): CXModule {.
    cdecl,
    importc: "clang_Module_getParent",
    dynlib: libclang
  .}

proc clang_Module_getName*(
  module: CXModule, # `CXModule`
): CXString {.
    cdecl,
    importc: "clang_Module_getName",
    dynlib: libclang
  .}

proc clang_Module_getFullName*(
  module: CXModule, # `CXModule`
): CXString {.
    cdecl,
    importc: "clang_Module_getFullName",
    dynlib: libclang
  .}

proc clang_Module_isSystem*(
  module: CXModule, # `CXModule`
): cint {.
    cdecl,
    importc: "clang_Module_isSystem",
    dynlib: libclang
  .}

proc clang_Module_getNumTopLevelHeaders*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
  module: CXModule, # `CXModule`
): cuint {.
    cdecl,
    importc: "clang_Module_getNumTopLevelHeaders",
    dynlib: libclang
  .}

proc clang_Module_getTopLevelHeader*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
  module: CXModule, # `CXModule`
  index: cuint, # `unsigned int`
): CXFile {.
    cdecl,
    importc: "clang_Module_getTopLevelHeader",
    dynlib: libclang
  .}

proc clang_CXXConstructor_isConvertingConstructor*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXConstructor_isConvertingConstructor",
    dynlib: libclang
  .}

proc clang_CXXConstructor_isCopyConstructor*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXConstructor_isCopyConstructor",
    dynlib: libclang
  .}

proc clang_CXXConstructor_isDefaultConstructor*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXConstructor_isDefaultConstructor",
    dynlib: libclang
  .}

proc clang_CXXConstructor_isMoveConstructor*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXConstructor_isMoveConstructor",
    dynlib: libclang
  .}

proc clang_CXXField_isMutable*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXField_isMutable",
    dynlib: libclang
  .}

proc clang_CXXMethod_isDefaulted*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXMethod_isDefaulted",
    dynlib: libclang
  .}

proc clang_CXXMethod_isPureVirtual*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXMethod_isPureVirtual",
    dynlib: libclang
  .}

proc clang_CXXMethod_isStatic*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXMethod_isStatic",
    dynlib: libclang
  .}

proc clang_CXXMethod_isVirtual*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXMethod_isVirtual",
    dynlib: libclang
  .}

proc clang_CXXRecord_isAbstract*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXRecord_isAbstract",
    dynlib: libclang
  .}

proc clang_EnumDecl_isScoped*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_EnumDecl_isScoped",
    dynlib: libclang
  .}

proc clang_CXXMethod_isConst*(
  c: CXCursor, # `CXCursor`
): cuint {.
    cdecl,
    importc: "clang_CXXMethod_isConst",
    dynlib: libclang
  .}

proc clang_getTemplateCursorKind*(
  c: CXCursor, # `CXCursor`
): CXCursorKind {.
    cdecl,
    importc: "clang_getTemplateCursorKind",
    dynlib: libclang
  .}

proc clang_getSpecializedCursorTemplate*(
  c: CXCursor, # `CXCursor`
): CXCursor {.
    cdecl,
    importc: "clang_getSpecializedCursorTemplate",
    dynlib: libclang
  .}

proc clang_getCursorReferenceNameRange*(
  c: CXCursor, # `CXCursor`
  nameFlags: cuint, # `unsigned int`
  pieceIndex: cuint, # `unsigned int`
): CXSourceRange {.
    cdecl,
    importc: "clang_getCursorReferenceNameRange",
    dynlib: libclang
  .}

type
  CXTokenKind* {.pure, size: sizeof(cint).} = enum
    CXToken_Punctuation
    CXToken_Keyword
    CXToken_Identifier
    CXToken_Literal
    CXToken_Comment

type
  CXToken* {.pure, bycopy.} = object
    int_data*: array[4, unsigned int] # `unsigned int [4]`
    ptr_data*: pointer # `void *`

proc clang_getToken*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
  location: CXSourceLocation, # `CXSourceLocation`
): ptr[CXToken] {.
    cdecl,
    importc: "clang_getToken",
    dynlib: libclang
  .}

proc clang_getTokenKind*(
  arg_1: CXToken, # `CXToken`
): CXTokenKind {.
    cdecl,
    importc: "clang_getTokenKind",
    dynlib: libclang
  .}

proc clang_getTokenSpelling*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
  arg_2: CXToken, # `CXToken`
): CXString {.
    cdecl,
    importc: "clang_getTokenSpelling",
    dynlib: libclang
  .}

proc clang_getTokenLocation*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
  arg_2: CXToken, # `CXToken`
): CXSourceLocation {.
    cdecl,
    importc: "clang_getTokenLocation",
    dynlib: libclang
  .}

proc clang_getTokenExtent*(
  arg_1: CXTranslationUnit, # `CXTranslationUnit`
  arg_2: CXToken, # `CXToken`
): CXSourceRange {.
    cdecl,
    importc: "clang_getTokenExtent",
    dynlib: libclang
  .}

proc clang_tokenize*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
  range: CXSourceRange, # `CXSourceRange`
  tokens: ptr[ptr[CXToken]], # `CXToken **`
  numTokens: ptr[cuint], # `unsigned int *`
): void {.
    cdecl,
    importc: "clang_tokenize",
    dynlib: libclang
  .}

proc clang_annotateTokens*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
  tokens: ptr[CXToken], # `CXToken *`
  numTokens: cuint, # `unsigned int`
  cursors: ptr[CXCursor], # `CXCursor *`
): void {.
    cdecl,
    importc: "clang_annotateTokens",
    dynlib: libclang
  .}

proc clang_disposeTokens*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
  tokens: ptr[CXToken], # `CXToken *`
  numTokens: cuint, # `unsigned int`
): void {.
    cdecl,
    importc: "clang_disposeTokens",
    dynlib: libclang
  .}

proc clang_getCursorKindSpelling*(
  kind: CXCursorKind, # `enum CXCursorKind`
): CXString {.
    cdecl,
    importc: "clang_getCursorKindSpelling",
    dynlib: libclang
  .}

proc clang_getDefinitionSpellingAndExtent*(
  arg_1: CXCursor, # `CXCursor`
  startBuf: ptr[ptr[cstring]], # `const char **`
  endBuf: ptr[ptr[cstring]], # `const char **`
  startLine: ptr[cuint], # `unsigned int *`
  startColumn: ptr[cuint], # `unsigned int *`
  endLine: ptr[cuint], # `unsigned int *`
  endColumn: ptr[cuint], # `unsigned int *`
): void {.
    cdecl,
    importc: "clang_getDefinitionSpellingAndExtent",
    dynlib: libclang
  .}

proc clang_enableStackTraces*(): void {.
    cdecl,
    importc: "clang_enableStackTraces",
    dynlib: libclang
  .}

proc clang_executeOnThread*(
  fn: proc(a0: pointer): void {.cdecl.}, # `void (*)(void *)`
  user_data: pointer, # `void *`
  stack_size: cuint, # `unsigned int`
): void {.
    cdecl,
    importc: "clang_executeOnThread",
    dynlib: libclang
  .}

type CXCompletionString* = distinct pointer # CXCompletionString

type
  CXCompletionResult* {.pure, bycopy.} = object
    cursorKind*: CXCursorKind # `enum CXCursorKind`
    completionString*: CXCompletionString # `CXCompletionString`

proc clang_getCompletionChunkKind*(
  completion_string: CXCompletionString, # `CXCompletionString`
  chunk_number: cuint, # `unsigned int`
): CXCompletionChunkKind {.
    cdecl,
    importc: "clang_getCompletionChunkKind",
    dynlib: libclang
  .}

proc clang_getCompletionChunkText*(
  completion_string: CXCompletionString, # `CXCompletionString`
  chunk_number: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_getCompletionChunkText",
    dynlib: libclang
  .}

proc clang_getCompletionChunkCompletionString*(
  completion_string: CXCompletionString, # `CXCompletionString`
  chunk_number: cuint, # `unsigned int`
): CXCompletionString {.
    cdecl,
    importc: "clang_getCompletionChunkCompletionString",
    dynlib: libclang
  .}

proc clang_getNumCompletionChunks*(
  completion_string: CXCompletionString, # `CXCompletionString`
): cuint {.
    cdecl,
    importc: "clang_getNumCompletionChunks",
    dynlib: libclang
  .}

proc clang_getCompletionPriority*(
  completion_string: CXCompletionString, # `CXCompletionString`
): cuint {.
    cdecl,
    importc: "clang_getCompletionPriority",
    dynlib: libclang
  .}

proc clang_getCompletionAvailability*(
  completion_string: CXCompletionString, # `CXCompletionString`
): CXAvailabilityKind {.
    cdecl,
    importc: "clang_getCompletionAvailability",
    dynlib: libclang
  .}

proc clang_getCompletionNumAnnotations*(
  completion_string: CXCompletionString, # `CXCompletionString`
): cuint {.
    cdecl,
    importc: "clang_getCompletionNumAnnotations",
    dynlib: libclang
  .}

proc clang_getCompletionAnnotation*(
  completion_string: CXCompletionString, # `CXCompletionString`
  annotation_number: cuint, # `unsigned int`
): CXString {.
    cdecl,
    importc: "clang_getCompletionAnnotation",
    dynlib: libclang
  .}

proc clang_getCompletionParent*(
  completion_string: CXCompletionString, # `CXCompletionString`
  kind: ptr[CXCursorKind], # `enum CXCursorKind *`
): CXString {.
    cdecl,
    importc: "clang_getCompletionParent",
    dynlib: libclang
  .}

proc clang_getCompletionBriefComment*(
  completion_string: CXCompletionString, # `CXCompletionString`
): CXString {.
    cdecl,
    importc: "clang_getCompletionBriefComment",
    dynlib: libclang
  .}

proc clang_getCursorCompletionString*(
  cursor: CXCursor, # `CXCursor`
): CXCompletionString {.
    cdecl,
    importc: "clang_getCursorCompletionString",
    dynlib: libclang
  .}

type
  CXCodeCompleteResults* {.pure, bycopy.} = object
    results*: ptr[CXCompletionResult] # `CXCompletionResult *`
    numResults*: cuint # `unsigned int`

proc clang_getCompletionNumFixIts*(
  results: ptr[CXCodeCompleteResults], # `CXCodeCompleteResults *`
  completion_index: cuint, # `unsigned int`
): cuint {.
    cdecl,
    importc: "clang_getCompletionNumFixIts",
    dynlib: libclang
  .}

proc clang_getCompletionFixIt*(
  results: ptr[CXCodeCompleteResults], # `CXCodeCompleteResults *`
  completion_index: cuint, # `unsigned int`
  fixit_index: cuint, # `unsigned int`
  replacement_range: ptr[CXSourceRange], # `CXSourceRange *`
): CXString {.
    cdecl,
    importc: "clang_getCompletionFixIt",
    dynlib: libclang
  .}

proc clang_defaultCodeCompleteOptions*(): cuint {.
    cdecl,
    importc: "clang_defaultCodeCompleteOptions",
    dynlib: libclang
  .}

proc clang_codeCompleteAt*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
  complete_filename: ptr[cstring], # `const char *`
  complete_line: cuint, # `unsigned int`
  complete_column: cuint, # `unsigned int`
  unsaved_files: ptr[CXUnsavedFile], # `struct CXUnsavedFile *`
  num_unsaved_files: cuint, # `unsigned int`
  options: cuint, # `unsigned int`
): ptr[CXCodeCompleteResults] {.
    cdecl,
    importc: "clang_codeCompleteAt",
    dynlib: libclang
  .}

proc clang_sortCodeCompletionResults*(
  results: ptr[CXCompletionResult], # `CXCompletionResult *`
  numResults: cuint, # `unsigned int`
): void {.
    cdecl,
    importc: "clang_sortCodeCompletionResults",
    dynlib: libclang
  .}

proc clang_disposeCodeCompleteResults*(
  results: ptr[CXCodeCompleteResults], # `CXCodeCompleteResults *`
): void {.
    cdecl,
    importc: "clang_disposeCodeCompleteResults",
    dynlib: libclang
  .}

proc clang_codeCompleteGetNumDiagnostics*(
  results: ptr[CXCodeCompleteResults], # `CXCodeCompleteResults *`
): cuint {.
    cdecl,
    importc: "clang_codeCompleteGetNumDiagnostics",
    dynlib: libclang
  .}

proc clang_codeCompleteGetDiagnostic*(
  results: ptr[CXCodeCompleteResults], # `CXCodeCompleteResults *`
  index: cuint, # `unsigned int`
): CXDiagnostic {.
    cdecl,
    importc: "clang_codeCompleteGetDiagnostic",
    dynlib: libclang
  .}

proc clang_codeCompleteGetContexts*(
  results: ptr[CXCodeCompleteResults], # `CXCodeCompleteResults *`
): culonglong {.
    cdecl,
    importc: "clang_codeCompleteGetContexts",
    dynlib: libclang
  .}

proc clang_codeCompleteGetContainerKind*(
  results: ptr[CXCodeCompleteResults], # `CXCodeCompleteResults *`
  isIncomplete: ptr[cuint], # `unsigned int *`
): CXCursorKind {.
    cdecl,
    importc: "clang_codeCompleteGetContainerKind",
    dynlib: libclang
  .}

proc clang_codeCompleteGetContainerUSR*(
  results: ptr[CXCodeCompleteResults], # `CXCodeCompleteResults *`
): CXString {.
    cdecl,
    importc: "clang_codeCompleteGetContainerUSR",
    dynlib: libclang
  .}

proc clang_codeCompleteGetObjCSelector*(
  results: ptr[CXCodeCompleteResults], # `CXCodeCompleteResults *`
): CXString {.
    cdecl,
    importc: "clang_codeCompleteGetObjCSelector",
    dynlib: libclang
  .}

proc clang_getClangVersion*(): CXString {.
    cdecl,
    importc: "clang_getClangVersion",
    dynlib: libclang
  .}

proc clang_toggleCrashRecovery*(
  isEnabled: cuint, # `unsigned int`
): void {.
    cdecl,
    importc: "clang_toggleCrashRecovery",
    dynlib: libclang
  .}

type CXInclusionVisitor* = distinct proc(a0: pointer, a1: ptr[CXSourceLocation], a2: cuint, a3: pointer): void {.cdecl.} # CXInclusionVisitor

proc clang_getInclusions*(
  tu: CXTranslationUnit, # `CXTranslationUnit`
  visitor: CXInclusionVisitor, # `CXInclusionVisitor`
  client_data: CXClientData, # `CXClientData`
): void {.
    cdecl,
    importc: "clang_getInclusions",
    dynlib: libclang
  .}

type
  CXEvalResultKind* {.pure, size: sizeof(cint).} = enum
    CXEval_Int = 1
    CXEval_Float = 2
    CXEval_ObjCStrLiteral = 3
    CXEval_StrLiteral = 4
    CXEval_CFStr = 5
    CXEval_Other = 6
    CXEval_UnExposed = 0

type CXEvalResult* = distinct pointer # CXEvalResult

proc clang_Cursor_Evaluate*(
  c: CXCursor, # `CXCursor`
): CXEvalResult {.
    cdecl,
    importc: "clang_Cursor_Evaluate",
    dynlib: libclang
  .}

proc clang_EvalResult_getKind*(
  e: CXEvalResult, # `CXEvalResult`
): CXEvalResultKind {.
    cdecl,
    importc: "clang_EvalResult_getKind",
    dynlib: libclang
  .}

proc clang_EvalResult_getAsInt*(
  e: CXEvalResult, # `CXEvalResult`
): cint {.
    cdecl,
    importc: "clang_EvalResult_getAsInt",
    dynlib: libclang
  .}

proc clang_EvalResult_getAsLongLong*(
  e: CXEvalResult, # `CXEvalResult`
): clonglong {.
    cdecl,
    importc: "clang_EvalResult_getAsLongLong",
    dynlib: libclang
  .}

proc clang_EvalResult_isUnsignedInt*(
  e: CXEvalResult, # `CXEvalResult`
): cuint {.
    cdecl,
    importc: "clang_EvalResult_isUnsignedInt",
    dynlib: libclang
  .}

proc clang_EvalResult_getAsUnsigned*(
  e: CXEvalResult, # `CXEvalResult`
): culonglong {.
    cdecl,
    importc: "clang_EvalResult_getAsUnsigned",
    dynlib: libclang
  .}

proc clang_EvalResult_getAsDouble*(
  e: CXEvalResult, # `CXEvalResult`
): cdouble {.
    cdecl,
    importc: "clang_EvalResult_getAsDouble",
    dynlib: libclang
  .}

proc clang_EvalResult_getAsStr*(
  e: CXEvalResult, # `CXEvalResult`
): ptr[cstring] {.
    cdecl,
    importc: "clang_EvalResult_getAsStr",
    dynlib: libclang
  .}

proc clang_EvalResult_dispose*(
  e: CXEvalResult, # `CXEvalResult`
): void {.
    cdecl,
    importc: "clang_EvalResult_dispose",
    dynlib: libclang
  .}

type CXRemapping* = distinct pointer # CXRemapping

proc clang_getRemappings*(
  path: ptr[cstring], # `const char *`
): CXRemapping {.
    cdecl,
    importc: "clang_getRemappings",
    dynlib: libclang
  .}

proc clang_getRemappingsFromFileList*(
  filePaths: ptr[ptr[cstring]], # `const char **`
  numFiles: cuint, # `unsigned int`
): CXRemapping {.
    cdecl,
    importc: "clang_getRemappingsFromFileList",
    dynlib: libclang
  .}

proc clang_remap_getNumFiles*(
  arg_1: CXRemapping, # `CXRemapping`
): cuint {.
    cdecl,
    importc: "clang_remap_getNumFiles",
    dynlib: libclang
  .}

proc clang_remap_getFilenames*(
  arg_1: CXRemapping, # `CXRemapping`
  index: cuint, # `unsigned int`
  original: ptr[CXString], # `CXString *`
  transformed: ptr[CXString], # `CXString *`
): void {.
    cdecl,
    importc: "clang_remap_getFilenames",
    dynlib: libclang
  .}

proc clang_remap_dispose*(
  arg_1: CXRemapping, # `CXRemapping`
): void {.
    cdecl,
    importc: "clang_remap_dispose",
    dynlib: libclang
  .}

type
  CXCursorAndRangeVisitor* {.pure, bycopy.} = object
    context*: pointer # `void *`
    visit*: proc(a0: pointer, a1: CXCursor, a2: CXSourceRange): CXVisitorResult {.cdecl.} # `enum CXVisitorResult (*)(void *, CXCursor, CXSourceRange)`

type
  CXResult* {.pure, size: sizeof(cint).} = enum
    CXResult_Success = 0
    CXResult_Invalid = 1
    CXResult_VisitBreak = 2

proc clang_findReferencesInFile*(
  cursor: CXCursor, # `CXCursor`
  file: CXFile, # `CXFile`
  visitor: CXCursorAndRangeVisitor, # `CXCursorAndRangeVisitor`
): CXResult {.
    cdecl,
    importc: "clang_findReferencesInFile",
    dynlib: libclang
  .}

proc clang_findIncludesInFile*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
  file: CXFile, # `CXFile`
  visitor: CXCursorAndRangeVisitor, # `CXCursorAndRangeVisitor`
): CXResult {.
    cdecl,
    importc: "clang_findIncludesInFile",
    dynlib: libclang
  .}

type CXIdxClientFile* = distinct pointer # CXIdxClientFile

type CXIdxClientEntity* = distinct pointer # CXIdxClientEntity

type CXIdxClientContainer* = distinct pointer # CXIdxClientContainer

type CXIdxClientASTFile* = distinct pointer # CXIdxClientASTFile

type
  CXIdxLoc* {.pure, bycopy.} = object
    ptr_data*: array[2, void *] # `void *[2]`
    int_data*: cuint # `unsigned int`

type
  CXIdxIncludedFileInfo* {.pure, bycopy.} = object
    hashLoc*: CXIdxLoc # `CXIdxLoc`
    filename*: ptr[cstring] # `const char *`
    file*: CXFile # `CXFile`
    isImport*: cint # `int`
    isAngled*: cint # `int`
    isModuleImport*: cint # `int`

type
  CXIdxImportedASTFileInfo* {.pure, bycopy.} = object
    file*: CXFile # `CXFile`
    module*: CXModule # `CXModule`
    loc*: CXIdxLoc # `CXIdxLoc`
    isImplicit*: cint # `int`

type
  CXIdxEntityKind* {.pure, size: sizeof(cint).} = enum
    CXIdxEntity_Unexposed = 0
    CXIdxEntity_Typedef = 1
    CXIdxEntity_Function = 2
    CXIdxEntity_Variable = 3
    CXIdxEntity_Field = 4
    CXIdxEntity_EnumConstant = 5
    CXIdxEntity_ObjCClass = 6
    CXIdxEntity_ObjCProtocol = 7
    CXIdxEntity_ObjCCategory = 8
    CXIdxEntity_ObjCInstanceMethod = 9
    CXIdxEntity_ObjCClassMethod = 10
    CXIdxEntity_ObjCProperty = 11
    CXIdxEntity_ObjCIvar = 12
    CXIdxEntity_Enum = 13
    CXIdxEntity_Struct = 14
    CXIdxEntity_Union = 15
    CXIdxEntity_CXXClass = 16
    CXIdxEntity_CXXNamespace = 17
    CXIdxEntity_CXXNamespaceAlias = 18
    CXIdxEntity_CXXStaticVariable = 19
    CXIdxEntity_CXXStaticMethod = 20
    CXIdxEntity_CXXInstanceMethod = 21
    CXIdxEntity_CXXConstructor = 22
    CXIdxEntity_CXXDestructor = 23
    CXIdxEntity_CXXConversionFunction = 24
    CXIdxEntity_CXXTypeAlias = 25
    CXIdxEntity_CXXInterface = 26

type
  CXIdxEntityLanguage* {.pure, size: sizeof(cint).} = enum
    CXIdxEntityLang_None = 0
    CXIdxEntityLang_C = 1
    CXIdxEntityLang_ObjC = 2
    CXIdxEntityLang_CXX = 3
    CXIdxEntityLang_Swift = 4

type
  CXIdxEntityCXXTemplateKind* {.pure, size: sizeof(cint).} = enum
    CXIdxEntity_NonTemplate = 0
    CXIdxEntity_Template = 1
    CXIdxEntity_TemplatePartialSpecialization = 2
    CXIdxEntity_TemplateSpecialization = 3

type
  CXIdxAttrKind* {.pure, size: sizeof(cint).} = enum
    CXIdxAttr_Unexposed = 0
    CXIdxAttr_IBAction = 1
    CXIdxAttr_IBOutlet = 2
    CXIdxAttr_IBOutletCollection = 3

type
  CXIdxAttrInfo* {.pure, bycopy.} = object
    kind*: CXIdxAttrKind # `CXIdxAttrKind`
    cursor*: CXCursor # `CXCursor`
    loc*: CXIdxLoc # `CXIdxLoc`

type
  CXIdxEntityInfo* {.pure, bycopy.} = object
    kind*: CXIdxEntityKind # `CXIdxEntityKind`
    templateKind*: CXIdxEntityCXXTemplateKind # `CXIdxEntityCXXTemplateKind`
    lang*: CXIdxEntityLanguage # `CXIdxEntityLanguage`
    name*: ptr[cstring] # `const char *`
    uSR*: ptr[cstring] # `const char *`
    cursor*: CXCursor # `CXCursor`
    attributes*: ptr[ptr[const CXIdxAttrInfo]] # `const CXIdxAttrInfo *const *`
    numAttributes*: cuint # `unsigned int`

type
  CXIdxContainerInfo* {.pure, bycopy.} = object
    cursor*: CXCursor # `CXCursor`

type
  CXIdxIBOutletCollectionAttrInfo* {.pure, bycopy.} = object
    attrInfo*: ptr[const CXIdxAttrInfo] # `const CXIdxAttrInfo *`
    objcClass*: ptr[const CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    classCursor*: CXCursor # `CXCursor`
    classLoc*: CXIdxLoc # `CXIdxLoc`

type
  CXIdxDeclInfoFlags* {.pure, size: sizeof(cint).} = enum
    CXIdxDeclFlag_Skipped = 1

type
  CXIdxDeclInfo* {.pure, bycopy.} = object
    entityInfo*: ptr[const CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    cursor*: CXCursor # `CXCursor`
    loc*: CXIdxLoc # `CXIdxLoc`
    semanticContainer*: ptr[const CXIdxContainerInfo] # `const CXIdxContainerInfo *`
    lexicalContainer*: ptr[const CXIdxContainerInfo] # `const CXIdxContainerInfo *`
    isRedeclaration*: cint # `int`
    isDefinition*: cint # `int`
    isContainer*: cint # `int`
    declAsContainer*: ptr[const CXIdxContainerInfo] # `const CXIdxContainerInfo *`
    isImplicit*: cint # `int`
    attributes*: ptr[ptr[const CXIdxAttrInfo]] # `const CXIdxAttrInfo *const *`
    numAttributes*: cuint # `unsigned int`
    flags*: cuint # `unsigned int`

type
  CXIdxObjCContainerKind* {.pure, size: sizeof(cint).} = enum
    CXIdxObjCContainer_ForwardRef = 0
    CXIdxObjCContainer_Interface = 1
    CXIdxObjCContainer_Implementation = 2

type
  CXIdxObjCContainerDeclInfo* {.pure, bycopy.} = object
    declInfo*: ptr[const CXIdxDeclInfo] # `const CXIdxDeclInfo *`
    kind*: CXIdxObjCContainerKind # `CXIdxObjCContainerKind`

type
  CXIdxBaseClassInfo* {.pure, bycopy.} = object
    base*: ptr[const CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    cursor*: CXCursor # `CXCursor`
    loc*: CXIdxLoc # `CXIdxLoc`

type
  CXIdxObjCProtocolRefInfo* {.pure, bycopy.} = object
    protocol*: ptr[const CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    cursor*: CXCursor # `CXCursor`
    loc*: CXIdxLoc # `CXIdxLoc`

type
  CXIdxObjCProtocolRefListInfo* {.pure, bycopy.} = object
    protocols*: ptr[ptr[const CXIdxObjCProtocolRefInfo]] # `const CXIdxObjCProtocolRefInfo *const *`
    numProtocols*: cuint # `unsigned int`

type
  CXIdxObjCInterfaceDeclInfo* {.pure, bycopy.} = object
    containerInfo*: ptr[const CXIdxObjCContainerDeclInfo] # `const CXIdxObjCContainerDeclInfo *`
    superInfo*: ptr[const CXIdxBaseClassInfo] # `const CXIdxBaseClassInfo *`
    protocols*: ptr[const CXIdxObjCProtocolRefListInfo] # `const CXIdxObjCProtocolRefListInfo *`

type
  CXIdxObjCCategoryDeclInfo* {.pure, bycopy.} = object
    containerInfo*: ptr[const CXIdxObjCContainerDeclInfo] # `const CXIdxObjCContainerDeclInfo *`
    objcClass*: ptr[const CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    classCursor*: CXCursor # `CXCursor`
    classLoc*: CXIdxLoc # `CXIdxLoc`
    protocols*: ptr[const CXIdxObjCProtocolRefListInfo] # `const CXIdxObjCProtocolRefListInfo *`

type
  CXIdxObjCPropertyDeclInfo* {.pure, bycopy.} = object
    declInfo*: ptr[const CXIdxDeclInfo] # `const CXIdxDeclInfo *`
    getter*: ptr[const CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    setter*: ptr[const CXIdxEntityInfo] # `const CXIdxEntityInfo *`

type
  CXIdxCXXClassDeclInfo* {.pure, bycopy.} = object
    declInfo*: ptr[const CXIdxDeclInfo] # `const CXIdxDeclInfo *`
    bases*: ptr[ptr[const CXIdxBaseClassInfo]] # `const CXIdxBaseClassInfo *const *`
    numBases*: cuint # `unsigned int`

type
  CXIdxEntityRefKind* {.pure, size: sizeof(cint).} = enum
    CXIdxEntityRef_Direct = 1
    CXIdxEntityRef_Implicit = 2

type
  CXSymbolRole* {.pure, size: sizeof(cint).} = enum
    CXSymbolRole_None = 0
    CXSymbolRole_Declaration = 1
    CXSymbolRole_Definition = 2
    CXSymbolRole_Reference = 4
    CXSymbolRole_Read = 8
    CXSymbolRole_Write = 16
    CXSymbolRole_Call = 32
    CXSymbolRole_Dynamic = 64
    CXSymbolRole_AddressOf = 128
    CXSymbolRole_Implicit = 256

type
  CXIdxEntityRefInfo* {.pure, bycopy.} = object
    kind*: CXIdxEntityRefKind # `CXIdxEntityRefKind`
    cursor*: CXCursor # `CXCursor`
    loc*: CXIdxLoc # `CXIdxLoc`
    referencedEntity*: ptr[const CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    parentEntity*: ptr[const CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    container*: ptr[const CXIdxContainerInfo] # `const CXIdxContainerInfo *`
    role*: CXSymbolRole # `CXSymbolRole`

type
  IndexerCallbacks* {.pure, bycopy.} = object
    abortQuery*: proc(a0: CXClientData, a1: pointer): cint {.cdecl.} # `int (*)(CXClientData, void *)`
    diagnostic*: proc(a0: CXClientData, a1: CXDiagnosticSet, a2: pointer): void {.cdecl.} # `void (*)(CXClientData, CXDiagnosticSet, void *)`
    enteredMainFile*: proc(a0: CXClientData, a1: CXFile, a2: pointer): CXIdxClientFile {.cdecl.} # `CXIdxClientFile (*)(CXClientData, CXFile, void *)`
    ppIncludedFile*: proc(a0: CXClientData, a1: ptr[const CXIdxIncludedFileInfo]): CXIdxClientFile {.cdecl.} # `CXIdxClientFile (*)(CXClientData, const CXIdxIncludedFileInfo *)`
    importedASTFile*: proc(a0: CXClientData, a1: ptr[const CXIdxImportedASTFileInfo]): CXIdxClientASTFile {.cdecl.} # `CXIdxClientASTFile (*)(CXClientData, const CXIdxImportedASTFileInfo *)`
    startedTranslationUnit*: proc(a0: CXClientData, a1: pointer): CXIdxClientContainer {.cdecl.} # `CXIdxClientContainer (*)(CXClientData, void *)`
    indexDeclaration*: proc(a0: CXClientData, a1: ptr[const CXIdxDeclInfo]): void {.cdecl.} # `void (*)(CXClientData, const CXIdxDeclInfo *)`
    indexEntityReference*: proc(a0: CXClientData, a1: ptr[const CXIdxEntityRefInfo]): void {.cdecl.} # `void (*)(CXClientData, const CXIdxEntityRefInfo *)`

proc clang_index_isEntityObjCContainerKind*(
  arg_1: CXIdxEntityKind, # `CXIdxEntityKind`
): cint {.
    cdecl,
    importc: "clang_index_isEntityObjCContainerKind",
    dynlib: libclang
  .}

proc clang_index_getObjCContainerDeclInfo*(
  arg_1: ptr[const CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[const CXIdxObjCContainerDeclInfo] {.
    cdecl,
    importc: "clang_index_getObjCContainerDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getObjCInterfaceDeclInfo*(
  arg_1: ptr[const CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[const CXIdxObjCInterfaceDeclInfo] {.
    cdecl,
    importc: "clang_index_getObjCInterfaceDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getObjCCategoryDeclInfo*(
  arg_1: ptr[const CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[const CXIdxObjCCategoryDeclInfo] {.
    cdecl,
    importc: "clang_index_getObjCCategoryDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getObjCProtocolRefListInfo*(
  arg_1: ptr[const CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[const CXIdxObjCProtocolRefListInfo] {.
    cdecl,
    importc: "clang_index_getObjCProtocolRefListInfo",
    dynlib: libclang
  .}

proc clang_index_getObjCPropertyDeclInfo*(
  arg_1: ptr[const CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[const CXIdxObjCPropertyDeclInfo] {.
    cdecl,
    importc: "clang_index_getObjCPropertyDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getIBOutletCollectionAttrInfo*(
  arg_1: ptr[const CXIdxAttrInfo], # `const CXIdxAttrInfo *`
): ptr[const CXIdxIBOutletCollectionAttrInfo] {.
    cdecl,
    importc: "clang_index_getIBOutletCollectionAttrInfo",
    dynlib: libclang
  .}

proc clang_index_getCXXClassDeclInfo*(
  arg_1: ptr[const CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[const CXIdxCXXClassDeclInfo] {.
    cdecl,
    importc: "clang_index_getCXXClassDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getClientContainer*(
  arg_1: ptr[const CXIdxContainerInfo], # `const CXIdxContainerInfo *`
): CXIdxClientContainer {.
    cdecl,
    importc: "clang_index_getClientContainer",
    dynlib: libclang
  .}

proc clang_index_setClientContainer*(
  arg_1: ptr[const CXIdxContainerInfo], # `const CXIdxContainerInfo *`
  arg_2: CXIdxClientContainer, # `CXIdxClientContainer`
): void {.
    cdecl,
    importc: "clang_index_setClientContainer",
    dynlib: libclang
  .}

proc clang_index_getClientEntity*(
  arg_1: ptr[const CXIdxEntityInfo], # `const CXIdxEntityInfo *`
): CXIdxClientEntity {.
    cdecl,
    importc: "clang_index_getClientEntity",
    dynlib: libclang
  .}

proc clang_index_setClientEntity*(
  arg_1: ptr[const CXIdxEntityInfo], # `const CXIdxEntityInfo *`
  arg_2: CXIdxClientEntity, # `CXIdxClientEntity`
): void {.
    cdecl,
    importc: "clang_index_setClientEntity",
    dynlib: libclang
  .}

type CXIndexAction* = distinct pointer # CXIndexAction

proc clang_IndexAction_create*(
  cIdx: CXIndex, # `CXIndex`
): CXIndexAction {.
    cdecl,
    importc: "clang_IndexAction_create",
    dynlib: libclang
  .}

proc clang_IndexAction_dispose*(
  arg_1: CXIndexAction, # `CXIndexAction`
): void {.
    cdecl,
    importc: "clang_IndexAction_dispose",
    dynlib: libclang
  .}

type
  CXIndexOptFlags* {.pure, size: sizeof(cint).} = enum
    CXIndexOpt_None = 0
    CXIndexOpt_SuppressRedundantRefs = 1
    CXIndexOpt_IndexFunctionLocalSymbols = 2
    CXIndexOpt_IndexImplicitTemplateInstantiations = 4
    CXIndexOpt_SuppressWarnings = 8
    CXIndexOpt_SkipParsedBodiesInSession = 16

proc clang_indexSourceFile*(
  arg_1: CXIndexAction, # `CXIndexAction`
  client_data: CXClientData, # `CXClientData`
  index_callbacks: ptr[IndexerCallbacks], # `IndexerCallbacks *`
  index_callbacks_size: cuint, # `unsigned int`
  index_options: cuint, # `unsigned int`
  source_filename: ptr[cstring], # `const char *`
  command_line_args: ptr[ptr[cstring]], # `const char *const *`
  num_command_line_args: cint, # `int`
  unsaved_files: ptr[CXUnsavedFile], # `struct CXUnsavedFile *`
  num_unsaved_files: cuint, # `unsigned int`
  out_TU: ptr[CXTranslationUnit], # `CXTranslationUnit *`
  tU_options: cuint, # `unsigned int`
): cint {.
    cdecl,
    importc: "clang_indexSourceFile",
    dynlib: libclang
  .}

proc clang_indexSourceFileFullArgv*(
  arg_1: CXIndexAction, # `CXIndexAction`
  client_data: CXClientData, # `CXClientData`
  index_callbacks: ptr[IndexerCallbacks], # `IndexerCallbacks *`
  index_callbacks_size: cuint, # `unsigned int`
  index_options: cuint, # `unsigned int`
  source_filename: ptr[cstring], # `const char *`
  command_line_args: ptr[ptr[cstring]], # `const char *const *`
  num_command_line_args: cint, # `int`
  unsaved_files: ptr[CXUnsavedFile], # `struct CXUnsavedFile *`
  num_unsaved_files: cuint, # `unsigned int`
  out_TU: ptr[CXTranslationUnit], # `CXTranslationUnit *`
  tU_options: cuint, # `unsigned int`
): cint {.
    cdecl,
    importc: "clang_indexSourceFileFullArgv",
    dynlib: libclang
  .}

proc clang_indexTranslationUnit*(
  arg_1: CXIndexAction, # `CXIndexAction`
  client_data: CXClientData, # `CXClientData`
  index_callbacks: ptr[IndexerCallbacks], # `IndexerCallbacks *`
  index_callbacks_size: cuint, # `unsigned int`
  index_options: cuint, # `unsigned int`
  arg_2: CXTranslationUnit, # `CXTranslationUnit`
): cint {.
    cdecl,
    importc: "clang_indexTranslationUnit",
    dynlib: libclang
  .}

proc clang_indexLoc_getFileLocation*(
  loc: CXIdxLoc, # `CXIdxLoc`
  indexFile: ptr[CXIdxClientFile], # `CXIdxClientFile *`
  file: ptr[CXFile], # `CXFile *`
  line: ptr[cuint], # `unsigned int *`
  column: ptr[cuint], # `unsigned int *`
  offset: ptr[cuint], # `unsigned int *`
): void {.
    cdecl,
    importc: "clang_indexLoc_getFileLocation",
    dynlib: libclang
  .}

proc clang_indexLoc_getCXSourceLocation*(
  loc: CXIdxLoc, # `CXIdxLoc`
): CXSourceLocation {.
    cdecl,
    importc: "clang_indexLoc_getCXSourceLocation",
    dynlib: libclang
  .}

type CXFieldVisitor* = distinct proc(a0: CXCursor, a1: pointer): enum CXVisitorResult {.cdecl.} # CXFieldVisitor

proc clang_Type_visitFields*(
  t: CXType, # `CXType`
  visitor: CXFieldVisitor, # `CXFieldVisitor`
  client_data: CXClientData, # `CXClientData`
): cuint {.
    cdecl,
    importc: "clang_Type_visitFields",
    dynlib: libclang
  .}

