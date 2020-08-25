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

type
  CXTargetInfoImpl* {.pure, bycopy.} = object

type CXTargetInfo* = distinct ptr[CXTargetInfoImpl] # CXTargetInfo

type
  CXTranslationUnitImpl* {.pure, bycopy.} = object

type CXTranslationUnit* = distinct ptr[CXTranslationUnitImpl] # CXTranslationUnit

type CXClientData* = distinct pointer # CXClientData

type
  CXUnsavedFile* {.pure, bycopy.} = object
    filename*: ptr[cstring] # `const char *`
    contents*: ptr[cstring] # `const char *`
    length*: culong # `unsigned long`

type
  CXAvailabilityKind* {.pure, size: sizeof(cint).} = enum
    CXAvailability_Available
    CXAvailability_Deprecated
    CXAvailability_NotAvailable
    CXAvailability_NotAccessible

type
  CXVersion* {.pure, bycopy.} = object
    major*: cint # `int`
    minor*: cint # `int`
    subminor*: cint # `int`

type
  CXCursor_ExceptionSpecificationKind* {.pure, size: sizeof(cint).} = enum
    CXCursor_ExceptionSpecificationKind_None
    CXCursor_ExceptionSpecificationKind_DynamicNone
    CXCursor_ExceptionSpecificationKind_Dynamic
    CXCursor_ExceptionSpecificationKind_MSAny
    CXCursor_ExceptionSpecificationKind_BasicNoexcept
    CXCursor_ExceptionSpecificationKind_ComputedNoexcept
    CXCursor_ExceptionSpecificationKind_Unevaluated
    CXCursor_ExceptionSpecificationKind_Uninstantiated
    CXCursor_ExceptionSpecificationKind_Unparsed
    CXCursor_ExceptionSpecificationKind_NoThrow

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
    data*: array[3, culonglong] # `unsigned long long [3]`

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
    ptr_data*: array[2, pointer] # `const void *[2]`
    int_data*: cuint # `unsigned int`

type
  CXSourceRange* {.pure, bycopy.} = object
    ptr_data*: array[2, pointer] # `const void *[2]`
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

type
  CXDiagnosticSeverity* {.pure, size: sizeof(cint).} = enum
    CXDiagnostic_Ignored = 0
    CXDiagnostic_Note = 1
    CXDiagnostic_Warning = 2
    CXDiagnostic_Error = 3
    CXDiagnostic_Fatal = 4

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

type
  CXLoadDiag_Error* {.pure, size: sizeof(cint).} = enum
    CXLoadDiag_None = 0
    CXLoadDiag_Unknown = 1
    CXLoadDiag_CannotLoad = 2
    CXLoadDiag_InvalidFile = 3

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

type
  CXDiagnosticDisplayOptions* {.pure, size: sizeof(cint).} = enum
    CXDiagnostic_DisplaySourceLocation = 1
    CXDiagnostic_DisplayColumn = 2
    CXDiagnostic_DisplaySourceRanges = 4
    CXDiagnostic_DisplayOption = 8
    CXDiagnostic_DisplayCategoryId = 16
    CXDiagnostic_DisplayCategoryName = 32

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

type
  CXTranslationUnit_Flags* {.pure, size: sizeof(cint).} = enum
    CXTranslationUnit_None = 0
    CXTranslationUnit_DetailedPreprocessingRecord = 1
    CXTranslationUnit_Incomplete = 2
    CXTranslationUnit_PrecompiledPreamble = 4
    CXTranslationUnit_CacheCompletionResults = 8
    CXTranslationUnit_ForSerialization = 16
    CXTranslationUnit_CXXChainedPCH = 32
    CXTranslationUnit_SkipFunctionBodies = 64
    CXTranslationUnit_IncludeBriefCommentsInCodeCompletion = 128
    CXTranslationUnit_CreatePreambleOnFirstParse = 256
    CXTranslationUnit_KeepGoing = 512
    CXTranslationUnit_SingleFileParse = 1024
    CXTranslationUnit_LimitSkipFunctionBodiesToPreamble = 2048
    CXTranslationUnit_IncludeAttributedTypes = 4096
    CXTranslationUnit_VisitImplicitAttributes = 8192
    CXTranslationUnit_IgnoreNonErrorsFromIncludedFiles = 16384
    CXTranslationUnit_RetainExcludedConditionalBlocks = 32768

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

type
  CXSaveTranslationUnit_Flags* {.pure, size: sizeof(cint).} = enum
    CXSaveTranslationUnit_None = 0

proc clang_defaultSaveOptions*(
  tU: CXTranslationUnit, # `CXTranslationUnit`
): cuint {.
    cdecl,
    importc: "clang_defaultSaveOptions",
    dynlib: libclang
  .}

type
  CXSaveError* {.pure, size: sizeof(cint).} = enum
    CXSaveError_None = 0
    CXSaveError_Unknown = 1
    CXSaveError_TranslationErrors = 2
    CXSaveError_InvalidTU = 3

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

type
  CXReparse_Flags* {.pure, size: sizeof(cint).} = enum
    CXReparse_None = 0

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

type
  CXTUResourceUsageKind* {.pure, size: sizeof(cint).} = enum
    CXTUResourceUsage_AST = 1
    CXTUResourceUsage_Identifiers = 2
    CXTUResourceUsage_Selectors = 3
    CXTUResourceUsage_GlobalCompletionResults = 4
    CXTUResourceUsage_SourceManagerContentCache = 5
    CXTUResourceUsage_AST_SideTables = 6
    CXTUResourceUsage_SourceManager_Membuffer_Malloc = 7
    CXTUResourceUsage_SourceManager_Membuffer_MMap = 8
    CXTUResourceUsage_ExternalASTSource_Membuffer_Malloc = 9
    CXTUResourceUsage_ExternalASTSource_Membuffer_MMap = 10
    CXTUResourceUsage_Preprocessor = 11
    CXTUResourceUsage_PreprocessingRecord = 12
    CXTUResourceUsage_SourceManager_DataStructures = 13
    CXTUResourceUsage_Preprocessor_HeaderSearch = 14
    CXTUResourceUsage_MEMORY_IN_BYTES_BEGIN
    CXTUResourceUsage_MEMORY_IN_BYTES_END
    CXTUResourceUsage_First
    CXTUResourceUsage_Last

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
  CXCursorKind* {.pure, size: sizeof(cint).} = enum
    CXCursor_UnexposedDecl = 1
    CXCursor_StructDecl = 2
    CXCursor_UnionDecl = 3
    CXCursor_ClassDecl = 4
    CXCursor_EnumDecl = 5
    CXCursor_FieldDecl = 6
    CXCursor_EnumConstantDecl = 7
    CXCursor_FunctionDecl = 8
    CXCursor_VarDecl = 9
    CXCursor_ParmDecl = 10
    CXCursor_ObjCInterfaceDecl = 11
    CXCursor_ObjCCategoryDecl = 12
    CXCursor_ObjCProtocolDecl = 13
    CXCursor_ObjCPropertyDecl = 14
    CXCursor_ObjCIvarDecl = 15
    CXCursor_ObjCInstanceMethodDecl = 16
    CXCursor_ObjCClassMethodDecl = 17
    CXCursor_ObjCImplementationDecl = 18
    CXCursor_ObjCCategoryImplDecl = 19
    CXCursor_TypedefDecl = 20
    CXCursor_CXXMethod = 21
    CXCursor_Namespace = 22
    CXCursor_LinkageSpec = 23
    CXCursor_Constructor = 24
    CXCursor_Destructor = 25
    CXCursor_ConversionFunction = 26
    CXCursor_TemplateTypeParameter = 27
    CXCursor_NonTypeTemplateParameter = 28
    CXCursor_TemplateTemplateParameter = 29
    CXCursor_FunctionTemplate = 30
    CXCursor_ClassTemplate = 31
    CXCursor_ClassTemplatePartialSpecialization = 32
    CXCursor_NamespaceAlias = 33
    CXCursor_UsingDirective = 34
    CXCursor_UsingDeclaration = 35
    CXCursor_TypeAliasDecl = 36
    CXCursor_ObjCSynthesizeDecl = 37
    CXCursor_ObjCDynamicDecl = 38
    CXCursor_CXXAccessSpecifier = 39
    # CXCursor_FirstDecl = 1
    # CXCursor_LastDecl = 39
    # CXCursor_FirstRef = 40 = 40
    CXCursor_ObjCSuperClassRef = 40
    CXCursor_ObjCProtocolRef = 41
    CXCursor_ObjCClassRef = 42
    CXCursor_TypeRef = 43
    CXCursor_CXXBaseSpecifier = 44
    CXCursor_TemplateRef = 45
    CXCursor_NamespaceRef = 46
    CXCursor_MemberRef = 47
    CXCursor_LabelRef = 48
    CXCursor_OverloadedDeclRef = 49
    CXCursor_VariableRef = 50
    CXCursor_LastRef
    CXCursor_FirstInvalid = 70
    # CXCursor_InvalidFile = 70
    CXCursor_NoDeclFound = 71
    CXCursor_NotImplemented = 72
    CXCursor_InvalidCode = 73
    CXCursor_LastInvalid
    # CXCursor_FirstExpr = 100
    CXCursor_UnexposedExpr = 100
    CXCursor_DeclRefExpr = 101
    CXCursor_MemberRefExpr = 102
    CXCursor_CallExpr = 103
    CXCursor_ObjCMessageExpr = 104
    CXCursor_BlockExpr = 105
    CXCursor_IntegerLiteral = 106
    CXCursor_FloatingLiteral = 107
    CXCursor_ImaginaryLiteral = 108
    CXCursor_StringLiteral = 109
    CXCursor_CharacterLiteral = 110
    CXCursor_ParenExpr = 111
    CXCursor_UnaryOperator = 112
    CXCursor_ArraySubscriptExpr = 113
    CXCursor_BinaryOperator = 114
    CXCursor_CompoundAssignOperator = 115
    CXCursor_ConditionalOperator = 116
    CXCursor_CStyleCastExpr = 117
    CXCursor_CompoundLiteralExpr = 118
    CXCursor_InitListExpr = 119
    CXCursor_AddrLabelExpr = 120
    CXCursor_StmtExpr = 121
    CXCursor_GenericSelectionExpr = 122
    CXCursor_GNUNullExpr = 123
    CXCursor_CXXStaticCastExpr = 124
    CXCursor_CXXDynamicCastExpr = 125
    CXCursor_CXXReinterpretCastExpr = 126
    CXCursor_CXXConstCastExpr = 127
    CXCursor_CXXFunctionalCastExpr = 128
    CXCursor_CXXTypeidExpr = 129
    CXCursor_CXXBoolLiteralExpr = 130
    CXCursor_CXXNullPtrLiteralExpr = 131
    CXCursor_CXXThisExpr = 132
    CXCursor_CXXThrowExpr = 133
    CXCursor_CXXNewExpr = 134
    CXCursor_CXXDeleteExpr = 135
    CXCursor_UnaryExpr = 136
    CXCursor_ObjCStringLiteral = 137
    CXCursor_ObjCEncodeExpr = 138
    CXCursor_ObjCSelectorExpr = 139
    CXCursor_ObjCProtocolExpr = 140
    CXCursor_ObjCBridgedCastExpr = 141
    CXCursor_PackExpansionExpr = 142
    CXCursor_SizeOfPackExpr = 143
    CXCursor_LambdaExpr = 144
    CXCursor_ObjCBoolLiteralExpr = 145
    CXCursor_ObjCSelfExpr = 146
    CXCursor_OMPArraySectionExpr = 147
    CXCursor_ObjCAvailabilityCheckExpr = 148
    CXCursor_FixedPointLiteral = 149
    CXCursor_LastExpr
    # CXCursor_FirstStmt = 200
    CXCursor_UnexposedStmt = 200
    CXCursor_LabelStmt = 201
    CXCursor_CompoundStmt = 202
    CXCursor_CaseStmt = 203
    CXCursor_DefaultStmt = 204
    CXCursor_IfStmt = 205
    CXCursor_SwitchStmt = 206
    CXCursor_WhileStmt = 207
    CXCursor_DoStmt = 208
    CXCursor_ForStmt = 209
    CXCursor_GotoStmt = 210
    CXCursor_IndirectGotoStmt = 211
    CXCursor_ContinueStmt = 212
    CXCursor_BreakStmt = 213
    CXCursor_ReturnStmt = 214
    CXCursor_GCCAsmStmt = 215
    # CXCursor_AsmStmt
    CXCursor_ObjCAtTryStmt = 216
    CXCursor_ObjCAtCatchStmt = 217
    CXCursor_ObjCAtFinallyStmt = 218
    CXCursor_ObjCAtThrowStmt = 219
    CXCursor_ObjCAtSynchronizedStmt = 220
    CXCursor_ObjCAutoreleasePoolStmt = 221
    CXCursor_ObjCForCollectionStmt = 222
    CXCursor_CXXCatchStmt = 223
    CXCursor_CXXTryStmt = 224
    CXCursor_CXXForRangeStmt = 225
    CXCursor_SEHTryStmt = 226
    CXCursor_SEHExceptStmt = 227
    CXCursor_SEHFinallyStmt = 228
    CXCursor_MSAsmStmt = 229
    CXCursor_NullStmt = 230
    CXCursor_DeclStmt = 231
    CXCursor_OMPParallelDirective = 232
    CXCursor_OMPSimdDirective = 233
    CXCursor_OMPForDirective = 234
    CXCursor_OMPSectionsDirective = 235
    CXCursor_OMPSectionDirective = 236
    CXCursor_OMPSingleDirective = 237
    CXCursor_OMPParallelForDirective = 238
    CXCursor_OMPParallelSectionsDirective = 239
    CXCursor_OMPTaskDirective = 240
    CXCursor_OMPMasterDirective = 241
    CXCursor_OMPCriticalDirective = 242
    CXCursor_OMPTaskyieldDirective = 243
    CXCursor_OMPBarrierDirective = 244
    CXCursor_OMPTaskwaitDirective = 245
    CXCursor_OMPFlushDirective = 246
    CXCursor_SEHLeaveStmt = 247
    CXCursor_OMPOrderedDirective = 248
    CXCursor_OMPAtomicDirective = 249
    CXCursor_OMPForSimdDirective = 250
    CXCursor_OMPParallelForSimdDirective = 251
    CXCursor_OMPTargetDirective = 252
    CXCursor_OMPTeamsDirective = 253
    CXCursor_OMPTaskgroupDirective = 254
    CXCursor_OMPCancellationPointDirective = 255
    CXCursor_OMPCancelDirective = 256
    CXCursor_OMPTargetDataDirective = 257
    CXCursor_OMPTaskLoopDirective = 258
    CXCursor_OMPTaskLoopSimdDirective = 259
    CXCursor_OMPDistributeDirective = 260
    CXCursor_OMPTargetEnterDataDirective = 261
    CXCursor_OMPTargetExitDataDirective = 262
    CXCursor_OMPTargetParallelDirective = 263
    CXCursor_OMPTargetParallelForDirective = 264
    CXCursor_OMPTargetUpdateDirective = 265
    CXCursor_OMPDistributeParallelForDirective = 266
    CXCursor_OMPDistributeParallelForSimdDirective = 267
    CXCursor_OMPDistributeSimdDirective = 268
    CXCursor_OMPTargetParallelForSimdDirective = 269
    CXCursor_OMPTargetSimdDirective = 270
    CXCursor_OMPTeamsDistributeDirective = 271
    CXCursor_OMPTeamsDistributeSimdDirective = 272
    CXCursor_OMPTeamsDistributeParallelForSimdDirective = 273
    CXCursor_OMPTeamsDistributeParallelForDirective = 274
    CXCursor_OMPTargetTeamsDirective = 275
    CXCursor_OMPTargetTeamsDistributeDirective = 276
    CXCursor_OMPTargetTeamsDistributeParallelForDirective = 277
    CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective = 278
    CXCursor_OMPTargetTeamsDistributeSimdDirective = 279
    CXCursor_BuiltinBitCastExpr = 280
    CXCursor_OMPMasterTaskLoopDirective = 281
    CXCursor_OMPParallelMasterTaskLoopDirective = 282
    CXCursor_OMPMasterTaskLoopSimdDirective = 283
    CXCursor_OMPParallelMasterTaskLoopSimdDirective = 284
    CXCursor_OMPParallelMasterDirective = 285
    CXCursor_LastStmt
    CXCursor_TranslationUnit = 300
    CXCursor_FirstAttr = 400
    # CXCursor_UnexposedAttr = 400
    CXCursor_IBActionAttr = 401
    CXCursor_IBOutletAttr = 402
    CXCursor_IBOutletCollectionAttr = 403
    CXCursor_CXXFinalAttr = 404
    CXCursor_CXXOverrideAttr = 405
    CXCursor_AnnotateAttr = 406
    CXCursor_AsmLabelAttr = 407
    CXCursor_PackedAttr = 408
    CXCursor_PureAttr = 409
    CXCursor_ConstAttr = 410
    CXCursor_NoDuplicateAttr = 411
    CXCursor_CUDAConstantAttr = 412
    CXCursor_CUDADeviceAttr = 413
    CXCursor_CUDAGlobalAttr = 414
    CXCursor_CUDAHostAttr = 415
    CXCursor_CUDASharedAttr = 416
    CXCursor_VisibilityAttr = 417
    CXCursor_DLLExport = 418
    CXCursor_DLLImport = 419
    CXCursor_NSReturnsRetained = 420
    CXCursor_NSReturnsNotRetained = 421
    CXCursor_NSReturnsAutoreleased = 422
    CXCursor_NSConsumesSelf = 423
    CXCursor_NSConsumed = 424
    CXCursor_ObjCException = 425
    CXCursor_ObjCNSObject = 426
    CXCursor_ObjCIndependentClass = 427
    CXCursor_ObjCPreciseLifetime = 428
    CXCursor_ObjCReturnsInnerPointer = 429
    CXCursor_ObjCRequiresSuper = 430
    CXCursor_ObjCRootClass = 431
    CXCursor_ObjCSubclassingRestricted = 432
    CXCursor_ObjCExplicitProtocolImpl = 433
    CXCursor_ObjCDesignatedInitializer = 434
    CXCursor_ObjCRuntimeVisible = 435
    CXCursor_ObjCBoxable = 436
    CXCursor_FlagEnum = 437
    CXCursor_ConvergentAttr = 438
    CXCursor_WarnUnusedAttr = 439
    CXCursor_WarnUnusedResultAttr = 440
    CXCursor_AlignedAttr = 441
    CXCursor_LastAttr
    CXCursor_PreprocessingDirective = 500
    CXCursor_MacroDefinition = 501
    CXCursor_MacroExpansion = 502
    CXCursor_MacroInstantiation
    # CXCursor_InclusionDirective = 503
    CXCursor_FirstPreprocessing
    CXCursor_LastPreprocessing
    CXCursor_ModuleImportDecl = 600
    CXCursor_TypeAliasTemplateDecl = 601
    CXCursor_StaticAssert = 602
    CXCursor_FriendDecl = 603
    CXCursor_FirstExtraDecl
    CXCursor_LastExtraDecl
    CXCursor_OverloadCandidate = 700

type
  CXCursor* {.pure, bycopy.} = object
    kind*: CXCursorKind # `enum CXCursorKind`
    xdata*: cint # `int`
    data*: array[3, pointer] # `const void *[3]`

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

type
  CXLinkageKind* {.pure, size: sizeof(cint).} = enum
    CXLinkage_Invalid
    CXLinkage_NoLinkage
    CXLinkage_Internal
    CXLinkage_UniqueExternal
    CXLinkage_External

proc clang_getCursorLinkage*(
  cursor: CXCursor, # `CXCursor`
): CXLinkageKind {.
    cdecl,
    importc: "clang_getCursorLinkage",
    dynlib: libclang
  .}

type
  CXVisibilityKind* {.pure, size: sizeof(cint).} = enum
    CXVisibility_Invalid
    CXVisibility_Hidden
    CXVisibility_Protected
    CXVisibility_Default

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

type
  CXLanguageKind* {.pure, size: sizeof(cint).} = enum
    CXLanguage_Invalid = 0
    CXLanguage_C
    CXLanguage_ObjC
    CXLanguage_CPlusPlus

proc clang_getCursorLanguage*(
  cursor: CXCursor, # `CXCursor`
): CXLanguageKind {.
    cdecl,
    importc: "clang_getCursorLanguage",
    dynlib: libclang
  .}

type
  CXTLSKind* {.pure, size: sizeof(cint).} = enum
    CXTLS_None = 0
    CXTLS_Dynamic
    CXTLS_Static

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

type
  CXCursorSetImpl* {.pure, bycopy.} = object

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
  CXTypeKind* {.pure, size: sizeof(cint).} = enum
    CXType_Invalid = 0
    CXType_Unexposed = 1
    CXType_Void = 2
    CXType_Bool = 3
    CXType_Char_U = 4
    CXType_UChar = 5
    CXType_Char16 = 6
    CXType_Char32 = 7
    CXType_UShort = 8
    CXType_UInt = 9
    CXType_ULong = 10
    CXType_ULongLong = 11
    CXType_UInt128 = 12
    CXType_Char_S = 13
    CXType_SChar = 14
    CXType_WChar = 15
    CXType_Short = 16
    CXType_Int = 17
    CXType_Long = 18
    CXType_LongLong = 19
    CXType_Int128 = 20
    CXType_Float = 21
    CXType_Double = 22
    CXType_LongDouble = 23
    CXType_NullPtr = 24
    CXType_Overload = 25
    CXType_Dependent = 26
    CXType_ObjCId = 27
    CXType_ObjCClass = 28
    CXType_ObjCSel = 29
    CXType_Float128 = 30
    CXType_Half = 31
    CXType_Float16 = 32
    CXType_ShortAccum = 33
    CXType_Accum = 34
    CXType_LongAccum = 35
    CXType_UShortAccum = 36
    CXType_UAccum = 37
    CXType_ULongAccum = 38
    CXType_FirstBuiltin
    CXType_LastBuiltin
    CXType_Complex = 100
    CXType_Pointer = 101
    CXType_BlockPointer = 102
    CXType_LValueReference = 103
    CXType_RValueReference = 104
    CXType_Record = 105
    CXType_Enum = 106
    CXType_Typedef = 107
    CXType_ObjCInterface = 108
    CXType_ObjCObjectPointer = 109
    CXType_FunctionNoProto = 110
    CXType_FunctionProto = 111
    CXType_ConstantArray = 112
    CXType_Vector = 113
    CXType_IncompleteArray = 114
    CXType_VariableArray = 115
    CXType_DependentSizedArray = 116
    CXType_MemberPointer = 117
    CXType_Auto = 118
    CXType_Elaborated = 119
    CXType_Pipe = 120
    CXType_OCLImage1dRO = 121
    CXType_OCLImage1dArrayRO = 122
    CXType_OCLImage1dBufferRO = 123
    CXType_OCLImage2dRO = 124
    CXType_OCLImage2dArrayRO = 125
    CXType_OCLImage2dDepthRO = 126
    CXType_OCLImage2dArrayDepthRO = 127
    CXType_OCLImage2dMSAARO = 128
    CXType_OCLImage2dArrayMSAARO = 129
    CXType_OCLImage2dMSAADepthRO = 130
    CXType_OCLImage2dArrayMSAADepthRO = 131
    CXType_OCLImage3dRO = 132
    CXType_OCLImage1dWO = 133
    CXType_OCLImage1dArrayWO = 134
    CXType_OCLImage1dBufferWO = 135
    CXType_OCLImage2dWO = 136
    CXType_OCLImage2dArrayWO = 137
    CXType_OCLImage2dDepthWO = 138
    CXType_OCLImage2dArrayDepthWO = 139
    CXType_OCLImage2dMSAAWO = 140
    CXType_OCLImage2dArrayMSAAWO = 141
    CXType_OCLImage2dMSAADepthWO = 142
    CXType_OCLImage2dArrayMSAADepthWO = 143
    CXType_OCLImage3dWO = 144
    CXType_OCLImage1dRW = 145
    CXType_OCLImage1dArrayRW = 146
    CXType_OCLImage1dBufferRW = 147
    CXType_OCLImage2dRW = 148
    CXType_OCLImage2dArrayRW = 149
    CXType_OCLImage2dDepthRW = 150
    CXType_OCLImage2dArrayDepthRW = 151
    CXType_OCLImage2dMSAARW = 152
    CXType_OCLImage2dArrayMSAARW = 153
    CXType_OCLImage2dMSAADepthRW = 154
    CXType_OCLImage2dArrayMSAADepthRW = 155
    CXType_OCLImage3dRW = 156
    CXType_OCLSampler = 157
    CXType_OCLEvent = 158
    CXType_OCLQueue = 159
    CXType_OCLReserveID = 160
    CXType_ObjCObject = 161
    CXType_ObjCTypeParam = 162
    CXType_Attributed = 163
    CXType_OCLIntelSubgroupAVCMcePayload = 164
    CXType_OCLIntelSubgroupAVCImePayload = 165
    CXType_OCLIntelSubgroupAVCRefPayload = 166
    CXType_OCLIntelSubgroupAVCSicPayload = 167
    CXType_OCLIntelSubgroupAVCMceResult = 168
    CXType_OCLIntelSubgroupAVCImeResult = 169
    CXType_OCLIntelSubgroupAVCRefResult = 170
    CXType_OCLIntelSubgroupAVCSicResult = 171
    CXType_OCLIntelSubgroupAVCImeResultSingleRefStreamout = 172
    CXType_OCLIntelSubgroupAVCImeResultDualRefStreamout = 173
    CXType_OCLIntelSubgroupAVCImeSingleRefStreamin = 174
    CXType_OCLIntelSubgroupAVCImeDualRefStreamin = 175
    CXType_ExtVector = 176

type
  CXCallingConv* {.pure, size: sizeof(cint).} = enum
    CXCallingConv_Default = 0
    CXCallingConv_C = 1
    CXCallingConv_X86StdCall = 2
    CXCallingConv_X86FastCall = 3
    CXCallingConv_X86ThisCall = 4
    CXCallingConv_X86Pascal = 5
    CXCallingConv_AAPCS = 6
    CXCallingConv_AAPCS_VFP = 7
    CXCallingConv_X86RegCall = 8
    CXCallingConv_IntelOclBicc = 9
    CXCallingConv_Win64 = 10
    CXCallingConv_X86_64Win64
    # CXCallingConv_X86_64SysV = 11
    CXCallingConv_X86VectorCall = 12
    CXCallingConv_Swift = 13
    CXCallingConv_PreserveMost = 14
    CXCallingConv_PreserveAll = 15
    CXCallingConv_AArch64VectorCall = 16
    CXCallingConv_Invalid = 100
    CXCallingConv_Unexposed = 200

type
  CXType* {.pure, bycopy.} = object
    kind*: CXTypeKind # `enum CXTypeKind`
    data*: array[2, pointer] # `void *[2]`

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

type
  CXTemplateArgumentKind* {.pure, size: sizeof(cint).} = enum
    CXTemplateArgumentKind_Null
    CXTemplateArgumentKind_Type
    CXTemplateArgumentKind_Declaration
    CXTemplateArgumentKind_NullPtr
    CXTemplateArgumentKind_Integral
    CXTemplateArgumentKind_Template
    CXTemplateArgumentKind_TemplateExpansion
    CXTemplateArgumentKind_Expression
    CXTemplateArgumentKind_Pack
    CXTemplateArgumentKind_Invalid

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
  ctype: CXType, # `CXType`
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

type
  CXTypeNullabilityKind* {.pure, size: sizeof(cint).} = enum
    CXTypeNullability_NonNull = 0
    CXTypeNullability_Nullable = 1
    CXTypeNullability_Unspecified = 2
    CXTypeNullability_Invalid = 3

proc clang_Type_getNullability*(
  t: CXType, # `CXType`
): CXTypeNullabilityKind {.
    cdecl,
    importc: "clang_Type_getNullability",
    dynlib: libclang
  .}

type
  CXTypeLayoutError* {.pure, size: sizeof(cint).} = enum
    CXTypeLayoutError_Invalid
    CXTypeLayoutError_Incomplete
    CXTypeLayoutError_Dependent
    CXTypeLayoutError_NotConstantSize
    CXTypeLayoutError_InvalidFieldName
    CXTypeLayoutError_Undeduced

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

type
  CXRefQualifierKind* {.pure, size: sizeof(cint).} = enum
    CXRefQualifier_None = 0
    CXRefQualifier_LValue
    CXRefQualifier_RValue

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

type
  CX_CXXAccessSpecifier* {.pure, size: sizeof(cint).} = enum
    CX_CXXInvalidAccessSpecifier
    CX_CXXPublic
    CX_CXXProtected
    CX_CXXPrivate

proc clang_getCXXAccessSpecifier*(
  arg_1: CXCursor, # `CXCursor`
): CX_CXXAccessSpecifier {.
    cdecl,
    importc: "clang_getCXXAccessSpecifier",
    dynlib: libclang
  .}

type
  CX_StorageClass* {.pure, size: sizeof(cint).} = enum
    CX_SC_Invalid
    CX_SC_None
    CX_SC_Extern
    CX_SC_Static
    CX_SC_PrivateExtern
    CX_SC_OpenCLWorkGroupLocal
    CX_SC_Auto
    CX_SC_Register

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

type
  CXChildVisitResult* {.pure, size: sizeof(cint).} = enum
    CXChildVisit_Break
    CXChildVisit_Continue
    CXChildVisit_Recurse

type CXCursorVisitor* = distinct proc(a0: CXCursor, a1: CXCursor, a2: pointer): CXChildVisitResult {.cdecl.} # CXCursorVisitor

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

type
  CXPrintingPolicyProperty* {.pure, size: sizeof(cint).} = enum
    CXPrintingPolicy_Indentation
    CXPrintingPolicy_SuppressSpecifiers
    CXPrintingPolicy_SuppressTagKeyword
    CXPrintingPolicy_IncludeTagDefinition
    CXPrintingPolicy_SuppressScope
    CXPrintingPolicy_SuppressUnwrittenScope
    CXPrintingPolicy_SuppressInitializers
    CXPrintingPolicy_ConstantArraySizeAsWritten
    CXPrintingPolicy_AnonymousTagLocations
    CXPrintingPolicy_SuppressStrongLifetime
    CXPrintingPolicy_SuppressLifetimeQualifiers
    CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors
    CXPrintingPolicy_Bool
    CXPrintingPolicy_Restrict
    CXPrintingPolicy_Alignof
    CXPrintingPolicy_UnderscoreAlignof
    CXPrintingPolicy_UseVoidForZeroParams
    CXPrintingPolicy_TerseOutput
    CXPrintingPolicy_PolishForDeclaration
    CXPrintingPolicy_Half
    CXPrintingPolicy_MSWChar
    CXPrintingPolicy_IncludeNewlines
    CXPrintingPolicy_MSVCFormatting
    CXPrintingPolicy_ConstantsAsWritten
    CXPrintingPolicy_SuppressImplicitBase
    CXPrintingPolicy_FullyQualifiedName
    CXPrintingPolicy_LastProperty

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
  CXNameRefFlags* {.pure, size: sizeof(cint).} = enum
    CXNameRange_WantQualifier = 1
    CXNameRange_WantTemplateArgs = 2
    CXNameRange_WantSinglePiece = 4

type
  CXTokenKind* {.pure, size: sizeof(cint).} = enum
    CXToken_Punctuation
    CXToken_Keyword
    CXToken_Identifier
    CXToken_Literal
    CXToken_Comment

type
  CXToken* {.pure, bycopy.} = object
    int_data*: array[4, cuint] # `unsigned int [4]`
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

type
  CXCompletionChunkKind* {.pure, size: sizeof(cint).} = enum
    CXCompletionChunk_Optional
    CXCompletionChunk_TypedText
    CXCompletionChunk_Text
    CXCompletionChunk_Placeholder
    CXCompletionChunk_Informative
    CXCompletionChunk_CurrentParameter
    CXCompletionChunk_LeftParen
    CXCompletionChunk_RightParen
    CXCompletionChunk_LeftBracket
    CXCompletionChunk_RightBracket
    CXCompletionChunk_LeftBrace
    CXCompletionChunk_RightBrace
    CXCompletionChunk_LeftAngle
    CXCompletionChunk_RightAngle
    CXCompletionChunk_Comma
    CXCompletionChunk_ResultType
    CXCompletionChunk_Colon
    CXCompletionChunk_SemiColon
    CXCompletionChunk_Equal
    CXCompletionChunk_HorizontalSpace
    CXCompletionChunk_VerticalSpace

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

type
  CXCodeComplete_Flags* {.pure, size: sizeof(cint).} = enum
    CXCodeComplete_IncludeMacros = 1
    CXCodeComplete_IncludeCodePatterns = 2
    CXCodeComplete_IncludeBriefComments = 4
    CXCodeComplete_SkipPreamble = 8
    CXCodeComplete_IncludeCompletionsWithFixIts = 16

type
  CXCompletionContext* {.pure, size: sizeof(cint).} = enum
    CXCompletionContext_Unexposed = 0
    CXCompletionContext_AnyType = 1
    CXCompletionContext_AnyValue = 2
    CXCompletionContext_ObjCObjectValue = 4
    CXCompletionContext_ObjCSelectorValue = 8
    CXCompletionContext_CXXClassTypeValue = 16
    CXCompletionContext_DotMemberAccess = 32
    CXCompletionContext_ArrowMemberAccess = 64
    CXCompletionContext_ObjCPropertyAccess = 128
    CXCompletionContext_EnumTag = 256
    CXCompletionContext_UnionTag = 512
    CXCompletionContext_StructTag = 1024
    CXCompletionContext_ClassTag = 2048
    CXCompletionContext_Namespace = 4096
    CXCompletionContext_NestedNameSpecifier = 8192
    CXCompletionContext_ObjCInterface = 16384
    CXCompletionContext_ObjCProtocol = 32768
    CXCompletionContext_ObjCCategory = 65536
    CXCompletionContext_ObjCInstanceMessage = 131072
    CXCompletionContext_ObjCClassMessage = 262144
    CXCompletionContext_ObjCSelectorName = 524288
    CXCompletionContext_MacroName = 1048576
    CXCompletionContext_NaturalLanguage = 2097152
    CXCompletionContext_IncludedFile = 4194304
    CXCompletionContext_Unknown

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
    CXEval_UnExposed = 0
    CXEval_Int = 1
    CXEval_Float = 2
    CXEval_ObjCStrLiteral = 3
    CXEval_StrLiteral = 4
    CXEval_CFStr = 5
    CXEval_Other = 6
    

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
  CXVisitorResult* {.pure, size: sizeof(cint).} = enum
    CXVisit_Break
    CXVisit_Continue

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
    ptr_data*: array[2, pointer] # `void *[2]`
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
    attributes*: ptr[ptr[CXIdxAttrInfo]] # `const CXIdxAttrInfo *const *`
    numAttributes*: cuint # `unsigned int`

type
  CXIdxContainerInfo* {.pure, bycopy.} = object
    cursor*: CXCursor # `CXCursor`

type
  CXIdxIBOutletCollectionAttrInfo* {.pure, bycopy.} = object
    attrInfo*: ptr[CXIdxAttrInfo] # `const CXIdxAttrInfo *`
    objcClass*: ptr[CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    classCursor*: CXCursor # `CXCursor`
    classLoc*: CXIdxLoc # `CXIdxLoc`

type
  CXIdxDeclInfoFlags* {.pure, size: sizeof(cint).} = enum
    CXIdxDeclFlag_Skipped = 1

type
  CXIdxDeclInfo* {.pure, bycopy.} = object
    entityInfo*: ptr[CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    cursor*: CXCursor # `CXCursor`
    loc*: CXIdxLoc # `CXIdxLoc`
    semanticContainer*: ptr[CXIdxContainerInfo] # `const CXIdxContainerInfo *`
    lexicalContainer*: ptr[CXIdxContainerInfo] # `const CXIdxContainerInfo *`
    isRedeclaration*: cint # `int`
    isDefinition*: cint # `int`
    isContainer*: cint # `int`
    declAsContainer*: ptr[CXIdxContainerInfo] # `const CXIdxContainerInfo *`
    isImplicit*: cint # `int`
    attributes*: ptr[ptr[CXIdxAttrInfo]] # `const CXIdxAttrInfo *const *`
    numAttributes*: cuint # `unsigned int`
    flags*: cuint # `unsigned int`

type
  CXIdxObjCContainerKind* {.pure, size: sizeof(cint).} = enum
    CXIdxObjCContainer_ForwardRef = 0
    CXIdxObjCContainer_Interface = 1
    CXIdxObjCContainer_Implementation = 2

type
  CXIdxObjCContainerDeclInfo* {.pure, bycopy.} = object
    declInfo*: ptr[CXIdxDeclInfo] # `const CXIdxDeclInfo *`
    kind*: CXIdxObjCContainerKind # `CXIdxObjCContainerKind`

type
  CXIdxBaseClassInfo* {.pure, bycopy.} = object
    base*: ptr[CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    cursor*: CXCursor # `CXCursor`
    loc*: CXIdxLoc # `CXIdxLoc`

type
  CXIdxObjCProtocolRefInfo* {.pure, bycopy.} = object
    protocol*: ptr[CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    cursor*: CXCursor # `CXCursor`
    loc*: CXIdxLoc # `CXIdxLoc`

type
  CXIdxObjCProtocolRefListInfo* {.pure, bycopy.} = object
    protocols*: ptr[ptr[CXIdxObjCProtocolRefInfo]] # `const CXIdxObjCProtocolRefInfo *const *`
    numProtocols*: cuint # `unsigned int`

type
  CXIdxObjCInterfaceDeclInfo* {.pure, bycopy.} = object
    containerInfo*: ptr[CXIdxObjCContainerDeclInfo] # `const CXIdxObjCContainerDeclInfo *`
    superInfo*: ptr[CXIdxBaseClassInfo] # `const CXIdxBaseClassInfo *`
    protocols*: ptr[CXIdxObjCProtocolRefListInfo] # `const CXIdxObjCProtocolRefListInfo *`

type
  CXIdxObjCCategoryDeclInfo* {.pure, bycopy.} = object
    containerInfo*: ptr[CXIdxObjCContainerDeclInfo] # `const CXIdxObjCContainerDeclInfo *`
    objcClass*: ptr[CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    classCursor*: CXCursor # `CXCursor`
    classLoc*: CXIdxLoc # `CXIdxLoc`
    protocols*: ptr[CXIdxObjCProtocolRefListInfo] # `const CXIdxObjCProtocolRefListInfo *`

type
  CXIdxObjCPropertyDeclInfo* {.pure, bycopy.} = object
    declInfo*: ptr[CXIdxDeclInfo] # `const CXIdxDeclInfo *`
    getter*: ptr[CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    setter*: ptr[CXIdxEntityInfo] # `const CXIdxEntityInfo *`

type
  CXIdxCXXClassDeclInfo* {.pure, bycopy.} = object
    declInfo*: ptr[CXIdxDeclInfo] # `const CXIdxDeclInfo *`
    bases*: ptr[ptr[CXIdxBaseClassInfo]] # `const CXIdxBaseClassInfo *const *`
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
    referencedEntity*: ptr[CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    parentEntity*: ptr[CXIdxEntityInfo] # `const CXIdxEntityInfo *`
    container*: ptr[CXIdxContainerInfo] # `const CXIdxContainerInfo *`
    role*: CXSymbolRole # `CXSymbolRole`

type
  IndexerCallbacks* {.pure, bycopy.} = object
    abortQuery*: proc(a0: CXClientData, a1: pointer): cint {.cdecl.} # `int (*)(CXClientData, void *)`
    diagnostic*: proc(a0: CXClientData, a1: CXDiagnosticSet, a2: pointer): void {.cdecl.} # `void (*)(CXClientData, CXDiagnosticSet, void *)`
    enteredMainFile*: proc(a0: CXClientData, a1: CXFile, a2: pointer): CXIdxClientFile {.cdecl.} # `CXIdxClientFile (*)(CXClientData, CXFile, void *)`
    ppIncludedFile*: proc(a0: CXClientData, a1: ptr[CXIdxIncludedFileInfo]): CXIdxClientFile {.cdecl.} # `CXIdxClientFile (*)(CXClientData, const CXIdxIncludedFileInfo *)`
    importedASTFile*: proc(a0: CXClientData, a1: ptr[CXIdxImportedASTFileInfo]): CXIdxClientASTFile {.cdecl.} # `CXIdxClientASTFile (*)(CXClientData, const CXIdxImportedASTFileInfo *)`
    startedTranslationUnit*: proc(a0: CXClientData, a1: pointer): CXIdxClientContainer {.cdecl.} # `CXIdxClientContainer (*)(CXClientData, void *)`
    indexDeclaration*: proc(a0: CXClientData, a1: ptr[CXIdxDeclInfo]): void {.cdecl.} # `void (*)(CXClientData, const CXIdxDeclInfo *)`
    indexEntityReference*: proc(a0: CXClientData, a1: ptr[CXIdxEntityRefInfo]): void {.cdecl.} # `void (*)(CXClientData, const CXIdxEntityRefInfo *)`

proc clang_index_isEntityObjCContainerKind*(
  arg_1: CXIdxEntityKind, # `CXIdxEntityKind`
): cint {.
    cdecl,
    importc: "clang_index_isEntityObjCContainerKind",
    dynlib: libclang
  .}

proc clang_index_getObjCContainerDeclInfo*(
  arg_1: ptr[CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[CXIdxObjCContainerDeclInfo] {.
    cdecl,
    importc: "clang_index_getObjCContainerDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getObjCInterfaceDeclInfo*(
  arg_1: ptr[CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[CXIdxObjCInterfaceDeclInfo] {.
    cdecl,
    importc: "clang_index_getObjCInterfaceDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getObjCCategoryDeclInfo*(
  arg_1: ptr[CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[CXIdxObjCCategoryDeclInfo] {.
    cdecl,
    importc: "clang_index_getObjCCategoryDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getObjCProtocolRefListInfo*(
  arg_1: ptr[CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[CXIdxObjCProtocolRefListInfo] {.
    cdecl,
    importc: "clang_index_getObjCProtocolRefListInfo",
    dynlib: libclang
  .}

proc clang_index_getObjCPropertyDeclInfo*(
  arg_1: ptr[CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[CXIdxObjCPropertyDeclInfo] {.
    cdecl,
    importc: "clang_index_getObjCPropertyDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getIBOutletCollectionAttrInfo*(
  arg_1: ptr[CXIdxAttrInfo], # `const CXIdxAttrInfo *`
): ptr[CXIdxIBOutletCollectionAttrInfo] {.
    cdecl,
    importc: "clang_index_getIBOutletCollectionAttrInfo",
    dynlib: libclang
  .}

proc clang_index_getCXXClassDeclInfo*(
  arg_1: ptr[CXIdxDeclInfo], # `const CXIdxDeclInfo *`
): ptr[CXIdxCXXClassDeclInfo] {.
    cdecl,
    importc: "clang_index_getCXXClassDeclInfo",
    dynlib: libclang
  .}

proc clang_index_getClientContainer*(
  arg_1: ptr[CXIdxContainerInfo], # `const CXIdxContainerInfo *`
): CXIdxClientContainer {.
    cdecl,
    importc: "clang_index_getClientContainer",
    dynlib: libclang
  .}

proc clang_index_setClientContainer*(
  arg_1: ptr[CXIdxContainerInfo], # `const CXIdxContainerInfo *`
  arg_2: CXIdxClientContainer, # `CXIdxClientContainer`
): void {.
    cdecl,
    importc: "clang_index_setClientContainer",
    dynlib: libclang
  .}

proc clang_index_getClientEntity*(
  arg_1: ptr[CXIdxEntityInfo], # `const CXIdxEntityInfo *`
): CXIdxClientEntity {.
    cdecl,
    importc: "clang_index_getClientEntity",
    dynlib: libclang
  .}

proc clang_index_setClientEntity*(
  arg_1: ptr[CXIdxEntityInfo], # `const CXIdxEntityInfo *`
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

type CXFieldVisitor* = distinct proc(a0: CXCursor, a1: pointer): CXVisitorResult {.cdecl.} # CXFieldVisitor

proc clang_Type_visitFields*(
  t: CXType, # `CXType`
  visitor: CXFieldVisitor, # `CXFieldVisitor`
  client_data: CXClientData, # `CXClientData`
): cuint {.
    cdecl,
    importc: "clang_Type_visitFields",
    dynlib: libclang
  .}

