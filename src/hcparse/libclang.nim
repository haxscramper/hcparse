
proc clang_createIndex*(): CXIndex =
  ##  Provides a shared context for creating translation units. It provides two options: - excludeDeclarationsFromPCH: When non-zero, allows enumeration of "local" declarations (when loading any new translation units). A "local" declaration is one that belongs in the translation unit itself and not in a precompiled header that was used by the translation unit. If zero, all declarations will be enumerated. Here is an example: Error: cannot render: rnCodeBlock This process of creating the 'pch', loading it separately, and using it (via -include-pch) allows 'excludeDeclsFromPCH' to remove redundant callbacks (which gives the indexer the same performance benefit as the compiler).
  impl

proc clang_disposeIndex*(): void =
  ##  Destroy the given index. The index must not be destroyed until all of the translation units created within that index have been destroyed.
  impl

proc clang_CXIndex_setGlobalOptions*(): void =
  ##  Sets general options associated with a CXIndex. For example: Error: cannot render: rnCodeBlock ** A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags.
  impl

proc clang_CXIndex_getGlobalOptions*(): cuint =
  ##  Gets the general options associated with a CXIndex. ** A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags that are associated with the given CXIndex object.
  impl

proc clang_CXIndex_setInvocationEmissionPathOption*(): void =
  ##  Sets the invocation emission path option in a CXIndex. The invocation emission path specifies a path which will contain log files for certain libclang invocations. A null value (default) implies that libclang invocations are not logged..
  impl

proc clang_getFileName*(): CXString =
  ##  Retrieve the complete file and path name of the given file.
  impl

proc clang_getFileTime*(): time_t =
  ##  Retrieve the last modification time of the given file.
  impl

proc clang_getFileUniqueID*(): int =
  ##  Retrieve the unique ID for the given Error: cannot render: rnLiteralBlock ** the file to get the ID for. ** stores the returned CXFileUniqueID. ** If there was a failure getting the unique ID, returns non-zero, otherwise returns 0.
  impl

proc clang_isFileMultipleIncludeGuarded*(): cuint =
  ##  Determine whether the given header is guarded against multiple inclusions, either with the conventional #ifndef/#define/#endif macro guards or with #pragma once.
  impl

proc clang_getFile*(): CXFile =
  ##  Retrieve a file handle within the given translation unit. ** the translation unit ** the name of the file. ** the file handle for the named file in the translation unit Error: cannot render: rnLiteralBlock or a NULL file handle if the file was not a part of this translation unit.
  impl

proc clang_getFileContents*(): cstring =
  impl

proc clang_File_isEqual*(): int =
  ##  Returns non-zero if the Error: cannot render: rnLiteralBlock and Error: cannot render: rnLiteralBlock point to the same file, or they are both NULL.
  impl

proc clang_File_tryGetRealPathName*(): CXString =
  ##  Returns the real path name of Error: cannot render: rnLiteralBlock An empty string may be returned. Use Error: cannot render: rnLiteralBlock in that case.
  impl

proc clang_getNullLocation*(): CXSourceLocation =
  ##  Retrieve a NULL (invalid) source location.
  impl

proc clang_equalLocations*(): cuint =
  ##  Determine whether two source locations, which must refer into the same translation unit, refer to exactly the same point in the source code. ** non-zero if the source locations refer to the same location, zero if they refer to different locations.
  impl

proc clang_getLocation*(): CXSourceLocation =
  ##  Retrieves the source location associated with a given file/line/column in a particular translation unit.
  impl

proc clang_getLocationForOffset*(): CXSourceLocation =
  ##  Retrieves the source location associated with a given character offset in a particular translation unit.
  impl

proc clang_Location_isInSystemHeader*(): int =
  ##  Returns non-zero if the given source location is in a system header.
  impl

proc clang_Location_isFromMainFile*(): int =
  ##  Returns non-zero if the given source location is in the main file of the corresponding translation unit.
  impl

proc clang_getNullRange*(): CXSourceRange =
  ##  Retrieve a NULL (invalid) source range.
  impl

proc clang_getRange*(): CXSourceRange =
  ##  Retrieve a source range given the beginning and ending source locations.
  impl

proc clang_equalRanges*(): cuint =
  ##  Determine whether two ranges are equivalent. ** non-zero if the ranges are the same, zero if they differ.
  impl

proc clang_Range_isNull*(): int =
  ##  Returns non-zero if Error: cannot render: rnLiteralBlock is null.
  impl

proc clang_getExpansionLocation*(): void =
  ##  Retrieve the file, line, column, and offset represented by the given source location. If the location refers into a macro expansion, retrieves the location of the macro expansion. ** the location within a source file that will be decomposed into its parts. ** [out] if non-NULL, will be set to the file to which the given source location points. ** [out] if non-NULL, will be set to the line to which the given source location points. ** [out] if non-NULL, will be set to the column to which the given source location points. ** [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
  impl

proc clang_getPresumedLocation*(): void =
  ##  Retrieve the file, line and column represented by the given source location, as specified in a # line directive. Example: given the following source code in a file somefile.c Error: cannot render: rnCodeBlock the location information returned by this function would be File: dummy.c Line: 124 Column: 12 whereas clang_getExpansionLocation would have returned File: somefile.c Line: 3 Column: 12 ** the location within a source file that will be decomposed into its parts. ** [out] if non-NULL, will be set to the filename of the source location. Note that filenames returned will be for "virtual" files, which don't necessarily exist on the machine running clang - e.g. when parsing preprocessed output obtained from a different environment. If a non-NULL value is passed in, remember to dispose of the returned value using Error: cannot render: rnLiteralBlock once you've finished with it. For an invalid source location, an empty string is returned. ** [out] if non-NULL, will be set to the line number of the source location. For an invalid source location, zero is returned. ** [out] if non-NULL, will be set to the column number of the source location. For an invalid source location, zero is returned.
  impl

proc clang_getInstantiationLocation*(): void =
  ##  Legacy API to retrieve the file, line, column, and offset represented by the given source location. This interface has been replaced by the newer interface #clang_getExpansionLocation(). See that interface's documentation for details.
  impl

proc clang_getSpellingLocation*(): void =
  ##  Retrieve the file, line, column, and offset represented by the given source location. If the location refers into a macro instantiation, return where the location was originally spelled in the source file. ** the location within a source file that will be decomposed into its parts. ** [out] if non-NULL, will be set to the file to which the given source location points. ** [out] if non-NULL, will be set to the line to which the given source location points. ** [out] if non-NULL, will be set to the column to which the given source location points. ** [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
  impl

proc clang_getFileLocation*(): void =
  ##  Retrieve the file, line, column, and offset represented by the given source location. If the location refers into a macro expansion, return where the macro was expanded or where the macro argument was written, if the location points at a macro argument. ** the location within a source file that will be decomposed into its parts. ** [out] if non-NULL, will be set to the file to which the given source location points. ** [out] if non-NULL, will be set to the line to which the given source location points. ** [out] if non-NULL, will be set to the column to which the given source location points. ** [out] if non-NULL, will be set to the offset into the buffer to which the given source location points.
  impl

proc clang_getRangeStart*(): CXSourceLocation =
  ##  Retrieve a source location representing the first character within a source range.
  impl

proc clang_getRangeEnd*(): CXSourceLocation =
  ##  Retrieve a source location representing the last character within a source range.
  impl

proc clang_getSkippedRanges*(): ptr[CXSourceRangeList] =
  ##  Retrieve all ranges that were skipped by the preprocessor. The preprocessor will skip lines when they are surrounded by an if/ifdef/ifndef directive whose condition does not evaluate to true.
  impl

proc clang_getAllSkippedRanges*(): ptr[CXSourceRangeList] =
  ##  Retrieve all ranges from all files that were skipped by the preprocessor. The preprocessor will skip lines when they are surrounded by an if/ifdef/ifndef directive whose condition does not evaluate to true.
  impl

proc clang_disposeSourceRangeList*(): void =
  ##  Destroy the given Error: cannot render: rnLiteralBlock 
  impl

proc clang_getNumDiagnosticsInSet*(): cuint =
  ##  Determine the number of diagnostics in a CXDiagnosticSet.
  impl

proc clang_getDiagnosticInSet*(): CXDiagnostic =
  ##  Retrieve a diagnostic associated with the given CXDiagnosticSet. ** the CXDiagnosticSet to query. ** the zero-based diagnostic number to retrieve. ** the requested diagnostic. This diagnostic must be freed via a call to Error: cannot render: rnLiteralBlock 
  impl

proc clang_loadDiagnostics*(): CXDiagnosticSet =
  ##  Deserialize a set of diagnostics from a Clang diagnostics bitcode file. ** The name of the file to deserialize. ** A pointer to a enum value recording if there was a problem        deserializing the diagnostics. ** A pointer to a CXString for recording the error string        if the file was not successfully loaded. ** A loaded CXDiagnosticSet if successful, and NULL otherwise.  These diagnostics should be released using clang_disposeDiagnosticSet().
  impl

proc clang_disposeDiagnosticSet*(): void =
  ##  Release a CXDiagnosticSet and all of its contained diagnostics.
  impl

proc clang_getChildDiagnostics*(): CXDiagnosticSet =
  ##  Retrieve the child diagnostics of a CXDiagnostic. This CXDiagnosticSet does not need to be released by clang_disposeDiagnosticSet.
  impl

proc clang_getNumDiagnostics*(): cuint =
  ##  Determine the number of diagnostics produced for the given translation unit.
  impl

proc clang_getDiagnostic*(): CXDiagnostic =
  ##  Retrieve a diagnostic associated with the given translation unit. ** the translation unit to query. ** the zero-based diagnostic number to retrieve. ** the requested diagnostic. This diagnostic must be freed via a call to Error: cannot render: rnLiteralBlock 
  impl

proc clang_getDiagnosticSetFromTU*(): CXDiagnosticSet =
  ##  Retrieve the complete set of diagnostics associated with a        translation unit. ** the translation unit to query.
  impl

proc clang_disposeDiagnostic*(): void =
  ##  Destroy a diagnostic.
  impl

proc clang_formatDiagnostic*(): CXString =
  ##  Format the given diagnostic in a manner that is suitable for display. This routine will format the given diagnostic to a string, rendering the diagnostic according to the various options given. The Error: cannot render: rnLiteralBlock function returns the set of options that most closely mimics the behavior of the clang compiler. ** The diagnostic to print. ** A set of options that control the diagnostic display, created by combining Error: cannot render: rnLiteralBlock values. ** A new string containing for formatted diagnostic.
  impl

proc clang_defaultDiagnosticDisplayOptions*(): cuint =
  ##  Retrieve the set of display options most similar to the default behavior of the clang compiler. ** A set of display options suitable for use with Error: cannot render: rnLiteralBlock 
  impl

proc clang_getDiagnosticSeverity*(): CXDiagnosticSeverity =
  ##  Determine the severity of the given diagnostic.
  impl

proc clang_getDiagnosticLocation*(): CXSourceLocation =
  ##  Retrieve the source location of the given diagnostic. This location is where Clang would print the caret ('^') when displaying the diagnostic on the command line.
  impl

proc clang_getDiagnosticSpelling*(): CXString =
  ##  Retrieve the text of the given diagnostic.
  impl

proc clang_getDiagnosticOption*(): CXString =
  ##  Retrieve the name of the command-line option that enabled this diagnostic. ** The diagnostic to be queried. ** If non-NULL, will be set to the option that disables this diagnostic (if any). ** A string that contains the command-line option used to enable this warning, such as "-Wconversion" or "-pedantic".
  impl

proc clang_getDiagnosticCategory*(): cuint =
  ##  Retrieve the category number for this diagnostic. Diagnostics can be categorized into groups along with other, related diagnostics (e.g., diagnostics under the same warning flag). This routine retrieves the category number for the given diagnostic. ** The number of the category that contains this diagnostic, or zero if this diagnostic is uncategorized.
  impl

proc clang_getDiagnosticCategoryName*(): CXString =
  ##  Retrieve the name of a particular diagnostic category.  This  is now deprecated.  Use clang_getDiagnosticCategoryText()  instead. ** A diagnostic category number, as returned by Error: cannot render: rnLiteralBlock ** The name of the given diagnostic category.
  impl

proc clang_getDiagnosticCategoryText*(): CXString =
  ##  Retrieve the diagnostic category text for a given diagnostic. ** The text of the given diagnostic category.
  impl

proc clang_getDiagnosticNumRanges*(): cuint =
  ##  Determine the number of source ranges associated with the given diagnostic.
  impl

proc clang_getDiagnosticRange*(): CXSourceRange =
  ##  Retrieve a source range associated with the diagnostic. A diagnostic's source ranges highlight important elements in the source code. On the command line, Clang displays source ranges by underlining them with '~' characters. ** the diagnostic whose range is being extracted. ** the zero-based index specifying which range to ** the requested source range.
  impl

proc clang_getDiagnosticNumFixIts*(): cuint =
  ##  Determine the number of fix-it hints associated with the given diagnostic.
  impl

proc clang_getDiagnosticFixIt*(): CXString =
  ##  Retrieve the replacement information for a given fix-it. Fix-its are described in terms of a source range whose contents should be replaced by a string. This approach generalizes over three kinds of operations: removal of source code (the range covers the code to be removed and the replacement string is empty), replacement of source code (the range covers the code to be replaced and the replacement string provides the new code), and insertion (both the start and end of the range point at the insertion location, and the replacement string provides the text to insert). ** The diagnostic whose fix-its are being queried. ** The zero-based index of the fix-it. ** The source range whose contents will be replaced with the returned replacement string. Note that source ranges are half-open ranges [a, b), so the source code should be replaced from a and up to (but not including) b. ** A string containing text that should be replace the source code indicated by the Error: cannot render: rnLiteralBlock 
  impl

proc clang_getTranslationUnitSpelling*(): CXString =
  ##  Get the original translation unit source file name.
  impl

proc clang_createTranslationUnitFromSourceFile*(): CXTranslationUnit =
  ##  Return the CXTranslationUnit for a given source file and the provided command line arguments one would pass to the compiler. Note: The 'source_filename' argument is optional.  If the caller provides a NULL pointer, the name of the source file is expected to reside in the specified command line arguments. Note: When encountered in 'clang_command_line_args', the following options are ignored:   '-c'   '-emit-ast'   '-fsyntax-only'   '-o <output file>'  (both '-o' and '<output file>' are ignored) ** The index object with which the translation unit will be associated. ** The name of the source file to load, or NULL if the source file is included in Error: cannot render: rnLiteralBlock ** The number of command-line arguments in Error: cannot render: rnLiteralBlock ** The command-line arguments that would be passed to the Error: cannot render: rnLiteralBlock executable if it were being invoked out-of-process. These command-line options will be parsed and will affect how the translation unit is parsed. Note that the following options are ignored: '-c', '-emit-ast', '-fsyntax-only' (which is the default), and '-o <output file>'. ** the number of unsaved file entries in Error: cannot render: rnLiteralBlock ** the files that have not yet been saved to disk but may be required for code completion, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns.
  impl

proc clang_createTranslationUnit*(): CXTranslationUnit =
  ##  Same as Error: cannot render: rnLiteralBlock but returns the Error: cannot render: rnLiteralBlock instead of an error code.  In case of an error this routine returns a Error: cannot render: rnLiteralBlock Error: cannot render: rnLiteralBlock without further detailed error codes.
  impl

proc clang_createTranslationUnit2*(): CXErrorCode =
  ##  Create a translation unit from an AST file (Error: cannot render: rnLiteralBlock ** A non-NULL pointer to store the created Error: cannot render: rnLiteralBlock ** Zero on success, otherwise returns an error code.
  impl

proc clang_defaultEditingTranslationUnitOptions*(): cuint =
  ##  Returns the set of flags that is suitable for parsing a translation unit that is being edited. The set of flags returned provide options for Error: cannot render: rnLiteralBlock to indicate that the translation unit is likely to be reparsed many times, either explicitly (via Error: cannot render: rnLiteralBlock or implicitly (e.g., by code completion (Error: cannot render: rnLiteralBlock The returned flag set contains an unspecified set of optimizations (e.g., the precompiled preamble) geared toward improving the performance of these routines. The set of optimizations enabled may change from one version to the next.
  impl

proc clang_parseTranslationUnit*(): CXTranslationUnit =
  ##  Same as Error: cannot render: rnLiteralBlock but returns the Error: cannot render: rnLiteralBlock instead of an error code.  In case of an error this routine returns a Error: cannot render: rnLiteralBlock Error: cannot render: rnLiteralBlock without further detailed error codes.
  impl

proc clang_parseTranslationUnit2*(): CXErrorCode =
  ##  Parse the given source file and the translation unit corresponding to that file. This routine is the main entry point for the Clang C API, providing the ability to parse a source file into a translation unit that can then be queried by other functions in the API. This routine accepts a set of command-line arguments so that the compilation can be configured in the same way that the compiler is configured on the command line. ** The index object with which the translation unit will be associated. ** The name of the source file to load, or NULL if the source file is included in Error: cannot render: rnLiteralBlock ** The command-line arguments that would be passed to the Error: cannot render: rnLiteralBlock executable if it were being invoked out-of-process. These command-line options will be parsed and will affect how the translation unit is parsed. Note that the following options are ignored: '-c', '-emit-ast', '-fsyntax-only' (which is the default), and '-o <output file>'. ** The number of command-line arguments in Error: cannot render: rnLiteralBlock ** the files that have not yet been saved to disk but may be required for parsing, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns. ** the number of unsaved file entries in Error: cannot render: rnLiteralBlock ** A bitmask of options that affects how the translation unit is managed but not its compilation. This should be a bitwise OR of the CXTranslationUnit_XXX flags. ** A non-NULL pointer to store the created Error: cannot render: rnLiteralBlock describing the parsed code and containing any diagnostics produced by the compiler. ** Zero on success, otherwise returns an error code.
  impl

proc clang_parseTranslationUnit2FullArgv*(): CXErrorCode =
  ##  Same as clang_parseTranslationUnit2 but requires a full command line for Error: cannot render: rnLiteralBlock including argv[0]. This is useful if the standard library paths are relative to the binary.
  impl

proc clang_defaultSaveOptions*(): cuint =
  ##  Returns the set of flags that is suitable for saving a translation unit. The set of flags returned provide options for Error: cannot render: rnLiteralBlock by default. The returned flag set contains an unspecified set of options that save translation units with the most commonly-requested data.
  impl

proc clang_saveTranslationUnit*(): int =
  ##  Saves a translation unit into a serialized representation of that translation unit on disk. Any translation unit that was parsed without error can be saved into a file. The translation unit can then be deserialized into a new Error: cannot render: rnLiteralBlock with Error: cannot render: rnLiteralBlock or, if it is an incomplete translation unit that corresponds to a header, used as a precompiled header when parsing other translation units. ** The translation unit to save. ** The file to which the translation unit will be saved. ** A bitmask of options that affects how the translation unit is saved. This should be a bitwise OR of the CXSaveTranslationUnit_XXX flags. ** A value that will match one of the enumerators of the CXSaveError enumeration. Zero (CXSaveError_None) indicates that the translation unit was saved successfully, while a non-zero value indicates that a problem occurred.
  impl

proc clang_suspendTranslationUnit*(): cuint =
  ##  Suspend a translation unit in order to free memory associated with it. A suspended translation unit uses significantly less memory but on the other side does not support any other calls than Error: cannot render: rnLiteralBlock to resume it or Error: cannot render: rnLiteralBlock to dispose it completely.
  impl

proc clang_disposeTranslationUnit*(): void =
  ##  Destroy the specified CXTranslationUnit object.
  impl

proc clang_defaultReparseOptions*(): cuint =
  ##  Returns the set of flags that is suitable for reparsing a translation unit. The set of flags returned provide options for Error: cannot render: rnLiteralBlock by default. The returned flag set contains an unspecified set of optimizations geared toward common uses of reparsing. The set of optimizations enabled may change from one version to the next.
  impl

proc clang_reparseTranslationUnit*(): int =
  ##  Reparse the source files that produced this translation unit. This routine can be used to re-parse the source files that originally created the given translation unit, for example because those source files have changed (either on disk or as passed via Error: cannot render: rnLiteralBlock The source code will be reparsed with the same command-line options as it was originally parsed. Reparsing a translation unit invalidates all cursors and source locations that refer into that translation unit. This makes reparsing a translation unit semantically equivalent to destroying the translation unit and then creating a new translation unit with the same command-line arguments. However, it may be more efficient to reparse a translation unit using this routine. ** The translation unit whose contents will be re-parsed. The translation unit must originally have been built with Error: cannot render: rnLiteralBlock ** The number of unsaved file entries in Error: cannot render: rnLiteralBlock ** The files that have not yet been saved to disk but may be required for parsing, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns. ** A bitset of options composed of the flags in CXReparse_Flags. The function Error: cannot render: rnLiteralBlock produces a default set of options recommended for most uses, based on the translation unit. ** 0 if the sources could be reparsed.  A non-zero error code will be returned if reparsing was impossible, such that the translation unit is invalid. In such cases, the only valid call for Error: cannot render: rnLiteralBlock is Error: cannot render: rnLiteralBlock  The error codes returned by this routine are described by the Error: cannot render: rnLiteralBlock enum.
  impl

proc clang_getTUResourceUsageName*(): cstring =
  ##  Returns the human-readable null-terminated C string that represents  the name of the memory category.  This string should never be freed.
  impl

proc clang_getCXTUResourceUsage*(): CXTUResourceUsage =
  ##  Return the memory usage of a translation unit.  This object  should be released with clang_disposeCXTUResourceUsage().
  impl

proc clang_disposeCXTUResourceUsage*(): void =
  impl

proc clang_getTranslationUnitTargetInfo*(): CXTargetInfo =
  ##  Get target information for this translation unit. The CXTargetInfo object cannot outlive the CXTranslationUnit object.
  impl

proc clang_TargetInfo_dispose*(): void =
  ##  Destroy the CXTargetInfo object.
  impl

proc clang_TargetInfo_getTriple*(): CXString =
  ##  Get the normalized target triple as a string. Returns the empty string in case of any error.
  impl

proc clang_TargetInfo_getPointerWidth*(): int =
  ##  Get the pointer width of the target in bits. Returns -1 in case of error.
  impl

proc clang_getNullCursor*(): CXCursor =
  ##  Retrieve the NULL cursor, which represents no entity.
  impl

proc clang_getTranslationUnitCursor*(): CXCursor =
  ##  Retrieve the cursor that represents the given translation unit. The translation unit cursor can be used to start traversing the various declarations within the given translation unit.
  impl

proc clang_equalCursors*(): cuint =
  ##  Determine whether two cursors are equivalent.
  impl

proc clang_Cursor_isNull*(): int =
  ##  Returns non-zero if Error: cannot render: rnLiteralBlock is null.
  impl

proc clang_hashCursor*(): cuint =
  ##  Compute a hash value for the given cursor.
  impl

proc clang_getCursorKind*(): CXCursorKind =
  ##  Retrieve the kind of the given cursor.
  impl

proc clang_isDeclaration*(): cuint =
  ##  Determine whether the given cursor kind represents a declaration.
  impl

proc clang_isInvalidDeclaration*(): cuint =
  ##  Determine whether the given declaration is invalid. A declaration is invalid if it could not be parsed successfully. ** non-zero if the cursor represents a declaration and it is invalid, otherwise NULL.
  impl

proc clang_isReference*(): cuint =
  ##  Determine whether the given cursor kind represents a simple reference. Note that other kinds of cursors (such as expressions) can also refer to other cursors. Use clang_getCursorReferenced() to determine whether a particular cursor refers to another entity.
  impl

proc clang_isExpression*(): cuint =
  ##  Determine whether the given cursor kind represents an expression.
  impl

proc clang_isStatement*(): cuint =
  ##  Determine whether the given cursor kind represents a statement.
  impl

proc clang_isAttribute*(): cuint =
  ##  Determine whether the given cursor kind represents an attribute.
  impl

proc clang_Cursor_hasAttrs*(): cuint =
  ##  Determine whether the given cursor has any attributes.
  impl

proc clang_isInvalid*(): cuint =
  ##  Determine whether the given cursor kind represents an invalid cursor.
  impl

proc clang_isTranslationUnit*(): cuint =
  ##  Determine whether the given cursor kind represents a translation unit.
  impl

proc clang_isPreprocessing*(): cuint =
  ## * Determine whether the given cursor represents a preprocessing element, such as a preprocessor directive or macro instantiation.
  impl

proc clang_isUnexposed*(): cuint =
  ## * Determine whether the given cursor represents a currently  unexposed piece of the AST (e.g., CXCursor_UnexposedStmt).
  impl

proc clang_getCursorLinkage*(): CXLinkageKind =
  ##  Determine the linkage of the entity referred to by a given cursor.
  impl

proc clang_getCursorVisibility*(): CXVisibilityKind =
  ##  Describe the visibility of the entity referred to by a cursor. This returns the default visibility if not explicitly specified by a visibility attribute. The default visibility may be changed by commandline arguments. ** The cursor to query. ** The visibility of the cursor.
  impl

proc clang_getCursorAvailability*(): CXAvailabilityKind =
  ##  Determine the availability of the entity that this cursor refers to, taking the current target platform into account. ** The cursor to query. ** The availability of the cursor.
  impl

proc clang_getCursorPlatformAvailability*(): int =
  ##  Determine the availability of the entity that this cursor refers to on any platforms for which availability information is known. ** The cursor to query. ** If non-NULL, will be set to indicate whether the entity is deprecated on all platforms. ** If non-NULL, will be set to the message text provided along with the unconditional deprecation of this entity. The client is responsible for deallocating this string. ** If non-NULL, will be set to indicate whether the entity is unavailable on all platforms. ** If non-NULL, will be set to the message text provided along with the unconditional unavailability of this entity. The client is responsible for deallocating this string. ** If non-NULL, an array of CXPlatformAvailability instances that will be populated with platform availability information, up to either the number of platforms for which availability information is available (as returned by this function) or Error: cannot render: rnLiteralBlock whichever is smaller. ** The number of elements available in the Error: cannot render: rnLiteralBlock array. ** The number of platforms (N) for which availability information is available (which is unrelated to Error: cannot render: rnLiteralBlock Note that the client is responsible for calling Error: cannot render: rnLiteralBlock to free each of the platform-availability structures returned. There are Error: cannot render: rnLiteralBlock availability_size) such structures.
  impl

proc clang_disposeCXPlatformAvailability*(): void =
  ##  Free the memory associated with a Error: cannot render: rnLiteralBlock structure.
  impl

proc clang_getCursorLanguage*(): CXLanguageKind =
  ##  Determine the "language" of the entity referred to by a given cursor.
  impl

proc clang_getCursorTLSKind*(): CXTLSKind =
  ##  Determine the "thread-local storage (TLS) kind" of the declaration referred to by a cursor.
  impl

proc clang_Cursor_getTranslationUnit*(): CXTranslationUnit =
  ##  Returns the translation unit that a cursor originated from.
  impl

proc clang_createCXCursorSet*(): CXCursorSet =
  ##  Creates an empty CXCursorSet.
  impl

proc clang_disposeCXCursorSet*(): void =
  ##  Disposes a CXCursorSet and releases its associated memory.
  impl

proc clang_CXCursorSet_contains*(): cuint =
  ##  Queries a CXCursorSet to see if it contains a specific CXCursor. ** non-zero if the set contains the specified cursor.
  impl

proc clang_CXCursorSet_insert*(): cuint =
  ##  Inserts a CXCursor into a CXCursorSet. ** zero if the CXCursor was already in the set, and non-zero otherwise.
  impl

proc clang_getCursorSemanticParent*(): CXCursor =
  ##  Determine the semantic parent of the given cursor. The semantic parent of a cursor is the cursor that semantically contains the given Error: cannot render: rnLiteralBlock For many declarations, the lexical and semantic parents are equivalent (the lexical parent is returned by Error: cannot render: rnLiteralBlock They diverge when declarations or definitions are provided out-of-line. For example: Error: cannot render: rnCodeBlock In the out-of-line definition of Error: cannot render: rnLiteralBlock the semantic parent is the class Error: cannot render: rnLiteralBlock of which this function is a member. The lexical parent is the place where the declaration actually occurs in the source code; in this case, the definition occurs in the translation unit. In general, the lexical parent for a given entity can change without affecting the semantics of the program, and the lexical parent of different declarations of the same entity may be different. Changing the semantic parent of a declaration, on the other hand, can have a major impact on semantics, and redeclarations of a particular entity should all have the same semantic context. In the example above, both declarations of Error: cannot render: rnLiteralBlock have Error: cannot render: rnLiteralBlock as their semantic context, while the lexical context of the first Error: cannot render: rnLiteralBlock is Error: cannot render: rnLiteralBlock and the lexical context of the second Error: cannot render: rnLiteralBlock is the translation unit. For global declarations, the semantic parent is the translation unit.
  impl

proc clang_getCursorLexicalParent*(): CXCursor =
  ##  Determine the lexical parent of the given cursor. The lexical parent of a cursor is the cursor in which the given Error: cannot render: rnLiteralBlock was actually written. For many declarations, the lexical and semantic parents are equivalent (the semantic parent is returned by Error: cannot render: rnLiteralBlock They diverge when declarations or definitions are provided out-of-line. For example: Error: cannot render: rnCodeBlock In the out-of-line definition of Error: cannot render: rnLiteralBlock the semantic parent is the class Error: cannot render: rnLiteralBlock of which this function is a member. The lexical parent is the place where the declaration actually occurs in the source code; in this case, the definition occurs in the translation unit. In general, the lexical parent for a given entity can change without affecting the semantics of the program, and the lexical parent of different declarations of the same entity may be different. Changing the semantic parent of a declaration, on the other hand, can have a major impact on semantics, and redeclarations of a particular entity should all have the same semantic context. In the example above, both declarations of Error: cannot render: rnLiteralBlock have Error: cannot render: rnLiteralBlock as their semantic context, while the lexical context of the first Error: cannot render: rnLiteralBlock is Error: cannot render: rnLiteralBlock and the lexical context of the second Error: cannot render: rnLiteralBlock is the translation unit. For declarations written in the global scope, the lexical parent is the translation unit.
  impl

proc clang_getOverriddenCursors*(): void =
  ##  Determine the set of methods that are overridden by the given method. In both Objective-C and C++, a method (aka virtual member function, in C++) can override a virtual method in a base class. For Objective-C, a method is said to override any method in the class's base class, its protocols, or its categories' protocols, that has the same selector and is of the same kind (class or instance). If no such method exists, the search continues to the class's superclass, its protocols, and its categories, and so on. A method from an Objective-C implementation is considered to override the same methods as its corresponding method in the interface. For C++, a virtual member function overrides any virtual member function with the same signature that occurs in its base classes. With multiple inheritance, a virtual member function can override several virtual member functions coming from different base classes. In all cases, this function determines the immediate overridden method, rather than all of the overridden methods. For example, if a method is originally declared in a class A, then overridden in B (which in inherits from A) and also in C (which inherited from B), then the only overridden method returned from this function when invoked on C's method will be B's method. The client may then invoke this function again, given the previously-found overridden methods, to map out the complete method-override set. ** A cursor representing an Objective-C or C++ method. This routine will compute the set of methods that this method overrides. ** A pointer whose pointee will be replaced with a pointer to an array of cursors, representing the set of overridden methods. If there are no overridden methods, the pointee will be set to NULL. The pointee must be freed via a call to Error: cannot render: rnLiteralBlock ** A pointer to the number of overridden functions, will be set to the number of overridden functions in the array pointed to by Error: cannot render: rnLiteralBlock 
  impl

proc clang_disposeOverriddenCursors*(): void =
  ##  Free the set of overridden cursors returned by Error: cannot render: rnLiteralBlock 
  impl

proc clang_getIncludedFile*(): CXFile =
  ##  Retrieve the file that is included by the given inclusion directive cursor.
  impl

proc clang_getCursor*(): CXCursor =
  ##  Map a source location to the cursor that describes the entity at that location in the source code. clang_getCursor() maps an arbitrary source location within a translation unit down to the most specific cursor that describes the entity at that location. For example, given an expression Error: cannot render: rnLiteralBlock + y, invoking clang_getCursor() with a source location pointing to "x" will return the cursor for "x"; similarly for "y". If the cursor points anywhere between "x" or "y" (e.g., on the + or the whitespace around it), clang_getCursor() will return a cursor referring to the "+" expression. ** a cursor representing the entity at the given source location, or a NULL cursor if no such entity can be found.
  impl

proc clang_getCursorLocation*(): CXSourceLocation =
  ##  Retrieve the physical location of the source constructor referenced by the given cursor. The location of a declaration is typically the location of the name of that declaration, where the name of that declaration would occur if it is unnamed, or some keyword that introduces that particular declaration. The location of a reference is where that reference occurs within the source code.
  impl

proc clang_getCursorExtent*(): CXSourceRange =
  ##  Retrieve the physical extent of the source construct referenced by the given cursor. The extent of a cursor starts with the file/line/column pointing at the first character within the source construct that the cursor refers to and ends with the last character within that source construct. For a declaration, the extent covers the declaration itself. For a reference, the extent covers the location of the reference (e.g., where the referenced entity was actually used).
  impl

proc clang_getCursorType*(): CXType =
  ##  Retrieve the type of a CXCursor (if any).
  impl

proc clang_getTypeSpelling*(): CXString =
  ##  Pretty-print the underlying type using the rules of the language of the translation unit from which it came. If the type is invalid, an empty string is returned.
  impl

proc clang_getTypedefDeclUnderlyingType*(): CXType =
  ##  Retrieve the underlying type of a typedef declaration. If the cursor does not reference a typedef declaration, an invalid type is returned.
  impl

proc clang_getEnumDeclIntegerType*(): CXType =
  ##  Retrieve the integer type of an enum declaration. If the cursor does not reference an enum declaration, an invalid type is returned.
  impl

proc clang_getEnumConstantDeclValue*(): clonglong =
  ##  Retrieve the integer value of an enum constant declaration as a signed  long long. If the cursor does not reference an enum constant declaration, LLONG_MIN is returned. Since this is also potentially a valid constant value, the kind of the cursor must be verified before calling this function.
  impl

proc clang_getEnumConstantDeclUnsignedValue*(): culonglong =
  ##  Retrieve the integer value of an enum constant declaration as an unsigned  long long. If the cursor does not reference an enum constant declaration, ULLONG_MAX is returned. Since this is also potentially a valid constant value, the kind of the cursor must be verified before calling this function.
  impl

proc clang_getFieldDeclBitWidth*(): int =
  ##  Retrieve the bit width of a bit field declaration as an integer. If a cursor that is not a bit field declaration is passed in, -1 is returned.
  impl

proc clang_Cursor_getNumArguments*(): int =
  ##  Retrieve the number of non-variadic arguments associated with a given cursor. The number of arguments can be determined for calls as well as for declarations of functions or methods. For other cursors -1 is returned.
  impl

proc clang_Cursor_getArgument*(): CXCursor =
  ##  Retrieve the argument cursor of a function or method. The argument cursor can be determined for calls as well as for declarations of functions or methods. For other cursors and for invalid indices, an invalid cursor is returned.
  impl

proc clang_Cursor_getNumTemplateArguments*(): int =
  ## Returns the number of template args of a function decl representing a template specialization. If the argument cursor cannot be converted into a template function declaration, -1 is returned. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, -7, true>(); The value 3 would be returned from this call.
  impl

proc clang_Cursor_getTemplateArgumentKind*(): CXTemplateArgumentKind =
  ##  Retrieve the kind of the I'th template argument of the CXCursor C. If the argument CXCursor does not represent a FunctionDecl, an invalid template argument kind is returned. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, -7, true>(); For I = 0, 1, and 2, Type, Integral, and Integral will be returned, respectively.
  impl

proc clang_Cursor_getTemplateArgumentType*(): CXType =
  ##  Retrieve a CXType representing the type of a TemplateArgument of a  function decl representing a template specialization. If the argument CXCursor does not represent a FunctionDecl whose I'th template argument has a kind of CXTemplateArgKind_Integral, an invalid type is returned. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, -7, true>(); If called with I = 0, "float", will be returned. Invalid types will be returned for I == 1 or 2.
  impl

proc clang_Cursor_getTemplateArgumentValue*(): clonglong =
  ##  Retrieve the value of an Integral TemplateArgument (of a function  decl representing a template specialization) as a signed long long. It is undefined to call this function on a CXCursor that does not represent a FunctionDecl or whose I'th template argument is not an integral value. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, -7, true>(); If called with I = 1 or 2, -7 or true will be returned, respectively. For I == 0, this function's behavior is undefined.
  impl

proc clang_Cursor_getTemplateArgumentUnsignedValue*(): culonglong =
  ##  Retrieve the value of an Integral TemplateArgument (of a function  decl representing a template specialization) as an unsigned long long. It is undefined to call this function on a CXCursor that does not represent a FunctionDecl or whose I'th template argument is not an integral value. For example, for the following declaration and specialization:   template <typename T, int kInt, bool kBool>   void foo() { ... }   template <>   void foo<float, 2147483649, true>(); If called with I = 1 or 2, 2147483649 or true will be returned, respectively. For I == 0, this function's behavior is undefined.
  impl

proc clang_equalTypes*(): cuint =
  ##  Determine whether two CXTypes represent the same type. ** non-zero if the CXTypes represent the same type and          zero otherwise.
  impl

proc clang_getCanonicalType*(): CXType =
  ##  Return the canonical type for a CXType. Clang's type system explicitly models typedefs and all the ways a specific type can be represented.  The canonical type is the underlying type with all the "sugar" removed.  For example, if 'T' is a typedef for 'int', the canonical type for 'T' would be 'int'.
  impl

proc clang_isConstQualifiedType*(): cuint =
  ##  Determine whether a CXType has the "const" qualifier set, without looking through typedefs that may have added "const" at a different level.
  impl

proc clang_Cursor_isMacroFunctionLike*(): cuint =
  ##  Determine whether a  CXCursor that is a macro, is function like.
  impl

proc clang_Cursor_isMacroBuiltin*(): cuint =
  ##  Determine whether a  CXCursor that is a macro, is a builtin one.
  impl

proc clang_Cursor_isFunctionInlined*(): cuint =
  ##  Determine whether a  CXCursor that is a function declaration, is an inline declaration.
  impl

proc clang_isVolatileQualifiedType*(): cuint =
  ##  Determine whether a CXType has the "volatile" qualifier set, without looking through typedefs that may have added "volatile" at a different level.
  impl

proc clang_isRestrictQualifiedType*(): cuint =
  ##  Determine whether a CXType has the "restrict" qualifier set, without looking through typedefs that may have added "restrict" at a different level.
  impl

proc clang_getAddressSpace*(): cuint =
  ##  Returns the address space of the given type.
  impl

proc clang_getTypedefName*(): CXString =
  ##  Returns the typedef name of the given type.
  impl

proc clang_getPointeeType*(): CXType =
  ##  For pointer types, returns the type of the pointee.
  impl

proc clang_getTypeDeclaration*(): CXCursor =
  ##  Return the cursor for the declaration of the given type.
  impl

proc clang_getDeclObjCTypeEncoding*(): CXString =
  ##  Returns the Objective-C type encoding for the specified declaration.
  impl

proc clang_Type_getObjCEncoding*(): CXString =
  ##  Returns the Objective-C type encoding for the specified CXType.
  impl

proc clang_getTypeKindSpelling*(): CXString =
  ##  Retrieve the spelling of a given CXTypeKind.
  impl

proc clang_getFunctionTypeCallingConv*(): CXCallingConv =
  ##  Retrieve the calling convention associated with a function type. If a non-function type is passed in, CXCallingConv_Invalid is returned.
  impl

proc clang_getResultType*(): CXType =
  ##  Retrieve the return type associated with a function type. If a non-function type is passed in, an invalid type is returned.
  impl

proc clang_getExceptionSpecificationType*(): int =
  ##  Retrieve the exception specification type associated with a function type. This is a value of type CXCursor_ExceptionSpecificationKind. If a non-function type is passed in, an error code of -1 is returned.
  impl

proc clang_getNumArgTypes*(): int =
  ##  Retrieve the number of non-variadic parameters associated with a function type. If a non-function type is passed in, -1 is returned.
  impl

proc clang_getArgType*(): CXType =
  ##  Retrieve the type of a parameter of a function type. If a non-function type is passed in or the function does not have enough parameters, an invalid type is returned.
  impl

proc clang_Type_getObjCObjectBaseType*(): CXType =
  ##  Retrieves the base type of the ObjCObjectType. If the type is not an ObjC object, an invalid type is returned.
  impl

proc clang_Type_getNumObjCProtocolRefs*(): cuint =
  ##  Retrieve the number of protocol references associated with an ObjC object/id. If the type is not an ObjC object, 0 is returned.
  impl

proc clang_Type_getObjCProtocolDecl*(): CXCursor =
  ##  Retrieve the decl for a protocol reference for an ObjC object/id. If the type is not an ObjC object or there are not enough protocol references, an invalid cursor is returned.
  impl

proc clang_Type_getNumObjCTypeArgs*(): cuint =
  ##  Retreive the number of type arguments associated with an ObjC object. If the type is not an ObjC object, 0 is returned.
  impl

proc clang_Type_getObjCTypeArg*(): CXType =
  ##  Retrieve a type argument associated with an ObjC object. If the type is not an ObjC or the index is not valid, an invalid type is returned.
  impl

proc clang_isFunctionTypeVariadic*(): cuint =
  ##  Return 1 if the CXType is a variadic function type, and 0 otherwise.
  impl

proc clang_getCursorResultType*(): CXType =
  ##  Retrieve the return type associated with a given cursor. This only returns a valid type if the cursor refers to a function or method.
  impl

proc clang_getCursorExceptionSpecificationType*(): int =
  ##  Retrieve the exception specification type associated with a given cursor. This is a value of type CXCursor_ExceptionSpecificationKind. This only returns a valid result if the cursor refers to a function or method.
  impl

proc clang_isPODType*(): cuint =
  ##  Return 1 if the CXType is a POD (plain old data) type, and 0  otherwise.
  impl

proc clang_getElementType*(): CXType =
  ##  Return the element type of an array, complex, or vector type. If a type is passed in that is not an array, complex, or vector type, an invalid type is returned.
  impl

proc clang_getNumElements*(): clonglong =
  ##  Return the number of elements of an array or vector type. If a type is passed in that is not an array or vector type, -1 is returned.
  impl

proc clang_getArrayElementType*(): CXType =
  ##  Return the element type of an array type. If a non-array type is passed in, an invalid type is returned.
  impl

proc clang_getArraySize*(): clonglong =
  ##  Return the array size of a constant array. If a non-array type is passed in, -1 is returned.
  impl

proc clang_Type_getNamedType*(): CXType =
  ##  Retrieve the type named by the qualified-id. If a non-elaborated type is passed in, an invalid type is returned.
  impl

proc clang_Type_isTransparentTagTypedef*(): cuint =
  ##  Determine if a typedef is 'transparent' tag. A typedef is considered 'transparent' if it shares a name and spelling location with its underlying tag type, as is the case with the NS_ENUM macro. ** non-zero if transparent and zero otherwise.
  impl

proc clang_Type_getNullability*(): CXTypeNullabilityKind =
  ##  Retrieve the nullability kind of a pointer type.
  impl

proc clang_Type_getAlignOf*(): clonglong =
  ##  Return the alignment of a type in bytes as per C++[expr.alignof]   standard. If the type declaration is invalid, CXTypeLayoutError_Invalid is returned. If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete   is returned. If the type declaration is a dependent type, CXTypeLayoutError_Dependent is   returned. If the type declaration is not a constant size type,   CXTypeLayoutError_NotConstantSize is returned.
  impl

proc clang_Type_getClassType*(): CXType =
  ##  Return the class type of an member pointer type. If a non-member-pointer type is passed in, an invalid type is returned.
  impl

proc clang_Type_getSizeOf*(): clonglong =
  ##  Return the size of a type in bytes as per C++[expr.sizeof] standard. If the type declaration is invalid, CXTypeLayoutError_Invalid is returned. If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete   is returned. If the type declaration is a dependent type, CXTypeLayoutError_Dependent is   returned.
  impl

proc clang_Type_getOffsetOf*(): clonglong =
  ##  Return the offset of a field named S in a record of type T in bits   as it would be returned by __offsetof__ as per C++11[18.2p4] If the cursor is not a record field declaration, CXTypeLayoutError_Invalid   is returned. If the field's type declaration is an incomplete type,   CXTypeLayoutError_Incomplete is returned. If the field's type declaration is a dependent type,   CXTypeLayoutError_Dependent is returned. If the field's name S is not found,   CXTypeLayoutError_InvalidFieldName is returned.
  impl

proc clang_Type_getModifiedType*(): CXType =
  ##  Return the type that was modified by this attributed type. If the type is not an attributed type, an invalid type is returned.
  impl

proc clang_Cursor_getOffsetOfField*(): clonglong =
  ##  Return the offset of the field represented by the Cursor. If the cursor is not a field declaration, -1 is returned. If the cursor semantic parent is not a record field declaration,   CXTypeLayoutError_Invalid is returned. If the field's type declaration is an incomplete type,   CXTypeLayoutError_Incomplete is returned. If the field's type declaration is a dependent type,   CXTypeLayoutError_Dependent is returned. If the field's name S is not found,   CXTypeLayoutError_InvalidFieldName is returned.
  impl

proc clang_Cursor_isAnonymous*(): cuint =
  ##  Determine whether the given cursor represents an anonymous tag or namespace
  impl

proc clang_Cursor_isAnonymousRecordDecl*(): cuint =
  ##  Determine whether the given cursor represents an anonymous record declaration.
  impl

proc clang_Cursor_isInlineNamespace*(): cuint =
  ##  Determine whether the given cursor represents an inline namespace declaration.
  impl

proc clang_Type_getNumTemplateArguments*(): int =
  ##  Returns the number of template arguments for given template specialization, or -1 if type Error: cannot render: rnLiteralBlock is not a template specialization.
  impl

proc clang_Type_getTemplateArgumentAsType*(): CXType =
  ##  Returns the type template argument of a template class specialization at given index. This function only returns template type arguments and does not handle template template arguments or variadic packs.
  impl

proc clang_Type_getCXXRefQualifier*(): CXRefQualifierKind =
  ##  Retrieve the ref-qualifier kind of a function or method. The ref-qualifier is returned for C++ functions or methods. For other types or non-C++ declarations, CXRefQualifier_None is returned.
  impl

proc clang_Cursor_isBitField*(): cuint =
  ##  Returns non-zero if the cursor specifies a Record member that is a   bitfield.
  impl

proc clang_isVirtualBase*(): cuint =
  ##  Returns 1 if the base class specified by the cursor with kind   CX_CXXBaseSpecifier is virtual.
  impl

proc clang_getCXXAccessSpecifier*(): CX_CXXAccessSpecifier =
  ##  Returns the access control level for the referenced object. If the cursor refers to a C++ declaration, its access control level within its parent scope is returned. Otherwise, if the cursor refers to a base specifier or access specifier, the specifier itself is returned.
  impl

proc clang_Cursor_getStorageClass*(): CX_StorageClass =
  ##  Returns the storage class for a function or variable declaration. If the passed in Cursor is not a function or variable declaration, CX_SC_Invalid is returned else the storage class.
  impl

proc clang_getNumOverloadedDecls*(): cuint =
  ##  Determine the number of overloaded declarations referenced by a Error: cannot render: rnLiteralBlock cursor. ** The cursor whose overloaded declarations are being queried. ** The number of overloaded declarations referenced by Error: cannot render: rnLiteralBlock If it is not a Error: cannot render: rnLiteralBlock cursor, returns 0.
  impl

proc clang_getOverloadedDecl*(): CXCursor =
  ##  Retrieve a cursor for one of the overloaded declarations referenced by a Error: cannot render: rnLiteralBlock cursor. ** The cursor whose overloaded declarations are being queried. ** The zero-based index into the set of overloaded declarations in the cursor. ** A cursor representing the declaration referenced by the given Error: cannot render: rnLiteralBlock at the specified Error: cannot render: rnLiteralBlock If the cursor does not have an associated set of overloaded declarations, or if the index is out of bounds, returns Error: cannot render: rnLiteralBlock 
  impl

proc clang_getIBOutletCollectionType*(): CXType =
  ##  For cursors representing an iboutletcollection attribute,  this function returns the collection element type. 
  impl

proc clang_visitChildren*(): cuint =
  ##  Visit the children of a particular cursor. This function visits all the direct children of the given cursor, invoking the given Error: cannot render: rnLiteralBlock function with the cursors of each visited child. The traversal may be recursive, if the visitor returns Error: cannot render: rnLiteralBlock The traversal may also be ended prematurely, if the visitor returns Error: cannot render: rnLiteralBlock ** the cursor whose child may be visited. All kinds of cursors can be visited, including invalid cursors (which, by definition, have no children). ** the visitor function that will be invoked for each child of Error: cannot render: rnLiteralBlock ** pointer data supplied by the client, which will be passed to the visitor each time it is invoked. ** a non-zero value if the traversal was terminated prematurely by the visitor returning Error: cannot render: rnLiteralBlock 
  impl

proc clang_getCursorUSR*(): CXString =
  ##  Retrieve a Unified Symbol Resolution (USR) for the entity referenced by the given cursor. A Unified Symbol Resolution (USR) is a string that identifies a particular entity (function, class, variable, etc.) within a program. USRs can be compared across translation units to determine, e.g., when references in one translation refer to an entity defined in another translation unit.
  impl

proc clang_constructUSR_ObjCClass*(): CXString =
  ##  Construct a USR for a specified Objective-C class.
  impl

proc clang_constructUSR_ObjCCategory*(): CXString =
  ##  Construct a USR for a specified Objective-C category.
  impl

proc clang_constructUSR_ObjCProtocol*(): CXString =
  ##  Construct a USR for a specified Objective-C protocol.
  impl

proc clang_constructUSR_ObjCIvar*(): CXString =
  ##  Construct a USR for a specified Objective-C instance variable and   the USR for its containing class.
  impl

proc clang_constructUSR_ObjCMethod*(): CXString =
  ##  Construct a USR for a specified Objective-C method and   the USR for its containing class.
  impl

proc clang_constructUSR_ObjCProperty*(): CXString =
  ##  Construct a USR for a specified Objective-C property and the USR  for its containing class.
  impl

proc clang_getCursorSpelling*(): CXString =
  ##  Retrieve a name for the entity referenced by this cursor.
  impl

proc clang_Cursor_getSpellingNameRange*(): CXSourceRange =
  ##  Retrieve a range for a piece that forms the cursors spelling name. Most of the times there is only one range for the complete spelling but for Objective-C methods and Objective-C message expressions, there are multiple pieces for each selector identifier. ** the index of the spelling name piece. If this is greater than the actual number of pieces, it will return a NULL (invalid) range. ** Reserved.
  impl

proc clang_PrintingPolicy_getProperty*(): cuint =
  ##  Get a property value for the given printing policy.
  impl

proc clang_PrintingPolicy_setProperty*(): void =
  ##  Set a property value for the given printing policy.
  impl

proc clang_getCursorPrintingPolicy*(): CXPrintingPolicy =
  ##  Retrieve the default policy for the cursor. The policy should be released after use with Error: cannot render: rnLiteralBlock 
  impl

proc clang_PrintingPolicy_dispose*(): void =
  ##  Release a printing policy.
  impl

proc clang_getCursorPrettyPrinted*(): CXString =
  ##  Pretty print declarations. ** The cursor representing a declaration. ** The policy to control the entities being printed. If NULL, a default policy is used. ** The pretty printed declaration or the empty string for other cursors.
  impl

proc clang_getCursorDisplayName*(): CXString =
  ##  Retrieve the display name for the entity referenced by this cursor. The display name contains extra information that helps identify the cursor, such as the parameters of a function or template or the arguments of a class template specialization.
  impl

proc clang_getCursorReferenced*(): CXCursor =
  ##  For a cursor that is a reference, retrieve a cursor representing the entity that it references. Reference cursors refer to other entities in the AST. For example, an Objective-C superclass reference cursor refers to an Objective-C class. This function produces the cursor for the Objective-C class from the cursor for the superclass reference. If the input cursor is a declaration or definition, it returns that declaration or definition unchanged. Otherwise, returns the NULL cursor.
  impl

proc clang_getCursorDefinition*(): CXCursor =
  ##   For a cursor that is either a reference to or a declaration  of some entity, retrieve a cursor that describes the definition of  that entity.  Some entities can be declared multiple times within a translation  unit, but only one of those declarations can also be a  definition. For example, given:  Error: cannot render: rnCodeBlock  there are three declarations of the function "f", but only the  second one is a definition. The clang_getCursorDefinition()  function will take any cursor pointing to a declaration of "f"  (the first or fourth lines of the example) or a cursor referenced  that uses "f" (the call to "f' inside "g") and will return a  declaration cursor pointing to the definition (the second "f"  declaration).  If given a cursor for which there is no corresponding definition,  e.g., because there is no definition of that entity within this  translation unit, returns a NULL cursor.
  impl

proc clang_isCursorDefinition*(): cuint =
  ##  Determine whether the declaration pointed to by this cursor is also a definition of that entity.
  impl

proc clang_getCanonicalCursor*(): CXCursor =
  ##  Retrieve the canonical cursor corresponding to the given cursor. In the C family of languages, many kinds of entities can be declared several times within a single translation unit. For example, a structure type can be forward-declared (possibly multiple times) and later defined: Error: cannot render: rnCodeBlock The declarations and the definition of Error: cannot render: rnLiteralBlock are represented by three different cursors, all of which are declarations of the same underlying entity. One of these cursor is considered the "canonical" cursor, which is effectively the representative for the underlying entity. One can determine if two cursors are declarations of the same underlying entity by comparing their canonical cursors. ** The canonical cursor for the entity referred to by the given cursor.
  impl

proc clang_Cursor_getObjCSelectorIndex*(): int =
  ##  If the cursor points to a selector identifier in an Objective-C method or message expression, this returns the selector index. After getting a cursor with #clang_getCursor, this can be called to determine if the location points to a selector identifier. ** The selector index if the cursor is an Objective-C method or message expression and the cursor is pointing to a selector identifier, or -1 otherwise.
  impl

proc clang_Cursor_isDynamicCall*(): int =
  ##  Given a cursor pointing to a C++ method call or an Objective-C message, returns non-zero if the method/message is "dynamic", meaning: For a C++ method: the call is virtual. For an Objective-C message: the receiver is an object instance, not 'super' or a specific class. If the method/message is "static" or the cursor does not point to a method/message, it will return zero.
  impl

proc clang_Cursor_getReceiverType*(): CXType =
  ##  Given a cursor pointing to an Objective-C message or property reference, or C++ method call, returns the CXType of the receiver.
  impl

proc clang_Cursor_getObjCPropertyAttributes*(): cuint =
  ##  Given a cursor that represents a property declaration, return the associated property attributes. The bits are formed from Error: cannot render: rnLiteralBlock ** Reserved for future use, pass 0.
  impl

proc clang_Cursor_getObjCPropertyGetterName*(): CXString =
  ##  Given a cursor that represents a property declaration, return the name of the method that implements the getter.
  impl

proc clang_Cursor_getObjCPropertySetterName*(): CXString =
  ##  Given a cursor that represents a property declaration, return the name of the method that implements the setter, if any.
  impl

proc clang_Cursor_getObjCDeclQualifiers*(): cuint =
  ##  Given a cursor that represents an Objective-C method or parameter declaration, return the associated Objective-C qualifiers for the return type or the parameter respectively. The bits are formed from CXObjCDeclQualifierKind.
  impl

proc clang_Cursor_isObjCOptional*(): cuint =
  ##  Given a cursor that represents an Objective-C method or property declaration, return non-zero if the declaration was affected by "@optional". Returns zero if the cursor is not such a declaration or it is "@required".
  impl

proc clang_Cursor_isVariadic*(): cuint =
  ##  Returns non-zero if the given cursor is a variadic function or method.
  impl

proc clang_Cursor_isExternalSymbol*(): cuint =
  ##  Returns non-zero if the given cursor points to a symbol marked with external_source_symbol attribute. ** If non-NULL, and the attribute is present, will be set to the 'language' string from the attribute. ** If non-NULL, and the attribute is present, will be set to the 'definedIn' string from the attribute. ** If non-NULL, and the attribute is present, will be set to non-zero if the 'generated_declaration' is set in the attribute.
  impl

proc clang_Cursor_getCommentRange*(): CXSourceRange =
  ##  Given a cursor that represents a declaration, return the associated comment's source range.  The range may include multiple consecutive comments with whitespace in between.
  impl

proc clang_Cursor_getRawCommentText*(): CXString =
  ##  Given a cursor that represents a declaration, return the associated comment text, including comment markers.
  impl

proc clang_Cursor_getBriefCommentText*(): CXString =
  ##  Given a cursor that represents a documentable entity (e.g., declaration), return the associated `` first paragraph.
  impl

proc clang_Cursor_getMangling*(): CXString =
  ##  Retrieve the CXString representing the mangled name of the cursor.
  impl

proc clang_Cursor_getCXXManglings*(): ptr[CXStringSet] =
  ##  Retrieve the CXStrings representing the mangled symbols of the C++ constructor or destructor at the cursor.
  impl

proc clang_Cursor_getObjCManglings*(): ptr[CXStringSet] =
  ##  Retrieve the CXStrings representing the mangled symbols of the ObjC class interface or implementation at the cursor.
  impl

proc clang_Cursor_getModule*(): CXModule =
  ##  Given a CXCursor_ModuleImportDecl cursor, return the associated module.
  impl

proc clang_getModuleForFile*(): CXModule =
  ##  Given a CXFile header file, return the module that contains it, if one exists.
  impl

proc clang_Module_getASTFile*(): CXFile =
  ##  ** a module object. ** the module file where the provided module object came from.
  impl

proc clang_Module_getParent*(): CXModule =
  ##  ** a module object. ** the parent of a sub-module or NULL if the given module is top-level, e.g. for 'std.vector' it will return the 'std' module.
  impl

proc clang_Module_getName*(): CXString =
  ##  ** a module object. ** the name of the module, e.g. for the 'std.vector' sub-module it will return "vector".
  impl

proc clang_Module_getFullName*(): CXString =
  ##  ** a module object. ** the full name of the module, e.g. "std.vector".
  impl

proc clang_Module_isSystem*(): int =
  ##  ** a module object. ** non-zero if the module is a system one.
  impl

proc clang_Module_getNumTopLevelHeaders*(): cuint =
  ##  ** a module object. ** the number of top level headers associated with this module.
  impl

proc clang_Module_getTopLevelHeader*(): CXFile =
  ##  ** a module object. ** top level header index (zero-based). ** the specified top level header associated with the module.
  impl

proc clang_CXXConstructor_isConvertingConstructor*(): cuint =
  ##  Determine if a C++ constructor is a converting constructor.
  impl

proc clang_CXXConstructor_isCopyConstructor*(): cuint =
  ##  Determine if a C++ constructor is a copy constructor.
  impl

proc clang_CXXConstructor_isDefaultConstructor*(): cuint =
  ##  Determine if a C++ constructor is the default constructor.
  impl

proc clang_CXXConstructor_isMoveConstructor*(): cuint =
  ##  Determine if a C++ constructor is a move constructor.
  impl

proc clang_CXXField_isMutable*(): cuint =
  ##  Determine if a C++ field is declared 'mutable'.
  impl

proc clang_CXXMethod_isDefaulted*(): cuint =
  ##  Determine if a C++ method is declared '= default'.
  impl

proc clang_CXXMethod_isPureVirtual*(): cuint =
  ##  Determine if a C++ member function or member function template is pure virtual.
  impl

proc clang_CXXMethod_isStatic*(): cuint =
  ##  Determine if a C++ member function or member function template is declared 'static'.
  impl

proc clang_CXXMethod_isVirtual*(): cuint =
  ##  Determine if a C++ member function or member function template is explicitly declared 'virtual' or if it overrides a virtual method from one of the base classes.
  impl

proc clang_CXXRecord_isAbstract*(): cuint =
  ##  Determine if a C++ record is abstract, i.e. whether a class or struct has a pure virtual member function.
  impl

proc clang_EnumDecl_isScoped*(): cuint =
  ##  Determine if an enum declaration refers to a scoped enum.
  impl

proc clang_CXXMethod_isConst*(): cuint =
  ##  Determine if a C++ member function or member function template is declared 'const'.
  impl

proc clang_getTemplateCursorKind*(): CXCursorKind =
  ##  Given a cursor that represents a template, determine the cursor kind of the specializations would be generated by instantiating the template. This routine can be used to determine what flavor of function template, class template, or class template partial specialization is stored in the cursor. For example, it can describe whether a class template cursor is declared with "struct", "class" or "union". ** The cursor to query. This cursor should represent a template declaration. ** The cursor kind of the specializations that would be generated by instantiating the template Error: cannot render: rnLiteralBlock If Error: cannot render: rnLiteralBlock is not a template, returns Error: cannot render: rnLiteralBlock 
  impl

proc clang_getSpecializedCursorTemplate*(): CXCursor =
  ##  Given a cursor that may represent a specialization or instantiation of a template, retrieve the cursor that represents the template that it specializes or from which it was instantiated. This routine determines the template involved both for explicit specializations of templates and for implicit instantiations of the template, both of which are referred to as "specializations". For a class template specialization (e.g., Error: cannot render: rnLiteralBlock this routine will return either the primary template (Error: cannot render: rnLiteralBlock or, if the specialization was instantiated from a class template partial specialization, the class template partial specialization. For a class template partial specialization and a function template specialization (including instantiations), this this routine will return the specialized template. For members of a class template (e.g., member functions, member classes, or static data members), returns the specialized or instantiated member. Although not strictly "templates" in the C++ language, members of class templates have the same notions of specializations and instantiations that templates do, so this routine treats them similarly. ** A cursor that may be a specialization of a template or a member of a template. ** If the given cursor is a specialization or instantiation of a template or a member thereof, the template or member that it specializes or from which it was instantiated. Otherwise, returns a NULL cursor.
  impl

proc clang_getCursorReferenceNameRange*(): CXSourceRange =
  ##  Given a cursor that references something else, return the source range covering that reference. ** A cursor pointing to a member reference, a declaration reference, or an operator call. ** A bitset with three independent flags: CXNameRange_WantQualifier, CXNameRange_WantTemplateArgs, and CXNameRange_WantSinglePiece. ** For contiguous names or when passing the flag CXNameRange_WantSinglePiece, only one piece with index 0 is available. When the CXNameRange_WantSinglePiece flag is not passed for a non-contiguous names, this index can be used to retrieve the individual pieces of the name. See also CXNameRange_WantSinglePiece. ** The piece of the name pointed to by the given cursor. If there is no name, or if the PieceIndex is out-of-range, a null-cursor will be returned.
  impl

proc clang_getToken*(): ptr[CXToken] =
  ##  Get the raw lexical token starting with the given location. ** the translation unit whose text is being tokenized. ** the source location with which the token starts. ** The token starting with the given location or NULL if no such token exist. The returned pointer must be freed with clang_disposeTokens before the translation unit is destroyed.
  impl

proc clang_getTokenKind*(): CXTokenKind =
  ##  Determine the kind of the given token.
  impl

proc clang_getTokenSpelling*(): CXString =
  ##  Determine the spelling of the given token. The spelling of a token is the textual representation of that token, e.g., the text of an identifier or keyword.
  impl

proc clang_getTokenLocation*(): CXSourceLocation =
  ##  Retrieve the source location of the given token.
  impl

proc clang_getTokenExtent*(): CXSourceRange =
  ##  Retrieve a source range that covers the given token.
  impl

proc clang_tokenize*(): void =
  ##  Tokenize the source code described by the given range into raw lexical tokens. ** the translation unit whose text is being tokenized. ** the source range in which text should be tokenized. All of the tokens produced by tokenization will fall within this source range, ** this pointer will be set to point to the array of tokens that occur within the given source range. The returned pointer must be freed with clang_disposeTokens() before the translation unit is destroyed. ** will be set to the number of tokens in the Error: cannot render: rnLiteralBlock array. 
  impl

proc clang_annotateTokens*(): void =
  ##  Annotate the given set of tokens by providing cursors for each token that can be mapped to a specific entity within the abstract syntax tree. This token-annotation routine is equivalent to invoking clang_getCursor() for the source locations of each of the tokens. The cursors provided are filtered, so that only those cursors that have a direct correspondence to the token are accepted. For example, given a function call Error: cannot render: rnLiteralBlock clang_getCursor() would provide the following cursors:   * when the cursor is over the 'f', a DeclRefExpr cursor referring to 'f'.   * when the cursor is over the '(' or the ')', a CallExpr referring to 'f'.   * when the cursor is over the 'x', a DeclRefExpr cursor referring to 'x'. Only the first and last of these cursors will occur within the annotate, since the tokens "f" and "x' directly refer to a function and a variable, respectively, but the parentheses are just a small part of the full syntax of the function call expression, which is not provided as an annotation. ** the translation unit that owns the given tokens. ** the set of tokens to annotate. ** the number of tokens in Error: cannot render: rnLiteralBlock ** an array of Error: cannot render: rnLiteralBlock cursors, whose contents will be replaced with the cursors corresponding to each token.
  impl

proc clang_disposeTokens*(): void =
  ##  Free the given set of tokens.
  impl

proc clang_getCursorKindSpelling*(): CXString =
  ##  `` These routines are used for testing and debugging, only, and should not be relied upon. @{
  impl

proc clang_getDefinitionSpellingAndExtent*(): void =
  impl

proc clang_enableStackTraces*(): void =
  impl

proc clang_executeOnThread*(): void =
  impl

proc clang_getCompletionChunkKind*(): CXCompletionChunkKind =
  ##  Determine the kind of a particular chunk within a completion string. ** the completion string to query. ** the 0-based index of the chunk in the completion string. ** the kind of the chunk at the index Error: cannot render: rnLiteralBlock 
  impl

proc clang_getCompletionChunkText*(): CXString =
  ##  Retrieve the text associated with a particular chunk within a completion string. ** the completion string to query. ** the 0-based index of the chunk in the completion string. ** the text associated with the chunk at index Error: cannot render: rnLiteralBlock 
  impl

proc clang_getCompletionChunkCompletionString*(): CXCompletionString =
  ##  Retrieve the completion string associated with a particular chunk within a completion string. ** the completion string to query. ** the 0-based index of the chunk in the completion string. ** the completion string associated with the chunk at index Error: cannot render: rnLiteralBlock 
  impl

proc clang_getNumCompletionChunks*(): cuint =
  ##  Retrieve the number of chunks in the given code-completion string.
  impl

proc clang_getCompletionPriority*(): cuint =
  ##  Determine the priority of this code completion. The priority of a code completion indicates how likely it is that this particular completion is the completion that the user will select. The priority is selected by various internal heuristics. ** The completion string to query. ** The priority of this completion string. Smaller values indicate higher-priority (more likely) completions.
  impl

proc clang_getCompletionAvailability*(): CXAvailabilityKind =
  ##  Determine the availability of the entity that this code-completion string refers to. ** The completion string to query. ** The availability of the completion string.
  impl

proc clang_getCompletionNumAnnotations*(): cuint =
  ##  Retrieve the number of annotations associated with the given completion string. ** the completion string to query. ** the number of annotations associated with the given completion string.
  impl

proc clang_getCompletionAnnotation*(): CXString =
  ##  Retrieve the annotation associated with the given completion string. ** the completion string to query. ** the 0-based index of the annotation of the completion string. ** annotation string associated with the completion at index Error: cannot render: rnLiteralBlock or a NULL string if that annotation is not available.
  impl

proc clang_getCompletionParent*(): CXString =
  ##  Retrieve the parent context of the given completion string. The parent context of a completion string is the semantic parent of the declaration (if any) that the code completion represents. For example, a code completion for an Objective-C method would have the method's class or protocol as its context. ** The code completion string whose parent is being queried. ** DEPRECATED: always set to CXCursor_NotImplemented if non-NULL. ** The name of the completion parent, e.g., "NSObject" if the completion string represents a method in the NSObject class.
  impl

proc clang_getCompletionBriefComment*(): CXString =
  ##  Retrieve the brief documentation comment attached to the declaration that corresponds to the given completion string.
  impl

proc clang_getCursorCompletionString*(): CXCompletionString =
  ##  Retrieve a completion string for an arbitrary declaration or macro definition cursor. ** The cursor to query. ** A non-context-sensitive completion string for declaration and macro definition cursors, or NULL for other kinds of cursors.
  impl

proc clang_getCompletionNumFixIts*(): cuint =
  ##  Retrieve the number of fix-its for the given completion index. Calling this makes sense only if CXCodeComplete_IncludeCompletionsWithFixIts option was set. ** The structure keeping all completion results ** The index of the completion ** The number of fix-its which must be applied before the completion at completion_index can be applied
  impl

proc clang_getCompletionFixIt*(): CXString =
  ##  Fix-its that *must* be applied before inserting the text for the corresponding completion. By default, clang_codeCompleteAt() only returns completions with empty fix-its. Extra completions with non-empty fix-its should be explicitly requested by setting CXCodeComplete_IncludeCompletionsWithFixIts. For the clients to be able to compute position of the cursor after applying fix-its, the following conditions are guaranteed to hold for replacement_range of the stored fix-its:  - Ranges in the fix-its are guaranteed to never contain the completion  point (or identifier under completion point, if any) inside them, except  at the start or at the end of the range.  - If a fix-it range starts or ends with completion point (or starts or  ends after the identifier under completion point), it will contain at  least one character. It allows to unambiguously recompute completion  point after applying the fix-it. The intuition is that provided fix-its change code around the identifier we complete, but are not allowed to touch the identifier itself or the completion point. One example of completions with corrections are the ones replacing '.' with '->' and vice versa: std::unique_ptr<std::vector<int>> vec_ptr; In 'vec_ptr.^', one of the completions is 'push_back', it requires replacing '.' with '->'. In 'vec_ptr->^', one of the completions is 'release', it requires replacing '->' with '.'. ** The structure keeping all completion results ** The index of the completion ** The index of the fix-it for the completion at completion_index ** The fix-it range that must be replaced before the completion at completion_index can be applied ** The fix-it string that must replace the code at replacement_range before the completion at completion_index can be applied
  impl

proc clang_defaultCodeCompleteOptions*(): cuint =
  ##  Returns a default set of code-completion options that can be passed toError: cannot render: rnLiteralBlock 
  impl

proc clang_codeCompleteAt*(): ptr[CXCodeCompleteResults] =
  ##  Perform code completion at a given location in a translation unit. This function performs code completion at a particular file, line, and column within source code, providing results that suggest potential code snippets based on the context of the completion. The basic model for code completion is that Clang will parse a complete source file, performing syntax checking up to the location where code-completion has been requested. At that point, a special code-completion token is passed to the parser, which recognizes this token and determines, based on the current location in the C/Objective-C/C++ grammar and the state of semantic analysis, what completions to provide. These completions are returned via a new Error: cannot render: rnLiteralBlock structure. Code completion itself is meant to be triggered by the client when the user types punctuation characters or whitespace, at which point the code-completion location will coincide with the cursor. For example, if Error: cannot render: rnLiteralBlock is a pointer, code-completion might be triggered after the "-" and then after the ">" in Error: cannot render: rnLiteralBlock When the code-completion location is after the ">", the completion results will provide, e.g., the members of the struct that "p" points to. The client is responsible for placing the cursor at the beginning of the token currently being typed, then filtering the results based on the contents of the token. For example, when code-completing for the expression Error: cannot render: rnLiteralBlock the client should provide the location just after the ">" (e.g., pointing at the "g") to this code-completion hook. Then, the client can filter the results based on the current token text ("get"), only showing those results that start with "get". The intent of this interface is to separate the relatively high-latency acquisition of code-completion results from the filtering of results on a per-character basis, which must have a lower latency. ** The translation unit in which code-completion should occur. The source files for this translation unit need not be completely up-to-date (and the contents of those source files may be overridden via Error: cannot render: rnLiteralBlock Cursors referring into the translation unit may be invalidated by this invocation. ** The name of the source file where code completion should be performed. This filename may be any file included in the translation unit. ** The line at which code-completion should occur. ** The column at which code-completion should occur. Note that the column should point just after the syntactic construct that initiated code completion, and not in the middle of a lexical token. ** the Files that have not yet been saved to disk but may be required for parsing or code completion, including the contents of those files.  The contents and name of these files (as specified by CXUnsavedFile) are copied when necessary, so the client only needs to guarantee their validity until the call to this function returns. ** The number of unsaved file entries in Error: cannot render: rnLiteralBlock ** Extra options that control the behavior of code completion, expressed as a bitwise OR of the enumerators of the CXCodeComplete_Flags enumeration. The Error: cannot render: rnLiteralBlock function returns a default set of code-completion options. ** If successful, a new Error: cannot render: rnLiteralBlock structure containing code-completion results, which should eventually be freed with Error: cannot render: rnLiteralBlock If code completion fails, returns NULL.
  impl

proc clang_sortCodeCompletionResults*(): void =
  ##  Sort the code-completion results in case-insensitive alphabetical order. ** The set of results to sort. ** The number of results in Error: cannot render: rnLiteralBlock 
  impl

proc clang_disposeCodeCompleteResults*(): void =
  ##  Free the given set of code-completion results.
  impl

proc clang_codeCompleteGetNumDiagnostics*(): cuint =
  ##  Determine the number of diagnostics produced prior to the location where code completion was performed.
  impl

proc clang_codeCompleteGetDiagnostic*(): CXDiagnostic =
  ##  Retrieve a diagnostic associated with the given code completion. ** the code completion results to query. ** the zero-based diagnostic number to retrieve. ** the requested diagnostic. This diagnostic must be freed via a call to Error: cannot render: rnLiteralBlock 
  impl

proc clang_codeCompleteGetContexts*(): culonglong =
  ##  Determines what completions are appropriate for the context the given code completion. ** the code completion results to query ** the kinds of completions that are appropriate for use along with the given code completion results.
  impl

proc clang_codeCompleteGetContainerKind*(): CXCursorKind =
  ##  Returns the cursor kind for the container for the current code completion context. The container is only guaranteed to be set for contexts where a container exists (i.e. member accesses or Objective-C message sends); if there is not a container, this function will return CXCursor_InvalidCode. ** the code completion results to query ** on return, this value will be false if Clang has complete information about the container. If Clang does not have complete information, this value will be true. ** the container kind, or CXCursor_InvalidCode if there is not a container
  impl

proc clang_codeCompleteGetContainerUSR*(): CXString =
  ##  Returns the USR for the container for the current code completion context. If there is not a container for the current context, this function will return the empty string. ** the code completion results to query ** the USR for the container
  impl

proc clang_codeCompleteGetObjCSelector*(): CXString =
  ##  Returns the currently-entered selector for an Objective-C message send, formatted like "initWithFoo:bar:". Only guaranteed to return a non-empty string for CXCompletionContext_ObjCInstanceMessage and CXCompletionContext_ObjCClassMessage. ** the code completion results to query ** the selector (or partial selector) that has been entered thus far for an Objective-C message send.
  impl

proc clang_getClangVersion*(): CXString =
  ##  Return a version string, suitable for showing to a user, but not        intended to be parsed (the format is not guaranteed to be stable).
  impl

proc clang_toggleCrashRecovery*(): void =
  ##  Enable/disable crash recovery. ** Flag to indicate if crash recovery is enabled.  A non-zero        value enables crash recovery, while 0 disables it.
  impl

proc clang_getInclusions*(): void =
  ##  Visit the set of preprocessor inclusions in a translation unit.   The visitor function is called with the provided data for every included   file.  This does not include headers included by the PCH file (unless one   is inspecting the inclusions in the PCH file itself).
  impl

proc clang_Cursor_Evaluate*(): CXEvalResult =
  ##  If cursor is a statement declaration tries to evaluate the statement and if its variable, tries to evaluate its initializer, into its corresponding type.
  impl

proc clang_EvalResult_getKind*(): CXEvalResultKind =
  ##  Returns the kind of the evaluated result.
  impl

proc clang_EvalResult_getAsInt*(): int =
  ##  Returns the evaluation result as integer if the kind is Int.
  impl

proc clang_EvalResult_getAsLongLong*(): clonglong =
  ##  Returns the evaluation result as a long long integer if the kind is Int. This prevents overflows that may happen if the result is returned with clang_EvalResult_getAsInt.
  impl

proc clang_EvalResult_isUnsignedInt*(): cuint =
  ##  Returns a non-zero value if the kind is Int and the evaluation result resulted in an unsigned integer.
  impl

proc clang_EvalResult_getAsUnsigned*(): culonglong =
  ##  Returns the evaluation result as an unsigned integer if the kind is Int and clang_EvalResult_isUnsignedInt is non-zero.
  impl

proc clang_EvalResult_getAsDouble*(): cdouble =
  ##  Returns the evaluation result as double if the kind is double.
  impl

proc clang_EvalResult_getAsStr*(): cstring =
  ##  Returns the evaluation result as a constant string if the kind is other than Int or float. User must not free this pointer, instead call clang_EvalResult_dispose on the CXEvalResult returned by clang_Cursor_Evaluate.
  impl

proc clang_EvalResult_dispose*(): void =
  ##  Disposes the created Eval memory.
  impl

proc clang_getRemappings*(): CXRemapping =
  ##  Retrieve a remapping. ** the path that contains metadata about remappings. ** the requested remapping. This remapping must be freed via a call to Error: cannot render: rnLiteralBlock Can return NULL if an error occurred.
  impl

proc clang_getRemappingsFromFileList*(): CXRemapping =
  ##  Retrieve a remapping. ** pointer to an array of file paths containing remapping info. ** number of file paths. ** the requested remapping. This remapping must be freed via a call to Error: cannot render: rnLiteralBlock Can return NULL if an error occurred.
  impl

proc clang_remap_getNumFiles*(): cuint =
  ##  Determine the number of remappings.
  impl

proc clang_remap_getFilenames*(): void =
  ##  Get the original and the associated filename from the remapping. ** If non-NULL, will be set to the original filename. ** If non-NULL, will be set to the filename that the original is associated with.
  impl

proc clang_remap_dispose*(): void =
  ##  Dispose the remapping.
  impl

proc clang_findReferencesInFile*(): CXResult =
  ##  Find references of a declaration in a specific file. ** pointing to a declaration or a reference of one. ** to search for references. ** callback that will receive pairs of CXCursor/CXSourceRange for each reference found. The CXSourceRange will point inside the file; if the reference is inside a macro (and not a macro argument) the CXSourceRange will be invalid. ** one of the CXResult enumerators.
  impl

proc clang_findIncludesInFile*(): CXResult =
  ##  Find #import/#include directives in a specific file. ** translation unit containing the file to query. ** to search for #import/#include directives. ** callback that will receive pairs of CXCursor/CXSourceRange for each directive found. ** one of the CXResult enumerators.
  impl

proc clang_index_isEntityObjCContainerKind*(): int =
  impl

proc clang_index_getObjCContainerDeclInfo*(): ptr[const CXIdxObjCContainerDeclInfo] =
  impl

proc clang_index_getObjCInterfaceDeclInfo*(): ptr[const CXIdxObjCInterfaceDeclInfo] =
  impl

proc clang_index_getObjCCategoryDeclInfo*(): ptr[const CXIdxObjCCategoryDeclInfo] =
  impl

proc clang_index_getObjCProtocolRefListInfo*(): ptr[
    const CXIdxObjCProtocolRefListInfo] =
  impl

proc clang_index_getObjCPropertyDeclInfo*(): ptr[const CXIdxObjCPropertyDeclInfo] =
  impl

proc clang_index_getIBOutletCollectionAttrInfo*(): ptr[
    const CXIdxIBOutletCollectionAttrInfo] =
  impl

proc clang_index_getCXXClassDeclInfo*(): ptr[const CXIdxCXXClassDeclInfo] =
  impl

proc clang_index_getClientContainer*(): CXIdxClientContainer =
  ##  For retrieving a custom CXIdxClientContainer attached to a container.
  impl

proc clang_index_setClientContainer*(): void =
  ##  For setting a custom CXIdxClientContainer attached to a container.
  impl

proc clang_index_getClientEntity*(): CXIdxClientEntity =
  ##  For retrieving a custom CXIdxClientEntity attached to an entity.
  impl

proc clang_index_setClientEntity*(): void =
  ##  For setting a custom CXIdxClientEntity attached to an entity.
  impl

proc clang_IndexAction_create*(): CXIndexAction =
  ##  An indexing action/session, to be applied to one or multiple translation units. ** The index object with which the index action will be associated.
  impl

proc clang_IndexAction_dispose*(): void =
  ##  Destroy the given index action. The index action must not be destroyed until all of the translation units created within that index action have been destroyed.
  impl

proc clang_indexSourceFile*(): int =
  ##  Index the given source file and the translation unit corresponding to that file via callbacks implemented through #IndexerCallbacks. ** pointer data supplied by the client, which will be passed to the invoked callbacks. ** Pointer to indexing callbacks that the client implements. ** Size of #IndexerCallbacks structure that gets passed in index_callbacks. ** A bitmask of options that affects how indexing is performed. This should be a bitwise OR of the CXIndexOpt_XXX flags. ** pointer to store a Error: cannot render: rnLiteralBlock that can be reused after indexing is finished. Set to Error: cannot render: rnLiteralBlock if you do not require it. ** 0 on success or if there were errors from which the compiler could recover.  If there is a failure from which there is no recovery, returns a non-zero Error: cannot render: rnLiteralBlock The rest of the parameters are the same as #clang_parseTranslationUnit.
  impl

proc clang_indexSourceFileFullArgv*(): int =
  ##  Same as clang_indexSourceFile but requires a full command line for Error: cannot render: rnLiteralBlock including argv[0]. This is useful if the standard library paths are relative to the binary.
  impl

proc clang_indexTranslationUnit*(): int =
  ##  Index the given translation unit via callbacks implemented through #IndexerCallbacks. The order of callback invocations is not guaranteed to be the same as when indexing a source file. The high level order will be:   -Preprocessor callbacks invocations   -Declaration/reference callbacks invocations   -Diagnostic callback invocations The parameters are the same as #clang_indexSourceFile. ** If there is a failure from which there is no recovery, returns non-zero, otherwise returns 0.
  impl

proc clang_indexLoc_getFileLocation*(): void =
  ##  Retrieve the CXIdxFile, file, line, column, and offset represented by the given CXIdxLoc. If the location refers into a macro expansion, retrieves the location of the macro expansion and if it refers into a macro argument retrieves the location of the argument.
  impl

proc clang_indexLoc_getCXSourceLocation*(): CXSourceLocation =
  ##  Retrieve the CXSourceLocation represented by the given CXIdxLoc.
  impl

proc clang_Type_visitFields*(): cuint =
  ##  Visit the fields of a particular type. This function visits all the direct fields of the given cursor, invoking the given Error: cannot render: rnLiteralBlock function with the cursors of each visited field. The traversal may be ended prematurely, if the visitor returns Error: cannot render: rnLiteralBlock ** the record type whose field may be visited. ** the visitor function that will be invoked for each field of Error: cannot render: rnLiteralBlock ** pointer data supplied by the client, which will be passed to the visitor each time it is invoked. ** a non-zero value if the traversal was terminated prematurely by the visitor returning Error: cannot render: rnLiteralBlock 
  impl
