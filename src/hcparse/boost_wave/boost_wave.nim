import hmisc/wrappers/wraphelp

import ./boost_wave_wrap
export boost_wave_wrap

# {.pragma: apiPtr, header: hdr, requiresinit, bycopy.}
# {.pragma: apiProc, dynlib: so, cdecl.}

# type
#   EntryHandling* = enum
#     ehSkip
#     ehProcess
#     ehRaise

# type WaveTokenHandle* {.apiPtr.} = object

# type WaveIteratorHandle* {.apiPtr.} = object


# type
#   WaveContextHandle* {.apiPtr.} = object
#   WaveContextImplHandle* {.apiPtr.} = object
#   WaveTokenListHandle* {.apiPtr.} = object

#   FoundWarningDirectiveCb = proc(
#     ctx: ptr WaveContextImplHandle,
#     message: ptr WaveTokenListHandle): EntryHandling

#   FoundWarningDirectiveImpl = proc(
#     ctx: ptr WaveContextImplHandle,
#     message: ptr WaveTokenListHandle,
#     env: pointer
#   ): EntryHandling {.cdecl.}

# proc setFoundWarningDirective*(
#     ctx: ptr WaveContextHandle,
#     impl: FoundWarningDirectiveImpl,
#     env: pointer
#   ) {.apiProc, importc: "wave_setFoundWarningDirective".}



proc setFoundWarningDirective*(
    ctx: ptr WaveContextHandle,
    impl: proc(
      ctx: ptr WaveContextImplHandle,
      message: ptr WaveTokenListHandle): EntryHandling
  ) =

  let env = rawEnv(impl)
  let impl = rawProc(impl)
  ctx.setFoundWarningDirective(cast[FoundWarningDirectiveImplType](impl), env)


proc setEvaluatedConditionalExpression*(
    ctx: ptr WaveContextHandle,
    impl: proc (
      ctx: ptr WaveContextImplHandle;
      directive: ptr WaveTokenHandle;
      expression: ptr WaveTokenListHandle;
      expressionValue: bool): bool
  ) =

  ##[

The callback is called, whenever the preprocessor has encountered a `#if`,
`#elif`, `#ifdef` or `#ifndef` directive. This hook gets passed the
non-expanded conditional expression (as it was given in the analysed source
file) and the result of the evaluation of this expression in the current
preprocessing context.

- The ctx parameter provides a reference to the context_type used during
  instantiation of the preprocessing iterators by the user. Note, this
  parameter was added for the Boost V1.35.0 release.
- The token parameter holds a reference to the evaluated directive token.
- The parameter expression holds the non-expanded token sequence comprising the
  evaluated expression.
- The parameter expression_value contains the result of the evaluation of the
  expression in the current preprocessing context.
- The return value defines, whether the given expression has to be evaluated
  again, allowing to decide which of the conditional branches should be
  expanded. You need to return 'true' from this hook function to force the
  expression to be re-evaluated. Note, this was changed from a 'void' for the
  Boost V1.35.0 release.

  ]##

  let env = rawEnv(impl)
  let impl = rawProc(impl)
  ctx.setEvaluatedConditionalExpression(
    cast[EvaluatedConditionalExpressionImplType](impl),
    env)

proc setExpandingFunctionLikeMacro*(
    ctx: ptr WaveContextHandle,
    impl: proc (
      ctx: ptr WaveContextImplHandle;
      macrodef: ptr WaveTokenHandle;
      formal_args: ptr WaveTokenVectorHandle;
      definition: ptr WaveTokenListHandle;
      macrocall: ptr WaveTokenHandle;
      arguments: ptr WaveTokenVectorHandle;
      seqstart: pointer;
      seqend: pointer): bool
   ) =

  ##[

The function expanding_function_like_macro is called, whenever a
function-like macro is to be expanded, i.e. before the actual expansion
starts.

- The ctx parameter provides a reference to the context_type used during
  instantiation of the preprocessing iterators by the user. Note, this
  parameter was added for the Boost V1.35.0 release.

- The macroname parameter marks the position where the macro to expand is
  defined. It contains the token which identifies the macro name used inside
  the corresponding macro definition.

- The formal_args parameter holds the formal arguments used during the
  definition of the macro.

- The definition parameter holds the macro definition for the macro to trace.
  This is a standard STL container which holds the token sequence identified
  during the macro definition as the macro replacement list.

- The macrocall parameter marks the position where this macro is invoked. It
  contains the token, which identifies the macro call inside the preprocessed
  input stream.

- The arguments parameter holds the macro arguments used during the
  invocation of the macro. This is a vector of standard STL containers which
  contain the token sequences identified at the position of the macro call as
  the arguments to be used during the macro expansion.

- The parameters seqstart and seqend point into the input token stream
  allowing to access the whole token sequence comprising the macro invocation
  (starting with the opening parenthesis and ending after the closing one).

- If the return value is true, the macro is not expanded, i.e. the overall
  macro invocation sequence, including the parameters are copied to the
  output without further processing . If the return value is false, the macro
  is expanded as expected.

]##

  let env = rawEnv(impl)
  let impl = rawProc(impl)
  ctx.setExpandingFunctionLikeMacro(cast[ExpandingFunctionLikeMacroImplType](impl), env)

proc setFoundIncludeDirective*(
    ctx: ptr WaveContextHandle,
    impl: proc(
      context: ptr WaveContextImplHandle;
      impl: cstring;
      include_next: bool): EntryHandling
  ) =

  ##[

The function found_include_directive is called whenever whenever a
`#include` directive was located..

The ctx parameter provides a reference to the context_type used during
instantiation of the preprocessing iterators by the user. Note, this
parameter was added for the Boost V1.35.0 release.

The parameter filename contains the (expanded) file name found after the
`#include` directive. This has the format `<file>`, `"file"` or `file`. The
formats `<file>` or `"file"` are used for #include directives found in the
preprocessed token stream, the format `file` is used for files specified
through the `--force_include` command line argument.

TODO document how specify `--force_include` arguments

The parameter include_next is set to true if the found directive was a
`#include_next` directive and the `BOOST_WAVE_SUPPORT_INCLUDE_NEXT`
preprocessing constant was defined to something `!= 0`.

If the return value is 'skip', the include directive is not executed, i.e.
the file to include is not loaded nor processed. The overall directive is
replaced by a single newline character. If the return value is 'process',
the directive is executed in a normal manner.

  ]##

  ctx.setFoundIncludeDirective(
    cast[FoundIncludeDirectiveImplType](rawProc(impl)),
    rawEnv(impl))

proc setDefinedMacro*(
    ctx: ptr WaveContextHandle,
    impl: proc (
      ctx: ptr WaveContextImplHandle;
      name: ptr WaveTokenHandle;
      is_functionlike: bool;
      parameters: ptr WaveTokenVectorHandle;
      definition: ptr WaveTokenListHandle;
      is_predefined: bool): void
  ) =

  ##[

The function defined_macro is called whenever a macro was defined
successfully.

The ctx parameter provides a reference to the context_type used during
instantiation of the preprocessing iterators by the user. Note, this
parameter was added for the Boost V1.35.0 release.

- The parameter name is a reference to the token holding the macro name.

- The parameter is_functionlike is set to true whenever the newly defined
  macro is defined as a function like macro.

- The parameter parameters holds the parameter tokens for the macro
  definition. If the macro has no parameters or if it is a object like
  macro, then this container is empty.

- The parameter definition contains the token sequence given as the
  replacement sequence (definition part) of the newly defined macro.

- The parameter is_predefined is set to true for all macros predefined
  during the initialisation pahase of the library.

  ]##

  ctx.setDefinedMacro(
    cast[DefinedMacroImplType](rawProc(impl)),
    rawEnv(impl))


proc setSkippedToken*(
    ctx: ptr WaveContextHandle,
    impl: proc (
      context: ptr WaveContextImplHandle;
      token: ptr WaveTokenHandle): void
  ) =

  ctx.setSkippedToken(
    cast[SkippedTokenImplType](rawProc(impl)),
    rawEnv(impl))

proc first*(ctx: ptr WaveContextHandle): ptr WaveIteratorHandle = ctx.beginIterator()
proc last*(ctx: ptr WaveContextHandle): ptr WaveIteratorHandle = ctx.endIterator()
proc getTok*(iter: ptr WaveIteratorHandle): ptr WaveTokenHandle = iter.iterGetTok()
proc advance*(iter: ptr WaveIteratorHandle) = iter.advanceIterator()
proc `!=`*(iter1, iter2: ptr WaveIteratorHandle): bool = neqIterator(iter1, iter2)
proc `==`*(iter1, iter2: ptr WaveIteratorHandle): bool {.error.}
proc getValue*(tok: ptr WaveTokenHandle): cstring = tok.tokGetValue()
proc kind*(tok: ptr WaveTokenHandle): WaveTokId = tok.tokGetId()

proc `$`*(t: ptr WaveTokenHandle): string =
  if not isNil(t):
    let val = t.getValue()
    if not isNil(val):
      return $val

proc `$`*(l: ptr WaveTokenListHandle): string = $tokenListToStr(l)
proc len*(l: ptr WaveTokenListHandle): int = tokenListLen(l)

proc len*(l: ptr WaveTokenVectorHandle): int = tokenVectorLen(l)
proc `[]`*(l: ptr WaveTokenVectorHandle, idx: int): ptr WaveTokenHandle =
  tokenVectorGetAt(l, cint(idx))

iterator items*(l: ptr WaveTokenVectorHandle): ptr WaveTokenHandle =
  for i in 0 ..< len(l):
    yield l[i]

proc first*(l: ptr WaveTokenListHandle): ptr WaveTokenListIteratorHandle = tokenListBeginIterator(l)
proc last*(l: ptr WaveTokenListHandle): ptr WaveTokenListIteratorHandle = tokenListEndIterator(l)
proc `!=`*(iter1, iter2: ptr WaveTokenListIteratorHandle): bool = neqListIterator(iter1, iter2)
proc `==`*(iter1, iter2: ptr WaveTokenListIteratorHandle): bool {.error.}
proc deref*(i: ptr WaveTokenListIteratorHandle): ptr WaveTokenHandle = listIterDeref(i)
proc advance*(i: ptr WaveTokenListIteratorHandle) = listIterAdvance(i)

iterator items*(l: ptr WaveTokenListHandle): ptr WaveTokenHandle =
  var iter1 = first(l)
  var iter2 = last(l)
  while iter1 != iter2:
    yield deref(iter1)
    advance(iter1)


# proc addMacroDefinition*(
#     ctx: ptr WaveContextHandle,
#     str: cstring,
#     isPredefined: bool
#   ) {.apiProc, importc: "wave_addMacroDefinition".}

#   ##[

# Adds a new macro definition to the macro symbol table. The parameter
# macrostring should contain the macro to define in the command line format,
# i.e. something like `MACRO(x)=definition`. The following table describes
# this format in more detail. The parameter is_predefined should be true
# while defining predefined macros, i.e. macros, which are not undefinable
# with an #undef directive from inside the preprocessed input stream. If this
# parameter is not given, it defaults to false.

# **Summary of possible formats for defining macros**

# ====================== ==================================
# macro definition       description
# ====================== ==================================
# `MACRO`                 define `MACRO` as `1`
# `MACRO=`                define `MACRO` as nothing (empty)
# `MACRO=definition`      define `MACRO` as definition
# `MACRO(x)`              define `MACRO(x)` as `1`
# `MACRO(x)=`             define `MACRO(x)` as nothing (empty)
# `MACRO(x)=definition`   define `MACRO(x)` as `definition`
# ====================== ===================================

# The function returns false, if the macro to define already was defined and
# the new definition is equivalent to the existing one, it returns true, if
# the new macro was successfully added to the macro symbol table.

# If the given macro definition resembles a redefinition and the new macro is
# not identical to the already defined macro (in the sense defined by the C++
# Standard), the function throws a corresponding `preprocess_exception` using
# `throw_exception` override for the active context.

# For C wrappers exception is stored in the context and can be checked for
# using `hasErrors`, and accessed using `popDiagnostics`

#   ]##

# proc addMacroDefinition*(
#     ctx: ptr WaveContextHandle,
#     str: string,
#     isPredefined: bool = false) =
#   addMacroDefinition(ctx, str.cstring, isPredefined)

# proc getMacroDefinition*(
#     ctx: ptr WaveContextHandle,
#     name: cstring,
#     isFunctionStyle: ptr bool,
#     isPredefined: ptr bool,
#     pos: ptr WavePosition,
#     parameters: ptr ptr WaveTokenVectorHandle,
#     definition: ptr ptr WaveTokenListHandle
#   ): bool {.apiProc, importc: "wave_getMacroDefinition".}

#   ##[

# Allows to retrieve all information known with regard to a macro definition.
# parameters

# - @arg{name} :: specifies the name of the macro the information should
#   be returned for.
# - @arg{isFunctionStyle}, @arg{isPredefined} :: whether the
#   macro has been defined as a function style macro or as a
#   predefined macro resp.
# - @arg{pos} :: will contain the position the
#   macro was defined at.
# - @arg{parameters} :: will contain the names of
#   the parameters the macro was defined with and the parameter definition will
#   contain the token sequence for the definition (macro body).

# The function returns true is the macro was defined and the requested
# information has been successfully retrieved, false otherwise.

#   ]##

# type
#   WaveMacroDefinition* = object
#     isFunctionStyle: bool
#     isPredefined: bool
#     pos: WavePosition
#     parameters: ptr WaveTokenVectorHandle
#     definition: ptr WaveTokenListHandle

# proc `=destroy`(def: var WaveMacroDefinition) =


# proc getMacroDefinition*(
#     ctx: ptr WaveContextHandle, name: string): WaveMacroDefinition =
#   getMacroDefinition(
#     ctx,
#     name.cstring,
#     addr result.isFunctionStyle,
#     addr result.isPredefined,
#     addr result.parameters,
#     addr result.definition
#   )


# type WaveProcessingHooksHandle {.apiPtr.} = object
