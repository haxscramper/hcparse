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

proc first*(ctx: ptr WaveContextHandle): ptr WaveIteratorHandle = ctx.beginIterator()
proc last*(ctx: ptr WaveContextHandle): ptr WaveIteratorHandle = ctx.endIterator()
proc getTok*(iter: ptr WaveIteratorHandle): ptr WaveTokenHandle = iter.iterGetTok()
proc advance*(iter: ptr WaveIteratorHandle) = iter.advanceIterator()
proc `!=`*(iter1, iter2: ptr WaveIteratorHandle): bool = neqIterator(iter1, iter2)
proc `==`*(iter1, iter2: ptr WaveIteratorHandle): bool {.error.}
proc getValue*(tok: ptr WaveTokenHandle): cstring = tok.tokGetValue()
proc kind*(tok: ptr WaveTokenHandle): WaveTokId = tok.tokGetId()




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
