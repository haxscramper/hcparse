import
  compiler/[ast, renderer],
  hcparse/[hc_parsefront, hc_codegen, hc_impls],
  hmisc/other/hpprint,
  hcparse/interop_ir/wrap_store,
  hmisc/algo/[namegen, hstring_algo],
  hmisc/other/[oswrap, hshell],
  hmisc/core/all

import std/[options, sets]

let dir = AbsDir(relToSource"../src/hcparse/boost_wave")

var fixConf = baseFixConf
fixConf.libName = "wave"

starthax()

func isSharedTypeName*(name: string): bool =
  const sharedNames = toHashSet [
    "void", "int", "float", "bool", "char", # ...
  ]

  return name in sharedNames

fixConf.fixNameImpl = proc(
    name: CxxNamePair,
    cache: var StringNameCache,
    context: CxxNameFixContext,
    conf: CxxFixConf
  ): string =
  if cache.knownRename(name.nim):
    return cache.getRename(name.nim)

  if ?context[cancLibName] and name.context == cncProc:
    result = name.nim.dropNormPrefix(context[cancLibName].get().nim)

  else:
    result = name.nim

  let (prefix, suffix) =
    case name.context:
      of cncType: ("", "T")
      of cncArg: ("arg", "")
      of cncProc: ("c", "")
      else: raise newImplementKindError(name.context)

  if name.context != cncType and isReservedNimType(result):
    result = prefix & result & suffix

  if isReservedNimIdent(result):
    result = prefix & result & suffix

  cache.newRename(name.nim, result)

let dynProcs = cxxDynlibVar("cwaveDl")

fixConf.getBind =
  proc(e: CxxEntry): CxxBind =
    if e of cekProc:
      dynProcs

    else:
      cxxHeader("wave_c_api.h")


let res = dir /. "boost_wave_wrap.nim"

var codegen = cCodegenConf

# codegen.declBinds = some (dynProcs, @{
#   "linux": cxxDynlib("libboost_cwave.so")
# })

let ir = expandViaCc(dir /. "wave_c_api.h", baseCParseConf).
    # printNumerated(330 .. 340).
    wrapViaTs(fixConf)

writeFile(
  res,
  """
import std/os
const boostWaveLibDir = currentSourcePath().splitFile().dir / "../../../lib"
const cwaveDl* = boostWaveLibDir / "libboost_cwave.so"


""" & ir.toString(codegen))


execShell shellCmd(nim, r, $(dir /. "boost_wave_test.nim"))
