import
  compiler/[ast, renderer],
  hcparse/[hc_parsefront, hc_codegen, hc_impls],
  hcparse/interop_ir/wrap_store,
  hmisc/algo/[namegen, hstring_algo],
  hmisc/other/oswrap,
  hmisc/core/all

import std/options

let dir = AbsDir(relToSource"../src/hcparse/boost_wave")

let code = expandViaCc(dir /. "wave_c_api.h", baseCParseConf)

var fixConf = baseFixConf
fixConf.libName = "wave"

starthax()

fixConf.fixNameImpl = proc(
    name: CxxNamePair,
    cache: var StringNameCache,
    context: CxxNameFixContext,
    conf: CxxFixConf
  ): string =
  if cache.knownRename(name.nim):
    return cache.getRename(name.nim)

  if ?context[cancLibName]:
    result = name.nim.dropNormPrefix(context[cancLibName].get().nim)

  else:
    result = name.nim

  cache.newRename(name.nim, result)

fixConf.getBind =
  proc(e: CxxEntry): CxxBind =
    if e of cekProc:
      cxxDynlib("libbost_wave.so")

    else:
      cxxHeader("wave_c_api.h")


writeFile(dir /. "boost_wave_wrap.nim", code.wrapViaTs(fixConf).toString(cCodegenConf))
