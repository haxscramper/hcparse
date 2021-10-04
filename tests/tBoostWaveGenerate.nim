import hmisc/preludes/unittest

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

suite "Generate":
  test "Boost wave C API":
    fixConf.onFixName():  
      if cache.knownRename(name.nim):
        return cache.getRename(name.nim)

      if ?context[cancLibName] and name.context == cncProc:
        result = name.nim.dropNormPrefix(context[cancLibName].get().nim)

      else:
        result = name.nim

      result = fixContextedName(name, result)
      cache.newRename(name.nim, result)

    let dynProcs = cxxDynlibVar("cwaveDl")

    fixConf.onGetBind():
      if entry of cekProc:
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
      lit3"""
        import std/os
        const boostWaveLibDir = currentSourcePath().splitFile().dir / "../../../lib"
        const cwaveDl* = boostWaveLibDir / "libboost_cwave.so"

        """ & ir.toString(codegen))

suite "Run":
  test "Compile and execute":
    execShell shellCmd(nim, r, $relToSource("tBoostWaveExecute.nim"))
