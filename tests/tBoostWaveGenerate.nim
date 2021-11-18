import hmisc/preludes/unittest

import
  compiler/[ast, renderer],
  hcparse,
  hmisc/other/hpprint,
  hmisc/algo/[namegen, hstring_algo],
  hmisc/other/[oswrap, hshell],
  hmisc/core/all

import std/[options, sets]

let dir = AbsDir(relToSource"../src/hcparse/read_boost_wave")

var fixConf = baseFixConf
fixConf.libName = "wave"
fixConf.typeStore = newTypeStore()

startHax()

suite "Generate wave file":
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


    let res = dir /. "boost_wave_wrap_tmp.nim"

    var codegen = cCodegenConf

    let lib = cxxLibImport("wave", @["wave_c_api.h"])

    let ir = expandViaCc(dir /. "wave_c_api.h", baseCParseConf).
      wrapViaTs(fixConf, lib).
      postFixEntries(fixConf, lib)


    writeFile(
      res,
      lit3"""
        import std/os
        const boostWaveLibDir = currentSourcePath().splitFile().dir / "../../../lib"
        const cwaveDl* = boostWaveLibDir / "libboost_cwave.so"

        """ & ir.toString(codegen))

    execShell shellCmd(nim, check, $res)

suite "Run generated wave file":
  test "Compile and execute":
    execShell shellCmd(nim, r, $relToSource("tBoostWaveExecute.nim"))
