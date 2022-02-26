import hmisc/preludes/unittest

import
  compiler/ast/[ast, renderer],
  hcparse,
  hmisc/other/hpprint,
  hmisc/algo/[namegen, hstring_algo],
  hmisc/other/[oswrap, hshell],
  hmisc/core/all

import std/[options, sets]

let dir = AbsDir(relToSource"../src/hcparse/read_boost_wave")

startHax()

suite "Generate wave file":
  let bindDl = false
  test "Boost wave C API":
    let fixConf = baseFixConf.withIt do:
      it.libName = "wave"
      it.typeStore = newTypeStore()

      it.onFixName():
        if cache.knownRename(name.nim):
          return cache.getRename(name.nim)

        if ?context[cancLibName] and name.context == cncProc:
          result = name.nim.dropNormPrefix(context[cancLibName].get().nim)

        else:
          result = name.nim

        result = fixContextedName(name, result)
        cache.newRename(name.nim, result)


      it.onGetBind():
        if entry of cekProc:
          if bindDL:
            cxxDynlibVar("cwaveDl")

          else:
            cxxLinkBind()

        else:
          cxxHeader("wave_c_api.h")


    let res = dir /. "boost_wave_wrap_tmp.nim"

    var codegen = cCodegenConf.withIt do:
      it.nameStyle = idsCamel

    let lib = cxxLibImport("wave", @["wave_c_api.h"])

    let ir = expandViaCc(dir /. "wave_c_api.h", baseCParseConf).
      wrapViaTs(fixConf, lib).
      postFixEntries(fixConf, lib)


    writeFile(
      res,
      if bindDl:
        lit3"""
          import std/os
          const boostWaveLibDir = currentSourcePath().splitFile().dir / "../../../lib"
          const cwaveDl* = boostWaveLibDir / "libboost_cwave.so"

          """ & ir.toString(codegen)

      else:
        "{.passc: \"-lboost_cwave\".}\n{.passl: \"-lboost_cwave\".}\n" &
        ir.toString(codegen)
    )

    execShell shellCmd(nim, check, $res)

suite "Run generated wave file":
  test "Compile and execute":
    execShell shellCmd(nim, r, $relToSource("tBoostWaveExecute.nim"))
