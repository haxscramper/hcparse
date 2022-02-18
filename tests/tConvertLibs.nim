import
  hcparse/[hc_parsefront, hc_impls],
  hcparse/processor/[hc_grouping],
  hcparse/codegen/[hc_codegen],
  hcparse/read_boost_wave/[hc_wavereader]


import hnimast/hast_common
import hmisc/algo/[ hparse_pegs, hstring_algo ]
import hmisc/preludes/unittest
import hmisc/other/oswrap

import compiler/ast

import std/[strutils, sequtils]

proc fixGit*(
    sys = dir / "sys"
    user = dir / "user"
    file = user /. "user_main.h"

  mkWithDirStructure dir:
    dir sys:
      file "sys_file.h": "#define sys_macro expanded\n"

    dir user:
      file "user_main.h":
        lit3"""
        #include "user_sub.h"
        sys_macro
        """
      file "user_sub.h":
        lit3"""
        #include <sys_file.h>
        """

  test "Subcontext with failed include":
    var cache = newWaveCache()
    var reader = newWaveReader(file, cache, baseCParseConf)
    expect WaveError as we:
      discard reader.getExpanded()

    check:
      we.diag.code == wekBadIncludeFile
      we.diag.line == 1
      "Could not find include file" in we.msg

  test "Subcontext with correct include":
    var cache = newWaveCache()
    var conf = baseCParseConf
    conf.sysIncludes.add sys.string
    var reader = newWaveReader(file, cache, conf)
    check:
      reader.getExpanded().strip() == "expanded"





suite "libgit":
  var enumMap: PegCallReplaceMap
  for prefix in @[
    "OBJECT",
    "REFERENCE",
    "BRANCH",
    "FILEMODE",
    "SUBMODULE_UPDATE",
    "SUBMODULE_IGNORE",
    "SUBMODULE_RECURSE"
  ]:
    enumMap.add(
      sequence(term("GIT_" & prefix), *term('_'), capture(*anyChar())),
      toReplaceHandler("g" & abbrevSnake(prefix) & "${snakeToCamel}")
    )


  var fixConf = baseFixConf

  fixConf.isIcpp = false
  fixConf.libName = "git"

  fixConf.onGetBind():
    case entry.kind:
      of cekProc: result = cxxDynlibVar("libgitDl")
      else: result = cxxNoBind()

  fixConf.onFixName():
    if cache.knownRename(name.nim):
      return cache.getRename(name.nim)

    result = keepNimIdentChars(name.nim)

    cache.newRename(name.nim, result)

  fixConf.typeStore = newTypeStore()

  let outDir = getTestTempDir()

  # test "Expand libgit":
  #   # This is not a tests, this is a fucking joke, but I have no idea how
  #   # to debug spontaneous nim closure failures when they are passed to the
  #   # boost wave side, so for now I just repeatedly run the same test file
  #   # until I get all entries wrapped correctly.

  #   mkDir outDir
  #   let lib = AbsDir"/usr/include/git2"
  #   var cache = newWaveCache()
  #   for file in walkDir(lib, AbsFile):
  #     if file.name() notin [
  #       "stdint" # Use this header only with Microsoft Visual C++ compilers!
  #     ]:
  #       let resFile = (outDir /. file.name()) &. "h"

  #       if not exists(resFile):
  #         var reader = newWaveReader(file, cache, baseCParseConf)
  #         resFile.writeFile(reader.getExpanded())


  # test "libgit types":
  #   var resultWrapped: seq[CxxFile]
  #   block:
  #     for file in walkDir(outDir, AbsFile, exts = @["h"]):
  #       resultWrapped.add wrapViaTs(file, outDir, fixConf)

  #   echo "Collected files"

  #   for fix in regroupFiles(resultWrapped):
  #     let res = outDir / fix.getFile().withExt("nim")
  #     res.writeFile($toNNode[PNode](fix, cCodegenConf))
