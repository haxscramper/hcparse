import hmisc/preludes/unittest
import hmisc/other/oswrap
import hcparse/[hc_parsefront, hc_codegen, hc_impls, hc_wavereader, hc_grouping]
import hnimast/hast_common
import hmisc/algo/hstring_algo
import hmisc/algo/hparse_pegs
import compiler/ast
import std/strutils

proc fixGit*(name: string, isType: bool): string =
  dropPrefix(name, "git_").snakeToCamelCase()

suite "Bug hunting for git":
  let
    dir = getTestTempDir()
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
    var reader = newWaveReader(file, cache)
    expect WaveError as we:
      discard reader.getExpanded()

    check:
      we.diag.code == wekBadIncludeFile
      we.diag.line == 1
      we.diag.column == 1
      "could not find include file" in we.msg

  test "Subcontext with correct include":
    var cache = newWaveCache()
    var reader = newWaveReader(file, cache, sysIncludes = @[$sys])
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

    case name.context:
      of cncEnumField:
        result = replaceInterpolAny(name.nim, enumMap)

      of cncType:
        if isSharedTypename(name.nim):
          result = name.nim

        else:
          result = cache.fixTypeName(name.nim)

      else:
        result = fixContextedName(name)

    cache.newRename(name.nim, result)

  fixConf.typeStore = newTypeStore()

  test "libgit types":
    let lib = AbsDir"/usr/include/git2"
    let file = lib /. "types.h"
    let res = getTestTempFile("nim")
    var cache = newWaveCache()

    let wrapped: CxxFile = wrapViaTsWave(
      file, lib, fixConf, cache,
      @[],
      @["/usr/include/sys", "/usr/include", "/usr/include/linux"])

    let fixWrapped = regroupFiles(@[wrapped])

    res.writeFile($toNNode[PNode](fixWrapped[0], cCodegenConf))
    echov res
