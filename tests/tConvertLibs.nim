import hmisc/preludes/unittest
import hmisc/other/oswrap
import hcparse/[hc_parsefront, hc_codegen, hc_impls]
import hnimast/hast_common
import hmisc/algo/hstring_algo
import hmisc/algo/hparse_pegs
import compiler/ast

proc fixGit*(name: string, isType: bool): string =
  dropPrefix(name, "git_").snakeToCamelCase()


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


  test "libgit types":
    let lib = AbsDir"/usr/include/git2"
    let file = lib /. "types.h"
    let res = getTestTempFile("nim")
    var cache = newWaveCache()

    echov res
    res.writeFile(
      $toNNode[PNode](wrapViaTs(file, lib, fixConf), cCodegenConf))
