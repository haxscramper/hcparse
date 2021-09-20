import hmisc/preludes/unittest
import hmisc/other/oswrap
import hcparse/[hc_parsefront, hc_codegen]
import hnimast/hast_common
import hmisc/algo/hstring_algo
import compiler/ast

proc fixGit*(name: string, isType: bool): string =
  dropPrefix(name, "git_").snakeToCamelCase()

suite "libgit":
  test "libgit types":
    let lib = AbsDir"/usr/include/git2"
    let file = lib /. "types.h"

    echo $toNNode[PNode](wrapViaTs(file, false, lib, fixGit), cCodegenConf)
