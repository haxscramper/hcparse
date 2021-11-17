import
  hmisc/preludes/unittest

import
  hcparse/read_deps/conan,
  hcparse

suite "conan 1":
  test "get deps":
    let dir = getTestTempDir(true)
    let info = getBuildInfo("libgit2", (1, 3, 0), dir)

    let root = info.findRootPath("libgit2")
    let resultGrouped = wrapCSharedLibViaTsWave(
      inDir       = root,
      outDir      = dir / "res",
      tmpDir      = dir / "tmp",
      libName     = "git2",
      packageName = "libgit2",
      ignoreIn    = @["stdint"]
    )
