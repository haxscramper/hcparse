import
  hmisc/preludes/unittest

import
  hcparse/read_deps/conan

suite "conan 1":
  test "get deps":
    let dir = getAppTempDir()
    let info = getBuildInfo("libgit2", (1, 3, 0), dir)
