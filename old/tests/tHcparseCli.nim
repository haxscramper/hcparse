import
  hmisc/preludes/unittest,
  hcparse/hcparse_cli

suite "Setup project using conan":
  test "libgit2 wrappers":
    let dir = getTestTempDir(true)
    echov dir

    withDir dir:
      hcparseCli(@["init", "conan", "zlib", "1.2.11"])
