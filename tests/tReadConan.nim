import
  hmisc/preludes/unittest

import
  hcparse/read_deps/conan,
  hcparse

suite "conan 1":
  test "get conan info":
    let dir = getTestTempDir(true)
    let info = getBuildInfo("libgit2", (1, 3, 0), dir)

  test "Wrap package with dependencies":
    let sshDir = getTestTempDir(true) / "ssh"
    mkDir sshDir

    let
      sshInfo = getBuildInfo("libssh2", (1, 9, 0), sshDir)
      sshRoot = sshInfo.findIncludePath("libssh2")[0]

    let sshGrouped = wrapCSharedLibViaTsWave(
      inDir       = sshRoot,
      outDir      = sshDir / "res",
      tmpDir      = sshDir / "tmp",
      libName     = "ssh2",
      packageName = "libssh2"
    )

    let gitDir = getTestTempDir(true) / "git"
    mkDir gitDir

    let
      gitInfo = getBuildInfo("libgit2", (1, 3, 0), gitDir)
      gitRoot = gitInfo.findIncludePath("libgit2")[0]

    let gitGrouped = wrapCSharedLibViaTsWave(
      inDir       = gitRoot / "git2",
      outDir      = gitDir / "res",
      tmpDir      = gitDir / "tmp",
      libName     = "git2",
      packageName = "libgit2",
      ignoreIn    = @["stdint"]
    )

    echov "git wrapper ok"
