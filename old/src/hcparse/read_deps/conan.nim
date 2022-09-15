import
  pkg/[jsony]

import
  std/[strformat]

import
  hmisc/core/all,
  hmisc/other/[oswrap, hshell]

import
  ../read_libclang/hc_types

type
  ConanDep* = object
    version*: string
    name*: string
    rootpath*: string
    description*: string
    includePaths*: seq[string]
    libPaths*: seq[string]
    cxxflags*: seq[string]
    cflags*: seq[string]


  ConanBuildInfo* = object
    dependencies*: seq[ConanDep]

proc findIncludePath*(info: ConanBuildInfo, lib: string): seq[AbsDir] =
  for dep in info.dependencies:
    if dep.name == lib:
      for path in dep.includePaths:
        result.add AbsDir(path)

proc getBuildInfo*(
    name: string,
    version: tuple[major, minor, patch: int],
    tempDir: AbsDir
  ): ConanBuildInfo =

  assertExists(tempDir)

  writeFile(tempDir /. "conanfile.txt", &"""
[requires]
{name}/{version.major}.{version.minor}.{version.patch}

[generators]
json
""")

  withDir tempDir:
    let cmd = shellCmd(conan, install, ".", "build" = "missing")
    let res = evalShell cmd

    result = readFile("conanbuildinfo.json").fromJson(ConanBuildInfo)

proc add*(conf: var ParseConf, info: ConanBuildInfo) =
  for dep in info.dependencies:
    for path in dep.includePaths:
      conf.includePaths.add AbsDir(path)
