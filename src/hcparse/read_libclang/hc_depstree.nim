#=====================  Dependency list generation  ======================#
# ~~~~ CLI helper path resolution ~~~~ #
proc getHCParseBinDir*(): AbsDir =
  ## Return absolute path `/bin` directory with helper cmdline tools;
  ## NOTE right now I have no idea how to handle dependencies like
  ## this - this is just a hacky solution.
  for dir in AbsDir(currentSourcePath()).parentDirs():
    if (dir /. "hcparse.nimble").fileExists():
      return dir / "bin"

  raise newException(
    IOError,
    "Could not find `hcparse.nimble` in any of the parent directories")

proc getHCParseBinPath*(name: string): AbsFile =
  ## Return absolute name of the helper cmdline tool `name`
  let bindir = getHCParseBinDir()
  let file = bindir /. name
  if fileExists(file):
    return file
  else:
    raise newException(IOError, "Could not find '" & $file & "'")

# ~~~~ Dependency tree construction ~~~~ #

type
  CDepsTree* = object
    file*: AbsFile
    name*: string
    deps*: seq[CDepsTree]

proc parseBuildDepsTree*(outs: string): CDepsTree =
  let depLines = outs.split("\n")
  var idx = 0
  info "deps list has ", depLines.len,  " lines"

  proc auxTree(): seq[CDepsTree] {.closure.} =
    while not depLines[idx].startsWith(Whitespace, "}"):
      if deplines[idx] =~ re".*?<(.*?)> (.*)":
        inc idx
        result.add CDepsTree(name: matches[0], file: matches[1].toAbsFile)
      elif deplines[idx] =~ re""".*?"(.*?)" (.*)""":
        inc idx
        result.add CDepsTree(name: matches[0], file: matches[1].toAbsFile)
      elif depLines[idx].startsWith(Whitespace, "{"):
        inc idx
        # echo "startin level ", depLines[idx]
        result.last.deps = auxTree()
      elif depLines[idx].isEmptyOrWhitespace():
        inc idx
        return

    inc idx


  return CDepsTree(deps: auxTree())

proc buildDepsTree*(file: AbsFile, args: seq[string]): CDepsTree =
  let bin = getHCParseBinPath("deps")
  assert file.fileExists()

  try:
    let (outs, _, _) = runShell makeGnuShellCmd($bin).withIt do:
      it.raw args.joinw()
      it.arg file

    result = parseBuildDepsTree(outs)
    result.file = file


  except ShellError:
    printShellError()
    echo "Arguments:"
    echo args.joinql()


proc immediateDeps*(d: CDepsTree): seq[AbsFile] =
  d.deps.mapIt(it.file)
