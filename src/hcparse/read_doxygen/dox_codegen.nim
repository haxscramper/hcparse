import 
  nimtraits/xml_to_types,
  hmisc/other/[oswrap, hshell],
  hnimast,
  hmisc/hdebug_misc

const dir = currentSourceDir()

proc doxygenXmlGenerate() =
  let compound = cwd() /. "dox_compound.nim"
  generateForXsd(cwd() /. "dox_compound.xsd").
    writeXsdGenerator(compound)

  let index = cwd() /. "dox_index.nim"
  generateForXsd(cwd() /. "dox_index.xsd").
    writeXsdGenerator(index)

  execShell shellCmd(nim, check, errormax = 2, $compound), limitErr = 30
  execShell shellCmd(nim, check, errormax = 2, $index), limitErr = 30

  echo "Doxygen generate done"

  execShell shellCmd(nim, r, "dox_xml.nim")

when isMainModule:
  if paramCount() == 0:
    startHax()
    doxygenXmlGenerate()

  else:
    case paramStr(0):
      of "doxygen": doxygenXmlGenerate()
