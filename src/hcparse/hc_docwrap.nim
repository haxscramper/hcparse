import
  ./hc_types, ./cxcommon, ./cxtypes, ./hc_typeconv,
  ./dox_xml,
  std/[strformat, tables, hashes, strutils, options, xmltree],
  hmisc/other/[colorlogger, oswrap],
  hmisc/algo/hstring_algo,
  hmisc/types/[hmap],
  hmisc/base_errors,
  hpprint

import
  ./dox_compound as DoxCompound,
  ./dox_index as DoxIndex


using ids: var RefidMap

proc addLocation(ids; id, name: string, loc: LocationType) =
  ids.map.mgetOrPut(loc.file, Map[int, seq[DoxRefid]]()).
    mgetOrPut(loc.line, @[]).add DoxRefid(
      refid: id,
      name: name,
      line: loc.line,
      column: loc.column.get()
    )

proc register(compound: DoxCompound.CompounddefType; ids) =
  case compound.kind:
    of dckDir:
      discard

    of dckClass:
      ids.addLocation(
        compound.id, compound.compoundname, compound.location.get())

      for section in compound.sectiondef:
        for member in section.memberdef:
          ids.addLocation(member.id, $member.name[0], member.location)

    of dckFile:
      discard

    else:
      pprint compound
      raise newImplementKindError(compound)


proc register(file: DoxCompound.DoxygenType; ids) =
  for compound in file.compounddef:
    register(compound, ids)


proc getRefidLocations*(doxDir: AbsDir): RefidMap =
  let index = indexForDir(doxDir)
  for item in index.compound:
    let file = item.fileForItem(doxDir).parseDoxygenFile()
    file.register(result)
