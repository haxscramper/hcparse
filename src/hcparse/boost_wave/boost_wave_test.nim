import ./boost_wave

proc main() =
  var ctx = newWaveContext(allocCStringArray(["#warning \"123\"\n"])[0], "<Unknown>".cstring)
  ctx.setFoundWarningDirective(
    proc(ctx: ptr CWaveContextImpl, message: ptr CWaveTokenList): EntryHandling =
      echo "Found warning directive"
      return ehSkip
  )

  var first: PWaveIterator = ctx.first()
  while first != ctx.last():
    let tok = first.getTok()
    echo "TOK: [", tok.getValue(), "] KIND: ", tok.getId()
    first.advance()

when isMainModule:
  main()
