import ./boost_wave

proc main() =
  let ctx = newWaveContext(allocCStringArray(["int main() {}"])[0], "<Unknown>".cstring)

  var first: PWaveIterator = ctx.first()
  while first != ctx.last():
    let tok = first.getTok()
    echo "TOK: [", tok.getValue(), "] KIND: ", tok.getId()
    first.advance()

when isMainModule:
  main()
