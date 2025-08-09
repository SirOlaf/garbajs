type
  StrSlice* = object
    p*: ptr UncheckedArray[char]
    z*: pointer # having the end pointer is more efficient than storing the length as it saves an operation during `inc`


proc len*(x: StrSlice): int {.inline.} =
  cast[int](x.z) - cast[int](x.p)

proc `$`*(x: StrSlice): string =
  result = newString(x.len)
  if x.len() > 0:
    copyMem(addr result[0], x.p, x.len)

proc inc*(x: var StrSlice, n: int = 1) {.inline.} =
  x.p = cast[ptr UncheckedArray[char]](addr x.p[n])

proc atEof*(p: StrSlice): bool {.inline.} =
  p.len == 0


proc toSlice*(buff: ptr UncheckedArray[char], len: int): StrSlice {.inline.} =
  StrSlice(p : buff, z : cast[ptr[UncheckedArray[char]]](cast[int](buff) + len))

proc toSlice*(buff: openArray[char]): StrSlice {.inline.} =
  toSlice(cast[ptr UncheckedArray[char]](addr buff[0]), buff.len())

template withSlice*(buff: openArray[char], name: untyped, body: untyped): untyped =
  var name = buff.toSlice()
  body


proc skipWhitespace*(p: var StrSlice) =
  while not p.atEof() and p.p[0] in {' ', '\n'}:
    inc p

proc expectChar*(p: var StrSlice, c: char) =
  p.skipWhitespace()
  if p.p[0] != c:
    raiseAssert "Expected " & c & " got " & p.p[0]
  inc p

proc peekChar*(p: var StrSlice): char {.inline.} =
  p.skipWhitespace()
  p.p[0]
