import std/[
  options,
  strutils,
  sets,
]

import strslice


type
  JsTokenKind* {.pure.} = enum
    jtInvalid
    jtEof

    jtSemicolon
    jtColon
    jtComma

    jtEq, jtEqEq, jtEqEqEq
    jtPlusEq, jtMinusEq

    jtLambdaArrow
    jtDot
    jtStar
    jtPlus
    jtMinus
    jtSlash
    jtAnd
    jtGreater
    jtLess
    jtExclamation

    jtOpenCurly
    jtCloseCurly
    jtOpenParen
    jtCloseParen
    jtOpenSquare
    jtCloseSquare

    jtReserved
    jtIdent
    jtLitNum
    jtLitStr

  JsToken* = object
    raw*: StrSlice
    kind*: JsTokenKind

  JsTokenStream* = object
    raw*: StrSlice
    forceSemicolon: bool

const jsReservedWords* = [
  # present
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "new",
  "null",
  "return",
  "super",
  "switch",
  "this",
  "throw",
  "true",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "with",

  # future
  "enum",
].toHashSet()


const automaticSemicolonWords = [
  "return",
  "continue",
  "break",
  "throw",
].toHashSet()


proc skipWhitespace*(p: var JsTokenStream) {.inline.} = p.raw.skipWhitespace()
proc atEof*(p: JsTokenStream): bool {.inline.} = p.raw.atEof()

proc takeJsIdentifier(p: var StrSlice): JsToken =
  result = JsToken(raw : p, kind : JsTokenKind.jtIdent)
  while p.p[0].isAlphaNumeric() or p.p[0] == '_':
    inc p
  result.raw.z = p.p
  if $result.raw in jsReservedWords:
    result.kind = JsTokenKind.jtReserved

proc takeJsToken*(p: var JsTokenStream): JsToken =
  template resSingleCharTok(resKind: JsTokenKind): untyped =
    var c = p.raw
    inc c
    result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : resKind)
    p.raw = c

  result = JsToken(
    kind : JsTokenKind.jtInvalid
  )

  while result.kind == JsTokenKind.jtInvalid:
    if p.forceSemicolon:
      p.forceSemicolon = false
      result.kind = JsTokenKind.jtSemicolon
      break

    p.skipWhitespace()
    if p.atEof():
      return JsToken(kind : JsTokenKind.jtEof)
    if p.raw.p[0].isAlphaAscii() or p.raw.p[0] == '_':
      result = p.raw.takeJsIdentifier()
      if p.raw.p[0] == '\n':
        if $result.raw in automaticSemicolonWords:
          p.forceSemicolon = true
    elif p.raw.p[0].isDigit():
      var c = p.raw
      inc c
      # TODO: Implement more numeric formats
      var
        isHex = false
        isDecimal = false
      while not c.atEof():
        let curComposite = $StrSlice(p : p.raw.p, z : c.p)
        if not c.p[0].isDigit():
          if c.p[0] == 'x' and curComposite == "0":
            isHex = true
          elif c.p[0] == '.' and not isDecimal:
            isDecimal = true
          elif isHex and c.p[0] in {'a'..'f'} + {'A'..'F'}:
            discard
          else:
            break
        inc c
      result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtLitNum)
      p.raw = c
    else:
      case p.raw.p[0]
      of ';': resSingleCharTok(JsTokenKind.jtSemicolon)
      of ':': resSingleCharTok(JsTokenKind.jtColon)
      of '.': resSingleCharTok(JsTokenKind.jtDot)
      of ',': resSingleCharTok(JsTokenKind.jtComma)

      of '{': resSingleCharTok(JsTokenKind.jtOpenCurly)
      of '}': resSingleCharTok(JsTokenKind.jtCloseCurly)
      of '(': resSingleCharTok(JsTokenKind.jtOpenParen)
      of ')': resSingleCharTok(JsTokenKind.jtCloseParen)
      of '[': resSingleCharTok(JsTokenKind.jtOpenSquare)
      of ']': resSingleCharTok(JsTokenKind.jtCloseSquare)

      of '"', '\'', '`':
        # TODO: Error newline during " and '
        let quoteChar = p.raw.p[0]
        var c = p.raw
        inc c
        var inEscape = false
        while not c.atEof():
          if c.p[0] == '\\':
            inEscape = true
          else:
            if c.p[0] == quoteChar and not inEscape:
              inc c
              break
            inEscape = false
          inc c
        result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtLitStr)
        p.raw = c
      of '=':
        var c = p.raw
        inc c
        if c.len() > 0:
          if c.p[0] == '=':
            inc c
            if c.len() > 0 and c.p[0] == '=':
              inc c
              result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtEqEqEq)
            else:
              result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtEqEq)
          elif c.p[0] == '>':
            inc c
            result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtLambdaArrow)
          else:
            result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtEq)
        p.raw = c

      # TODO = variants for all
      of '&': resSingleCharTok(JsTokenKind.jtAnd)
      of '+':
        var c = p.raw
        inc c
        if c.len() > 0 and c.p[0] == '=':
          inc c
          result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtPlusEq)
        else:
          result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtPlus)
        p.raw = c
      of '-':
        var c = p.raw
        inc c
        if c.len() > 0 and c.p[0] == '=':
          inc c
          result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtMinusEq)
        else:
          result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtMinus)
        p.raw = c
      of '>': resSingleCharTok(JsTokenKind.jtGreater)
      of '<': resSingleCharTok(JsTokenKind.jtLess)
      of '!': resSingleCharTok(JsTokenKind.jtExclamation)
      of '*': resSingleCharTok(JsTokenKind.jtStar)
      of '/':
        var c = p.raw
        inc c
        if c.len() > 0 and c.p[0] == '/':
          # single comment
          inc c
          while not c.atEof() and c.p[0] != '\n':
            inc c
        elif c.len() > 0 and c.p[0] == '*':
          # multiline comment
          inc c
          while not c.atEof():
            if c.len() > 1 and c.p[0] == '*' and c.p[1] == '/':
              break
            inc c
          inc c
        else:
          result = JsToken(raw : StrSlice(p : p.raw.p, z : c.p), kind : JsTokenKind.jtSlash)
        p.raw = c
      else:
        raiseAssert "Unexpected character in slice: " & $p

proc peekJsToken*(p: JsTokenStream): JsToken {.inline.} =
  var p = p
  result = p.takeJsToken()

proc expectJsToken*(p: var JsTokenStream, kind: JsTokenKind, raw: Option[string] = none(string)): JsToken {.discardable.} =
  result = p.takeJsToken()
  if result.kind != kind or (raw.isSome() and $result.raw != raw.unsafeGet()):
    let a = if raw.isSome(): "(" & raw.unsafeGet() & ")" else: ""
    raiseAssert "Unexpected token: " & $result & " wanted " & $kind & a

template withJsTokenStream*(buff: openArray[char], name: untyped, body: untyped): untyped =
  var name = JsTokenStream(
    raw : buff.toSlice(),
  )
  body


when isMainModule:
  var testData = readFile("test.js")
  testData.withJsTokenStream(p):
    p.skipWhitespace()
    while not p.atEof():
      echo p.takeJsToken()
      p.skipWhitespace()
