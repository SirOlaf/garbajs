import std/[
  options,
  strutils,
  sets,
]

import strslice
import tokens


# TODO: All the while loops should abandon at eof
# TODO: A lot of functions are extremely non-compliant/liberal and will accept broken code
# Must check at least the precendence rules at some point


type
  JsNodeKind* {.pure.} = enum
    jnInvalid

    jnFunctionLambdaExpr
    jnArrowLambdaExpr
    jnFunctionDefStmt

    jnCodeBlock # '{' [stmts] '}'
    jnTupleExpr # All the () things, parameters, grouped expressions
    jnIfExpr
    jnIfBranchExpr

    jnLet
    jnConst
    jnAsgnExpr
    jnIdent

    jnNumLit
    jnStrLit
    jnBoolLit
    jnObjectExpr
    jnNewExpr

    jnCallExpr
    jnSquareAccessExpr # arr[expr]

    jnReturnStmt
    jnThisExpr

    jnBinExpr
    jnDotExpr

  JsObjectElem* = object
    key: JsNode
    value: JsNode

  JsNode* = ref object
    case kind: JsNodeKind
    of jnFunctionLambdaExpr:
      functionLambdaParamsTuple: JsNode
      functionLambdaBody: JsNode
    of jnArrowLambdaExpr:
      arrowLambdaParamsTuple: JsNode
      arrowLambdaBody: JsNode
    of jnFunctionDefStmt:
      functionDefName: JsToken
      functionDefParamsTuple: JsNode
      functionDefBody: JsNode

    of jnCodeBlock:
      codeBlockStmts: seq[JsNode]
    of jnTupleExpr:
      tupleElems: seq[JsNode]
    of jnIfExpr:
      ifBranches: seq[JsNode]
      elseBranch: Option[JsNode]
    of jnIfBranchExpr:
      ifBranchCond: JsNode
      ifBranchBody: JsNode

    of jnLet:
      letAsgn: JsNode
    of jnConst:
      constAsgn: JsNode
    of jnAsgnExpr:
      asgnLhs: JsNode
      asgnOperator: JsToken
      asgnRhs: JsNode
    of jnIdent:
      identName: StrSlice

    of jnNumLit:
      numLitRaw: StrSlice
    of jnStrLit:
      strLitRaw: StrSlice
    of jnBoolLit:
      boolLitRaw: StrSlice
    of jnObjectExpr:
      objectElems: seq[JsObjectElem]
    of jnNewExpr:
      newTarget: JsNode

    of jnCallExpr:
      callTarget: JsNode
      callParams: seq[JsNode]
    of jnSquareAccessExpr:
      squareAccessTarget: JsNode
      squareAccessInnerExpr: JsNode
    of jnReturnStmt:
      returnExpr: Option[JsNode]

    of jnBinExpr:
      binLhs: JsNode
      binOperator: JsToken
      binRhs: JsNode
    of jnDotExpr:
      dotLhs: JsNode
      dotRhs: StrSlice
    else:
      discard

proc render(x: JsNode): string =
  result = newString(0)
  case x.kind
  of JsNodeKind.jnLet:
    result &= "let " & x.letAsgn.render()
  of JsNodeKind.jnConst:
    result &= "const " & x.constAsgn.render()
  of JsNodeKind.jnAsgnExpr:
    result &= x.asgnLhs.render() & " " & $x.asgnOperator.raw & " " & x.asgnRhs.render()
  of JsNodeKind.jnIdent:
    result &= $x.identName
  of JsNodeKind.jnNumLit:
    result &= $x.numLitRaw
  of JsNodeKind.jnStrLit:
    result &= $x.strLitRaw
  of JsNodeKind.jnDotExpr:
    result &= x.dotLhs.render() & "." & $x.dotRhs
  of JsNodeKind.jnCallExpr:
    result &= x.callTarget.render() & "("
    var paramStrs = newSeq[string]()
    for param in x.callParams:
      paramStrs.add(param.render())
    result &= paramStrs.join(", ") & ")"
  of JsNodeKind.jnTupleExpr:
    result &= "("
    var partsStrs = newSeq[string]()
    for elem in x.tupleElems:
      partsStrs.add(elem.render())
    result &= partsStrs.join(", ") & ")"
  of JsNodeKind.jnBinExpr:
    result &= x.binLhs.render() & " " & $x.binOperator.raw & " " & x.binRhs.render()
  of JsNodeKind.jnIfExpr:
    result &= x.ifBranches[0].render() & "\n"
    for i in 1 ..< x.ifBranches.len():
      result &= "else " & x.ifBranches[i].render()
    if x.elseBranch.isSome():
      result &= "else " & x.elseBranch.unsafeGet().render()
  of JsNodeKind.jnIfBranchExpr:
    result &= "if " & x.ifBranchCond.render() & " " & x.ifBranchBody.render()
  of JsNodeKind.jnCodeBlock:
    var lines = newSeq[string]()
    for s in x.codeBlockStmts:
      lines.add(s.render())
    result &= "{\n" & lines.join("\n") & "\n}"
  of JsNodeKind.jnArrowLambdaExpr:
    result &= x.arrowLambdaParamsTuple.render() & " => " & x.arrowLambdaBody.render()
  of JsNodeKind.jnFunctionLambdaExpr:
    result &= "function " & x.functionLambdaParamsTuple.render() & " " & x.functionLambdaBody.render()
  of JsNodeKind.jnFunctionDefStmt:
    result &= "function " & $x.functionDefName.raw & x.functionDefParamsTuple.render() & " " & x.functionDefBody.render()

  of JsNodeKind.jnReturnStmt:
    result &= "return"
    if x.returnExpr.isSome():

      result &= " " & x.returnExpr.unsafeGet().render()
  of JsNodeKind.jnThisExpr:
    result &= "this"
  of JsNodeKind.jnBoolLit:
    result &= $x.boolLitRaw
  of JsNodeKind.jnObjectExpr:
    result &= "{"
    var elemStrs = newSeq[string]()
    for elem in x.objectElems:
      elemStrs.add(elem.key.render() & " : " & elem.value.render())
    if elemStrs.len() == 0:
      result &= "}"
    else:
      result &= "\n" & elemStrs.join(",\n") & "\n}"
  of JsNodeKind.jnNewExpr:
    result &= "new " & x.newTarget.render()
  of JsNodeKind.jnSquareAccessExpr:
    result &= x.squareAccessTarget.render() & "[" & x.squareAccessInnerExpr.render() & "]"
  else:
    raiseAssert "Unimplemented: " & $x.kind


proc parseExpression(p: var JsTokenStream): JsNode
proc parseStmt(p: var JsTokenStream): JsNode

proc parseTupleExpression(p: var JsTokenStream): JsNode =
  var elems = newSeq[JsNode]()
  p.expectJsToken(JsTokenKind.jtOpenParen)
  while p.peekJsToken().kind != JsTokenKind.jtEof:
    if p.peekJsToken().kind == JsTokenKind.jtCloseParen:
      discard p.takeJsToken()
      break
    elems.add(p.parseExpression())
    if p.peekJsToken().kind == JsTokenKind.jtComma:
      discard p.takeJsToken()
  result = JsNode(
    kind : JsNodeKind.jnTupleExpr,
    tupleElems : elems,
  )

proc parseCodeBlock(p: var JsTokenStream): JsNode =
  var stmts = newSeq[JsNode]()
  p.expectJsToken(JsTokenKind.jtOpenCurly)
  p.skipWhitespace()
  while not p.atEof() and p.peekJsToken().kind != JsTokenKind.jtCloseCurly:
    stmts.add(p.parseStmt())
    while p.peekJsToken().kind == JsTokenKind.jtSemicolon:
      discard p.takeJsToken()
  p.expectJsToken(JsTokenKind.jtCloseCurly)

  result = JsNode(
    kind : JsNodeKind.jnCodeBlock,
    codeBlockStmts : stmts,
  )


proc parseReservedExpression(p: var JsTokenStream): JsNode =
  let tok = p.takeJsToken()
  case $tok.raw
  of "function":
    let params = p.parseTupleExpression()
    let body = p.parseCodeBlock()
    result = JsNode(
      kind : JsNodeKind.jnFunctionLambdaExpr,
      functionLambdaParamsTuple : params,
      functionLambdaBody : body,
    )
  of "if":
    result = JsNode(
      kind : JsNodeKind.jnIfExpr,
      ifBranches : newSeq[JsNode](),
      elseBranch : none(JsNode),
    )
    let tupleExpr = p.parseTupleExpression()
    let branchBody = p.parseStmt()
    result.ifBranches.add(JsNode(
      kind : JsNodeKind.jnIfBranchExpr,
      ifBranchCond : tupleExpr,
      ifBranchBody : branchBody,
    ))

    var next = p.peekJsToken()
    while next.kind == JsTokenKind.jtReserved and $next.raw == "else":
      discard p.takeJsToken()
      next = p.peekJsToken()
      if next.kind == JsTokenKind.jtReserved and $next.raw == "if":
        # else if
        discard p.takeJsToken()
        let ex = p.parseTupleExpression()
        let body = p.parseStmt()
        result.ifBranches.add(JsNode(
          kind : JsNodeKind.jnIfBranchExpr,
          ifBranchCond : ex,
          ifBranchBody : body,
        ))
      else:
        # else
        result.elseBranch = some p.parseStmt()
        break
  of "this":
    result = JsNode(kind : JsNodeKind.jnThisExpr)
  of "true", "false":
    result = JsNode(kind : JsNodeKind.jnBoolLit, boolLitRaw : tok.raw)
  of "new":
    result = JsNode(kind : JsNodeKind.jnNewExpr, newTarget : p.parseExpression())
  else:
    raiseAssert "Unimplemented: " & $tok


proc parseTerminalExpression(p: var JsTokenStream): JsNode =
  let tok = p.peekJsToken()
  case tok.kind
  of JsTokenKind.jtIdent:
    result = JsNode(
      kind : JsNodeKind.jnIdent,
      identName : tok.raw,
    )
    discard p.takeJsToken()
  of JsTokenKind.jtLitNum:
    result = JsNode(
      kind : JsNodeKind.jnNumLit,
      numLitRaw : tok.raw,
    )
    discard p.takeJsToken()
  of JsTokenKind.jtLitStr:
    result = JsNode(
      kind : JsNodeKind.jnStrLit,
      strLitRaw : tok.raw,
    )
    discard p.takeJsToken()
  of JsTokenKind.jtReserved:
    result = p.parseReservedExpression()
  of JsTokenKind.jtOpenParen:
    result = p.parseTupleExpression()
  of JsTokenKind.jtOpenCurly:
    # object { a: "b" }
    discard p.takeJsToken()
    var elems = newSeq[JsObjectElem]()
    while p.peekJsToken().kind != JsTokenKind.jtCloseCurly:
      let key = p.parseExpression()
      p.expectJsToken(JsTokenKind.jtColon)
      let value = p.parseExpression()
      elems.add(JsObjectElem(
        key : key,
        value : value,
      ))
      if p.peekJsToken().kind == JsTokenKind.jtComma:
        discard p.takeJsToken()
    p.expectJsToken(JsTokenKind.jtCloseCurly)
    result = JsNode(
      kind : JsNodeKind.jnObjectExpr,
      objectElems : elems,
    )
  else:
    raiseAssert "Unimplemented terminal tok: " & $p.raw

proc parseCallParamsList(p: var JsTokenStream): seq[JsNode] =
  result = newSeq[JsNode]()
  p.expectJsToken(JsTokenKind.jtOpenParen)
  while p.peekJsToken().kind != JsTokenKind.jtEof:
    case p.peekJsToken().kind
    of JsTokenKind.jtComma:
      discard p.takeJsToken()
    of JsTokenKind.jtCloseParen:
      discard p.takeJsToken()
      break
    else:
      result.add(p.parseExpression())

proc parsePostfixExpression(p: var JsTokenStream): JsNode =
  result = p.parseTerminalExpression()

  var next = p.peekJsToken()
  while true:
    case next.kind
    of JsTokenKind.jtDot:
      discard p.takeJsToken()
      let rhs = p.expectJsToken(JsTokenKind.jtIdent)
      result = JsNode(
        kind : JsNodeKind.jnDotExpr,
        dotLhs : result,
        dotRhs : rhs.raw,
      )
    of JsTokenKind.jtOpenParen:
      let params = p.parseCallParamsList()
      result = JsNode(
        kind : JsNodeKind.jnCallExpr,
        callTarget : result,
        callParams : params,
      )
    of JsTokenKind.jtLambdaArrow:
      discard p.takeJsToken()
      let body = p.parseStmt()
      result = JsNode(
        kind : JsNodeKind.jnArrowLambdaExpr,
        arrowLambdaParamsTuple : result,
        arrowLambdaBody : body,
      )
    of JsTokenKind.jtOpenSquare:
      discard p.takeJsToken()
      result = JsNode(
        kind : JsNodeKind.jnSquareAccessExpr,
        squareAccessTarget : result,
        squareAccessInnerExpr : p.parseExpression()
      )
      p.expectJsToken(JsTokenKind.jtCloseSquare)
    else:
      break
    next = p.peekJsToken()


proc parseUnaryExpression(p: var JsTokenStream): JsNode =
  p.parsePostfixExpression()

const jsMulExprTokKinds = { JsTokenKind.jtStar }
proc parseMulExpression(p: var JsTokenStream): JsNode =
  result = p.parseUnaryExpression()
  while p.peekJsToken().kind in jsMulExprTokKinds:
    let symbol = p.takeJsToken()
    let rhs = p.parseUnaryExpression()
    result = JsNode(
      kind : JsNodeKind.jnBinExpr,
      binLhs : result,
      binOperator : symbol,
      binRhs : rhs,
    )

const jsAddExprTokKinds = { JsTokenKind.jtPlus }
proc parseAddExpression(p: var JsTokenStream): JsNode =
  result = p.parseMulExpression()
  while p.peekJsToken().kind in jsAddExprTokKinds:
    let symbol = p.takeJsToken()
    let rhs = p.parseMulExpression()
    result = JsNode(
      kind : JsNodeKind.jnBinExpr,
      binLhs : result,
      binOperator : symbol,
      binRhs : rhs,
    )

const jsBitwiseBinaryExprTokKinds = { JsTokenKind.jtAnd }
proc parseBitwiseAndExpression(p: var JsTokenStream): JsNode =
  result = p.parseAddExpression()
  while p.peekJsToken().kind in jsBitwiseBinaryExprTokKinds:
    let symbol = p.takeJsToken()
    let rhs = p.parseAddExpression()
    result = JsNode(
      kind : JsNodeKind.jnBinExpr,
      binLhs : result,
      binOperator : symbol,
      binRhs : rhs,
    )

const jsComparisonOperatorTokKinds = { JsTokenKind.jtGreater, JsTokenKind.jtLess }
proc parseComparisonExpression(p: var JsTokenStream): JsNode =
  result = p.parseBitwiseAndExpression()
  if p.peekJsToken().kind in jsComparisonOperatorTokKinds:
    let operator = p.takeJsToken()
    let rhs = p.parseBitwiseAndExpression()
    result = JsNode(
      kind : JsNodeKind.jnBinExpr,
      binLhs : result,
      binOperator : operator,
      binRhs : rhs,
    )

const jsEqualityOperatorTokKinds = { JsTokenKind.jtEqEq, JsTokenKind.jtEqEqEq }
proc parseEqualityExpression(p: var JsTokenStream): JsNode =
  result = p.parseComparisonExpression()
  if p.peekJsToken().kind in jsEqualityOperatorTokKinds:
    let operator = p.takeJsToken()
    let rhs = p.parseComparisonExpression()
    result = JsNode(
      kind : JsNodeKind.jnBinExpr,
      binLhs : result,
      binOperator : operator,
      binRhs : rhs,
    )

const jsAssignmentOperatorTokKinds = { JsTokenKind.jtEq, JsTokenKind.jtPlusEq, JsTokenKind.jtMinusEq }
proc parseAssignmentExpression(p: var JsTokenStream): JsNode =
  result = p.parseEqualityExpression()
  if p.peekJsToken().kind in jsAssignmentOperatorTokKinds:
    let operator = p.takeJsToken()
    let rhs = p.parseEqualityExpression()
    result = JsNode(
      kind : JsNodeKind.jnAsgnExpr,
      asgnLhs : result,
      asgnOperator : operator,
      asgnRhs : rhs,
    )

proc parseExpression(p: var JsTokenStream): JsNode =
  p.parseAssignmentExpression()



proc parseStmt(p: var JsTokenStream): JsNode =
  let next = p.peekJsToken()
  case next.kind
  of JsTokenKind.jtIdent:
    var isLet = false
    if $next.raw == "let":
      discard p.takeJsToken()
      if p.peekJsToken().kind == JsTokenKind.jtIdent:
        # let declaration
        isLet = true
        let asgn = p.parseAssignmentExpression()
        doAssert asgn.kind == JsNodeKind.jnAsgnExpr
        result = JsNode(kind : JsNodeKind.jnLet, letAsgn : asgn)
    if not isLet:
      result = p.parseExpression()
      if p.peekJsToken().kind == JsTokenKind.jtEq:
        discard p.takeJsToken()
        let rhs = p.parseExpression()
        result = JsNode(
          kind : JsNodeKind.jnAsgnExpr,
          asgnLhs : result,
          asgnRhs : rhs,
        )
  of JsTokenKind.jtReserved:
    case $next.raw
    of "const":
      discard p.takeJsToken()
      let asgn = p.parseAssignmentExpression()
      doAssert asgn.kind == JsNodeKind.jnAsgnExpr
      result = JsNode(
        kind : JsNodeKind.jnConst,
        constAsgn : asgn,
      )
    of "return":
      discard p.takeJsToken()
      var retExpr = none(JsNode)
      if p.peekJsToken().kind == JsTokenKind.jtSemicolon:
        discard p.takeJsToken()
      else:
        retExpr = some p.parseExpression()
      result = JsNode(
        kind : JsNodeKind.jnReturnStmt,
        returnExpr : retExpr,
      )
    of "function":
      discard p.takeJsToken()
      let name = p.expectJsToken(JsTokenKind.jtIdent)
      let params = p.parseTupleExpression()
      let body = p.parseStmt()
      result = JsNode(
        kind : JsNodeKind.jnFunctionDefStmt,
        functionDefName : name,
        functionDefParamsTuple : params,
        functionDefBody : body,
      )
    else:
      result = p.parseExpression()
  of JsTokenKind.jtOpenCurly:
    result = p.parseCodeBlock()
  else:
    result = p.parseExpression()
    #raiseAssert "Bad token: " & $next
  while p.peekJsToken().kind == JsTokenKind.jtSemicolon:
    discard p.takeJsToken()

proc parseJs(p: var JsTokenStream): seq[JsNode] =
  result = newSeq[JsNode]()
  p.skipWhitespace()
  while not p.atEof():
    result.add(p.parseStmt())


when isMainModule:
  import std/[monotimes, times,]

  var testData = readFile("test2.js")
  testData.withJsTokenStream(p):
    let s = getMonoTime()
    let parsed = p.parseJs()
    let t = getMonoTime() - s

    for node in parsed:
      echo node.render()
    echo t
