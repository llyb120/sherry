package main

import (
    "fmt"
    "strconv"
)

type TokenType int

const (
    TOKEN_EOF TokenType = iota
    TOKEN_IDENT
    TOKEN_NUMBER
    TOKEN_PLUS
    TOKEN_MINUS
    TOKEN_MULTIPLY
    TOKEN_DIVIDE
    TOKEN_ASSIGN
    TOKEN_LPAREN
    TOKEN_RPAREN
    TOKEN_LBRACE
    TOKEN_RBRACE
    TOKEN_IF
    TOKEN_ELSE
    TOKEN_WHILE
    TOKEN_FUNC
    TOKEN_RETURN
    TOKEN_SEMICOLON
    TOKEN_COMMA
    TOKEN_EQUAL
    TOKEN_NOT_EQUAL
    TOKEN_LESS
    TOKEN_LESS_EQUAL
    TOKEN_GREATER
    TOKEN_GREATER_EQUAL
)

type Token struct {
    Type  TokenType
    Value string
}

type Lexer struct {
    input  string
    pos    int
    readPos int
    ch     byte
}

func NewLexer(input string) *Lexer {
    l := &Lexer{input: input}
    l.readChar()
    return l
}

func (l *Lexer) readChar() {
    if l.readPos >= len(l.input) {
        l.ch = 0
    } else {
        l.ch = l.input[l.readPos]
    }
    l.pos = l.readPos
    l.readPos++
}

func (l *Lexer) NextToken() Token {
    var tok Token

    l.skipWhitespace()

    switch l.ch {
    case '=':
        if l.peekChar() == '=' {
            ch := l.ch
            l.readChar()
            tok = Token{Type: TOKEN_EQUAL, Value: string(ch) + string(l.ch)}
        } else {
            tok = Token{Type: TOKEN_ASSIGN, Value: string(l.ch)}
        }
    case '+':
        tok = Token{Type: TOKEN_PLUS, Value: string(l.ch)}
    case '-':
        tok = Token{Type: TOKEN_MINUS, Value: string(l.ch)}
    case '*':
        tok = Token{Type: TOKEN_MULTIPLY, Value: string(l.ch)}
    case '/':
        tok = Token{Type: TOKEN_DIVIDE, Value: string(l.ch)}
    case '(':
        tok = Token{Type: TOKEN_LPAREN, Value: string(l.ch)}
    case ')':
        tok = Token{Type: TOKEN_RPAREN, Value: string(l.ch)}
    case '{':
        tok = Token{Type: TOKEN_LBRACE, Value: string(l.ch)}
    case '}':
        tok = Token{Type: TOKEN_RBRACE, Value: string(l.ch)}
    case ';':
        tok = Token{Type: TOKEN_SEMICOLON, Value: string(l.ch)}
    case ',':
        tok = Token{Type: TOKEN_COMMA, Value: string(l.ch)}
    case '<':
        if l.peekChar() == '=' {
            ch := l.ch
            l.readChar()
            tok = Token{Type: TOKEN_LESS_EQUAL, Value: string(ch) + string(l.ch)}
        } else {
            tok = Token{Type: TOKEN_LESS, Value: string(l.ch)}
        }
    case '>':
        if l.peekChar() == '=' {
            ch := l.ch
            l.readChar()
            tok = Token{Type: TOKEN_GREATER_EQUAL, Value: string(ch) + string(l.ch)}
        } else {
            tok = Token{Type: TOKEN_GREATER, Value: string(l.ch)}
        }
    case '!':
        if l.peekChar() == '=' {
            ch := l.ch
            l.readChar()
            tok = Token{Type: TOKEN_NOT_EQUAL, Value: string(ch) + string(l.ch)}
        } else {
            tok = Token{Type: TOKEN_EOF, Value: string(l.ch)}
        }
    case 0:
        tok.Value = ""
        tok.Type = TOKEN_EOF
    default:
        if isLetter(l.ch) {
            tok.Value = l.readIdentifier()
            tok.Type = l.lookupIdent(tok.Value)
            return tok
        } else if isDigit(l.ch) {
            tok.Type = TOKEN_NUMBER
            tok.Value = l.readNumber()
            return tok
        } else {
            tok = Token{Type: TOKEN_EOF, Value: string(l.ch)}
        }
    }

    l.readChar()
    return tok
}

func (l *Lexer) skipWhitespace() {
    for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
        l.readChar()
    }
}

func (l *Lexer) readIdentifier() string {
    position := l.pos
    for isLetter(l.ch) {
        l.readChar()
    }
    return l.input[position:l.pos]
}

func (l *Lexer) readNumber() string {
    position := l.pos
    for isDigit(l.ch) || l.ch == '.' {
        l.readChar()
    }
    return l.input[position:l.pos]
}

func (l *Lexer) peekChar() byte {
    if l.readPos >= len(l.input) {
        return 0
    }
    return l.input[l.readPos]
}

func isLetter(ch byte) bool {
    return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch byte) bool {
    return '0' <= ch && ch <= '9'
}

func (l *Lexer) lookupIdent(ident string) TokenType {
    switch ident {
    case "if":
        return TOKEN_IF
    case "else":
        return TOKEN_ELSE
    case "while":
        return TOKEN_WHILE
    case "func":
        return TOKEN_FUNC
    case "return":
        return TOKEN_RETURN
    default:
        return TOKEN_IDENT
    }
}

type Node interface {
    TokenLiteral() string
}

type Statement interface {
    Node
    statementNode()
}

type Expression interface {
    Node
    expressionNode()
}

type Program struct {
    Statements []Statement
}

func (p *Program) TokenLiteral() string {
    if len(p.Statements) > 0 {
        return p.Statements[0].TokenLiteral()
    }
    return ""
}

type ExpressionStatement struct {
    Token      Token
    Expression Expression
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Value }

type IntegerLiteral struct {
    Token Token
    Value float64
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Value }

type Identifier struct {
    Token Token
    Value string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Value }

type PrefixExpression struct {
    Token    Token
    Operator string
    Right    Expression
}

func (pe *PrefixExpression) expressionNode()      {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Value }

type InfixExpression struct {
    Token    Token
    Left     Expression
    Operator string
    Right    Expression
}

func (ie *InfixExpression) expressionNode()      {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Value }

type Parser struct {
    l      *Lexer
    curToken  Token
    peekToken Token
}

func NewParser(l *Lexer) *Parser {
    p := &Parser{l: l}
    p.nextToken()
    p.nextToken()
    return p
}

func (p *Parser) nextToken() {
    p.curToken = p.peekToken
    p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *Program {
    program := &Program{}
    program.Statements = []Statement{}

    for p.curToken.Type != TOKEN_EOF {
        stmt := p.parseStatement()
        if stmt != nil {
            program.Statements = append(program.Statements, stmt)
        }
        p.nextToken()
    }

    return program
}

func (p *Parser) parseStatement() Statement {
    switch p.curToken.Type {
    default:
        return p.parseExpressionStatement()
    }
}

func (p *Parser) parseExpressionStatement() *ExpressionStatement {
    stmt := &ExpressionStatement{Token: p.curToken}

    stmt.Expression = p.parseExpression(LOWEST)

    if p.peekTokenIs(TOKEN_SEMICOLON) {
        p.nextToken()
    }

    return stmt
}

const (
    _ int = iota
    LOWEST
    EQUALS
    LESSGREATER
    SUM
    PRODUCT
    PREFIX
    CALL
)

func (p *Parser) parseExpression(precedence int) Expression {
    prefix := p.prefixParseFns(p.curToken.Type)
    if prefix == nil {
        return nil
    }
    leftExp := prefix()

    for !p.peekTokenIs(TOKEN_SEMICOLON) && precedence < p.peekPrecedence() {
        infix := p.infixParseFns(p.peekToken.Type)
        if infix == nil {
            return leftExp
        }

        p.nextToken()

        leftExp = infix(leftExp)
    }

    return leftExp
}

func (p *Parser) prefixParseFns(tokenType TokenType) func() Expression {
    switch tokenType {
    case TOKEN_IDENT:
        return p.parseIdentifier
    case TOKEN_NUMBER:
        return p.parseIntegerLiteral
    case TOKEN_MINUS:
        return p.parsePrefixExpression
    case TOKEN_LPAREN:
        return p.parseGroupedExpression
    default:
        return nil
    }
}

func (p *Parser) infixParseFns(tokenType TokenType) func(Expression) Expression {
    switch tokenType {
    case TOKEN_PLUS, TOKEN_MINUS, TOKEN_MULTIPLY, TOKEN_DIVIDE:
        return p.parseInfixExpression
    default:
        return nil
    }
}

func (p *Parser) parseIdentifier() Expression {
    return &Identifier{Token: p.curToken, Value: p.curToken.Value}
}

func (p *Parser) parseIntegerLiteral() Expression {
    lit := &IntegerLiteral{Token: p.curToken}

    value, err := strconv.ParseFloat(p.curToken.Value, 64)
    if err != nil {
        return nil
    }

    lit.Value = value
    return lit
}

func (p *Parser) parsePrefixExpression() Expression {
    expression := &PrefixExpression{
        Token:    p.curToken,
        Operator: p.curToken.Value,
    }

    p.nextToken()

    expression.Right = p.parseExpression(PREFIX)

    return expression
}

func (p *Parser) parseInfixExpression(left Expression) Expression {
    expression := &InfixExpression{
        Token:    p.curToken,
        Operator: p.curToken.Value,
        Left:     left,
    }

    precedence := p.curPrecedence()
    p.nextToken()
    expression.Right = p.parseExpression(precedence)

    return expression
}

func (p *Parser) parseGroupedExpression() Expression {
    p.nextToken()
    exp := p.parseExpression(LOWEST)
    if !p.expectPeek(TOKEN_RPAREN) {
        return nil
    }
    return exp
}

func (p *Parser) expectPeek(t TokenType) bool {
    if p.peekTokenIs(t) {
        p.nextToken()
        return true
    }
    return false
}

func (p *Parser) peekTokenIs(t TokenType) bool {
    return p.peekToken.Type == t
}

func (p *Parser) peekPrecedence() int {
    if p, ok := precedences[p.peekToken.Type]; ok {
        return p
    }
    return LOWEST
}

func (p *Parser) curPrecedence() int {
    if p, ok := precedences[p.curToken.Type]; ok {
        return p
    }
    return LOWEST
}

var precedences = map[TokenType]int{
    TOKEN_EQUAL:          EQUALS,
    TOKEN_NOT_EQUAL:      EQUALS,
    TOKEN_LESS:           LESSGREATER,
    TOKEN_GREATER:        LESSGREATER,
    TOKEN_PLUS:           SUM,
    TOKEN_MINUS:          SUM,
    TOKEN_MULTIPLY:       PRODUCT,
    TOKEN_DIVIDE:         PRODUCT,
    TOKEN_LPAREN:         CALL,
}

type Evaluator struct{}

func (e *Evaluator) Eval(node Node) interface{} {
    switch node := node.(type) {
    case *Program:
        return e.evalStatements(node.Statements)
    case *ExpressionStatement:
        return e.Eval(node.Expression)
    case *IntegerLiteral:
        return node.Value
    case *PrefixExpression:
        right := e.Eval(node.Right)
        return e.evalPrefixExpression(node.Operator, right)
    case *InfixExpression:
        left := e.Eval(node.Left)
        right := e.Eval(node.Right)
        return e.evalInfixExpression(node.Operator, left, right)
    }
    return nil
}

func (e *Evaluator) evalStatements(stmts []Statement) interface{} {
    var result interface{}
    for _, statement := range stmts {
        result = e.Eval(statement)
    }
    return result
}

func (e *Evaluator) evalPrefixExpression(operator string, right interface{}) interface{} {
    switch operator {
    case "-":
        return -right.(float64)
    default:
        return nil
    }
}

func (e *Evaluator) evalInfixExpression(operator string, left, right interface{}) interface{} {
    leftVal, leftOk := left.(float64)
    rightVal, rightOk := right.(float64)

    if !leftOk || !rightOk {
        return nil
    }

    switch operator {
    case "+":
        return leftVal + rightVal
    case "-":
        return leftVal - rightVal
    case "*":
        return leftVal * rightVal
    case "/":
        if rightVal == 0 {
            return nil // 处理除以零的情况
        }
        return leftVal / rightVal
    default:
        return nil
    }
}

func main() {
    evaluator := &Evaluator{}

    testCases := []struct {
        input    string
        expected float64
    }{
        {"1 + (2 - 4) * 5", -9},
        {"2.5 * 4 + 8.5", 18.5},
        {"10 / 4 + 7", 9.5},
        {"(3 + 5) * (2 - 1)", 8},
        {"-5 + 10 * 2", 15},
    }

    for _, tc := range testCases {
        l := NewLexer(tc.input)
        p := NewParser(l)
        program := p.ParseProgram()

        result := evaluator.Eval(program)
        fmt.Printf("输入: %s\n", tc.input)
        fmt.Printf("期望结果: %v\n", tc.expected)
        fmt.Printf("实际结果: %v\n", result)
        if result != nil {
            if result.(float64) != tc.expected {
                fmt.Println("错误：结果不匹配！")
            }
        } else {
            fmt.Println("错误：计算结果为 nil")
        }
        fmt.Println()
    }
}