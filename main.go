package main

import (
	"fmt"
	"reflect"
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
	TOKEN_BREAK

	TOKEN_MODULO
	TOKEN_STRING

	TOKEN_LBRACKET
	TOKEN_RBRACKET
	TOKEN_COLON

	TOKEN_AND
	TOKEN_OR
	TOKEN_TRUE
	TOKEN_FALSE
	TOKEN_BANG

	TOKEN_PLUS_ASSIGN
	TOKEN_MINUS_ASSIGN

	TOKEN_DOT
)

type Token struct {
	Type  TokenType
	Value string
}

type Lexer struct {
	input   string
	pos     int
	readPos int
	ch      byte
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
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_PLUS_ASSIGN, Value: string(ch) + string(l.ch)}
		} else {
			tok = Token{Type: TOKEN_PLUS, Value: string(l.ch)}
		}
	case '-':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_MINUS_ASSIGN, Value: string(ch) + string(l.ch)}
		} else {
			tok = Token{Type: TOKEN_MINUS, Value: string(l.ch)}
		}
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
	case '&':
		if l.peekChar() == '&' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_AND, Value: string(ch) + string(l.ch)}
		}
	case '|':
		if l.peekChar() == '|' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_OR, Value: string(ch) + string(l.ch)}
		}
	case '!':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_NOT_EQUAL, Value: string(ch) + string(l.ch)}
		} else {
			tok = Token{Type: TOKEN_BANG, Value: string(l.ch)}
		}
	case '%':
		tok = Token{Type: TOKEN_MODULO, Value: string(l.ch)}
	case '"':
		tok.Type = TOKEN_STRING
		tok.Value = l.readString()
	case '\'':
		tok.Type = TOKEN_STRING
		tok.Value = l.readString()
	case '[':
		tok = Token{Type: TOKEN_LBRACKET, Value: string(l.ch)}
	case ']':
		tok = Token{Type: TOKEN_RBRACKET, Value: string(l.ch)}
	case ':':
		tok = Token{Type: TOKEN_COLON, Value: string(l.ch)}
	case '.':
		tok = Token{Type: TOKEN_DOT, Value: string(l.ch)}
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

func (l *Lexer) readString() string {
	position := l.pos + 1
	for {
		l.readChar()
		if l.ch == '"' || l.ch == '\'' || l.ch == 0 {
			break
		}
	}
	return l.input[position:l.pos]
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
	case "break":
		return TOKEN_BREAK
	case "true":
		return TOKEN_TRUE
	case "false":
		return TOKEN_FALSE
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

type IndexExpression struct {
	Token Token
	Left  Expression
	Index Expression
}

func (ie *IndexExpression) expressionNode()      {}
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Value }

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
	l         *Lexer
	curToken  Token
	peekToken Token
	errors    []string
}

func NewParser(l *Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}
	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) peekError(t TokenType) {
	msg := fmt.Sprintf("期望下一个token为 %d，实际得到的是 %d",
		t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
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
	LOGICAL_OR  // ||
	LOGICAL_AND // &&
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	CALL
	INDEX // array[index]  // 添加这一行
)

func (p *Parser) parseExpression(precedence int) Expression {
	prefix := p.prefixParseFns(p.curToken.Type)
	if prefix == nil {
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(TOKEN_SEMICOLON) && precedence < p.peekPrecedence() {
		if p.peekTokenIs(TOKEN_DOT) {
			p.nextToken()
			leftExp = p.parseDotExpression(leftExp)
		} else if p.peekTokenIs(TOKEN_LBRACKET) {
			p.nextToken()
			leftExp = p.parseIndexExpression(leftExp)
		} else {
			infix := p.infixParseFns(p.peekToken.Type)
			if infix == nil {
				return leftExp
			}
			p.nextToken()
			leftExp = infix(leftExp)
		}
	}

	// 处理赋值操作
	if p.peekTokenIs(TOKEN_ASSIGN) || p.peekTokenIs(TOKEN_PLUS_ASSIGN) || p.peekTokenIs(TOKEN_MINUS_ASSIGN) {
		operator := p.peekToken.Type
		p.nextToken()
		p.nextToken()
		value := p.parseExpression(LOWEST)
		return &AssignExpression{Left: leftExp, Value: value, Operator: operator}
	}

	return leftExp
}

func (p *Parser) prefixParseFns(tokenType TokenType) func() Expression {
	switch tokenType {
	case TOKEN_IDENT:
		return p.parseIdentifier
	case TOKEN_NUMBER:
		return p.parseIntegerLiteral
	case TOKEN_MINUS, TOKEN_BANG:
		return p.parsePrefixExpression
	case TOKEN_LPAREN:
		return p.parseGroupedExpression
	case TOKEN_STRING:
		return p.parseStringLiteral
	case TOKEN_LBRACE:
		return p.parseObjectLiteral
	case TOKEN_LBRACKET:
		return p.parseArrayLiteral
	case TOKEN_TRUE, TOKEN_FALSE:
		return p.parseBoolean
	case TOKEN_FUNC:
		return p.parseFunctionLiteral
	// case TOKEN_DOT: // 添加这一行
	// 	return p.parseDotExpression
	// return func() Expression { return p.parseFunctionLiteral() } // 包装返回值
	// return p.parseFunctionLiteral // 添加对匿名函数的解析
	default:
		return nil
	}
}

func (p *Parser) parseDotExpression(left Expression) Expression {
	exp := &DotExpression{Token: p.curToken, Left: left}

	if !p.expectPeek(TOKEN_IDENT) {
		return nil
	}

	exp.Property = &Identifier{Token: p.curToken, Value: p.curToken.Value}
	return exp
}

func (p *Parser) parseStringLiteral() Expression {
	return &StringLiteral{Token: p.curToken, Value: p.curToken.Value}
}

func (p *Parser) parseObjectLiteral() Expression {
	obj := &ObjectLiteral{Token: p.curToken}
	obj.Pairs = make(map[Expression]Expression)

	for !p.peekTokenIs(TOKEN_RBRACE) {
		p.nextToken()
		key := p.parseExpression(LOWEST)

		if !p.expectPeek(TOKEN_COLON) {
			return nil
		}

		p.nextToken()
		value := p.parseExpression(LOWEST)

		obj.Pairs[key] = value

		if !p.peekTokenIs(TOKEN_RBRACE) && !p.expectPeek(TOKEN_COMMA) {
			return nil
		}
	}

	if !p.expectPeek(TOKEN_RBRACE) {
		return nil
	}

	return obj
}

func (p *Parser) parseExpressionList(end TokenType) []Expression {
	var list []Expression

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(TOKEN_COMMA) {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list
}

func (p *Parser) parseArrayLiteral() Expression {
	array := &ArrayLiteral{Token: p.curToken}
	array.Elements = p.parseExpressionList(TOKEN_RBRACKET)
	return array
}

func (p *Parser) parseBoolean() Expression {
	return &Boolean{Token: p.curToken, Value: p.curToken.Type == TOKEN_TRUE}
}

func (p *Parser) infixParseFns(tokenType TokenType) func(Expression) Expression {
	switch tokenType {
	case TOKEN_PLUS, TOKEN_MINUS, TOKEN_MULTIPLY, TOKEN_DIVIDE,
		TOKEN_EQUAL, TOKEN_LESS, TOKEN_LESS_EQUAL,
		TOKEN_GREATER, TOKEN_GREATER_EQUAL, TOKEN_MODULO:
		return p.parseInfixExpression
	case TOKEN_LPAREN:
		return p.parseCallExpression
	case TOKEN_LBRACKET:
		return p.parseIndexExpression
	case TOKEN_AND, TOKEN_OR, TOKEN_NOT_EQUAL:
		return p.parseInfixExpression
	default:
		return nil
	}
}

func (p *Parser) parseIndexExpression(left Expression) Expression {
	exp := &IndexExpression{Token: p.curToken, Left: left}

	p.nextToken()
	exp.Index = p.parseExpression(LOWEST)

	if p.peekTokenIs(TOKEN_ASSIGN) {
		exp.Index = nil
	} else if !p.expectPeek(TOKEN_RBRACKET) {
		return nil
	}

	return exp
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
	TOKEN_EQUAL:         EQUALS,
	TOKEN_NOT_EQUAL:     EQUALS,
	TOKEN_LESS:          LESSGREATER,
	TOKEN_LESS_EQUAL:    LESSGREATER,
	TOKEN_GREATER:       LESSGREATER,
	TOKEN_GREATER_EQUAL: LESSGREATER,
	TOKEN_PLUS:          SUM,
	TOKEN_MINUS:         SUM,
	TOKEN_MULTIPLY:      PRODUCT,
	TOKEN_DIVIDE:        PRODUCT,
	TOKEN_LPAREN:        CALL,
	TOKEN_MODULO:        PRODUCT,
	TOKEN_LBRACKET:      INDEX,
	TOKEN_AND:           LOGICAL_AND,
	TOKEN_OR:            LOGICAL_OR,
	TOKEN_DOT:           CALL,
}

type Evaluator struct {
	env         map[string]interface{}
	breakSignal bool
}

func NewEvaluator() *Evaluator {
	e := &Evaluator{
		env:         make(map[string]interface{}),
		breakSignal: false,
	}
	RegisterStdLib(e)
	return e
}

func (e *Evaluator) evalStatements(stmts []Statement) interface{} {
	var result interface{}
	// 先执行函数定义语句
	for _, statement := range stmts {
		if _, ok := statement.(*FunctionStatement); ok {
			result = e.Eval(statement)
		}
	}
	// 再执行其他语句
	for _, statement := range stmts {
		if _, ok := statement.(*FunctionStatement); !ok {
			result = e.Eval(statement)
		}
	}
	return result
}

func (e *Evaluator) evalPrefixExpression(operator string, right interface{}) interface{} {
	switch operator {
	case "!":
		return !isTruthy(right)
	case "-":
		return -right.(float64)
	default:
		return nil
	}
}

func (e *Evaluator) evalInfixExpression(operator string, left, right interface{}) interface{} {
	switch operator {
	case "+":
		switch leftVal := left.(type) {
		case float64:
			if rightVal, ok := right.(float64); ok {
				return leftVal + rightVal
			}
		case string:
			if rightVal, ok := right.(string); ok {
				return leftVal + rightVal
			}
		case []interface{}:
			if rightVal, ok := right.([]interface{}); ok {
				return append(leftVal, rightVal...)
			}

		}
	case "-":
		if leftVal, ok := left.(float64); ok {
			if rightVal, ok := right.(float64); ok {
				return leftVal - rightVal
			}
		}
	case "*":
		if leftVal, ok := left.(float64); ok {
			if rightVal, ok := right.(float64); ok {
				return leftVal * rightVal
			}
		}
	case "/":
		if leftVal, ok := left.(float64); ok {
			if rightVal, ok := right.(float64); ok {
				if rightVal == 0 {
					return nil // 处理除以零的情况
				}
				return leftVal / rightVal
			}
		}
	case "<":
		if leftVal, ok := left.(float64); ok {
			if rightVal, ok := right.(float64); ok {
				return leftVal < rightVal
			}
		}
	case "<=":
		if leftVal, ok := left.(float64); ok {
			if rightVal, ok := right.(float64); ok {
				return leftVal <= rightVal
			}
		}
	case ">":
		if leftVal, ok := left.(float64); ok {
			if rightVal, ok := right.(float64); ok {
				return leftVal > rightVal
			}
		}
	case ">=":
		if leftVal, ok := left.(float64); ok {
			if rightVal, ok := right.(float64); ok {
				return leftVal >= rightVal
			}
		}
	case "==":
		return left == right
	case "!=":
		return left != right
	case "%":
		if leftVal, ok := left.(float64); ok {
			if rightVal, ok := right.(float64); ok {
				return float64(int(leftVal) % int(rightVal))
			}
		}
	case "&&":
		leftVal, ok := left.(bool)
		if !ok {
			return nil
		}
		rightVal, ok := right.(bool)
		if !ok {
			return nil
		}
		return leftVal && rightVal
	case "||":
		leftVal, ok := left.(bool)
		if !ok {
			return nil
		}
		rightVal, ok := right.(bool)
		if !ok {
			return nil
		}
		return leftVal || rightVal
	}
	return nil
}

type AssignExpression struct {
	Token    Token
	Left     Expression
	Value    Expression
	Operator TokenType
}

func (ae *AssignExpression) expressionNode()      {}
func (ae *AssignExpression) TokenLiteral() string { return "=" }

type Boolean struct {
	Token Token
	Value bool
}

func (b *Boolean) expressionNode()      {}
func (b *Boolean) TokenLiteral() string { return b.Token.Value }

type StringLiteral struct {
	Token Token
	Value string
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Value }

type ObjectLiteral struct {
	Token Token
	Pairs map[Expression]Expression
}

func (ol *ObjectLiteral) expressionNode()      {}
func (ol *ObjectLiteral) TokenLiteral() string { return ol.Token.Value }

type ArrayLiteral struct {
	Token    Token
	Elements []Expression
}

func (al *ArrayLiteral) expressionNode()      {}
func (al *ArrayLiteral) TokenLiteral() string { return al.Token.Value }

type IfStatement struct {
	Token       Token
	Condition   Expression
	Consequence *BlockStatement
	Alternative *BlockStatement
}

func (is *IfStatement) statementNode()       {}
func (is *IfStatement) TokenLiteral() string { return is.Token.Value }

type WhileStatement struct {
	Token     Token
	Condition Expression
	Body      *BlockStatement
}

func (ws *WhileStatement) statementNode()       {}
func (ws *WhileStatement) TokenLiteral() string { return ws.Token.Value }

type BlockStatement struct {
	Token      Token
	Statements []Statement
}

func (bs *BlockStatement) statementNode()       {}
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Value }

type BreakStatement struct {
	Token Token
}

func (bs *BreakStatement) statementNode()       {}
func (bs *BreakStatement) TokenLiteral() string { return bs.Token.Value }

func (p *Parser) parseIfStatement() *IfStatement {
	stmt := &IfStatement{Token: p.curToken}

	p.nextToken() // 跳过 'if' 关键字
	stmt.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(TOKEN_LBRACE) {
		fmt.Println("Error: Expected '{' before if body")
		return nil
	}

	stmt.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(TOKEN_ELSE) {
		p.nextToken()

		if !p.expectPeek(TOKEN_LBRACE) {
			fmt.Println("Error: Expected '{' before else body")
			return nil
		}

		stmt.Alternative = p.parseBlockStatement()
	}

	return stmt
}

func (p *Parser) parseWhileStatement() *WhileStatement {
	stmt := &WhileStatement{Token: p.curToken}

	p.nextToken() // 跳过 'while' 关键字
	stmt.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(TOKEN_LBRACE) {
		fmt.Println("Error: Expected '{' before while body")
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

func (p *Parser) parseBlockStatement() *BlockStatement {
	block := &BlockStatement{Token: p.curToken}
	block.Statements = []Statement{}

	p.nextToken()

	for !p.curTokenIs(TOKEN_RBRACE) && !p.curTokenIs(TOKEN_EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}

	return block
}

func (p *Parser) curTokenIs(t TokenType) bool {
	return p.curToken.Type == t
}

func (e *Evaluator) evalIfStatement(is *IfStatement) interface{} {
	if is == nil {
		return nil
	}
	condition := e.Eval(is.Condition)
	if isTruthy(condition) {
		return e.Eval(is.Consequence)
	} else if is.Alternative != nil {
		return e.Eval(is.Alternative)
	}
	return nil
}

func (e *Evaluator) evalWhileStatement(ws *WhileStatement) interface{} {
	var result interface{}
	for {
		condition := e.Eval(ws.Condition)
		if !isTruthy(condition) {
			break
		}
		result = e.Eval(ws.Body)
		if e.breakSignal {
			e.breakSignal = false
			break
		}
	}
	return result
}

func (e *Evaluator) evalBlockStatement(bs *BlockStatement) interface{} {
	var result interface{}
	for _, stmt := range bs.Statements {
		result = e.Eval(stmt)
		if e.breakSignal {
			// 如果检测到 breakSignal，立即停止执行
			break
		}
	}
	return result
}

func (e *Evaluator) evalIdentifier(i *Identifier) interface{} {
	if val, ok := e.env[i.Value]; ok {
		return val
	}
	return i.Value
}

func (e *Evaluator) evalAssignStatement(as *AssignStatement) interface{} {
	val := e.Eval(as.Value)

	switch as.Operator {
	case TOKEN_PLUS_ASSIGN:
		if existingVal, ok := e.env[as.Name.Value]; ok {
			val = e.evalInfixExpression("+", existingVal, val)
		}
	case TOKEN_MINUS_ASSIGN:
		if existingVal, ok := e.env[as.Name.Value]; ok {
			val = e.evalInfixExpression("-", existingVal, val)
		}
	}

	e.env[as.Name.Value] = val
	return val
}

func (e *Evaluator) evalCallExpression(function interface{}, arguments []Expression) interface{} {
	switch fn := function.(type) {
	case *FunctionStatement:
		return e.evalFunctionCall(fn, arguments)
	case *FunctionLiteral:
		return e.evalFunctionLiteralCall(fn, arguments)
	case func(args ...interface{}) interface{}:
		// 评估参数
		evaluatedArgs := []interface{}{}
		for _, arg := range arguments {
			evaluatedArgs = append(evaluatedArgs, e.Eval(arg))
		}
		// 调用外部绑定的函数
		return fn(evaluatedArgs...)
	default:
		return nil
	}
}

func (e *Evaluator) evalFunctionLiteralCall(fn *FunctionLiteral, arguments []Expression) interface{} {
	// 保存当前环境
	oldEnv := e.env

	// 创建新的局部环境
	env := make(map[string]interface{})

	// 先将参数值存入局部环境
	for i, param := range fn.Parameters {
		if i < len(arguments) {
			env[param.Value] = e.Eval(arguments[i])
		} else {
			env[param.Value] = nil
		}
	}

	// 合并外部环境到局部环境
	for k, v := range oldEnv {
		if _, exists := env[k]; !exists {
			env[k] = v
		}
	}

	// 设置新的局部环境
	e.env = env

	// 评估函数体
	var result interface{}
	for _, stmt := range fn.Body.Statements {
		e.Eval(stmt)
		if returnValue, exists := e.env["__return__"]; exists {
			// 恢复原环境
			e.env = oldEnv
			return returnValue
		}
	}

	// 恢复原环境
	e.env = oldEnv

	return result
}

func (e *Evaluator) evalFunctionCall(fn *FunctionStatement, arguments []Expression) interface{} {
	// 保存当前环境
	oldEnv := e.env

	// 创建新的局部环境
	env := make(map[string]interface{})

	// 先将参数值存入局部环境
	for i, param := range fn.Parameters {
		if i < len(arguments) {
			env[param.Value] = e.Eval(arguments[i])
		} else {
			env[param.Value] = nil
		}
	}

	// 合并外部环境到局部环境
	for k, v := range oldEnv {
		if _, exists := env[k]; !exists {
			env[k] = v
		}
	}

	// 设置新的局部环境
	e.env = env

	// 评估函数体
	var result interface{}
	for _, stmt := range fn.Body.Statements {
		e.Eval(stmt)
		if returnValue, exists := e.env["__return__"]; exists {
			// 恢复原环境
			e.env = oldEnv
			return returnValue
		}
	}

	// 恢复原环境
	e.env = oldEnv

	return result
}

// 添加 ReturnStatement 结构
type ReturnStatement struct {
	Token       Token
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode()       {}
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Value }

// 在 Parser 中添加对 return 语句的解析
func (p *Parser) parseReturnStatement() *ReturnStatement {
	stmt := &ReturnStatement{Token: p.curToken}

	p.nextToken()

	stmt.ReturnValue = p.parseExpression(LOWEST)

	if p.peekTokenIs(TOKEN_SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// 在 parseStatement 函数中添加对 return 语句的处理
func (p *Parser) parseStatement() Statement {
	switch p.curToken.Type {
	case TOKEN_IDENT:
		if p.peekTokenIs(TOKEN_ASSIGN) || p.peekTokenIs(TOKEN_PLUS_ASSIGN) || p.peekTokenIs(TOKEN_MINUS_ASSIGN) {
			return p.parseAssignStatement()
		}
		return p.parseExpressionStatement()
	case TOKEN_IF:
		return p.parseIfStatement()
	case TOKEN_WHILE:
		return p.parseWhileStatement()
	case TOKEN_BREAK:
		return &BreakStatement{Token: p.curToken}
	case TOKEN_FUNC:
		return p.parseFunctionStatement()
	case TOKEN_RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

// 在 Evaluator 的 Eval 方法中添加对 ReturnStatement 的处理
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
	case *IfStatement:
		return e.evalIfStatement(node)
	case *WhileStatement:
		return e.evalWhileStatement(node)
	case *BlockStatement:
		return e.evalBlockStatement(node)
	case *Identifier:
		return e.evalIdentifier(node)
	case *AssignStatement:
		return e.evalAssignStatement(node)
	case *BreakStatement:
		e.breakSignal = true
		return nil
	case *FunctionStatement:
		e.env[node.Name.Value] = node
		return nil
	case *CallExpression:
		function := e.Eval(node.Function)
		if function == nil {
			return nil
		}
		return e.evalCallExpression(function, node.Arguments)
	case *ReturnStatement:
		returnValue := e.Eval(node.ReturnValue)
		e.env["__return__"] = returnValue
		return returnValue
	case *StringLiteral:
		return node.Value
	case *ObjectLiteral:
		return e.evalObjectLiteral(node)
	case *ArrayLiteral:
		return e.evalArrayLiteral(node)
	case *IndexExpression:
		left := e.Eval(node.Left)
		index := e.Eval(node.Index)
		return e.evalIndexExpression(left, index)
	case *Boolean:
		return node.Value
	case *FunctionLiteral:
		return node
	case *AssignExpression:
		return e.evalAssignExpression(node)
	case *DotExpression:
		return e.evalDotExpression(node)
	}
	return nil
}

func (e *Evaluator) evalDotExpression(de *DotExpression) interface{} {
	left := e.Eval(de.Left)
	if obj, ok := left.(map[string]interface{}); ok {
		return obj[de.Property.Value]
	}
	return nil
}

func (e *Evaluator) evalAssignExpression(ae *AssignExpression) interface{} {
	value := e.Eval(ae.Value)

	// 处理普通赋值操作
	switch left := ae.Left.(type) {
	case *Identifier:
		e.env[left.Value] = value
	case *DotExpression:
		return e.evalDotExpressionAssign(left, value, ae.Operator)
	case *IndexExpression:
		return e.evalIndexExpressionAssign(left, value, ae.Operator)
	}

	return value
}

func (e *Evaluator) evalIndexExpressionAssign(left *IndexExpression, value interface{}, operator TokenType) interface{} {
	array := e.Eval(left.Left)
	switch array := array.(type) {
	case map[string]interface{}:
		key, ok := e.Eval(left.Index).(string)
		if !ok {
			return nil
		}
		switch operator {
		case TOKEN_ASSIGN:
			array[key] = value
		case TOKEN_PLUS_ASSIGN:
			array[key] = e.evalInfixExpression("+", array[key], value)
		case TOKEN_MINUS_ASSIGN:
			array[key] = e.evalInfixExpression("-", array[key], value)
		}
	case []interface{}:
		if left.Index == nil {
			// 直接追加赋值
			// if arr, ok := array.([]interface{}); ok {
			array = append(array, value)
			e.env[left.Left.(*Identifier).Value] = array
			// }
		} else {
			index := int(e.Eval(left.Index).(float64))
			// if arr, ok := array.([]interface{}); ok {
			// 确保数组有足够的长度
			for len(array) <= index {
				array = append(array, nil)
			}
			array[index] = value
			e.env[left.Left.(*Identifier).Value] = array
			// }
		}
	}
	return value
}

func (e *Evaluator) evalDotExpressionAssign(de *DotExpression, value interface{}, operator TokenType) interface{} {
	obj := e.Eval(de.Left)
	if objMap, ok := obj.(map[string]interface{}); ok {
		switch operator {
		case TOKEN_ASSIGN:
			objMap[de.Property.Value] = value
		case TOKEN_PLUS_ASSIGN:
			if oldValue, exists := objMap[de.Property.Value]; exists {
				objMap[de.Property.Value] = e.evalInfixExpression("+", oldValue, value)
			}
		case TOKEN_MINUS_ASSIGN:
			if oldValue, exists := objMap[de.Property.Value]; exists {
				objMap[de.Property.Value] = e.evalInfixExpression("-", oldValue, value)
			}
		}
		return objMap[de.Property.Value]
	}
	return nil
}

func (e *Evaluator) evalIndexExpression(left, index interface{}) interface{} {
	switch left := left.(type) {
	case []interface{}:
		idx, ok := index.(float64)
		if !ok {
			return nil
		}
		if int(idx) < 0 || int(idx) >= len(left) {
			return nil
		}
		return left[int(idx)]
	case map[string]interface{}:
		key, ok := index.(string)
		if !ok {
			return nil
		}
		return left[key]
	}
	return nil
}

func (e *Evaluator) evalObjectLiteral(node *ObjectLiteral) interface{} {
	obj := make(map[string]interface{})
	for keyNode, valueNode := range node.Pairs {
		key := e.Eval(keyNode).(string)
		value := e.Eval(valueNode)
		obj[key] = value
	}
	return obj
}

func (e *Evaluator) evalArrayLiteral(node *ArrayLiteral) interface{} {
	var elements []interface{}
	for _, element := range node.Elements {
		elements = append(elements, e.Eval(element))
	}
	return elements
}

func (p *Parser) parseFunctionStatement() *FunctionStatement {
	stmt := &FunctionStatement{Token: p.curToken}

	if !p.expectPeek(TOKEN_IDENT) {
		return nil
	}

	stmt.Name = &Identifier{Token: p.curToken, Value: p.curToken.Value}

	if !p.expectPeek(TOKEN_LPAREN) {
		return nil
	}

	stmt.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(TOKEN_LBRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

func (p *Parser) parseFunctionParameters() []*Identifier {
	params := []*Identifier{}

	if p.peekTokenIs(TOKEN_RPAREN) {
		p.nextToken()
		return params
	}

	p.nextToken()

	ident := &Identifier{Token: p.curToken, Value: p.curToken.Value}
	params = append(params, ident)

	for p.peekTokenIs(TOKEN_COMMA) {
		p.nextToken()
		p.nextToken()
		ident := &Identifier{Token: p.curToken, Value: p.curToken.Value}
		params = append(params, ident)
	}

	if !p.expectPeek(TOKEN_RPAREN) {
		return nil
	}

	return params
}

func (p *Parser) parseCallExpression(function Expression) Expression {
	exp := &CallExpression{Token: p.curToken, Function: function}
	exp.Arguments = p.parseCallArguments()
	return exp
}

func (p *Parser) parseCallArguments() []Expression {
	args := []Expression{}

	if p.peekTokenIs(TOKEN_RPAREN) {
		p.nextToken()
		return args
	}

	p.nextToken()
	args = append(args, p.parseExpression(LOWEST))

	for p.peekTokenIs(TOKEN_COMMA) {
		p.nextToken()
		p.nextToken()
		args = append(args, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(TOKEN_RPAREN) {
		return nil
	}

	return args
}

func (ce *CallExpression) expressionNode()      {}
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Value }

func isTruthy(obj interface{}) bool {
	switch v := obj.(type) {
	case bool:
		return v
	case float64:
		return v != 0
	default:
		return false
	}
}

type AssignStatement struct {
	Token    Token
	Name     *Identifier
	Value    Expression
	Operator TokenType // 添加 Operator 字段
}

func (as *AssignStatement) statementNode()       {}
func (as *AssignStatement) TokenLiteral() string { return as.Token.Value }

func (p *Parser) parseAssignStatement() *AssignStatement {
	stmt := &AssignStatement{Token: p.curToken}
	stmt.Name = &Identifier{Token: p.curToken, Value: p.curToken.Value}

	if !p.expectPeek(TOKEN_ASSIGN) && !p.expectPeek(TOKEN_PLUS_ASSIGN) && !p.expectPeek(TOKEN_MINUS_ASSIGN) {
		return nil
	}

	operator := p.curToken.Type

	p.nextToken()

	// 支持将函数赋值给变量
	if p.curToken.Type == TOKEN_FUNC {
		stmt.Value = p.parseFunctionLiteral()
	} else {
		stmt.Value = p.parseExpression(LOWEST)
	}

	if p.peekTokenIs(TOKEN_SEMICOLON) {
		p.nextToken()
	}

	// 处理 += 和 -=
	if operator == TOKEN_PLUS_ASSIGN || operator == TOKEN_MINUS_ASSIGN {
		stmt.Operator = operator
	}

	return stmt
}

func (p *Parser) parseFunctionLiteral() Expression {
	lit := &FunctionLiteral{Token: p.curToken}

	if !p.expectPeek(TOKEN_LPAREN) {
		return nil
	}

	lit.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(TOKEN_LBRACE) {
		return nil
	}

	lit.Body = p.parseBlockStatement()

	return lit
}

type DotExpression struct {
	Token    Token
	Left     Expression
	Property *Identifier
}

func (de *DotExpression) expressionNode()      {}
func (de *DotExpression) TokenLiteral() string { return de.Token.Value }

type FunctionLiteral struct {
	Token      Token
	Parameters []*Identifier
	Body       *BlockStatement
}

func (fl *FunctionLiteral) expressionNode()      {}
func (fl *FunctionLiteral) TokenLiteral() string { return fl.Token.Value }

type FunctionStatement struct {
	Token      Token
	Name       *Identifier
	Parameters []*Identifier
	Body       *BlockStatement
}

func (fs *FunctionStatement) statementNode()       {}
func (fs *FunctionStatement) TokenLiteral() string { return fs.Token.Value }

type CallExpression struct {
	Token     Token
	Function  Expression
	Arguments []Expression
}

func main() {
	tests := []struct {
		input    string
		expected interface{}
	}{
		{"2 + 3", 5.0},
		{"2 - 3", -1.0},
		{"2 * 3", 6.0},
		{"2 / 3", 2.0 / 3.0},
		{"2 + 3 * 4", 14.0},
		{"(2 + 3) * 4", 20.0},
		{"1 + (2 - 4) * 5", -9.0},
		{"2.5 * 4 + 8.5", 18.5},
		{"10 / 4 + 7", 9.5},
		{"(3 + 5) * (2 - 1)", 8.0},
		{"-5 + 10 * 2", 15.0},
		{"a = 0; while a < 5 { a = a + 1; if a == 3 { break } }; a", 3.0},
		{"x = 0 y = 1 z = 2 func add(x, y) { return x + y + z; }; add(2, 3)", 7.0},
		{"func factorial(n) { if n == 0 { return 1 } return n * factorial(n - 1) }; factorial(5)", 120.0},
		{"func fibonacci(n) { if n <= 1 { return n } return fibonacci(n - 1) + fibonacci(n - 2) }; fibonacci(6)", 8.0},
		{"x = 10; y = 20; if x < y { x = x + 5 }; x", 15.0},
		{"a = 1; b = 2; c = 3; if a + b == 3 { c = c + 1 }; c", 4.0},
		{"x = 0; while x < 10 { if x % 2 == 0 { x = x + 1 } else { x = x + 2 } }; x", 11.0},
		{"\"hello\" + \" \" + \"world\"", "hello world"},
		{"[1, 2, 3][1]", 2.0},
		{"{ \"key\": \"value\" }[\"key\"]", "value"},
		{"a = { \"x\": 10, \"y\": 20 }; a[\"x\"] + a[\"y\"]", 30.0},
		{"true", true},
		{"false", false},
		{"1 == 1", true},
		{"1 != 2", true},
		{"1 < 2", true},
		{"2 > 1", true},
		{"1 <= 1", true},
		{"1 >= 1", true},
		{"!(true)", false},
		{"!(false)", true},
		{"true && true", true},
		{"true && false", false},
		{"false && true", false},
		{"false && false", false},
		{"true || true", true},
		{"true || false", true},
		{"false || true", true},
		{"false || false", false},
		{"a += 5", 5.0},
		{"a = 10; a -= 3", 7.0},
		{"b = 2; b += 3; b += 4", 9.0},
		// {"c = 0; for i = 0; i < 5; i++ { c += 1 }; c", 5.0},
		{"x = 10; x += 2; x -= 5", 7.0},
		{"y = 20; y -= 10; y += 5", 15.0},

		{"a = []; a[] = 2; a[0]", 2.0},
		{"b = [1, 2, 3]; b[1] = 5; b[1]", 5.0},
		{"c = []; c[2] = 3; c", []interface{}{nil, nil, 3.0}},
		{"d = [1]; d[3] = 4; d", []interface{}{1.0, nil, nil, 4.0}},
		{"e = [1, 2]; e[0] = e[0] + 10; e[0]", 11.0},

		{"factorial = func(n) { if n == 0 { return 1 } return n * factorial(n - 1) }; factorial(5)", 120.0},
		{"isEven = func(n) { if n == 0 { return true } return isOdd(n - 1) }; isOdd = func(n) { if n == 0 { return false } return isEven(n - 1) }; isEven(10)", true},
		{"sum = func(a, b) { return a + b }; sum(3, 4)", 7.0},
		{"max = func(a, b) { if a > b { return a } return b }; max(10, 20)", 20.0},
		{"merge = func(a, b) { return a + b }; merge([1, 2], [3, 4])", []interface{}{1.0, 2.0, 3.0, 4.0}},
		{"result = [] len(result)", 0.0},
		{"test = func(arr) { print(arr) }; test([1,2,3])", nil},
		{`
		map = func(arr, fn) {
			result = [] ;
			i = 0;
			while i < len(arr) {
				result = append(result, fn(arr[i]));
				i = i + 1
			};
			return result
		};
		map([1, 2, 3], func(x) {
			return x * 2
		})`, []interface{}{2.0, 4.0, 6.0}},

		{"obj = {a: 2, b: {c: [1, 2, 3]}}; obj.a", 2.0},
		{"obj = {x: 10, y: 20}; obj.x", 10.0},
		{"obj = {x: 10, y: 20}; obj.y", 20.0},
		{"obj = {x: 10, y: 20}; obj.z = 30; obj.z", 30.0},
		{"obj = {x: 10}; obj.x += 5; obj.x", 15.0},
		{"obj = {x: 10}; obj.x -= 3; obj.x", 7.0},
		{`obj = {x: 10}; obj["x"] -= 3; obj["x"]`, 7.0},
		{`obj = {x: 10}; obj['x'] += 3; obj['x']`, 13.0},
		{`obj = {x: 10}; obj[x] += 4; obj[x]`, 14.0},
		{"obj = {x: 10}; obj = {y: 20}; obj.x", nil},
		{"obj = {x: 10}; obj = {x: 30}; obj.x", 30.0},
		{"obj = {a: 2, b: {c: [1, 2, 3]}}; obj.b.c[1]", 2.0},
		// {"filter = func(arr, fn) { result = [] ; for i = 0; i < len(arr); i++ { if fn(arr[i]) { result = append(result, arr[i]) } }; return result }; filter([1, 2, 3, 4], func(x) { return x % 2 == 0 })", []interface{}{2.0, 4.0}},
		// {"reduce = func(arr, fn, acc) { for i = 0; i < len(arr); i++ { acc = fn(acc, arr[i]) }; return acc }; reduce([1, 2, 3], func(acc, x) { return acc + x }, 0)", 6.0},
		// {"fibonacci = func(n) { if n <= 1 { return n } return fibonacci(n - 1) + fibonacci(n - 2) }; fibonacci(10)", 55.0},
		// {"reverse = func(s) { result = \"\"; for i = len(s) - 1; i >= 0; i-- { result = result + s[i] }; return result }; reverse(\"hello\")", "olleh"},
	}

	for _, tt := range tests {
		l := NewLexer(tt.input)
		p := NewParser(l)
		program := p.ParseProgram()
		if len(p.errors) > 0 {
			fmt.Printf("Parser errors for input: %s\n", tt.input)
			for _, err := range p.errors {
				fmt.Println(err)
			}
			continue
		}
		evaluator := NewEvaluator()

		result := evaluator.Eval(program)
		if reflect.DeepEqual(result, tt.expected) {
			fmt.Printf("Test passed: %s\n", tt.input)
		} else {
			fmt.Printf("Test failed: %s\n", tt.input)
			fmt.Printf("Expected: %v, got: %v\n", tt.expected, result)
		}
	}

}
