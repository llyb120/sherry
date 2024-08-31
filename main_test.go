package main

import (
	"fmt"
	"os"
	"testing"
)

func TestEval(t *testing.T) {
	data, err := os.ReadFile("1.rule")
	if err != nil {
		t.Fatalf("Failed to read file: %v", err)
	}
	println("要执行的代码" + string(data))

	l := NewLexer(string(data))
	p := NewParser(l)
	program := p.ParseProgram()
	if len(p.errors) > 0 {
		fmt.Printf("Parser errors for input: %s\n", data)
		for _, err := range p.errors {
			fmt.Println(err)
		}
	}
	e := NewEvaluator()
	e.Eval(program)
}
