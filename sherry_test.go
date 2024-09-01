package sherry

import (
	"fmt"
	"os"
	"reflect"
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

func TestBase(t *testing.T) {
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
		{"fibonacci = func(n) { if n <= 1 { return n } return fibonacci(n - 1) + fibonacci(n - 2) }; fibonacci(10)", 55.0},
		// {"reverse = func(s) { result = \"\"; for i = len(s) - 1; i >= 0; i-- { result = result + s[i] }; return result }; reverse(\"hello\")", "olleh"},
		{"fn = func(a, b) { return func(c) { return a + b + c } }; fn(1, 2)(3)", 6.0},
		{
			`fn = func(a, b) {
				return func(c) {
					return a + b + c
				}
			}

			closure = fn(1, 2)
			closure(3)`, 6.0},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			l := NewLexer(tt.input)
			p := NewParser(l)
			program := p.ParseProgram()
			if len(p.errors) > 0 {
				fmt.Printf("Parser errors for input: %s\n", tt.input)
				for _, err := range p.errors {
					fmt.Println(err)
				}
				return
			}
			evaluator := NewEvaluator()

			result := evaluator.Eval(program)
			if reflect.DeepEqual(result, tt.expected) {
				// fmt.Printf("Test passed: %s\n", tt.input)
				t.Logf("Test passed: %s\n", tt.input)
			} else {
				t.Errorf("Test failed: %s\n", tt.input)
				t.Errorf("Expected: %v, got: %v\n", tt.expected, result)
			}
		})
	}
}

func TestHelloWorld(t *testing.T) {
	evaluator := NewEvaluator()
	evaluator.EvalString("println('hello world')")
}
