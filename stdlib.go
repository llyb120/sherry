package main

import "fmt"

func RegisterStdLib(e *Evaluator) {
	e.env["len"] = func(args ...interface{}) interface{} {
		if len(args) != 1 {
			return nil
		}
		switch arg := args[0].(type) {
		case string:
			return float64(len(arg))
		case []interface{}:
			return float64(len(arg))
		default:
			return nil
		}
	}
	e.env["append"] = func(args ...interface{}) interface{} {
		if len(args) < 2 {
			return nil
		}
		arr, ok := args[0].([]interface{})
		if !ok {
			return nil
		}
		for _, item := range args[1:] {
			arr = append(arr, item)
		}
		return arr
	}
	e.env["print"] = func(args ...interface{}) interface{} {
		fmt.Print(args...)
		return nil
	}
	e.env["println"] = func(args ...interface{}) interface{} {
		fmt.Println(args...)
		return nil
	}
}
