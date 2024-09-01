## Sherry

Sherry 是基于GO编写一种简单而灵活的脚本语言，具有动态类型和直观的语法。

### 优势

1. 动态类型：Sherry 是一种动态类型语言，变量无需显式声明类型。
2. 直观语法：Sherry 的语法简洁明了，基本和GO一样，无需额外的学习成本。
3. 内置数据结构：Sherry 提供了数组、对象等常用的数据结构。
4. 函数式编程：Sherry 支持函数式编程，可以方便地进行函数嵌套和闭包操作。
6. 便于集成，单文件无任何依赖，可以轻松引入项目而不担心冲突。


### 快速上手
```go
package main

func main() {
	evaluator := NewEvaluator()
	evaluator.EvalString("println('hello world')")
}
```

### 基础语法

#### 1. 变量声明
```
a = 1
b = 2
c = a + b
d = "hello"
e = [1, 2, 3]
f = {
    name: "张三"
    age: 18
}
```

#### 2. 条件语句
```
if a > b {
	println('a > b')
} else if a == b {
	println('a == b')
} else {
	println('a < b')
}


#### 3. 循环语句
```
i = 0
while i < 10 {
	println(i)
	i = i + 1

    if i == 5 {
        break
    }
}

// 注意：Sherry 的循环语句只有 while 一种，没有 for 循环
```

#### 4. 函数定义
```
// 直接定义函数
func add(a, b) {
	return a + b
}

// 使用变量定义
minus = func(a, b) {
	return a - b
}
```

#### 5. 调用函数
```
add(1, 2)
minus(1, 2)

// 不同于GO的是，Sherry 目前最多支持一个返回值，如果需要返回多个值，请使用数组或对象
```

#### 6. 数组
```
arr = [1, 2, 3]
println(arr[0])
```

#### 7. 对象
```
obj = {
    name: "张三"
    age: 18
}
println(obj.name)
```

#### 8. 闭包
```
fn = func(a, b) {
    return func(c) {
        return a + b + c
    }
}

closure = fn(1, 2)
closure(3)
```

### 内置函数

#### 1. println
```
println('hello world')
```

#### 2. print
```
print('hello world')
```

#### 3. len
```
len([1, 2, 3])
```

