# <center>Note of Python</center>

---

## 第1章 基础知识

### 1.1 交互式解释器

- 无需分号,如果愿意也可以加上分号,但不会有任何影响(除非后面还有其他的代码)

### 1.2 算法是什么

- 类似与流程或者菜谱,尽可能地详尽地描述如何完成某项任务

### 1.3 数和表达式


```
>>> 1 / 2
0.5
```

结果为浮点数(float)

```
>>> 1 / 1
1.0
```

尽管是1但是仍是浮点数

- 如果想要舍弃小数部分

```
>>> 1 // 2
0
```

- 如果想要取余

```
>>> 1 % 2
1
```

x % y 等价于 x - ((x // y) * y)

- 负数的取余 - 特殊

```
>>> 10 % 3
1

>>> 10 % -3
-2

>>> -10 % 3
2

>>> -10 % -3
-1
```

这些结果是因为等价的式子 x - ((x // y) * y)

- 负数的整除 - 特殊

```
>>> 10 // 3
3

>>> 10 // -3
-4

>>> -10 // 3
-4

>>> -10 // -3
3
```

总结: 结果为负数的情况下,圆整后将离0更远. 这意味着对于,-10 // 3
将向下圆整到-4,而不是向上圆整到 -3

- 乘方运算符

```
>>> 2 ** 3
8
>>> -3 ** 2
-9

>>> (-3) ** 2
9
```

注意乘方的优先级比求负高

***十六进制八进制和二进制***

```
> > > 0xAF
175
### 十六进制 Hex

>>>0o10
8
### 八进制 Octal

>>> 0b1011010010
722
### 二进制 Binary
```

这些算法都以0开头

### 1.4 变量

**变量**时表示(或*指向*)特定值的名称 #(指向地址)

```
>>> x = 3
```

**赋值**

> 在Python中,名称(标识符)只能由字母,数字和下划线(_)构成,且不能以数字打头.  
> 因此Plan9是合法的,而9Plan不是

### 1.5 语句

```
>>> 2 * 2
4

>>> print(2 * 2)
4
```

### 1.6 获取用户输入

```
>>> input("The meaning of life: ")
The meaning of life: 42
'42'
```

执行第一行, 打印字符"The meaning of life: "提示用户输入相应信息 #
这里的'42'是字符串

```
>>> x = input("x: ") 
x: 34 

>>> y = input("y: ") 
y: 42 

>>> print(int(x) * int(y)) 
1428
```

### 1.7 函数

幂运算的函数pow()

```
>>> 2 ** 3 
8 

>>> pow(2, 3) 
8
```

类似于pow等标准函数为**内置函数**

**调用**函数pow(),提供**实参**2,3

- 绝对值函数与圆整函数

```
>>> abs(-10) 
10 

>>> 2 // 3 
0 

>>> round(2 / 3) 
1.0
```

向最接近圆整round,向下圆整floor

### 1.8 模块

```
>>> import math 
>>> math.floor(32.9) 
32
```

我们使用import导入模块，再以module.function的方式使用模块中的函数

```
>>> math.ceil(32.3) 
33 

>>> math.ceil(32) 
32
```

向上圆整

```
>>> from math import sqrt 
>>> sqrt(9) 
3.0
```

通过使用命令import的变种from module import
function，可在调用函数时不指定模块前缀。

#### 1.8.1 cmath和复数

- sqrt被提供负数时

```
>>> from math import sqrt 
>>> sqrt(-1) 
Traceback (most recent call last): 
...
ValueError: math domain error
```

某些平台显示

```
>>> sqrt(-1) 
nan
```

> nan具有特殊含义，指的是“非数值”（not a number）

- 导入处理**复数**的模块

```
>>> import cmath 
>>> cmath.sqrt(-1) 
1j
```

注意到这里没有使用from ... import
...。如果使用了这种import命令，将无法使用常规函数sqrt

```
>>> (1 + 3j) * (9 + 4j) 
(-3 + 31j)
```

> Python没有专门表示虚数的类型，而将虚数视为实部为零的复数

#### 1.8.2 回到未来

跳过SKIP

### 1.9 保存并执行程序

跳过SKIP

#### 1.9.1 从命令提示符运行Python脚本

```
C:\>python hello.py
```

#### 1.9.2 让脚本像普通程序一样

跳过SKIP

#### 1.9.3 注释

在代码中，井号(#)后面到行尾的所有内容都将被忽略

```
# 打印圆的周长：
print(2 * pi * radius)
```

注释让程序更容易理解

### 1.10 字符串

```
print("Hello, world!")
```

"Hello, world!"是**字符串**

#### 1.10.1 单引号字符串以及对引号转义

单引号与双引号完全相同 但在特殊情况下有帮助

```
>>> "Let's go!" 
"Let's go!" 

>>> '"Hello, world!" she said' 
'"Hello, world!" she said'
```

使用反斜杠（\）对引号进行转义

```
>>> 'Let\'s go!' 
"Let's go!"
```

#### 1.10.2 拼接字符串

```
>>> "Hello, " + "world!" 
'Hello, world!' 
>>> x = "Hello, " 
>>> y = "world!" 
>>> x + y 
'Hello, world!'
```

#### 1.10.3 字符串表示str和repr

换行符\n

```
>>> "Hello,\nworld!" 
'Hello,\nworld!' 

>>> print("Hello,\nworld!") 
Hello, 
world!
```

repr会获得值的合法Python表达式表示

```
>>> print(repr("Hello,\nworld!")) 
'Hello,\nworld!' 
>>> print(str("Hello,\nworld!")) 
Hello, 
world!
```

#### 1.10.4 长字符串、原始字符串和字节

1. 长字符串  
   要表示很长的字符串（跨越多行的字符串），可使用三引号（而不是普通引号）。

```
print('''This is a very long string. It continues here. 
And it's not over yet. "Hello, world!" 
Still here.''')
```

还可使用三个双引号

> 常规字符串也可横跨多行。只要在行尾加上反斜杠，反斜杠和换行符将被转义，即被忽略。例如，如果编写如下代码：  
> print("Hello, \  
> world!")  
> 它将打印Hello, world!。这种处理手法也适用于表达式和语句

2. 原始字符串  
   字符串中包含换行符

```
>>> print('Hello,\nworld!') 
Hello, 
world!
```

对反斜杠本身进行转义

```
>>> print('C:\\nowhere') 
C:\nowhere
```

运用原始字符串r

```
>>> print(r'C:\nowhere') 
C:\nowhere 

>>> print(r'C:\Program Files\fnord\foo\bar\baz\frozz\bozz') 
C:\Program Files\fnord\foo\bar\baz\frozz\bozz
```

原始字符串的最后一个字符不能是反斜杠

```
>>> print(r"This is illegal\") 
SyntaxError: EOL while scanning string literal

>>> print(r'C:\Program Files\foo\bar' '\\') 
C:\Program Files\foo\bar\
```

3. Unicode、bytes和bytearray

```
>>> "\u00C6" 
'Æ' 

>>> "\U0001F60A" 
'☺ '

>>> "This is a cat: \N{Cat}" 
'This is a cat: '
```

使用ASCII、UTF-8和UTF-32编码将字符串转换为bytes

```
>>> "Hello, world!".encode("ASCII") 
b'Hello, world!' 

>>> "Hello, world!".encode("UTF-8") 
b'Hello, world!' 

>>> "Hello, world!".encode("UTF-32") 
b'\xff\xfe\x00\x00H\x00\x00\x00e\x00\x00\x00l\x00\x00\x00l\x00\x00\x00o\x00\x00\x00,\x00\ 
x00\x00 \x00\x00\x00w\x00\x00\x00o\x00\x00\x00r\x00\x00\x00l\x00\x00\x00d\x00\x00\x00!\x00\ 
x00\x00'
```

```
>>> len("How long is this?".encode("UTF-8")) 
17 

>>> len("How long is this?".encode("UTF-32")) 
72
```

只要字符串包含较怪异的字符，ASCII和UTF-8之间的差别便显现出来了

```
>>> "Hællå, wørld!".encode("ASCII") 
Traceback (most recent call last): 
 ... 
UnicodeEncodeError: 'ascii' codec can't encode character '\xe6' in position 1: ordinal not 
in range(128)
```

可向encode提供另一个实参，告诉它如何处理错误

```
>>> "Hællå, wørld!".encode("ASCII", "ignore") 
b'Hll, wrld!' 

>>> "Hællå, wørld!".encode("ASCII", "replace") 
b'H?ll?, w?rld!' 

>>> "Hællå, wørld!".encode("ASCII", "backslashreplace") 
b'H\\xe6ll\\xe5, w\\xf8rld!' 

>>> "Hællå, wørld!".encode("ASCII", "xmlcharrefreplace") 
b'H&#230;ll&#229;, w&#248;rld!'
```

几乎在所有情况下，都最好使用UTF-8。事实上，它也是默认使用的编码

```
>>> "Hællå, wørld!".encode() 
b'H\xc3\xa6ll\xc3\xa5, w\xc3\xb8rld!'

>>> b'H\xc3\xa6ll\xc3\xa5, w\xc3\xb8rld!'.decode() 
'Hællå, wørld!'
```

可不使用方法encode和decode，而直接创建bytes和str（即字符串）对象

```
>>> bytes("Hællå, wørld!", encoding="utf-8") 
b'H\xc3\xa6ll\xc3\xa5, w\xc3\xb8rld!' 

>>> str(b'H\xc3\xa6ll\xc3\xa5, w\xc3\xb8rld!', encoding="utf-8") 
'Hællå, wørld!'
```

编码和解码的最重要用途之一是，将文本存储到磁盘文件中

bytearray是可修改的字符串

```
>>> x = bytearray(b"Hello!") 
>>> x[1] = ord(b"u") 
>>> x 
bytearray(b'Hullo!')
```

### 1.11 小结

- **算法**：算法犹如菜谱，告诉你如何完成特定的任务。从本质上说，编写计算机程序就是
  使用计算机能够理解的语言（如Python）描述一种算法。这种对机器友好的描述被称为程
  序，主要由表达式和语句组成。
- **表达式**：表达式为程序的一部分，结果为一个值。例如，2 + 2就是一个表达式，结果为
  4。简单表达式是使用运算符（如+或%）和函数（如pow）将字面值（如2或"Hello"）组
  合起来得到的。通过组合简单的表达式，可创建复杂的表达式，如(2 + 2) *(3 - 1)。表
  达式还可能包含变量。
- **变量**：变量是表示值的名称。通过赋值，可将新值赋给变量，如x = 2。赋值是一种语句。
- **语句**：语句是让计算机执行特定操作的指示。这种操作可能是修改变量（通过赋值）、将
  信息打印到屏幕上（如print("Hello, world!")）、导入模块或执行众多其他任务。
- **函数**：Python函数类似于数学函数，它们可能接受参数，并返回结果（在第6章学习编写
  自定义函数时，你将发现函数实际上可以在返回前做很多事情）。
- **模块**：模块是扩展，可通过导入它们来扩展Python的功能。例如，模块math包含多个很有
  用的函数。
- **程序**：你通过练习学习了如何编写、保存和运行Python程序。
- **字符串**：字符串非常简单。它们其实就是一段文本，其中的字符是用Unicode码点表示的。
  然而，对于字符串，需要学习的知识有很多。本章介绍了很多表示字符串的方式，第3章
  将介绍众多字符串用法。

#### 1.11.1 本章介绍的新函数

| 函 数                                | 描 述                                                  |
|:------------------------------------|:-------------------------------------------------------|
| abs(number)                         | 返回指定数的绝对值                                        |
| bytes(string, encoding\[, errors\]) | 对指定的字符串进行编码，并以指定的方式处理错误                 |
| cmath.sqrt(number)                  | 返回平方根；可用于负数                                     |
| float(object)                       | 将字符串或数字转换为浮点数                                  |
| help(\[object\])                    | 提供交互式帮助                                           |
| input(prompt)                       | 以字符串的方式获取用户输入                                  |
| int(object)                         | 将字符串或数转换为整数                                     |
| math.ceil(number)                   | 以浮点数的方式返回向上圆整的结果                             |
| math.floor(number)                  | 以浮点数的方式返回向下圆整的结果                             |
| math.sqrt(number)                   | 返回平方根；不能用于负数                                   |
| pow(x, y\[, z\])                    | 返回x的y次方对z求模的结果                                  |
| print(object, ...)                  | 将提供的实参打印出来，并用空格分隔                           |
| repr(object)                        | 返回指定值的字符串表示                                     |
| round(number\[, ndigits\])          | 四舍五入为指定的精度，正好为5时舍入到偶数                     |
| str(object)                         | 将指定的值转换为字符串。用于转换bytes时，可指定编码和错误处理方式 |

#### 1.11.2 预告

跳过SKIP

## 第2章 列表与元组

