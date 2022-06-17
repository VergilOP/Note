# Note of Python

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

### 2.1 序列概述

列表和元组的主要不同在于，列表是可以修改的，而元组不可以

这意味着列表适用于需要中途添加元素的情形，而元组适用于出于某种考虑需要禁止修改序列的情形

```
>>> edward = ['Edward Gumby', 42]
>>> john = ['John Smith', 50] 
>>> database = [edward, john] 
>>> database 
[['Edward Gumby', 42], ['John Smith', 50]]
```

> Python支持一种数据结构的基本概念，名为容器（container）。容器基本上就是可包含其他对象的对象。两种主要的容器是序列（如列表和元组）和映射（如字典）。在序列中，每个元素都有编号，而在映射中，每个元素都有名称（也叫键）。

### 2.2 通用的序列操作

有几种操作适用于所有序列，包括索引、切片、相加、相乘和成员资格检查

#### 2.2.1 索引

```
>>> greeting = 'Hello' 
>>> greeting[0] 
'H'
```

当你使用负数索引时，Python将从右（即从最后一个元素）开始往左数，因此1是最后一个元素的位置

```
>>> greeting[-1] 
'o'
```

如果函数调用返回一个序列，可直接对其执行索引操作

```
>>> fourth = input('Year: ')[3] 
Year: 2005 

>>> fourth 
'5'
```

#### 2.2.2 切片

使用切片（slicing）来访问特定范围内的元素

```
>>> tag = '<a href="http://www.python.org">Python web site</a>' 
>>> tag[9:30] 
'http://www.python.org' 

>>> tag[32:-4] 
'Python web site'
```

1.绝妙的简写

```
>>> numbers[-3:-1] 
[8, 9]

>>> numbers[-3:0] 
[]

>>> numbers[-3:] 
[8, 9, 10]

>>> numbers[:3] 
[1, 2, 3]

>>> numbers[:] 
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

2. 更大的步长

在普通切片中，步长为1

```
>>> numbers[0:10:1] 
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

>>> numbers[0:10:2] 
[1, 3, 5, 7, 9] 

>>> numbers[3:6:3] 
[4]
```

```
>>> numbers[::4] 
[1, 5, 9]
```

负数步长

```
>>> numbers[8:3:-1] 
[9, 8, 7, 6, 5] 

>>> numbers[10:0:-2] 
[10, 8, 6, 4, 2] 

>>> numbers[0:10:-2] 
[] 

>>> numbers[::-2] 
[10, 8, 6, 4, 2]

>>> numbers[5::-2] 
[6, 4, 2]
 
>>> numbers[:5:-2] 
[10, 8]
```

#### 2.2.3 序列相加

```
>>> [1, 2, 3] + [4, 5, 6] 
[1, 2, 3, 4, 5, 6] 

>>> 'Hello,' + 'world!' 
'Hello, world!' 

>>> [1, 2, 3] + 'world!' 
Traceback (innermost last): 
 File "<pyshell>", line 1, in ? 
 [1, 2, 3] + 'world!' 
TypeError: can only concatenate list (not "string") to list
```

#### 2.2.4 乘法

```
>>> 'python' * 5 
'pythonpythonpythonpythonpython' 

>>> [42] * 10 
[42, 42, 42, 42, 42, 42, 42, 42, 42, 42]
```

要将列表的长度初始化为10

```
>>> sequence = [None] * 10 
>>> sequence 
[None, None, None, None, None, None, None, None, None, None]
```

#### 2.2.5 成员资格

```
>>> permissions = 'rw' 
>>> 'w' in permissions 
True 

>>> 'x' in permissions 
False 

>>> users = ['mlh', 'foo', 'bar'] 
>>> input('Enter your user name: ') in users 
Enter your user name: mlh 
True 

>>> subject = '$$$ Get rich now!!! $$$' 
>>> '$$$' in subject 
True
```

长度、最小值和最大值

```
>>> numbers = [100, 34, 678] 
>>> len(numbers) 
3 

>>> max(numbers) 
678 

>>> min(numbers)
34 

>>> max(2, 3) 
3 

>>> min(9, 3, 2, 5) 
2
```

### 2.3 列表: Python的主力

SKIP跳过

#### 2.3.1 函数list

```
>>> list('Hello') 
['H', 'e', 'l', 'l', 'o']
```

#### 2.3.2 基本的列表操作

1. 修改列表: 给元素赋值

```
>>> x = [1, 1, 1] 
>>> x[1] = 2 
>>> x 
[1, 2, 1]
```

2. 删除元素

```
>>> names = ['Alice', 'Beth', 'Cecil', 'Dee-Dee', 'Earl'] 
>>> del names[2] 
>>> names 
['Alice', 'Beth', 'Dee-Dee', 'Earl']
```

3. 给切片赋值

```
>>> name = list('Perl') 
>>> name 
['P', 'e', 'r', 'l'] 

>>> name[2:] = list('ar') 
>>> name 
['P', 'e', 'a', 'r']

>>> name = list('Perl') 
>>> name[1:] = list('ython') 
>>> name 
['P', 'y', 't', 'h', 'o', 'n']
```

切片赋值还可在不替换原有元素的情况下插入新元素。

```
>>> numbers = [1, 5] 
>>> numbers[1:1] = [2, 3, 4] 
>>> numbers 
[1, 2, 3, 4, 5]
```

采取相反的措施来删除切片

```
>>> numbers 
[1, 2, 3, 4, 5] 

>>> numbers[1:4] = [] 
>>> numbers 
[1, 5]
```

#### 2.3.3 列表方法

1. append

方法append用于将一个对象附加到列表末尾

```
>>> lst = [1, 2, 3] 
>>> lst.append(4) 
>>> lst 
[1, 2, 3, 4]
```

2. clear

方法clear就地清空列表的内容

```
>>> lst = [1, 2, 3] 
>>> lst.clear() 
>>> lst 
[]
```

3. copy

方法 copy 复制列表. 常规复制只是将另一个名称关联到列表

```
>>> a = [1, 2, 3] 
>>> b = a 
>>> b[1] = 4 
>>> a 
[1, 4, 3]
```

```
>>> a = [1, 2, 3] 
>>> b = a.copy() 
>>> b[1] = 4 
>>> a 
[1, 2, 3]
```

4. count

方法count计算指定的元素在列表中出现了多少次

```
>>> ['to', 'be', 'or', 'not', 'to', 'be'].count('to') 
2 

>>> x = [[1, 2], 1, 1, [2, 1, [1, 2]]] 
>>> x.count(1) 
2 

>>> x.count([1, 2]) 
1
```

5. extend

方法extend让你能够同时将多个值附加到列表末尾

```
>>> a = [1, 2, 3] 
>>> b = [4, 5, 6] 
>>> a.extend(b) 
>>> a 
[1, 2, 3, 4, 5, 6]
```

这可能看起来类似于拼接，但存在一个重要差别，那就是将修改被扩展的序列（这里是a）。  
在常规拼接中，情况是返回一个全新的序列。

```
>>> a = [1, 2, 3] 
>>> b = [4, 5, 6] 
>>> a + b 
[1, 2, 3, 4, 5, 6] 

>>> a 
[1, 2, 3]
```

要获得与extend相同的效果

```
>>> a = [1, 2, 3] 
>>> b = [4, 5, 6] 
>>> a[len(a):] = b 
>>> a 
[1, 2, 3, 4, 5, 6]
```

虽然可行，但*可读性不是很高*

6. index

方法index在列表中查找指定值第一次出现的索引

```
>>> knights = ['We', 'are', 'the', 'knights', 'who', 'say', 'ni'] 
>>> knights.index('who') 
4 

>>> knights.index('herring') 
Traceback (innermost last): 
 File "<pyshell>", line 1, in ? 
 knights.index('herring') 
ValueError: list.index(x): x not in list
```

然而，搜索'herring'时引发了异常，因为根本就没有找到这个单词

7. insert

方法insert用于将一个对象插入列表。

```
>>> numbers = [1, 2, 3, 5, 6, 7] 
>>> numbers.insert(3, 'four') 
>>> numbers 
[1, 2, 3, 'four', 5, 6, 7]
```

与extend一样，也可使用切片赋值来获得与insert一样的效果

```
>>> numbers = [1, 2, 3, 5, 6, 7] 
>>> numbers[3:3] = ['four'] 
>>> numbers 
[1, 2, 3, 'four', 5, 6, 7]
```

虽然可行，但*可读性不是很高*

8. pop

方法pop从列表中删除一个元素（末尾为最后一个元素），并返回这一元素

```
>>> x = [1, 2, 3] 
>>> x.pop() 
3 

>>> x 
[1, 2] 

>>> x.pop(0) 
1 

>>> x 
[2]
```

> pop是唯一既修改列表又返回一个非None值的列表方法。

使用pop可实现一种常见的数据结构——栈（stack）  
后进先出（LIFO） - Python没有提供push，但可使用append来替代  
先进先出（FIFO） - 可使用insert(0, ...)代替append

9. remove

方法remove用于删除第一个为指定值的元素

```
>>> x = ['to', 'be', 'or', 'not', 'to', 'be'] 
>>> x.remove('be') 
>>> x 
['to', 'or', 'not', 'to', 'be'] 

>>> x.remove('bee') 
Traceback (innermost last): 
 File "<pyshell>", line 1, in ? 
 x.remove('bee') 
ValueError: list.remove(x): x not in list
```

不同于pop的是，它修改列表，但不返回任何值

10. reverse

方法reverse按相反的顺序排列列表中的元素

```
>>> x = [1, 2, 3] 
>>> x.reverse() 
>>> x 
[3, 2, 1]
```

注意到reverse修改列表，但不返回任何值

11. sort

方法sort用于对列表就地排序

```
>>> x = [4, 6, 2, 1, 7, 9] 
>>> x.sort() 
>>> x 
[1, 2, 4, 6, 7, 9]
```

```
>>> x = [4, 6, 2, 1, 7, 9] 
>>> y = x.sort() # Don't do this! 
>>> print(y) 
None
```

sort修改x且不返回任何值

正确方法

```
>>> x = [4, 6, 2, 1, 7, 9] 
>>> y = x.copy() 
>>> y.sort() 
>>> x 
[4, 6, 2, 1, 7, 9] 

>>> y 
[1, 2, 4, 6, 7, 9]
```

为获取排序后的列表的副本，另一种方式是使用函数sorted

```
>>> x = [4, 6, 2, 1, 7, 9] 
>>> y = sorted(x) 
>>> x 
[4, 6, 2, 1, 7, 9] 

>>> y 
[1, 2, 4, 6, 7, 9]
```

这个函数可用于任何序列，但总是返回一个列表

```
>>> sorted('Python') 
['P', 'h', 'n', 'o', 't', 'y']
```

12. 高级排序

要根据长度对元素进行排序，可将参数key设置为函数len

```
>>> x = ['aardvark', 'abalone', 'acme', 'add', 'aerate'] 
>>> x.sort(key=len) 
>>> x 
['add', 'acme', 'aerate', 'abalone', 'aardvark']
```

对于另一个关键字参数reverse，只需将其指定为一个真值，以指出是否要按相反的顺序对列表进行排序

```
>>> x = [4, 6, 2, 1, 7, 9] 
>>> x.sort(reverse=True) 
>>> x 
[9, 7, 6, 4, 2, 1]
```

### 2.4 元组：不可修改的序列

元组也是序列，唯一的差别在于元组是不能修改的

只要将一些值用逗号分隔，就能自动创建一个元组

```
>>> 1, 2, 3 
(1, 2, 3)

>>> (1, 2, 3) 
(1, 2, 3)

>>> () 
()

>>> 42 
42 

>>> 42, 
(42,)

>>> (42,) 
(42,)

>>> 3 * (40 + 2) 
126 
>>> 3 * (40 + 2,) 
(42, 42, 42)
```

函数tuple的工作原理与list很像：它将一个序列作为参数，并将其转换为元组

```
>>> tuple([1, 2, 3]) 
(1, 2, 3) 

>>> tuple('abc') 
('a', 'b', 'c') 

>>> tuple((1, 2, 3)) 
(1, 2, 3)
```

```
>>> x = 1, 2, 3 
>>> x[1] 
2 

>>> x[0:2] 
(1, 2)
```

元组的切片也是元组，就像列表的切片也是列表一样

### 2.5 小结

- **序列**：序列是一种数据结构，其中的元素带编号（编号从0开始）。列表、字符串和元组都属于序列，其中列表是可变的（你可修改其内容），而元组和字符串是不可变的（一旦创建，内容就是固定的）。要访问序列的一部分，可使用切片操作：提供两个指定切片起始和结束位置的索引。要修改列表，可给其元素赋值，也可使用赋值语句给切片赋值。
- **成员资格**：要确定特定的值是否包含在序列（或其他容器）中，可使用运算符in。将运算符in用于字符串时情况比较特殊——这样可查找子串。
- **方法**：一些内置类型（如列表和字符串，但不包括元组）提供了很多有用的方法。方法有点像函数，只是与特定的值相关联。方法是面向对象编程的一个重要方面，这将在第7
  章介绍

#### 2.5.1 本章介绍的新函数

| 函 数          | 描 述                                  |
|:--------------|:--------------------------------------|
| len(seq)      | 返回序列的长度                           |
| list(seq)     | 将序列转换为列表                         |
| max(args)     | 返回序列或一组参数中的最大值                |
| min(args)     | 返回序列和一组参数中的最小值                |
| reversed(seq) | 让你能够反向迭代序列                      |
| sorted(seq)   | 返回一个有序列表，其中包含指定序列中的所有元素 |
| tuple(seq)    | 将序列转换为元组                         |

#### 2.5.2 预告

跳过SKIP

## 第3章 使用字符串

### 3.1 字符串基本操作

所有标准序列操作（索引、切片、乘法、成员资格检查、长度、最小值和最大值）都适用于字符串，但别忘了字符串是不可变的，因此所有的元素赋值和切片赋值都是非法的

```
>>> website = 'http://www.python.org' 
>>> website[-3:] = 'com' 
Traceback (most recent call last): 
 File "<pyshell#19>", line 1, in ? 
 website[-3:] = 'com' 
TypeError: object doesn't support slice assignment
```

### 3.2 设置字符串的格式: 精简版

使用字符串格式设置运算符——百分号

```
>>> format = "Hello, s. s enough for ya?" % %
>>> values = ('world', 'Hot') 
>>> format values %
'Hello, world. Hot enough for ya?'
```

%s称为转换说明符，指出了要将值插入什么地方  
s意味着将值视为字符串进行格式设置  
%.3f将值的格式设置为包含3位小数的浮点数

另一种解决方案是所谓的模板字符串

```
>>> from string import Template 
>>> tmpl = Template("Hello, $who! $what enough for ya?") 
>>> tmpl.substitute(who="Mars", what="Dusty") 
'Hello, Mars! Dusty enough for ya?'
```

在最简单的情况下，替换字段没有名称或将索引用作名称

```
>>> "{}, {} and {}".format("first", "second", "third") 
'first, second and third' 

>>> "{0}, {1} and {2}".format("first", "second", "third") 
'first, second and third'

>>> "{3} {0} {2} {1} {3} {0}".format("be", "not", "or", "to") 
'to be or not to be'
```

```
>>> from math import pi 
>>> "{name} is approximately {value:.2f}.".format(value=pi, name="π") 
'π is approximately 3.14.'
```

关键字参数的排列顺序无关紧要  
指定了格式说明符.2f，并使用冒号将其与字段名隔开

```
>>> "{name} is approximately {value}.".format(value=pi, name="π") 
'π is approximately 3.141592653589793.'
```

如果变量与替换字段同名，还可使用一种简写

```
>>> from math import e 
>>> f"Euler's constant is roughly {e}." 
"Euler's constant is roughly 2.718281828459045."

>>> "Euler's constant is roughly {e}.".format(e=e) 
"Euler's constant is roughly 2.718281828459045."
```

### 3.3 设置字符串的格式: 完整版

每个值都被插入字符串中，以替换用花括号括起的替换字段

```
>>> "{{ceci n'est pas une replacement field}}".format() 
"{ceci n'est pas une replacement field}"
```

- **字段名**：索引或标识符，指出要设置哪个值的格式并使用结果来替换该字段。除指定值外，还可指定值的特定部分，如列表的元素
- **转换标志**：跟在叹号后面的单个字符。当前支持的字符包括r（表示repr）、s（表示str）
  和a（表示ascii）。如果你指定了转换标志，将不使用对象本身的格式设置机制，而是使用指定的函数将对象转换为字符串，再做进一步的格式设置
- **格式说明符**：跟在冒号后面的表达式（这种表达式是使用微型格式指定语言表示的）。格式说明符让我们能够详细地指定最终的格式，包括格式类型（如字符串、浮点数或十六进制数），字段宽度和数的精度，如何显示符号和千位分隔符，以及各种对齐和填充方式

#### 3.3.1 替换字段名

```
>>> "{foo} {} {bar} {}".format(1, 2, bar=4, foo=3) 
'3 1 4 2'

>>> "{foo} {1} {bar} {0}".format(1, 2, bar=4, foo=3) 
'3 2 4 1'

>>> fullname = ["Alfred", "Smoketoomuch"] 
>>> "Mr {name[1]}".format(name=fullname) 
'Mr Smoketoomuch' 

>>> import math 
>>> tmpl = "The {mod.__name__} module defines the value {mod.pi} for π" 
>>> tmpl.format(mod=math) 
'The math module defines the value 3.141592653589793 for π'
```

#### 3.3.2 基本转换

```
>>> print("{pi!s} {pi!r} {pi!a}".format(pi="π")) 
π 'π' '\u03c0'
```

上述三个标志（s、r和a）指定分别使用str、repr和ascii进行转换

可在格式说明（即冒号后面）使用字符f(表示定点数)

```
>>> "The number is {num}".format(num=42) 
'The number is 42' 

>>> "The number is {num:f}".format(num=42) 
'The number is 42.000000'

>>> "The number is {num:b}".format(num=42) 
'The number is 101010'
```

| 类型 | 含 义                                                                          |
|:----|:-------------------------------------------------------------------------------|
| b   | 将整数表示为二进制数                                                              |
| c   | 将整数解读为Unicode码点                                                           |
| d   | 将整数视为十进制数进行处理，这是整数默认使用的说明符                                    |
| e   | 使用科学表示法来表示小数（用e来表示指数）                                             |
| E   | 与e相同，但使用E来表示指数                                                         |
| f   | 将小数表示为定点数                                                                |
| F   | 与f相同，但对于特殊值（nan和inf），使用大写表示                                       |
| g   | 自动在定点表示法和科学表示法之间做出选择。这是默认用于小数的说明符，但在默认情况下至少有1位小数 |
| G   | 与g相同，但使用大写来表示指数和特殊值                                                |
| n   | 与g相同，但插入随区域而异的数字分隔符                                                |
| o   | 将整数表示为八进制数                                                              |
| s   | 保持字符串的格式不变，这是默认用于字符串的说明符                                        |
| x   | 将整数表示为十六进制数并使用小写字母                                                 |
| X   | 与x相同，但使用大写字母                                                            |
| %   | 将数表示为百分比值（乘以100，按说明符f设置格式，再在后面加上%）                          |

#### 3.3.3 宽度、精度和千位分隔符

```
>>> "{num:10}".format(num=3) 
' 3' 

>>> "{name:10}".format(name="Bob") 
'Bob '
```

数和字符串的对齐方式不同

精度也是使用整数指定的，但需要在它前面加上一个表示小数点的句点

```
>>> "Pi day is {pi:.2f}".format(pi=pi) 
'Pi day is 3.14'
```

可同时指定宽度和精度

```
>>> "{pi:10.2f}".format(pi=pi) 
' 3.14'
```

可使用逗号来指出你要添加千位分隔符

```
>>> 'One googol is {:,}'.format(10**100) 
'One googol is 10,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,00 
0,000,000,000,000,000,000,000,000,000,000,000,000,000,000'
```

#### 3.3.4 符号、对齐和用 0 填充

在指定宽度和精度的数前面，可添加一个标志  
这个标志可以是零、加号、减号或空格，其中零表示使用0来填充数字

```
>>> '{:010.2f}'.format(pi) 
'0000003.14'
```

要指定左对齐、右对齐和居中，可分别使用<、>和^

```
>>> print('{0:<10.2f}\n{0:^10.2f}\n{0:>10.2f}'.format(pi)) 
3.14 
 3.14 
  3.14
```

可以使用填充字符来扩充对齐说明符，这样将使用指定的字符而不是默认的空格来填充

```
>>> "{:$^15}".format(" WIN BIG ") 
'$$$ WIN BIG $$$'
```

说明符=，它指定将填充字符放在符号和数字之间

```
>>> print('{0:10.2f}\n{1:10.2f}'.format(pi, -pi)) 
    3.14 
   -3.14 
   
>>> print('{0:10.2f}\n{1:=10.2f}'.format(pi, -pi)) 
    3.14 
-   3.14
```

正数加上符号，可使用说明符+

```
>>> print('{0:-.2}\n{1:-.2}'.format(pi, -pi)) #默认设置
3.1 
-3.1 

>>> print('{0:+.2}\n{1:+.2}'.format(pi, -pi)) 
+3.1 
-3.1 

>>> print('{0: .2}\n{1: .2}'.format(pi, -pi)) 
 3.1 
-3.1
```

井号（#）选项，你可将其放在符号说明符和宽度之间（如果指定了这两种设置）。这个选项将触发另一种转换方式，转换细节随类型而异

```
>>> "{:b}".format(42) 
'101010' 

>>> "{:#b}".format(42) 
'0b101010'
```

对于各种十进制数，它要求必须包含小数点

```
>>> "{:g}".format(42) 
'42' 

>>> "{:#g}".format(42) 
'42.0000'
```

### 3.4 字符串方法

跳过SKIP

#### 3.4.1 center

方法center通过在两边添加填充字符（默认为空格）让字符串居中

```
>>> "The Middle by Jimmy Eat World".center(39) 
' The Middle by Jimmy Eat World ' 
>>> "The Middle by Jimmy Eat World".center(39, "*") 
'*****The Middle by Jimmy Eat World*****'
```

#### 3.4.2 find

方法find在字符串中查找子串。如果找到，就返回子串的第一个字符的索引，否则返回-1

```
>>> 'With a moo-moo here, and a moo-moo there'.find('moo') 
7 

>>> title = "Monty Python's Flying Circus" 
>>> title.find('Monty') 
0 

>>> title.find('Python')
6 

>>> title.find('Flying') 
15 

>>> title.find('Zirquss') 
-1

>>> subject = '$$$ Get rich now!!! $$$' 
>>> subject.find('$$$') 
0

>>> subject = '$$$ Get rich now!!! $$$' 
>>> subject.find('$$$') 
0 

>>> subject.find('$$$', 1) # 只指定了起点
20 

>>> subject.find('!!!') 
16 

>>> subject.find('!!!', 0, 16) # 同时指定了起点和终点
-1
```

#### 3.4.3 join

join是一个非常重要的字符串方法，其作用与split相反，用于合并序列的元素

```
>>> seq = [1, 2, 3, 4, 5] 
>>> sep = '+' 
>>> sep.join(seq) # 尝试合并一个数字列表
Traceback (most recent call last): 
 File "<stdin>", line 1, in ? 
TypeError: sequence item 0: expected string, int found 
>>> seq = ['1', '2', '3', '4', '5'] 
>>> sep.join(seq) # 合并一个字符串列表
'1+2+3+4+5' 
>>> dirs = '', 'usr', 'bin', 'env' 
>>> '/'.join(dirs) 
'/usr/bin/env' 
>>> print('C:' + '\\'.join(dirs)) 
C:\usr\bin\env
```

#### 3.4.4 lower

方法lower返回字符串的小写版本

```
>>> 'Trondheim Hammer Dance'.lower() 
'trondheim hammer dance'
```

#### 3.4.5 replace

方法replace将指定子串都替换为另一个字符串，并返回替换后的结果

```
>>> 'This is a test'.replace('is', 'eez') 
'Theez eez a test'
```

#### 3.4.6 split

split是一个非常重要的字符串方法，其作用与join相反，用于将字符串拆分为序列

```
>>> '1+2+3+4+5'.split('+') 
['1', '2', '3', '4', '5'] 

>>> '/usr/bin/env'.split('/') 
['', 'usr', 'bin', 'env'] 

>>> 'Using the default'.split() 
['Using', 'the', 'default']
```

#### 3.4.7 strip

方法strip将字符串开头和末尾的空白（但不包括中间的空白）删除，并返回删除后的结果

```
>>> ' internal whitespace is kept '.strip() 
'internal whitespace is kept'

>>> '*** SPAM * for * everyone!!! ***'.strip(' *!') 
'SPAM * for * everyone'
```

#### 3.4.8 translate

使用translate前必须创建一个转换表

```
>>> table = str.maketrans('cs', 'kz')

>>> 'this is an incredible test'.translate(table) 
'thiz iz an inkredible tezt'
```

#### 3.4.9 判断字符串是否满足特定的条件

很多字符串方法都以is打头，如isspace、isdigit和isupper，它们判断字符串是否具有特定的性质（如包含的字符全为空白、数字或大写）。如果字符串具备特定的性质，这些方法就返回True，否则返回False。

### 3.5 小结

- **字符串格式设置**：求模运算符（%）可用于将值合并为包含转换标志（如%s）的字符串，这让你能够以众多方式设置值的格式，如左对齐或右对齐，指定字段宽度和精度，添加符号（正号或负号）以及在左边填充0等。
- **字符串方法**：字符串有很多方法，有些很有用（如split和join），有些很少用到（如istitle和capitalize）。

#### 3.5.1 本章介绍的新函数

| 函 数                        | 描 述                                                         |
|:----------------------------|:--------------------------------------------------------------|
| string.capwords(s\[, sep\]) | 使用split根据sep拆分s，将每项的首字母大写，再以空格为分隔符将它们合并起来 |
| ascii(obj)                  | 创建指定对象的ASCII表示                                          |

## 第4章 当索引行不通时

### 4.1 字典的用途

下面是Python字典的一些用途：
- 表示棋盘的状态，其中每个键都是由坐标组成的元组；
- 存储文件修改时间，其中的键为文件名；
- 数字电话/地址簿。

### 4.2 创建和使用字典

```python
phonebook = {'Alice': '2341', 'Beth': '9102', 'Cecil': '3258'}
```

字典由**键**及其相应的**值**组成，这种键值对称为**项**（item）  
每个键与其值之间都用冒号（:）分隔，项之间用逗号分隔，而整个字典放在花括号内  
空字典（没有任何项）用两个花括号表示，类似于下面这样：{}

#### 4.2.1 函数 dict

可使用函数dict从其他映射（如其他字典）或键值对序列创建字典

```
>>> items = [('name', 'Gumby'), ('age', 42)] 
>>> d = dict(items) 
>>> d 
{'age': 42, 'name': 'Gumby'} 

>>> d['name'] 
'Gumby'
```

还可使用关键字实参来调用这个函数

```
>>> d = dict(name='Gumby', age=42) 
>>> d 
{'age': 42, 'name': 'Gumby'}
```

#### 4.2.2 基本的字典操作

字典的基本行为在很多方面都类似于序列。
- len(d)返回字典d包含的项（键值对）数。
- d\[k\]返回与键k相关联的值。
- d\[k\] = v将值v关联到键k。
- del d\[k\]删除键为k的项。
- k in d检查字典d是否包含键为k的项

虽然字典和列表有多个相同之处，但也有一些重要的不同之处
- 键的类型：字典中的键可以是整数，但并非必须是整数。字典中的键可以是任何不可变的类型，如浮点数（实数）、字符串或元组。
- 自动添加：即便是字典中原本没有的键，也可以给它赋值，这将在字典中创建一个新项。然而，如果不使用append或其他类似的方法，就不能给列表中没有的元素赋值。
- 成员资格：表达式k in d（其中d是一个字典）查找的是键而不是值，而表达式v in
  l（其中l是一个列表）查找的是值而不是索引。这看似不太一致，但你习惯后就会觉得相当自然。毕竟如果字典包含指定的键，检查相应的值就很容易。

```
>>> x = [] 
>>> x[42] = 'Foobar' 
Traceback (most recent call last): 
 File "<stdin>", line 1, in ? 
IndexError: list assignment index out of range 

>>> x = {} 
>>> x[42] = 'Foobar' 
>>> x 
{42: 'Foobar'}
```

#### 4.2.3 将字符串格式设置功能用于字典

```
>>> phonebook 
{'Beth': '9102', 'Alice': '2341', 'Cecil': '3258'} 

>>> "Cecil's phone number is {Cecil}.".format_map(phonebook) 
"Cecil's phone number is 3258."
```

#### 4.2.4 字典方法

1. clear

方法clear删除所有的字典项，这种操作是就地执行的（就像list.sort一样），因此什么都不返回（或者说返回None）

```
>>> d = {} 
>>> d['name'] = 'Gumby' 
>>> d['age'] = 42 
>>> d 
{'age': 42, 'name': 'Gumby'} 

>>> returned_value = d.clear() 
>>> d 
{} 

>>> print(returned_value) 
None
```

看两个场景

```
>>> x = {} 
>>> y = x 
>>> x['key'] = 'value' 
>>> y 
{'key': 'value'} 
>>> x = {} 
>>> x = {} 
{'key': 'value'}

>>> x = {} 
>>> y = x 
>>> x['key'] = 'value' 
>>> y 
{'key': 'value'} 
>>> x.clear() 
>>> y 
{}
```

在这两个场景中，x和y最初都指向同一个字典。在第一个场景中，我通过将一个空字典赋给x来“清空”它。这对y没有任何影响，它依然指向原来的字典。这种行为可能正是你想要的，但要删除原来字典的所有元素，必须使用clear。如果这样做，y也将是空的，如第二个场景所示

2. copy

方法copy返回一个新字典，其包含的键值对与原来的字典相同（这个方法执行的是浅复制，因为值本身是原件，而非副本）

```
>>> x = {'username': 'admin', 'machines': ['foo', 'bar', 'baz']} 
>>> y = x.copy() 
>>> y['username'] = 'mlh' 
>>> y['machines'].remove('bar') 
>>> y 
{'username': 'mlh', 'machines': ['foo', 'baz']} 
>>> x 
{'username': 'admin', 'machines': ['foo', 'baz']}
```

如你所见，当替换副本中的值时，原件不受影响。然而，如果修改副本中的值（就地修改而不是替换），原件也将发生变化，因为原件指向的也是被修改的值（如这个示例中的'machines'列表所示）

为避免这种问题，一种办法是执行深复制，即同时复制值及其包含的所有值，等等。为此，可使用模块copy中的函数deepcopy

```
>>> from copy import deepcopy 
>>> d = {} 
>>> d['names'] = ['Alfred', 'Bertrand'] 
>>> c = d.copy() 
>>> dc = deepcopy(d) 
>>> d['names'].append('Clive') 
>>> c 
{'names': ['Alfred', 'Bertrand', 'Clive']} 
>>> dc 
{'names': ['Alfred', 'Bertrand']}
```

3. fromkeys

方法fromkeys创建一个新字典，其中包含指定的键，且每个键对应的值都是None

```
>>> {}.fromkeys(['name', 'age']) 
{'age': None, 'name': None}

>>> dict.fromkeys(['name', 'age'], '(unknown)') 
{'age': '(unknown)', 'name': '(unknown)'}
```

4. get

方法get为访问字典项提供了宽松的环境。  
通常，如果你试图访问字典中没有的项，将引发错误

```
>>> d = {} 
>>> print(d['name']) 
Traceback (most recent call last): 
 File "<stdin>", line 1, in ? 
KeyError: 'name'
```

而使用get不会这样

```
>>> print(d.get('name')) 
None

>>> d.get('name', 'N/A') 
'N/A'
```

5. items

方法items返回一个包含所有字典项的列表，其中每个元素都为(key,
value)的形式。字典项在列表中的排列顺序不确定

```
>>> d = {'title': 'Python Web Site', 'url': 'http://www.python.org', 'spam': 0} 
>>> d.items() 
dict_items([('url', 'http://www.python.org'), ('spam', 0), ('title', 'Python Web Site')])
```

返回值属于一种名为字典视图的特殊类型

可确定其长度以及对其执行成员资格检查

```
>>> it = d.items() 
>>> len(it) 
3 

>>> ('spam', 0) in it 
True
```

视图的一个优点是不复制，它们始终是底层字典的反映，即便你修改了底层字典亦如此

```
>>> d['spam'] = 1 
>>> ('spam', 0) in it 
False 

>>> d['spam'] = 0 
>>> ('spam', 0) in it 
True
```

要将字典项复制到列表中

```
>>> list(d.items()) 
[('spam', 0), ('title', 'Python Web Site'), ('url', 'http://www.python.org')]
```

6. keys

方法keys返回一个字典视图，其中包含指定字典中的键

7. pop

方法pop可用于获取与指定键相关联的值，并将该键值对从字典中删除。

```
>>> d = {'x': 1, 'y': 2} 
>>> d.pop('x') 
1 
>>> d 
{'y': 2}
```

8. popitem

方法popitem类似于list.pop，但list.pop弹出列表中的最后一个元素，而popitem随机地弹出一个字典项，因为字典项的顺序是不确定的，没有“最后一个元素”的概念。如果你要以高效地方式逐个删除并处理所有字典项，这可能很有用，因为这样无需先获取键列表

```
>>> d = {'url': 'http://www.python.org', 'spam': 0, 'title': 'Python Web Site'} 
>>> d.popitem() 
('url', 'http://www.python.org') 

>>> d 
{'spam': 0, 'title': 'Python Web Site'}
```

虽然popitem类似于列表方法pop，但字典没有与append（它在列表末尾添加一个元素）对应
的方法。这是因为字典是无序的，类似的方法毫无意义

> 如果希望方法popitem以可预测的顺序弹出字典项，请参阅模块collections中的OrderedDict类

9. setdefault

方法setdefault有点像get，因为它也获取与指定键相关联的值，但除此之外，setdefault
还在字典不包含指定的键时，在字典中添加指定的键值对

```
>>> d = {} 
>>> d.setdefault('name', 'N/A') 
'N/A' 
>>> d 
{'name': 'N/A'} 
>>> d['name'] = 'Gumby' 
>>> d.setdefault('name', 'N/A') 
'Gumby' 
>>> d 
{'name': 'Gumby'}
```

指定的键不存在时，setdefault返回指定的值并相应地更新字典

如果指定的键存在，就返回其值，并保持字典不变。与get一样，值是可选的；如果没有指定，默认为None

```
>>> d = {} 
>>> print(d.setdefault('name')) 
None 

>>> d 
{'name': None}
```

10. update

方法update使用一个字典中的项来更新另一个字典

```
>>> d = { 
... 'title': 'Python Web Site', 
... 'url': 'http://www.python.org', 
... 'changed': 'Mar 14 22:09:15 MET 2016' 
... }

>>> x = {'title': 'Python Language Website'} 
>>> d.update(x) 
>>> d 
{'url': 'http://www.python.org', 'changed': 
'Mar 14 22:09:15 MET 2016', 'title': 'Python Language Website'}
```

对于通过参数提供的字典，将其项添加到当前字典中。如果当前字典包含键相同的项，就替换它

11. values

方法values返回一个由字典中的值组成的字典视图。不同于方法keys，方法values返回的视图可能包含重复的值

```
>>> d = {} 
>>> d[1] = 1 
>>> d[2] = 2 
>>> d[3] = 3 
>>> d[4] = 1 
>>> d.values() 
dict_values([1, 2, 3, 1])
```

### 4.3 小结

- 映射：映射让你能够使用任何不可变的对象（最常用的是字符串和元组）来标识其元素。Python只有一种内置的映射类型，那就是字典。
- 将字符串格式设置功能用于字典：要对字典执行字符串格式设置操作，不能使用format和命名参数，而必须使用format_map。
- 字典方法：字典有很多方法，这些方法的调用方式与列表和字符串的方法相同

#### 4.3.1 本章介绍的新函数

| 函 数      | 描 述                        |
|:----------|:-----------------------------|
| dict(seq) | 从键值对、映射或关键字参数创建字典 |

## 第5章 条件、循环及其他语句

### 5.1 再谈 print 和 import

#### 5.1.1 打印多个参数

```
>>> print('Age:', 42) 
Age: 42

>>> name = 'Gumby' 
>>> salutation = 'Mr.' 
>>> greeting = 'Hello,' 
>>> print(greeting, salutation, name) 
Hello, Mr. Gumby
```

可自定义分隔符

```
>>> print("I", "wish", "to", "register", "a", "complaint", sep="_") 
I_wish_to_register_a_complaint
```

可自定义结束字符串，以替换默认的换行符

```
print('Hello,', end='') 
print('world!')

Hello, world!
```

#### 5.1.2 导入时重命名

```
>>> import math as foobar 
>>> foobar.sqrt(4) 
2.0

>>> from math import sqrt as foobar 
>>> foobar(4) 
2.0

from module1 import open as open1 
from module2 import open as open2
```

### 5.2 赋值魔法

#### 5.2.1 序列解包

可同时（并行）给多个变量赋值

```
>>> x, y, z = 1, 2, 3 
>>> print(x, y, z) 
1 2 3
```

交换多个变量的值

```
>>> x, y = y, x 
>>> print(x, y, z) 
2 1 3
```

这里执行的操作称为序列解包（或可迭代对象解包）：将一个序列（或任何可迭代对象）解包，并将得到的值存储到一系列变量中

```
>>> values = 1, 2, 3 
>>> values 
(1, 2, 3) 
>>> x, y, z = values 
>>> x 
1
```

假设要从字典中随便获取（或删除）一个键值对，可使用方法popitem，它随便获取一个键值对并以元组的方式返回。接下来，可直接将返回的元组解包到两个变量中

```
>>> scoundrel = {'name': 'Robin', 'girlfriend': 'Marion'} 
>>> key, value = scoundrel.popitem() 
>>> key 
'girlfriend' 
>>> value 
'Marion'
```

解包的序列包含的元素个数必须与你在等号左边列出的目标个数相同，否则Python将引发异常

```
>>> x, y, z = 1, 2 
Traceback (most recent call last): 
 File "<stdin>", line 1, in <module> 
ValueError: need more than 2 values to unpack 

>>> x, y, z = 1, 2, 3, 4 
Traceback (most recent call last): 
 File "<stdin>", line 1, in <module> 
ValueError: too many values to unpack
```

可使用星号运算符（*）来收集多余的值，这样无需确保值和变量的个数相同

```
>>> a, b, *rest = [1, 2, 3, 4] 
>>> rest 
[3, 4]

>>> name = "Albus Percival Wulfric Brian Dumbledore" 
>>> first, *middle, last = name.split() 
>>> middle 
['Percival', 'Wulfric', 'Brian']

>>> a, *b, c = "abc" 
>>> a, b, c 
('a', ['b'], 'c')
```

#### 5.2.2 链式赋值

链式赋值是一种快捷方式，用于将多个变量关联到同一个值

```python
x = y = somefunction()

#等价
y = somefunction() 
x = y

#不等价
x = somefunction() 
y = somefunction()
```

#### 5.2.3 增强赋值

```
>>> x = 2 
>>> x += 1 
>>> x *= 2 
>>> x 
6

>>> fnord = 'foo' 
>>> fnord += 'bar' 
>>> fnord *= 2 
>>> fnord 
'foobarfoobar'
```

### 5.3 代码块：缩进的乐趣

```
this is a line 
this is another line: 
 this is another block 
 continuing the same block 
 the last line of this block 
phew, there we escaped the inner block
```

### 5.4 条件和条件语句

#### 5.4.1 这正是布尔值的用武之地

下面的值都将被解释器视为假：  
False None 0 "" () \[\] {}

```
>>> True 
True 
>>> False 
False 
>>> True == 1 
True 
>>> False == 0 
True 
>>> True + False + 42 
43

>>> bool('I think, therefore I am') 
True 
>>> bool(42) 
True 
>>> bool('') 
False 
>>> bool(0) 
False
```

> 虽然[]和""都为假（即bool([]) == bool("") == False），但它们并不相等（即[] !=
> ""）。对其他各种为假的对象来说，情况亦如此（一个更显而易见的例子是() != False）

#### 5.4.2 有条件地执行和 if 语句

```python
name = input('What is your name? ') 
if name.endswith('Gumby'): 
 print('Hello, Mr. Gumby')
```

#### 5.4.3 else 子句

```python
name = input('What is your name?') 
if name.endswith('Gumby'): 
 print('Hello, Mr. Gumby') 
else: 
 print('Hello, stranger')
 
status = "friend" if name.endswith("Gumby") else "stranger"
```

#### 5.4.4 elif 子句

```python
num = int(input('Enter a number: ')) 
if num > 0: 
 print('The number is positive') 
elif num < 0: 
 print('The number is negative') 
else: 
 print('The number is zero')
```

#### 5.4.5 代码块嵌套

```python
name = input('What is your name? ') 
if name.endswith('Gumby'): 
 if name.startswith('Mr.'): 
 print('Hello, Mr. Gumby') 
 elif name.startswith('Mrs.'): 
 print('Hello, Mrs. Gumby') 
 else: 
 print('Hello, Gumby') 
else: 
 print('Hello, stranger')
```

#### 5.4.6 更复杂的条件

1. 比较运算符

在条件表达式中，最基本的运算符可能是比较运算符，它们用于执行比较

| 表 达 式    | 描 述                  |
|:-----------|:----------------------|
| x == y     | x 等于y                |
| x < y      | x小于y                 |
| x > y      | x大于y                 |
| x >= y     | x大于或等于y            |
| x <= y     | x小于或等于y            |
| x != y     | x不等于y                |
| x is y     | x和y是同一个对象         |
| x is not y | x和y是不同的对象         |
| x in y     | x是容器（如序列）y的成员   |
| x not in y | x不是容器（如序列）y的成员 |

2. 布尔运算符

```python
number = int(input('Enter a number between 1 and 10: ')) 
if number <= 10 and number >= 1: 
 print('Great!') 
else: 
 print('Wrong!')
```

#### 5.4.7 断言

```python
if not condition: 
 crash program
```

程序在错误条件出现时立即崩溃

```
>>> age = 10 
>>> assert 0 < age < 100 
>>> age = -1 
>>> assert 0 < age < 100 
Traceback (most recent call last): 
 File "<stdin>", line 1, in ? 
AssertionError
```

```
>>> age = -1 
>>> assert 0 < age < 100, 'The age must be realistic' 
Traceback (most recent call last): 
 File "<stdin>", line 1, in ? 
AssertionError: The age must be realistic
```

### 5.5 循环

#### 5.5.1 while 循环

```python
x = 1 
while x <= 100: 
 print(x) 
 x += 1
 
name = '' 
while not name: 
 name = input('Please enter your name: ') 
print('Hello, {}!'.format(name))
```

#### 5.5.2 for 循环

```python
words = ['this', 'is', 'an', 'ex', 'parrot'] 
for word in words: 
 print(word)
 
numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] 
for number in numbers: 
 print(number)
```

#### 5.5.3 迭代字典

```python
d = {'x': 1, 'y': 2, 'z': 3} 
for key in d: 
 print(key, 'corresponds to', d[key])
 
for key, value in d.items(): 
 print(key, 'corresponds to', value)
```

#### 5.5.4 一些迭代工具

1. 并行迭代

```python
names = ['anne', 'beth', 'george', 'damon'] 
ages = [12, 45, 32, 102]
for i in range(len(names)): 
 print(names[i], 'is', ages[i], 'years old')
```

置函数zip将两个序列“缝合”起来，并返回一个由元组组成的序列

```
>>> list(zip(names, ages)) 
[('anne', 12), ('beth', 45), ('george', 32), ('damon', 102)]
```

“缝合”后，可在循环中将元组解包

```python
for name, age in zip(names, ages): 
 print(name, 'is', age, 'years old')
```

当序列的长度不同时，函数zip将在最短的序列用完后停止“缝合”

```
>>> list(zip(range(5), range(100000000))) 
[(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]
```

2. 迭代时获取索引

```python
for string in strings: 
 if 'xxx' in string: 
 index = strings.index(string) # 在字符串列表中查找字符串
 strings[index] = '[censored]'

index = 0 
for string in strings: 
 if 'xxx' in string: 
 strings[index] = '[censored]' 
 index += 1
 
for index, string in enumerate(strings): 
 if 'xxx' in string: 
 strings[index] = '[censored]'
```

3. 反向迭代和排序后再迭代

```
>>> sorted([4, 3, 6, 8, 3]) 
[3, 3, 4, 6, 8] 
>>> sorted('Hello, world!') 
[' ', '!', ',', 'H', 'd', 'e', 'l', 'l', 'l', 'o', 'o', 'r', 'w'] 
>>> list(reversed('Hello, world!')) 
['!', 'd', 'l', 'r', 'o', 'w', ' ', ',', 'o', 'l', 'l', 'e', 'H'] 
>>> ''.join(reversed('Hello, world!')) 
'!dlrow ,olleH'
```

#### 5.5.5 跳出循环

1. break

```python
from math import sqrt 
for n in range(99, 0, -1): 
 root = sqrt(n) 
 if root == int(root): 
 print(n) 
 break
```

2. continue

```python
for x in seq: 
 if condition1: continue 
 if condition2: continue 
 if condition3: continue 
 do_something() 
 do_something_else() 
 do_another_thing() 
 etc()
```

3. while True/break成例

```python
word = 'dummy' 
while word: 
 word = input('Please enter a word: ') 
 # 使用这个单词做些事情：
 print('The word was', word)
 
while True: 
 word = input('Please enter a word: ') 
 if not word: break 
 # 使用这个单词做些事情：
 print('The word was ', word)
```

#### 5.5.6 循环中的 else 子句

```python
broke_out = False 
for x in seq: 
 do_something(x) 
 if condition(x): 
 broke_out = True 
 break 
 do_something_else(x) 
if not broke_out: 
 print("I didn't break out!")
 
from math import sqrt 
for n in range(99, 81, -1): 
 root = sqrt(n) 
 if root == int(root): 
 print(n) 
 break 
else: 
 print("Didn't find it!")
```

### 5.6 简单推导

```
>>> [x * x for x in range(10)] 
[0, 1, 4, 9, 16, 25, 36, 49, 64, 81]

>>> [x*x for x in range(10) if x 3 == 0] %
[0, 9, 36, 81]

>>> [(x, y) for x in range(3) for y in range(3)] 
[(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
```

```python
result = [] 
for x in range(3): 
 for y in range(3) 
 result.append((x, y))
```

```
>>> girls = ['alice', 'bernice', 'clarice'] 
>>> boys = ['chris', 'arnold', 'bob'] 
>>> [b+'+'+g for b in boys for g in girls if b[0] == g[0]] 
['chris+clarice', 'arnold+alice', 'bob+bernice']
```

### 5.7 三人行

#### 5.7.1 什么都不做

```python
if name == 'Ralph Auldus Melish': 
 print('Welcome!') 
elif name == 'Enid': 
 # 还未完成……
 pass 
elif name == 'Bill Gates': 
 print('Access Denied')
```

#### 5.7.2 使用 del 删除

```
>>> x = 1 
>>> del x 
>>> x 
Traceback (most recent call last): 
 File "<pyshell#255>", line 1, in ? 
 x 
NameError: name 'x' is not defined
```

#### 5.7.3 使用 exec 和 eval 执行字符串及计算其结果

1. exec

函数exec将字符串作为代码执行

```
>>> exec("print('Hello, world!')") 
Hello, world!

>>> from math import sqrt 
>>> exec("sqrt = 1") 
>>> sqrt(4) 
Traceback (most recent call last): 
 File "<pyshell#18>", line 1, in ? 
 sqrt(4) 
TypeError: object is not callable: 1
```

添加第二个参数——字典，用作代码字符串的命名空间

```
>>> from math import sqrt 
>>> scope = {} 
>>> exec('sqrt = 1', scope) 
>>> sqrt(4) 
2.0 
>>> scope['sqrt'] 
1

>>> len(scope) 
2 
>>> scope.keys() 
['sqrt', '__builtins__']
```

2. eval

eval计算用字符串表示的Python表达式的值，并返回结果

```
>>> eval(input("Enter an arithmetic expression: ")) 
Enter an arithmetic expression: 6 + 18 * 2 
42
```

### 5.8 小结

- **打印语句**：你可使用print语句来打印多个用逗号分隔的值。如果print语句以逗号结尾，后续print语句将在当前行接着打印。
- **导入语句**：有时候，你不喜欢要导入的函数的名称——可能是因为你已将这个名称用作他用。在这种情况下，可使用import
  ... as ...语句在本地重命名函数。
- **赋值语句**：通过使用奇妙的序列解包和链式赋值，可同时给多个变量赋值；而通过使用增强赋值，可就地修改变量。
- **代码块**：代码块用于通过缩进将语句编组。代码块可用于条件语句和循环中，还可用于函数和类定义中（这将在本书后面介绍）。
- **条件语句**：条件语句根据条件（布尔表达式）决定是否执行后续代码块。通过使用if/elif/else，可将多个条件语句组合起来。条件语句的一个变种是条件表达式，如a
  if b else c。
- **断言**：断言断定某件事（一个布尔表达式）为真，可包含说明为何必须如此的字符串。如果指定的表达式为假，断言将导致程序停止执行（或引发第8章将介绍的异常）。最好尽早将错误揪出来，免得它潜藏在程序中，直到带来麻烦。
- **循环**：你可针对序列中的每个元素（如特定范围内的每个数）执行代码块，也可在条件为真时反复执行代码块。要跳过代码块中余下的代码，直接进入下一次迭代，可使用continue语句；要跳出循环，可使用break语句。另外，你还可在循环末尾添加一个else子句，它将在没有执行循环中的任何break语句时执行。
- **推导**：推导并不是语句，而是表达式。它们看起来很像循环，因此我将它们放在循环中讨论。通过列表推导，可从既有列表创建出新列表，这是通过对列表元素调用函数、剔除不想要的函数等实现的。推导功能强大，但在很多情况下，使用普通循环和条件语句也可完成任务，且代码的可读性可能更高。使用类似于列表推导的表达式可创建出字典。
- **pass、del、exec和eval**：pass语句什么都不做，但适合用作占位符。del语句用于删除变量或数据结构的成员，但不能用于删除值。函数exec用于将字符串作为Python程序执行。函数eval计算用字符串表示的表达式并返回结果

#### 5.8.1 本章介绍的新函数

| 函 数                                    | 描 述                                                             |
|:----------------------------------------|:------------------------------------------------------------------|
| chr(n)                                  | 返回一个字符串，其中只包含一个字符，这个字符对应于传入的顺序值n（0 ≤n < 256） |
| eval(source\[,globals\[,locals\]\])     | 计算并返回字符串表示的表达式的结果                                      |
| exec(source\[, globals\[, locals\]\])   | 将字符串作为语句执行                                                 |
| enumerate(seq)                          | 生成可迭代的索引值对                                               |
| ord(c)                                  | 接受一个只包含一个字符的字符串，并返回这个字符的顺序值（一个整数）            |
| range(\[start,\] stop\[, step\])        | 创建一个由整数组成的列表                                              |
| reversed(seq)                           | 按相反的顺序返回seq中的值，以便用于迭代                                 |
| sorted(seq\[,cmp\]\[,key\]\[,reverse\]) | 返回一个列表，其中包含seq中的所有值且这些值是经过排序的                    |
| xrange(\[start,\] stop\[, step\])       | 创建一个用于迭代的xrange对象                                          |
| zip(seq1, seq2,...)                     | 创建一个适合用于并行迭代的新序列                                        |

## 第6章 抽象

### 6.1 懒惰是一种美德

斐波那契数

```python
fibs = [0, 1] 
for i in range(8): 
 fibs.append(fibs[-2] + fibs[-1])
```

### 6.2 抽象和结构

如下载网页、计算使用频率、打印每个单词的使用频率

```python
page = download_page() 
freqs = compute_frequencies(page) 
for word, freq in freqs: 
 print(word, freq)
```

### 6.3 自定义函数

使用def（表示定义函数）语句

```python
def hello(name): 
 return 'Hello, ' + name + '!'
 
def fibs(num): 
 result = [0, 1] 
 for i in range(num-2): 
 result.append(result[-2] + result[-1]) 
 return result
```

#### 6.3.1 给函数编写文档

文档字符串

```python
def square(x): 
 'Calculates the square of the number x.' 
 return x * x

>>> square.__doc__ 
'Calculates the square of the number x.'
```

> __doc__是函数的一个属性

#### 6.3.2 其实并不是函数的函数

```python
def test(): 
 print('This is printed') 
 return 
 print('This is not')
 
>>> print(x) 
None
```

所有的函数都返回值。如果你没有告诉它们该返回什么，将返回None

### 6.4 参数魔法

#### 6.4.1 值从哪里来

> 在def语句中，位于函数名后面的变量通常称为形参，而调用函数时提供的值称为实参，但本书基本不对此做严格的区分。在很重要的情况下，我会将实参称为值，以便将其与类似于变量的形参区分开来

#### 6.4.2 我能修改参数吗

在函数内部重新关联参数（即给它赋值）时，函数外部的变量不受影响

> 参数存储在局部作用域内

```
>>> names = ['Mrs. Entity', 'Mrs. Thing'] 
>>> n = names[:]
```

现在n和names包含两个**相等**但**不同**的列表。

```
>>> n is names 
False 
>>> n == names 
True
```

1. 为何要修改参数

编写一个程序，让它存储姓名，并让用户能够根据名字、中间名或姓找人

```python
storage = {} 
storage['first'] = {} 
storage['middle'] = {} 
storage['last'] = {}
```

要将作者加入这个数据结构中，可以像下面这样做

```python
me = 'Magnus Lie Hetland' 
storage['first']['Magnus'] = [me] 
storage['middle']['Lie'] = [me] 
storage['last']['Hetland'] = [me]
```

要获取中间名为Lie的人员名单

```
>>> storage['middle']['Lie'] 
['Magnus Lie Hetland']
```

添加我的妹妹

```python
>>> my_sister = 'Anne Lie Hetland' 
>>> storage['first'].setdefault('Anne', []).append(my_sister) 
>>> storage['middle'].setdefault('Lie', []).append(my_sister) 
>>> storage['last'].setdefault('Hetland', []).append(my_sister) 
>>> storage['first']['Anne'] 
['Anne Lie Hetland'] 
>>> storage['middle']['Lie'] 
['Magnus Lie Hetland', 'Anne Lie Hetland']
```

来创建一个初始化数据结构的函数

```python
def init(data): 
 data['first'] = {} 
 data['middle'] = {} 
 data['last'] = {}
 
>>> storage = {} 
>>> init(storage) 
>>> storage 
{'middle': {}, 'last': {}, 'first': {}}
```

> 在字典中，键的排列顺序是不固定的，因此打印字典时，每次的顺序都可能不同。如果你在解释器中打印出来的顺序不同，请不用担心

编写获取人员姓名的函数，再接着编写存储人员姓名的函数

```python
def lookup(data, label, name): 
 return data[label].get(name)
 
>>> lookup(storage, 'middle', 'Lie') 
['Magnus Lie Hetland']
```

编写将人员存储到数据结构中的函数

```python
def store(data, full_name): 
    names = full_name.split() 
    if len(names) == 2: names.insert(1, '') 
    labels = 'first', 'middle', 'last' 
    for label, name in zip(labels, names): 
        people = lookup(data, label, name) 
        if people: 
            people.append(full_name) 
        else: 
            data[label][name] = [full_name]
            
>>> MyNames = {} 
>>> init(MyNames) 
>>> store(MyNames, 'Magnus Lie Hetland') 
>>> lookup(MyNames, 'middle', 'Lie') 
['Magnus Lie Hetland']
>>> store(MyNames, 'Robin Hood') 
>>> store(MyNames, 'Robin Locksley') 
>>> lookup(MyNames, 'first', 'Robin') 
['Robin Hood', 'Robin Locksley'] 
>>> store(MyNames, 'Mr. Gumby') 
>>> lookup(MyNames, 'middle', '') 
['Robin Hood', 'Robin Locksley', 'Mr. Gumby']
```

```
1. 将参数data和full_name提供给这个函数。这些参数被设置为从外部获得的值。
2. 通过拆分full_name创建一个名为names的列表。
3. 如果names的长度为2（只有名字和姓），就将中间名设置为空字符串。
4. 将'first'、'middle'和'last'存储在元组labels中（也可使用列表，这里使用元组只是为了省略方括号）。
5. 使用函数zip将标签和对应的名字合并，以便对每个标签名字对执行如下操作：
   - 获取属于该标签和名字的列表；
   - 将full_name附加到该列表末尾或插入一个新列表
```

2. 如果参数是不可变的

在有些语言（如C++、Pascal和Ada）中，经常需要给参数赋值并让这种修改影响函数外部的变量。在Python中，没法直接这样做，只能修改参数对象本身

```
>>> def inc(x): return x + 1 
... 
>>> foo = 10 
>>> foo = inc(foo) 
>>> foo 
11

>>> def inc(x): x[0] = x[0] + 1 
... 
>>> foo = [10] 
>>> inc(foo) 
>>> foo 
[11]
```

#### 6.4.3 关键字参数和默认值

有时候，参数的排列顺序可能难以记住，尤其是参数很多时。为了简化调用工作，可指定参数的名称

```python
def hello_1(greeting, name): 
 print('{}, {}!'.format(greeting, name))

>>> hello_1(greeting='Hello', name='world') 
Hello, world!
```

像这样使用名称指定的参数称为**关键字参数**，主要优点是有助于澄清各个参数的作用

```
>>> store('Mr. Brainsample', 10, 20, 13, 5)

>>> store(patient='Mr. Brainsample', hour=10, minute=20, day=13, month=5)
```

关键字参数最大的优点在于，可以指定默认值

```python
def hello_3(greeting='Hello', name='world'): 
 print('{}, {}!'.format(greeting, name))
```

像这样给参数指定默认值后，调用函数时可不提供它！可以根据需要，一个参数值也不提供、提供部分参数值或提供全部参数值

```
>>> hello_3() 
Hello, world! 

>>> hello_3('Greetings') 
Greetings, world! 

>>> hello_3('Greetings', 'universe') 
Greetings, universe!

>>> hello_3(name='Gumby') 
Hello, Gumby!
```

#### 6.4.4 收集参数

```python
def print_params(*params): 
 print(params)
```

参数前面的星号将提供的所有值都放在一个元组中  
如果没有可供收集的参数，params将是一个空元组

```
>>> print_params('Testing') 
('Testing',)

>>> print_params(1, 2, 3) 
(1, 2, 3)
```

带星号的参数也可放在其他位置（而不是最后），但不同的是，在这种情况下你需要做些额外的工作：使用名称来指定后续参数

```
>>> def in_the_middle(x, *y, z): 
... print(x, y, z) 
... 

>>> in_the_middle(1, 2, 3, 4, 5, z=7) 
1 (2, 3, 4, 5) 7 

>>> in_the_middle(1, 2, 3, 4, 5, 7) 
Traceback (most recent call last): 
 File "<stdin>", line 1, in <module> 
TypeError: in_the_middle() missing 1 required keyword-only argument: 'z'
```

要收集关键字参数，可使用两个星号。

```
>>> def print_params_3(**params): 
... print(params) 
... 

>>> print_params_3(x=1, y=2, z=3) 
{'z': 3, 'x': 1, 'y': 2}
```

```python
def print_params_4(x, y, z=3, *pospar, **keypar): 
 print(x, y, z) 
 print(pospar) 
 print(keypar)

>>> print_params_4(1, 2, 3, 5, 6, 7, foo=1, bar=2) 
1 2 3 
(5, 6, 7) 
{'foo': 1, 'bar': 2} 
>>> print_params_4(1, 2) 
1 2 3 
() 
{}
```

在姓名存储示例中使用这种技术

```python
def store(data, *full_names): 
    for full_name in full_names: 
        names = full_name.split() 
        if len(names) == 2: names.insert(1, '') 
        labels = 'first', 'middle', 'last' 
        for label, name in zip(labels, names): 
            people = lookup(data, label, name) 
            if people: 
                people.append(full_name) 
            else: 
                data[label][name] = [full_name]
```

#### 6.4.5 分配参数

不是收集参数，而是分配参数。这是通过在调用函数（而不是定义函数）时使用运算符*实现的

```python
def add(x, y): 
    return x + y
params = (1, 2)

>>> add(*params) 
3
```

```
>>> params = {'name': 'Sir Robin', 'greeting': 'Well met'} 
>>> hello_3(**params) 
Well met, Sir Robin!
```

如果在定义和调用函数时都使用*或**，将只传递元组或字典。因此还不如不使用它们，还可省却些麻烦。

```
>>> def with_stars(**kwds): 
... print(kwds['name'], 'is', kwds['age'], 'years old') 
... 
>>> def without_stars(kwds): 
... print(kwds['name'], 'is', kwds['age'], 'years old') 
... 
>>> args = {'name': 'Mr. Gumby', 'age': 42} 
>>> with_stars(**args) 
Mr. Gumby is 42 years old 
>>> without_stars(args) 
Mr. Gumby is 42 years old
```

#### 6.4.6 练习使用参数

```python
def story(**kwds): 
    return 'Once upon a time, there was a ' \ 
        '{job} called {name}.'.format_map(kwds) 
def power(x, y, *others): 
    if others: 
        print('Received redundant parameters:', others) 
    return pow(x, y) 
def interval(start, stop=None, step=1): 
    'Imitates range() for step > 0' 
    if stop is None: # 如果没有给参数stop指定值，
        start, stop = 0, start # 就调整参数start和stop的值
    result = [] 
    i = start # 从start开始往上数
    while i < stop: # 数到stop位置
        result.append(i) # 将当前数的数附加到result末尾
        i += step # 增加到当前数和step（> 0）之和
    return result
```

下面来尝试调用这些函数

```
>>> print(story(job='king', name='Gumby')) 
Once upon a time, there was a king called Gumby. 
>>> print(story(name='Sir Robin', job='brave knight')) 
Once upon a time, there was a brave knight called Sir Robin. 
>>> params = {'job': 'language', 'name': 'Python'} 
>>> print(story(**params)) 
Once upon a time, there was a language called Python. 
>>> del params['job'] 
>>> print(story(job='stroke of genius', **params)) 
Once upon a time, there was a stroke of genius called Python. 
>>> power(2, 3) 
8 
>>> power(3, 2) 
9 
>>> power(y=3, x=2) 
8 
>>> params = (5,) * 2 
>>> power(*params) 
3125 
>>> power(3, 3, 'Hello, world') 
Received redundant parameters: ('Hello, world',) 
27 
>>> interval(10) 
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9] 
>>> interval(1, 5) 
[1, 2, 3, 4] 
>>> interval(3, 12, 4) 
[3, 7, 11] 
>>> power(*interval(3, 7)) 
Received redundant parameters: (5, 6) 
81
```

### 6.5 作用域

将变量视为指向值的名称  
几乎与使用字典时一样（字典中的键指向值），只是你使用的是“看不见”的字典  
这种“看不见的字典”称为命名空间或作用域

```
>>> def foo(): x = 42 
... 
>>> x = 1 
>>> foo() 
>>> x 
1
```

在这里，函数foo修改（重新关联）了变量x，但当你最终查看时，它根本没变

### 6.6 递归

这个函数中的递归称为无穷递归（就像以while
True打头且不包含break和return语句的循环被称为无限循环一样），因为它从理论上说永远不会结束
- **基线条件**（针对最小的问题）：满足这种条件时函数将直接返回一个值。
- **递归条件**：包含一个或多个调用，这些调用旨在解决问题的一部分

#### 6.6.1 两个经典案例：阶乘和幂

```python
def factorial(n): 
 if n == 1: 
 return 1 
 else: 
 return n * factorial(n - 1)
```

#### 6.6.2 另一个经典案例：二分查找

```python
def search(sequence, number, lower, upper): 
    if lower == upper: 
        assert number == sequence[upper] 
        return upper 
    else: 
        middle = (lower + upper) // 2 
        if number > sequence[middle]: 
            return search(sequence, number, middle + 1, upper) 
        else: 
            return search(sequence, number, lower, middle)
```

### 6.7 小结

- **抽象**：抽象是隐藏不必要细节的艺术。通过定义处理细节的函数，可让程序更抽象。
- **函数定义**：函数是使用def语句定义的。函数由语句块组成，它们从外部接受值（参数），并可能返回一个或多个值（计算结果）。
- **参数**：函数通过参数（调用函数时被设置的变量）接收所需的信息。在Python中，参数有两类：位置参数和关键字参数。通过给参数指定默认值，可使其变成可选的。
- **作用域**：变量存储在作用域（也叫命名空间）中。在Python中，作用域分两大类：全局作用域和局部作用域。作用域可以嵌套。
- **递归**：函数可调用自身，这称为递归。可使用递归完成的任何任务都可使用循环来完成，但有时使用递归函数的可读性更高。
- **函数式编程**：Python提供了一些函数式编程工具，其中包括lambda表达式以及函数map、filter和reduce。

### 6.7.1 本章介绍的新函数

| 函 数                              | 描 述                                                     |
|:----------------------------------|:----------------------------------------------------------|
| map(func, seq\[, seq, ...\])      | 对序列中的所有元素执行函数                                     |
| filter(func, seq)                 | 返回一个列表，其中包含对其执行函数时结果为真的所有元素              |
| reduce(func, seq\[, initial\])    | 等价于 func(func(func(seq\[0\], seq\[1\]), seq\[2\]), ...) |
| sum(seq)                          | 返回 seq 中所有元素的和                                      |
| apply(func\[, args\[, kwargs\]\]) | 调用函数（还提供要传递给函数的参数）                             |

## 第7章 再谈抽象

