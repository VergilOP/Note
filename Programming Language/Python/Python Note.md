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

| 函 数                      | 描 述                                                         |
|:--------------------------|:--------------------------------------------------------------|
| string.capwords(s\[, sep\]) | 使用split根据sep拆分s，将每项的首字母大写，再以空格为分隔符将它们合并起来 |
| ascii(obj)                | 创建指定对象的ASCII表示                                          |

## 第4章 当索引行不通时

