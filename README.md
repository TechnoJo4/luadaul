# daul

<p align="center">
  <img width="256px" src="logo/daul.png"/>
</p>

daul is a language that transpiles to Lua. It aims to simplify the use of
functional-style code by eliminating the distinction between statements and
expressions.

<table>
<tr>
  <td>Daul</td> <td>Generated/Equivalent Lua code</td>
</tr>
<tr>

<td>

Tables:
```kotlin
// Use [] instead of {}
val arr = [ 1, 2, 3 ]

// Use "key:" instead of "[key] ="
val tbl = [ "arr": arr ]
val zeroindexed = [ 0: 1, 2, 3 ];
```

</td>
<td>

```lua
local arr={1,2,3};
local tbl={["arr"]=arr};
local zeroindexed={[0]=1,2,3};
```

</td>
</tr>

<tr>
<td>

Statements and blocks:
```kotlin
// Example TODO
```

</td>
<td>

```lua
-- Example TODO
```

</td>
</tr>

<tr>
<td>

Constant variables:
```kotlin
var x = 0
x = 1
val y = 2
y = 3
```

</td>
<td>

Compiler error:
```
 4 | y = 3
     ~
Variable 'y' was declared as constant (val)
```

</td>
</tr>

<tr>
<td>

Functions:
```kotlin
val f1 = \arg1, arg2 -> {
    print(arg1)
    arg1 + arg2
}

// Block can be omitted for a single expression
val f2 = \x -> 1+x

// Arguments can be omitted when there are none
val f3 = \[ "a": "b" ]
val f4 = \{
  print("f4")
  f3()
}
```

</td>
<td>

```lua
local f1 = (function(arg1,arg2)
    print(arg1);
    return (arg1+arg2);
end);

local f2 = (function(x)
    return (1+x);
end);

local f3 = (function()
    return {["a"]="b"};
end);

local f4 = (function()
    print("f4");
    return f3();
end);
```

</td>
</tr>
</table>
