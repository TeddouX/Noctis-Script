
# Language Documentation

Noctis script is a mix of a procedural programming language and an object oriented programming language (like CPP). That means you can define functions and objects in the script, and unlike python lines of code outside of functions will not be executed. **Every line ends with a semicolon** `;`, and each scope is contained between curly braces `{}`.

## Functions

Every function definition starts with the `func` keyword. It is followed by the function's name. Then, between parenthesis are the arguments in this format: `arg_name: type`, there can be any number of arguments *(0 or more)*. After that, the return type is specified like this: `-> type`, if no return type is specified, void is implied. The body of the function is contained between curly braces.

*Example:*

```
func main(first_arg: int, second_arg: float) -> bool
{
    (...)
}

func other_func()
{

}
```

Here, `other_func`'s return type is implied as void.

Values are returned from functions using the `return` keyword, if the function isn't void, the `return` keyword has to be followed by a value.

## Variables

Variables are defined with the keyword `var`, then the variable's name, then the type like this: `: type` and you can end the declaration there with a semicolon `;` or you can give it a value with an equals `=` sign.

*Example:*

```
var my_variable: int = 10;
var without_value: int;
```

### Global Variables

Global variables are variables that are defined outside of a function or an object. They can be accessed from anywhere in the script.

*Example:*

```
var global_var: bool = false;

func main() -> void
{
    var local_var: bool = global_var;
}

func other_function() -> bool
{
    return global_var;
}
```

## Objects

Objects are like classes in C++ or in any other language. They are used to store values and have methods that modify those values. They are defined using the `obj` keyword followed by the object's name, then curly braces to start their body. 

*Example:*

```
obj Vec3 
{
    (...)
}
```

### Member Variables

Member variables are used to store values inside of objects. They can be `public` or `private`. If no access modifier is specified, `private` will be implied.

*Example:*

```
obj Vec3
{
    public var mag: float = 10.0;
    private var x: float = 0.5;
    var y: float = 10.10;
}
```

Here `x` and `y` are private, that means only methods inside the object can access them, and `mag` is public, meaning it can be accessed by anything.

Member variables can be accessed like this:

```
func main()
{
    var obj: Vec3 = new Vec3();
    var obj_mag: float = obj.mag;
}
```

### Methods

Methods are functions that are defined inside the object. They can access private variables inside that object. Like member variables, they can be `public` or `private`. If no access modifier is specified, `private` will be implied.

*Example:*

```
obj AnotherObject
{
    private var _private_member: float;

    public func some_method()
    {
        var a: float = x; // No error because we are inside the object
    }

    private func bla()
    {
        (...)
    }

    func some_other_method()
    {
        (...)
    }
}
```

Here `some_other_method` is implied as being private because no access modifier was specified.