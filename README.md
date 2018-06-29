# scemplate - A scala templating library and expression evaluator

Scemplate is a templating library designed to work well from Scala and to be easy to use. It has features along the lines
of other templating libraries such as [Jinja](http://jinja.pocoo.org/), [Freemarker](https://freemarker.apache.org/),
[mustache](https://mustache.github.io/) and others but aims to be fairly minimal. 

* SBT: `"com.github.ajrnz" %% "scemplate" % "0.5.0"`
* Mill: `ivy"com.github.ajrnz::scemplate:0.5.0"`

## Features
* A general purpose templating language. Not optimized for any special use case (eg HTML). 
  There are plenty of optimized libraries out there eg [ScalaTags](http://www.lihaoyi.com/scalatags)

* Templates are evaluated at runtime and can be modified without using the the compiler. 
This way you can easily modify external templates without having to changing your code. Content resides in the 
template where it belongs

* Missing substitutions are error - no one likes sending/receiving emails of the form "Dear %Firstname%,"

* Lean - It's about 160k plus [fastparse](http://www.lihaoyi.com/fastparse) (490k)

* Easy to use

## Templates
There is only one magic character in the templates. That is `$`. This means to convert a document into a template you 
only have to replace all $ with the escaped version `$$`. After that, basic substitutions are similar to standard scala 
string interpolations. ie. `My name is $name` or `My name is ${name}`. Let's look at some examples. 


## A complete example
Let's say we are doing a simple mail merge. We might define a template:

    ${person.address.street}
    ${person.address.town}
    ${person.address.postCode}
    
    Subject: $subject
    
    Dear ${firstName(person.name)},
    
    As a person ${if age > cutoffAge}over${else}under${endif} $cutoffAge

The full code to define the data and template and to render it is below:
```scala
    import ajr.scemplate._
    import ajr.scemplate.implicits._

    case class Address(street: String, town: String, postCode: String)
    case class Person(name: String, age: Int, height: Double, email: String, address: Address)
  
    object Address {
      implicit def toTV(value: Address): TemplateValue = CaseClassEncoder.gen[Address].encode(value)
    }
    object Person {
      implicit def toTV(value: Person): TemplateValue = CaseClassEncoder.gen[Person].encode(value)
    }
  
    val address = Address("1 The Mall", "London", "SW1A 1AA")
    val person = Person("John Doe", 21, 1.76, "john@doe.com", address)
  
    val context = Context()
      .withValues(
        "subject" -> "On offer this week...",
        "person" -> person,
        "cutoffAge" -> 30
      )
      .withFunctions(
        "firstName" -> function(_.asString.split(" ").head)
      )
  
    val templateText =
      """${person.address.street}
        |${person.address.town}
        |${person.address.postCode}
        |
        |Subject: $subject
        |
        |Dear ${firstName(person.name)},
        |
        |As a person ${if person.age>cutoffAge}over${else}under${endif} ${cutoffAge}....
        |""".stripMargin

    val template = new Template(templateText)
    val result = template.render(context)
```

The `result` would be the following

    1 The Mall
    London
    SW1A 1AA
    
    Subject: On offer this week...
    
    Dear John,
    
    As a person under 30....

Essentially you provide a template and a context (which a map) and then ask for the template to be rendered.
Note that the two implicit `def` declarations in the `Address` and `Person` objects take care of serializing the case
class into the context. The serializers are built at compile time and do not use reflection.


## A more complicated example

This example shows other language constructs which are available.

```scala
${macro renderTran(tran,indent)}
${rep(" ", indent)}${tran.date} ${leftAlign(tran.description,8)} ${rightAlign(formatCurrency(tran.value), 10)}
${endmacro}
$title as at: $date  /  Branch: $branchId
Accounts
${for account in accounts}

Name:    ${account.person.name} (${if account.active}Active${else}Inactive${endif})
Balance: ${formatCurrency(account.balance)}${if account.balance < 0.0} Overdrawn${endif}

${if length(account.transactions) > 0}
Transactions
${for tran in account.transactions}
${renderTran(tran,2)}
${endfor}
${endif}
${endfor}
```

Note that the functions being called here (`leftAlign`, `rightAlign`, `formatCurrency`, etc) are user defined can be 
easily defined in scala.

The full source code for these examples can be found in the test's source code.

## Language features
The `$` character is the single character which activates the template language. Except for simple replacements, 
the dollar should be followed by braces ie `${...}`

### Types
All variables in templates are typed. They all have their own wrapper class (like boxing) which derives from a
`TemplateValue` class.
The following types exist:
* String - `StringValue`
* Integer - `IntValue`
* Double - `DoubleValue`
* Boolean - `BooleanValue`
* Array - `ArrayValue` 
* Map - `MapValue`

Types can be converted to each other via the following methods on `TemplateValue`
* `.asString`
* `.asInt`
* `.asDouble`
* `.asBoolean`

These methods are only valid for conversions which make sense. Inappropriate calls will yield an exception.
These methods are also useful when defining functions in scala. Arrays and Maps can be dereferenced using the standard 
apply `(index)` notation. 

### Expressions
Within the template language you can write arbitrary expressions using the following operators

`+`, `-`, `*`, `/`, `%`, `&&`, `||`, `==`, `!=`, `>`, `>=`, `<`, `<=`, `(`, `)` 


### Functions
Functions can be called as in scala using the `name(arg, ...)` notation. The are added to a context using the
`withFunctions()` method. Make sure you have imported `ajr.scemplate.implicits._` beforehand, this will take care of
converting regular scala classes to those used by the template renderer. There are a set of helper methods called 
`function` which also aid in this process. For example to define a function which repeats a string a number of times 
you could write the following:

    val context = Context()
      .withFunctions("repeat" -> function((str, reps) = str.asString * reps.asInt)
    
The function's arguments are of type `TemplateValue` and need to first be converted to their primitive equivalents 
before being used. The return type can be a standard scala type as the implicits will lift it back into a 
`TemplateValue` type automatically.
      
#### The `defined()` function
There is one special function `defined(<var>)` which can be used to determine whether or not a variable has been declared at.
This can be useful if you want to do something based on whether a variable has been defined or not. For example to 
implement a default value where always having to set the variable would be unidomatic eg. activating a debug flag. 


### Macros

    ${macro name(arg1, arg2, ...)}
    Text where arg1, arg2, etc take on passed values
    ${endmacro}

Macros are like functions that are defined in the template. They are called in the same manner as functions.
While they have less expressive power than a scala function they often make more sense when they contain a lot
of text which really belongs in the template. Macros are only available within the scope in which they are defined.

### Conditionals

    ${if condition}
    then text...
    ${else}
    else text
    ${endif}

The `${else}` is optional. The condition can be any conditional expression.

### For loops

    ${for var in list}
    <repeated text where $var is defined>
    ${endif}

`list` should be an ArrayValue (converted from a scala `Seq`). 
 
 ### Case classes
 Case classes can be put into the context, like any other variable, if a case class encoder has been defined. A case class encoder is just a function which converts the case to a `TemplateValue`. 

 ```scala
     implicit def toTemplateValue(conv: T => TemplateValue) = ...
 ```
 
 These functions are usually defined implicitly for convenience. They can be defined automatically at runtime via a macro.
 If defined in the companion object they will be used automatically where required. Ie:
 
 ```scala
 object MyClass {
   implicit def toTV(value: MyClass): TemplateValue = CaseClassEncoder.gen[MyClass].encode(value)
 }

 ``` 

In the template case class members can be accessed via the name they were inserted into the context with and then by 
the usual dotted notation to access individual members.
 
 ### Newlines
 A single newline (if present) is swallowed at the end of: loop, if/then and macro constructs this makes the templates
 look more readable. Without this feature a template of the form:
 
     ${for var in range(0,3)}
     This is line ${var+1}
     ${endif}

would render:


    This is line 1
    
    This is line 2
    
    This is line 3

as there are technically two newlines within `for` construct.
You would then need to do the following to get the 'desired' output

     ${for var in range(0,3)}This is line ${var+1}
     ${endif}

If a newline is being removed where it is wanted just insert another one manually.

## Expression evaluation
Sometimes it is useful to be able to evaluate expressions dynamically. A helper class is provided to do this: 
`TemplateExpression`. It works just like Template except that the `${}` characters wrapping the expression are not
required. It also means that the return type does not have to be a string. Expressions are evaluated using the
`eval()` method which returns a `TemplateValue` which can be converted to a primitive type via one of the `asXXX`
methods (see above). Eg:
```scala
val tmpl = TemplateExpression("count > (n*n + n)/2")
val greater = tmpl.eval(context).asBoolean
```
 
 
## Performance
It's quite fast. Templates are compiled to an AST at runtime and can be reused. They execute very quickly. 
If you need better performance then you should probably be compiling your templates to scala with something like TWIRL. 


## The `DirectoryRenderer`
The directory renderer can be used to render whole directories of templates. For example for app configuration files.

The main static method is `renderTree` which has the following signature:

```scala
  def renderTree(context: Context,
                 reader: DirectoryReader, writer: DirectoryWriter,
                 isTemplate: RelPath => Boolean = _ => true,
                 renamePath: RelPath => RelPath = identity)
```

* `reader` - is a class which generates file and directory paths to be rendered as templates. Both the file path and
  the file content are substituted
* `writer` - is a function which takes a relative path and optional file content (Content of `None` creates a directory)
* `isTemplate` - can be used to specify only certain paths/extensions etc are templates and the others should be 
  copied verbatim
* `renamePath` - can be used to modify a path before it is written. For example to remove `.tmpl` from file names

`DirectoryRenderer` provides two `DirectoryReader` implementations:

```scala
def fileSystemReader(basePath: Path, ignorePath: RelPath => Boolean = _ => false): DirectoryReader
```     

which reads all files from a `basePath` with the option to ignore some entries.

And:

```scala
def resourceReader(basePath: String, ignorePath: RelPath => Boolean = _ => false, clazz: Class[_] = getClass): DirectoryReader
```

`resourceReader` reads entries from resources again with the option to ignore entries. A `Class` can be provided from
which the class loader containing the resources can be found. Note that in order to enumerate resources they need to
be either jar or file based.

There is a single implementation of `DirectoryWriter`:

```scala
def fileSystemWriter(basePath: Path, overwrite: Boolean = false): DirectoryWriter
```  

This writes files to the file system below `basePath`. It will throw an exception if the file already exists unless the
`overwrite` parameter is set to true. Regardless of substitutions or renames, `fileSystemWriter` will refuse to write
files outside `basePath`.

The `Path` and `RelPath` classes used here come from [ammonite-ops](http://ammonite.io/#Ammonite-Ops).

Use:
```scala
import ammonite.ops._
```
to bring the relevant classes and implicits into scope.


## Future work
* Ability to load and merge macros with template
* Make a number of standard functions available for use within the template
* Publish for scala.js and scala-native
* Suggestions welcome...

## Change log

### 0.5.0

- First release
