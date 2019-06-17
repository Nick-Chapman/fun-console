# fun-console

Functional language console. Making use of haskeline & Earley parsing libraries.

Haskeline gives us a nice console, with persistent history. EarleyM is my own parser-combinator library, which provides monadic combinators for left-recursive, left-unfactored and ambiguous grammars.

The core expression Ast is minimal, with normal-order evaluation semantics.

For convenience the persistent .history file is reloaded on startup, and can be edited freely by hand.


#### `thrice thrice` example

    stack run

    1> inc x = x + 1
    2> thrice f x = f (f (f x))
    3> thrice thrice inc 0


#### The syntax is minimal but convenient

- applications: `a b c`
- multi-var abstractions: `\a b c. E`
- infix op: `a + b + c`
- built in numbers: `42`
- top-level defs: `id = `\x.x`
- allowing params: `twice f x = f (f x)`

The syntax is flexible when passing lambdas as function args, making the following example perfectly legal.

    num = y \num p. p 1 \b h. dub (num h) + b 0 1

The lambda bodies extend as far to the right as possible. The reduction in required parenthesis being rather nice I think.

There are sample programs in the `fun/` subdir. To play, copy or cut and paste the file into .history before starting the console.


#### Example: `fun/unary.fun` -- non-primitive lambda-encoded numerics

This example implements lambda-encoded numbers, with addition and multiplication. Then defines the factorial function. Even the required recursion is defined non-primitively via the standard (normal order) y combinator.

To see the results, we convert to the predefined numbers.

    $ cp fun/unary.fun .history
    $ stack run


#### Example: `fun/binary.fun` -- non-primitive lambda-encoded binary numerics

This example goes a step further and defines the numerics using binary rather than unary encoding.

    $ cp fun/binary.fun .history
    $ stack run


#### Let syntax (example of an ambiguous grammar)

We have support for let syntax: `let X = EXP in EXP`, using `let` and `in` as keywords. However, `let` and `in` also remain legal as identifiers. This makes our grammar rather ambiguous. We use brackets to select the interpretation when we need to:

    > let x = 1 + 2 in x + x
    parse error: AmbiguityError (Ambiguity "<start>" 0 18) : let x = 1 + 2 in x + x

This is probably the interpretation we intended:

    > (let x = 1 + 2 in x + x)
    6

As opposed to this one, which defines `let` as a function of `x`, with a silly expression body which tries to apply the literal `2` as a function, and also references the undefined identifier `in`:

    > let x = (1 + 2 in x + x)
    > let
    <closure> (adds:0, hats:0, apps:0)
    > let 1
    eval error: cant apply a number as a function (adds:0, hats:0, apps:1)


Here is a more sensible example of using the let-syntax. No ambiguity issues here:

    > f x = let y = x + x in y + y
    > f 100
    400

Alternatively, we can rewrite the above function `f` using `let` and `in` for bound variables instead of `x` and `y`. Everything still works. Wild!

    > f let = let in = let + let in in + in
    > f 100
    400
