# HasLangInterpreter

This is a simple interpreter for a Haskell-like Language. Essentially we are doing a syntax analysis of expressions, and checking if they are valid or not. We check the validity by producing a syntax-tree. Furthermore, we most adhere to the correct precedence and associativity as laid out...



For example, an expression like:

```scala
let
        x   :: [Int]        = [3, 1, 4];
        length :: [Int] -> Int
        length [] = 0.
        length h:t = 1 + length t
        in
        length x
```

Should produce the following syntax tree:

```scala
Program(
  LetExp(
    Vector(Defn(
            IdnDef("x", ListType(IntType())),
            Vector(FunLine("", Vector(),
                           ListExp(Vector(IntExp(3),
                                     IntExp(1), IntExp(4)))))),
           Defn(
            IdnDef("length", FunType(ListType(IntType()),
                                     IntType())),
            Vector(FunLine("length", Vector(ListPat(Vector())),
                           IntExp(0)),
                   FunLine("length",
                           Vector(ConsPat(IdentPat("h"),
                                          IdentPat("t"))),
                           PlusExp(IntExp(1),
                                   AppExp(IdnUse("length"),
                                          IdnUse("t")))))
                        )),
     AppExp (IdnUse ("length"), IdnUse ("x"))))
```

