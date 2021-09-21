/**
 * HasLang syntax analysis tests.
 *
 * Copyright 2021, Anthony Sloane, Matthew Roberts, Kym Haines, Macquarie University, All rights reserved.
 */

package haslang

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the syntax analyser works correctly.  I.e., it accepts
 * correct input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

    import HasLangTree._

    val parsers = new SyntaxAnalysis (positions)
    import parsers._

    // Tests of parsing basic expressions

    test ("equal expression") {
        exp ("a == 1") should parseTo[HasLangNode] (EqualExp (IdnUse ("a"), IntExp (1)))
    }

    test ("less than expression") {
        exp ("a < 1") should parseTo[HasLangNode] (LessExp (IdnUse ("a"), IntExp (1)))
    }

    test ("addition expression") {
        exp ("a + 1") should parseTo[HasLangNode] (PlusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("subtraction expression") {
        exp ("a - 1") should parseTo[HasLangNode] (MinusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("multiplication expression") {
        exp ("a * 1") should parseTo[HasLangNode] (StarExp (IdnUse ("a"), IntExp (1)))
    }

    test ("division expression") {
        exp ("a / 1") should parseTo[HasLangNode] (SlashExp (IdnUse ("a"), IntExp (1)))
    }

    test ("integer expression") {
        exp ("823") should parseTo[HasLangNode] (IntExp (823))
    }

    test ("true expression") {
        exp ("true") should parseTo[HasLangNode] (BoolExp (true))
    }

    test ("false expression") {
        exp ("false") should parseTo[HasLangNode] (BoolExp (false))
    }

    test ("identifier expression") {
        exp ("v123") should parseTo[HasLangNode] (IdnUse ("v123"))
    }

    test ("parenthesized expression") {
        exp ("(a + 5)") should parseTo[HasLangNode] (PlusExp (IdnUse ("a"), IntExp (5)))
    }

    test ("application expression 1") {
        exp ("a b") should parseTo[HasLangNode] (AppExp (IdnUse ("a"), IdnUse ("b")))
    }

    test ("expression containing an application expression") {
        exp ("1 + foo 2") should parseTo[HasLangNode] (PlusExp(IntExp(1), AppExp (IdnUse ("foo"), IntExp (2))))
    }

    test ("if expression") {
        exp ("if (true) then 3 else 4") should parseTo[HasLangNode] (IfExp (BoolExp (true), IntExp (3), IntExp (4)))
    }

    test ("lambda expression") {
        exp ("\\a :: Int -> a + 1") should parseTo[Exp] (LamExp(
                            IdnDef("a", IntType()),
                            PlusExp(IdnUse("a"), IntExp(1))))
    }

    test ("basic type") {
        tipe ("Bool") should parseTo[Type] (BoolType())
    }

    test ("parsing unit type") {
        tipe ("()") should parseTo[Type] (UnitType())
    }

    test ("parsing list type") {
        tipe ("[Int]") should parseTo[Type] (ListType(IntType()))
    }

    test ("parsing tuple type") {
        tipe ("(Int,Bool,[Bool])") should parseTo[Type] (TupleType(Vector(IntType(), BoolType(), ListType(BoolType()))))
    }

    test ("parsing function type") {
      tipe ("Int->Bool->[Int]") should parseTo[Type] (FunType(IntType(), FunType(BoolType(), ListType(IntType()))))
    }

    test ("parsing bracketted function type") {
      tipe ("(Int->Bool)->[Int]") should parseTo[Type] (FunType(FunType(IntType(), BoolType()), ListType(IntType())))
    }

    test ("empty list") {
        exp ("[]") should parseTo[HasLangNode] (ListExp (Vector()))
    }

    test ("cons expression") {
        exp ("3 : []") should parseTo[HasLangNode] (ConsExp (IntExp (3), ListExp (Vector())))
    }

    test ("list expression") {
        exp ("[3, 4, 5]") should parseTo[HasLangNode] (ListExp (Vector(IntExp(3), IntExp(4), IntExp(5))))
    }

    test ("tuple expression") {
        exp ("(3, 4, 5)") should parseTo[HasLangNode] (TupleExp (Vector(IntExp(3), IntExp(4), IntExp(5))))
    }

    test ("underscore pattern") {
        pat ("_") should parseTo[Pat] (AnyPat())
    }

    test ("literal pattern") {
        pat ("3") should parseTo[Pat] (LiteralPat(IntExp(3)))
    }

    test ("list pattern") {
        pat ("[3, _, 5]") should parseTo[Pat] (ListPat(Vector(LiteralPat(IntExp(3)), AnyPat(), LiteralPat(IntExp(5)))))
    }

    test ("cons pattern") {
        pat ("3 : []") should parseTo[Pat] (ConsPat(LiteralPat(IntExp(3)), ListPat(Vector())))
    }

    test ("tuple pattern") {
        pat ("(3, _, 5)") should parseTo[Pat] (TuplePat(Vector(LiteralPat(IntExp(3)), AnyPat(), LiteralPat(IntExp(5)))))
    }

    test ("simple function line") {
        funline ("fac 0 = 1") should parseTo[FunLine] (FunLine("fac", Vector(LiteralPat(IntExp(0))), IntExp(1)))
    }

    test ("more complicated function line") {
        funline ("length h:t = 1 + length t") should parseTo[FunLine] (FunLine("length", Vector(ConsPat(IdentPat("h"), IdentPat("t"))), PlusExp(IntExp(1), AppExp(IdnUse("length"), IdnUse("t")))))
    }

    test ("simple variable") {
        defn ("x :: Int = 100") should parseTo[Defn] (Defn(IdnDef("x", IntType()), Vector(FunLine("", Vector(), IntExp(100)))))
    }

    test ("function with two lines") {
      defn ("""inc :: Int -> Int
               inc n = n + 1
            """) should parseTo[Defn] (Defn(
                  IdnDef("inc", FunType(IntType(), IntType())),
                  Vector(FunLine("inc", Vector(IdentPat("n")),
                                 PlusExp(IdnUse("n"), IntExp(1))))))
    }

    test ("function with three lines") {
      defn ("""fac :: Int -> Int
               fac 0 = 1.
               fac n = n * fac (n - 1)
            """) should parseTo[Defn] (Defn(
                  IdnDef("fac", FunType(IntType(), IntType())),
                  Vector(FunLine("fac", Vector(LiteralPat(IntExp(0))),
                                 IntExp(1)),
                         FunLine("fac", Vector(IdentPat("n")),
                                 StarExp(IdnUse("n"),
                                         AppExp(IdnUse("fac"),
                                                MinusExp(IdnUse("n"),
                                                         IntExp(1))))))))
    }

    test ("one definition") {
      definitions ("""x   :: Int        = 100
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100))))))
    }

    test ("one definition with lambda") {
      definitions ("""inc :: Int -> Int = \a :: Int -> a + 1
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                              IdnDef("a", IntType()),
                                              PlusExp(IdnUse("a"), IntExp(1)))))
                                        )))
    }

    test ("two definitions") {
      definitions ("""x   :: Int        = 100;
                      y   :: Bool       = false
                """) should parseTo[Vector[Defn]] (Vector(
                       Defn(IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                       Defn(IdnDef("y", BoolType()),
                            Vector(FunLine("", Vector(), BoolExp(false))))))
    }

    test ("let with one definition") {
      program ("""let
                    x   :: Int        = 100
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100))))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("let with two definitions") {
      program ("""let
                    x   :: Int        = 100;
                    y   :: Bool       = false
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                             IdnDef("x", IntType()),
                             Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                             IdnDef("y", BoolType()),
                             Vector(FunLine("", Vector(), BoolExp(false))))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("program with two definitions including lambda") {
      program ("""let
                    x   :: Int        = 100;
                    inc :: Int -> Int = \a :: Int -> a + 1
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                              IdnDef("a", IntType()),
                                              PlusExp(IdnUse("a"), IntExp(1)))))
                                        )),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("program with definitions including lambda and multiline fun") {
      program ("""let
                    x   :: Int        = 100;
                    inc :: Int -> Int = \a :: Int -> a + 1;
                    length :: [Int] -> Int
                    length [] = 0.
                    length h:t = 1 + length t
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                            IdnDef("a", IntType()),
                                            PlusExp(IdnUse("a"), IntExp(1)))))),
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
                     AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    // FIXME: more tests here...

    
    // we can dynamically make typles as long as we like...
     test ("====CUSTOM TEST CASES START HERE==== \n- tuple type(s) should be dynamic") {
        tipe ("(Int,Bool,[Bool],Int)") should parseTo[Type] (TupleType(Vector(IntType(), BoolType(), ListType(BoolType()),IntType())))
    }

    // test one more than test case...
    test ("function with three definitions") {
      definitions ("""x   :: Int        = 100;
                      y   :: Bool       = false;
                      z   :: Int        = 420
                """) should parseTo[Vector[Defn]] (Vector(
                       Defn(IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                       Defn(IdnDef("y", BoolType()),
                            Vector(FunLine("", Vector(), BoolExp(false)))),
                        Defn(IdnDef("z", IntType()),
                            Vector(FunLine("", Vector(), IntExp(420))))))
    }


    // again test one more than test case...
    test ("function with four definitions") {
      defn ("""fac :: Int -> Int
               fac 0 = 1.
               fac 1 = 1.
               fac n = n * fac (n - 1)
            """) should parseTo[Defn] (Defn(
                  IdnDef("fac", FunType(IntType(), IntType())),
                  Vector(FunLine("fac", Vector(LiteralPat(IntExp(0))),
                                 IntExp(1)),
                         FunLine("fac", Vector(LiteralPat(IntExp(1))),
                                 IntExp(1)),
                         FunLine("fac", Vector(IdentPat("n")),
                                 StarExp(IdnUse("n"),
                                         AppExp(IdnUse("fac"),
                                                MinusExp(IdnUse("n"),
                                                         IntExp(1))))))))
    }
    

    // this should factor to
    // (2 * 3) + 4
    test ("mixed precedence and assoc") {
      program ("2 * 3 + 4") should parseTo[Program] (Program (PlusExp (StarExp (IntExp (2), IntExp (3)), IntExp (4))))
    }

    // should prase to
    // 1 - (2 - 3)
    test ("precedence and assoc all sub") {
      program ("1 - 2 - 3") should parseTo[Program] (Program (MinusExp (IntExp (1), MinusExp (IntExp (2), IntExp (3)))))
    }

    // should parse to:
    // (1 + 2) + 3
    test ("precedence and assoc all add") {
      program ("1 + 2 + 3") should parseTo[Program] (Program(
          PlusExp (PlusExp (IntExp (1), IntExp (2)), IntExp (3))))
    }

    // should parse to:
    // (1 * 3) * 4
    test ("precedence and assoc all star") {
      program ("1 * 3 * 4") should parseTo[Program] (Program(
          StarExp (StarExp (IntExp (1), IntExp (3)), IntExp (4))))
    }

    // should parse to 
    // x / (y / z) 
    // because we go left to right...   
    test ("precedence and assoc all slash") {
      program ("x / y / z") should parseTo[Program] (Program 
      (SlashExp (IdnUse ("x"), SlashExp (IdnUse ("y"), IdnUse ("z")))))
    }

    // should parse to
    // (5 + (2 * 6)) + 2
    test ("advanced precedence and assoc") {
      program ("5 + 2 * 6 + 2") should parseTo[Program] (Program 
      (PlusExp (
        PlusExp (IntExp (5), StarExp (IntExp (2), IntExp (6))),
        IntExp (2))))
    }
    
    // test for recursive cons calls
    test ("chained cons") {
      program ("1 : 2 : 3 : 4") should parseTo[Program] (Program 
        (ConsExp(IntExp (1),
         ConsExp(IntExp (2), 
            ConsExp (IntExp (3), IntExp (4))))))
    }

    // cons expression
    // that contains arithmetic 
    // should be 
    // a : (3 + 4)
    test ("cons with simple arithmetic") {
      program ("a : 3 + 4") should parseTo[Program] (Program 
      (ConsExp (IdnUse ("a"), 
        PlusExp(
            IntExp (3), IntExp (4)))))
    }

    // should be 
    // (a + b) : (c + d)
    test ("cons with more complex arithmetic") {
      program ("a + b : c + d") should parseTo[Program] (Program (
            ConsExp (
                PlusExp (IdnUse ("a"), IdnUse ("b")),
                PlusExp (IdnUse ("c"), IdnUse ("d")))))
    }


    // should be 
    // lambda a = a + (lambda b = 10 * a)
    test ("lambda in a lambda") {
      program ("\\a :: Int -> a + \\b :: Int -> 10 * a") should parseTo[Program] (Program (
        LamExp (
            IdnDef ("a", IntType ()),
            PlusExp (
                IdnUse ("a"),
                    LamExp (
                        IdnDef ("b", IntType ()),
                        StarExp (IntExp (10), IdnUse ("a")))))))
    }


    // should be able to pick up expressions inside the if statments
    // and also statements in the `then` and `else` spots too
    test ("complex if statement") {
      program ("if (x + 1 == 2) then (4 : 20) else (6 : 9)") should parseTo[Program] (Program (
        IfExp (
            // if
            EqualExp ( 
                // x + 1
                PlusExp (IdnUse ("x"), IntExp (1)), 
                // ==
                // 2
                IntExp (2)), 
            // then  4 : 20
            ConsExp (IntExp (4), IntExp (20)), 
            // else  6 : 9
            ConsExp (IntExp (6), IntExp (9))))) 
    }

    // note this is a valid expression in python...
    // but not a valid expression in Haskell
    // something like: '((1 + 1 == 2) || (2 == 3))' is valid in Haskell though. 
    test ("another if statement with complex statement") {
      program ("if ((1 + 1 == 2) + 2 == 3) then (1) else (0)") should parseTo[Program] (Program(
    IfExp (
        EqualExp (
            PlusExp (
                EqualExp(
                    PlusExp(
                        IntExp (1), IntExp (1)), 
                    IntExp (2)),
            IntExp (2)),
        IntExp (3)),
    IntExp (1), 
    IntExp (0))))
    }

    // length function given in assignments specs
    //
    // This is the function:
    //
    // let
    //     x   :: [Int]        = [3, 1, 4];
    //     length :: [Int] -> Int
    //     length [] = 0.
    //     length h:t = 1 + length t
    // in
    //     length x
    
    test ("length function (from assignment spec)") {
      program ("""let
        x   :: [Int]        = [3, 1, 4];
        length :: [Int] -> Int
        length [] = 0.
        length h:t = 1 + length t
        in
        length x
        """) should parseTo[Program] (Program(
    // test case is also in assignment spec sheet
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
     AppExp (IdnUse ("length"), IdnUse ("x")))))
    }



        // should be able to pick up expressions inside the if statments
    // and also statements in the `then` and `else` spots too
    test ("statement with all operators") {
      program ("(1 / 2 + 3 / 4 * 5 < 5) == true") should parseTo[Program] (Program (
    EqualExp (
        LessExp (
            PlusExp (
                SlashExp (IntExp (1), IntExp (2)),
               // (1 / 2) 
               // +
               // 3 / (4 * 5)
                SlashExp (IntExp (3), StarExp (IntExp (4), IntExp (5)))),
        // < 5
            IntExp (5)),
        // ==
        // true
        BoolExp (true))))
    }

    // we are forced to put '()' around the lists
    // this would give:
    // [3,1,4,1,5,9]
    test ("cons with lists") {
      program ("([3,1,4]) :  ([1,5]) : ([9])") should parseTo[Program] (Program(
    ConsExp (
        ListExp (Vector (IntExp (3), IntExp (1), IntExp (4))),
        ConsExp (
            ListExp (Vector (IntExp (1), IntExp (5))),
            ListExp (Vector (IntExp (9)))))))

    }


   

}
