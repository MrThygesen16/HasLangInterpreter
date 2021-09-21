/**
 * HasLang syntax analyser.
 *
 * Copyright 2021, Anthony Sloane, Matthew Roberts, Kym Haines, Macquarie University, All rights reserved.
 */

package haslang

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for HasLang.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import HasLangTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        exp ^^ Program

    lazy val literal : PackratParser[Exp] =
        "false" ^^^ BoolExp (false) |
        "true" ^^^ BoolExp (true) |
        integer ^^ (s => IntExp (s.toInt)) 

    // most specific
    lazy val factor : PackratParser[Exp] =
        // FIXME
        ("[]") ^^ 
            {case  "[]" => ListExp(Vector())} |
        (factor ~ factor) ^^ 
            {AppExp} |
        literal |
        identifier ^^ IdnUse |
        "(" ~> exp <~ ")" |
        failure ("exp expected")
    
    // highest precedence 
    lazy val other : PackratParser[Exp] =
        ("let" ~> (definitions) ~ ("in" ~> exp)) ^^
            {case d ~ e => LetExp(d,e)} |
        (("\\" ~> identifier) ~ ("::" ~> tipe) ~ ("->" ~> exp)) ^^
            {case i ~ t ~ e => LamExp(IdnDef(i,t),e)} |
        factor


    lazy val muldiv : PackratParser[Exp] =
        (muldiv ~ ("*" ~> other)) ^^ 
            {StarExp } |
        (other ~ ("/" ~> muldiv)) ^^
            {SlashExp} |
        other


    lazy val addsub : PackratParser[Exp] =
        (addsub ~ ("+" ~> muldiv)) ^^
            {PlusExp} |
        (muldiv ~ ("-" ~> addsub)) ^^
            {MinusExp} |
        muldiv


    lazy val lstcon : PackratParser[Exp] =
        ((addsub <~ ":") ~ lstcon) ^^
            {ConsExp} | 
        addsub


    lazy val eqchk : PackratParser[Exp] =
        (eqchk ~ ("==" ~> eqchk)) ^^
            {EqualExp} |
        (eqchk ~ ("<" ~> eqchk)) ^^
            {LessExp} |
        lstcon

    lazy val ifex : PackratParser[Exp] =
        ((("if" ~> "(") ~> (exp <~ ")"))  ~ ("then" ~> (exp <~ "else")) ~ exp) ^^
            {IfExp} |
        eqchk

    // least specific / lowest precedence
    lazy val exp : PackratParser[Exp] =
        // FIXME
        ("[" ~> exp ~ (("," ~> exp)* ) <~ "]") ^^
            {case e1 ~ e2 => ListExp(e1 +: e2)} |
        ("(" ~> exp ~ (("," ~> exp)+ ) <~ ")") ^^
            {case e1 ~ e2 => TupleExp(e1 +: e2)} |
        ifex
        

    lazy val definitions : PackratParser[Vector[Defn]] =
        // FIXME
         (((defn <~ ";")+ ) ~ defn) ^^
            {case d1 ~ d2 => d1 :+ d2 } |
        ((defn+) ~ (defn*)) ^^
            {case d1 ~ d2 => d1 ++ d2 }
       


    lazy val defn : PackratParser[Defn] =
        // FIXME
        (idndef ~ ("=" ~> exp)) ^^
            {case i ~ e => Defn(i,Vector(FunLine("",Vector(),e)))} | 
        (idndef ~ (funline+) ) ^^
            {case i ~ f => Defn(i,f) } 
       
        //"fixme" ^^^ Defn(IdnDef("fixme", IntType()), Vector())


    lazy val funline : PackratParser[FunLine] =
        // FIXME
        (identifier ~ (pat+) ~ ("=" ~> exp) <~ ".") ^^
            {FunLine} |
        (identifier ~ (pat+) ~ ("=" ~> exp)) ^^
            {FunLine} 
      
      
    lazy val pat : PackratParser[Pat] =
        // FIXME
        ("[" ~> (pat*) ~ (("," ~> pat)* ) <~ "]") ^^ 
            {case p1 ~ p2 => ListPat(p1 ++ p2)} |
        ("(" ~> pat ~ (("," ~> pat)+ ) <~ ")") ^^ 
            {case p1 ~ p2 => TuplePat(p1 +: p2)} |
        ((pat <~ ":") ~ pat) ^^
            {case p1 ~ p2  => ConsPat(p1,p2)} |
        "_" ^^^ AnyPat() |
        basicpat 
     

    lazy val basicpat : PackratParser[Pat] =
        // FIXME
        literal ^^ LiteralPat |
        identifier ^^ IdentPat 


    lazy val tipe : PackratParser[Type] =
        // FIXME
        (("(" ~> tipe) ~ ("->" ~> tipe) ~ (")" ~> ("->" ~> tipe))) ^^
            {case t1 ~ t2 ~ t3 => FunType(FunType(t1,t2),t3)} |
        (tipe ~ ("->" ~> tipe)) ^^
            {case t1 ~ t2 => FunType(t1,t2)} |
        ("[" ~> tipe <~ "]") ^^ 
            {case t => ListType(t)} |
        ("(" ~> tipe <~ ")") ^^
            {case t => t} |
        ("(" ~> tipe ~ (("," ~> tipe)+ ) <~ ")") ^^
            {case t1 ~ t2 => TupleType(t1 +: t2)} |
        basictipe 
     


    lazy val basictipe : PackratParser[Type] =
        // FIXME
        "Int" ^^^ IntType() |
        "Bool" ^^^ BoolType() |
        "()" ^^^ UnitType() 
        
       


    // NOTE: You should not change anything below here...
    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        (identifier <~ "::") ~ tipe ^^ IdnDef

    val keywordStrings =
        List ("let", "else", "false", "if", "then", "true", "in")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    lazy val whitespaceParser : PackratParser[Any] =
        rep ( """\s""".r | comment)

    lazy val comment : PackratParser[Any] =
        "{-" ~ rep (not ("-}") ~ (comment | any)) ~ "-}" |
        "--.*(\n|\\z)".r

}
