package tiger_lang
import util.parsing.combinator.RegexParsers
import Absyn._

object Parser extends RegexParsers {
  def decs: Parser[List[Dec]] =
    rep(dec)

  def dec: Parser[Dec] =
    tyDec | varDec | funDec
  
  def tyDec: Parser[Dec] =
    ("type"~>tyId)~("="~>ty) ^^ {
      case tyId~ty => TypeDec(tyId, ty)
    }
  
  def tyId: Parser[String] =
    """[a-zA-Z][a-zA-Z0-9]*""".r
  
  def ty: Parser[Ty] = (
    "{"~>tyFields<~"}" ^^ RecordTy.apply |
    "array"~"of"~>tyId ^^ ArrayTy.apply |
    tyId ^^ NameTy.apply
  )
    
  def tyFields: Parser[List[Field]] =
    repsep((id<~":")~tyId, ",") ^^ {
      case xs => xs.map{case n~ty => Field(n, ty)}
    }

  def varDec: Parser[Dec] =
    "var"~>id~opt(":"~>tyId)~(":="~>exp) ^^ {
      case _id ~ Some(_tyId) ~ _exp => VarDec(_id, Some(_tyId), _exp)
      case _id ~ None ~ _exp => VarDec(_id, None, _exp)
    }
  
  def id: Parser[String] =
    """[a-zA-Z][a-zA-Z0-9]*""".r

  def funDec: Parser[Dec] =
    "function"~>id~("("~>tyFields<~")")~opt(":"~>tyId)~("="~>exp) ^^ {
      case _id~_tyFields~Some(_tyId)~_exp => FunctionDec(List(FunDec(_id, _tyFields, Some(_tyId), _exp)))
      case _id~_tyFields~None~_exp => FunctionDec(List(FunDec(_id, _tyFields, None, _exp)))
    }

  def rawExp: Parser[Exp] = (
    "nil" ^^^ NilExp |
    assignExp |
    ifExp |
    whileExp |
    forExp |
    breakExp |
    letExp |
    mkRecord |
    mkArray |
    call |
    lvalue ^^ VarExp.apply |
    seq |
    integer |
    string
  )

  def seq: Parser[Exp] =
    "("~>repsep(exp, ";")<~")" ^^ SeqExp.apply
  
  def integer: Parser[Exp] =
    opt("-")~"""[1-9][0-9]*""".r ^^ {
      case None~num => IntExp(num.toInt)
      case Some("-")~num => IntExp(-num.toInt)
    }

  def string: Parser[Exp] =
    "\"[^\"]*\"".r ^^ StringExp.apply

  def call: Parser[Exp] =
    id~("("~>repsep(exp, ",")<~")") ^^ {
      case _id~exps => CallExp(_id, exps)
    }
  
  def lvalue: Parser[Var] =
    id~rep(lvalueRest) ^^ {
      case _id~Nil => SimpleVar(_id)
      case _id~xs => xs.reduceLeft(_.andThen(_))(SimpleVar(_id))
    }

  def lvalueRest: Parser[Var => Var] =
    "."~>id ^^ {case _id => (v: Var) => FieldVar(v, _id)} |
    "["~>exp<~"]" ^^ {case _exp => (v: Var) => SubscriptVar(v, _exp)}

  def exp: Parser[Exp] = binExp1
  def binExp1: Parser[Exp] = {
    binExp2~rep("|"~>binExp2) ^^ {
      case exp~Nil => exp
      case exp~exps => exps.foldLeft(exp){(a,b)=>OpExp(b, OrOp, a)}
    } }
  def binExp2: Parser[Exp] = {
    binExp3~rep("&"~>binExp3) ^^ {
      case exp~Nil => exp
      case exp~exps => exps.foldLeft(exp){(a,b)=>OpExp(b, AndOp, a)}
    } }
  def binExp3: Parser[Exp] = {
    binExp4~opt(
      "="~binExp4 |
      ">="~binExp4 |
      "<="~binExp4 |
      "<"~binExp4 |
      "<>"~binExp4 |
      ">"~binExp4
    ) ^^ {
      case exp~None => exp
      case exp1~Some("="~exp2)  => OpExp(exp1, EqOp, exp2)
      case exp1~Some(">="~exp2) => OpExp(exp1, EqOp, exp2)
      case exp1~Some("<="~exp2) => OpExp(exp1, EqOp, exp2)
      case exp1~Some("<"~exp2)  => OpExp(exp1, EqOp, exp2)
      case exp1~Some("<>"~exp2) => OpExp(exp1, NeqOp, exp2)
      case exp1~Some(">"~exp2)  => OpExp(exp1, EqOp, exp2)
    } }
  def binExp4: Parser[Exp] = {
    binExp5~rep(
      "+"~binExp5 |
      "-"~binExp5
    ) ^^ {
      case exp~Nil => exp
      case exp1~("+"~exp2 :: exps) =>(exp2 :: exps.map{case _~e => e}).foldLeft(exp1){(a,b)=>OpExp(b, PlusOp, a)}
      case exp1~("-"~exp2 :: exps) =>(exp2 :: exps.map{case _~e => e}).foldLeft(exp1){(a,b)=>OpExp(b, MinusOp, a)}
    } }
  def binExp5: Parser[Exp] =
    rawExp~rep(
      "*"~rawExp |
      "/"~rawExp
    ) ^^ {
      case exp~Nil => exp
      case exp1~("*"~exp2 :: exps) =>(exp2 :: exps.map{case _~e => e}).foldLeft(exp1){(a,b)=>OpExp(b, TimesOp, a)}
      case exp1~("/"~exp2 :: exps) =>(exp2 :: exps.map{case _~e => e}).foldLeft(exp1){(a,b)=>OpExp(b, DivideOp, a)}
    }

  def mkRecord: Parser[Exp] =
    (tyId<~"{")~repsep((id<~"=")~exp, ",")<~"}" ^^ {
      case tyId~fields =>
    RecordExp(fields.map{case a~b => (a,b)}, tyId)
    }

  def mkArray: Parser[Exp] =
    (tyId<~"[")~(exp<~"]")~("of"~>exp) ^^ {
      case tyId~exp1~exp2 => ArrayExp(tyId, exp1, exp2)
    }

  def assignExp: Parser[Exp] =
    (lvalue<~":=")~exp ^^ {
      case lvalue~exp => AssignExp(lvalue, exp)
    }

  def ifExp: Parser[Exp] =
    ("if"~>exp<~"then")~exp~opt("else"~>exp) ^^ {
      case exp1~exp2~exp3opt => IfExp(exp1, exp2, exp3opt)
    }

  def whileExp: Parser[Exp] =
    ("while"~>exp<~"do")~exp ^^ {
      case exp1~exp2 => WhileExp(exp1, exp2)
    }

  def forExp: Parser[Exp] =
    ("for"~>id<~":=")~(exp<~"to")~(exp<~"do")~exp ^^ {
      case id~exp1~exp2~exp3 => ForExp(id, exp1, exp2, exp3)
    }

  def breakExp: Parser[Exp] =
    "break" ^^^ BreakExp

  def letExp: Parser[Exp] =
    ("let"~>decs<~"in")~repsep(exp, ";")<~"end" ^^ {
      case decs~List(exp) => LetExp(decs, exp)
      case decs~exps => LetExp(decs, SeqExp(exps))
    }
  
  def parse(input: String) = parseAll(exp, input).get
}