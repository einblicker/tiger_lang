package tiger_lang

object Absyn {
  sealed trait Var
  case class SimpleVar(s: String) extends Var
  case class FieldVar(v: Var, s: String) extends Var
  case class SubscriptVar(v: Var, e: Exp) extends Var

  sealed trait Exp
  case class VarExp(v: Var) extends Exp
  case object NilExp extends Exp
  case class IntExp(i: Int) extends Exp
  case class StringExp(s: String) extends Exp
  case class CallExp(f: String, a: List[Exp]) extends Exp
  case class OpExp(l: Exp, o: Oper, r: Exp) extends Exp
  case class RecordExp(fs: List[(String, Exp)], ty: String) extends Exp
  case class SeqExp(es: List[Exp]) extends Exp
  case class AssignExp(v: Var, e: Exp) extends Exp
  case class IfExp(tes: Exp, t: Exp, e: Option[Exp]) extends Exp
  case class WhileExp(t: Exp, b: Exp) extends Exp
  case class ForExp(v: String, l: Exp, h: Exp, b: Exp) extends Exp
  case object BreakExp extends Exp
  case class LetExp(ds: List[Dec], b: Exp) extends Exp
  case class ArrayExp(ty: String, s: Exp, i: Exp) extends Exp

  sealed trait Dec
  case class FunctionDec(ds: List[FunDec]) extends Dec
  case class VarDec(n: String, ty: Option[String], i: Exp) extends Dec
  case class TypeDec(n: String, ty: Ty) extends Dec

  sealed trait Ty
  case class NameTy(s: String) extends Ty
  case class RecordTy(fs: List[Field]) extends Ty
  case class ArrayTy(s: String) extends Ty

  sealed trait Oper
  case object PlusOp extends Oper
  case object MinusOp extends Oper
  case object TimesOp extends Oper
  case object DivideOp extends Oper
  case object EqOp extends Oper
  case object NeqOp extends Oper
  case object LtOp extends Oper
  case object LeOp extends Oper
  case object GtOp extends Oper
  case object GeOp extends Oper
  case object OrOp extends Oper
  case object AndOp extends Oper

  case class Field(n: String, ty: String)
  case class FunDec(n: String, ps: List[Field], r: Option[String], b: Exp)
}
