package org.planx.sh.parsing.shop2

import scala.util.parsing.combinator.syntactical.StdTokenParsers

trait SHOP2Parser extends StdTokenParsers {
  type Tokens = SHOP2Tokens
  val lexical = new SHOP2Lexer

  // Domain Definition
  lazy val domain_def = "(" ~> "defdomain" ~> domain_name ~ rep(domain_item) <~ ")" ^^ {
    case name ~ items => DomainDef(name, items)
  }

  lazy val domain_name = name

  lazy val domain_item = operator_def | method_def | axiom_def

  // Operator Definition
  lazy val operator_def = "(" ~> ":operator" ~> operator_head ~ precondition_list ~ effect_list <~ ")" ^^ {
    case head ~ precs ~ effs => OperatorDef(head._1, head._2, precs, effs)
  }

  lazy val operator_head = "(" ~> name ~ parameter_list <~ ")" ^^ {
    case n ~ params => (n, params)
  }

  lazy val parameter_list = rep(variable)

  lazy val precondition_list = opt("(" ~> rep(atomic_formula) <~ ")") ^^ {
    case Some(precs) => precs
    case None => List()
  }

  lazy val effect_list = opt("(" ~> rep(atomic_formula) <~ ")") ^^ {
    case Some(effs) => effs
    case None => List()
  }

  // Method Definition
  lazy val method_def = "(" ~> ":method" ~> method_head ~ method_body <~ ")" ^^ {
    case head ~ body => MethodDef(head._1, head._2, body)
  }

  lazy val method_head = "(" ~> name ~ parameter_list <~ ")" ^^ {
    case n ~ params => (n, params)
  }

  lazy val method_body = rep(task_list | atomic_formula)

  lazy val axiom_def = "(" ~> ":axiom" ~> name ~ rep(atomic_formula) <~ ")" ^^ {
    case name ~ preconditions => AxiomDef(name, preconditions)
  }

  lazy val task_list: Parser[Task] = unordered_task_list | ordered_task_list | task_atom

  lazy val unordered_task_list = "(" ~> "unordered" ~> rep(task_list) <~ ")" ^^ {case tasks => UnorderedTaskList(tasks)}

  lazy val ordered_task_list = "(" ~> "sequence" ~> rep(task_list) <~ ")" ^^ {case tasks => OrderedTaskList(tasks)}

  lazy val task_atom: Parser[Task] = primitive_task | compound_task

  private lazy val primitive_task = accept("primitive task", { case lexical.TaskLit(s) => PrimitiveTask(s.toLowerCase) })

  private lazy val compound_task = "(" ~> name ~ rep(term) <~ ")" ^^ {
    case taskName ~ args => CompoundTask(taskName, args)
  }
  // Atomic Formula (wie in HPDL)
  lazy val atomic_formula = simple_predicate | proper_predicate
  lazy val simple_predicate = "(" ~> predicate_name <~ ")" ^^ {case pn => Predicate(pn)}
  lazy val proper_predicate = "(" ~> predicate_name ~ terms <~ ")" ^^ {case pn ~ t => Predicate(pn, t)}

  lazy val predicate_name = name
  lazy val terms = rep1(term)
  lazy val term = name | variable | number | list_term | call_term

  private lazy val call_term = "(" ~> "call" ~> name ~ rep(term) <~ ")" ^^ {
    case function ~ args => s"(call $function ${args.mkString(" ")})"
  }

  private lazy val list_term = "(" ~> rep1(term) <~ ")" ^^ (terms => s"(${terms.mkString(" ")})")



  // Basis Parser
  lazy val name = accept("name", { case lexical.IdLit(s) => s.toLowerCase })
  lazy val variable = accept("variable", { case lexical.VarIdLit(s) => s.toLowerCase })
  lazy val number = accept("number", { case lexical.IntLit(s) => s case lexical.FloatLit(s) => s})
}

// TODO: Datenstrukturen müssen noch in die richtigen Klassen gepackt werden
case class DomainDef(name: String, items: List[DomainItem])
sealed trait DomainItem
case class OperatorDef(name: String, params: List[String], preconditions: List[Predicate], effects: List[Predicate]) extends DomainItem
case class MethodDef(name: String, params: List[String], body: List[Any]) extends DomainItem
case class Predicate(name: String, args: List[String] = List())

sealed trait Task
case class UnorderedTaskList(tasks: List[Task]) extends Task
case class OrderedTaskList(tasks: List[Task]) extends Task
case class PrimitiveTask(name: String) extends Task
case class CompoundTask(name: String, args: List[String]) extends Task
case class AxiomDef(name: String, preconditions: List[Predicate]) extends DomainItem
case class CallTerm(function: String, args: List[String])

