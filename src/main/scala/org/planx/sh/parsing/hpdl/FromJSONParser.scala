package org.planx.sh.parsing.hpdl

import spray.json._
import DefaultJsonProtocol._

import java.io.File
import scala.io.Source
import org.planx.sh.problem.{Add, Axiom, Constant, Delete, Domain, DomainMethod, DomainOperator, DomainTask, DomainType, EmptyEffect, ForallEffect, Method, NumericAssignment, Objects, Operator, Predicate, Problem, Task, TaskList, Term, Var}
import org.planx.sh.solving.{Bindable, Expression, ExpressionAnd, ExpressionAtomic, ExpressionNil, ExpressionNot, ExpressionOr, Function, InstanceUnifier, State, TaskUnifier}


class FromJSONParser {

  def parseFile(filename: String): (String, List[String], Problem, List[Operator], List[Task]) = {
    val source = Source.fromFile(filename)
    val jsonStr = try source.mkString finally source.close()
    val json = jsonStr.parseJson.asJsObject

    val domainName = json.fields.keys.head
    val fullJson = json.fields(domainName).asJsObject

    val domainJson = fullJson.fields("domain").asJsObject
    val problemJson = fullJson.fields("problem").asJsObject

    val domainRequirements = fullJson.fields("requirements").convertTo[List[String]]

    val domaintypes = buildDomainTypes(domainJson)
    val domainPredicates = buildDomainPredicates(domainJson)
    val domainFunctions = buildDomainFunctions(domainJson)
    val domainOperators = buildDomainOperators(domainJson)
    val domainTasks = buildDomainTasks(domainJson)
    val domainAxioms = buildDomainAxioms(domainJson)

    val problemRequirements = buildProblemRequirements(problemJson)
    val problemObjects = buildProblemObjects(problemJson)
    val problemInitState = buildProblemInitState(problemJson)
    val problemGoalTasks = buildProblemGoalTasks(problemJson)


    val domain = Domain(name = domainName, requirements = domainRequirements, types = Nil, predicates = Nil, functions = Nil, _operators = Nil, uncoupledTasks = Nil, axioms = Nil)
    //TODO: missing ProblemName (is it a Problem?
    //TODO: missing Problem Requirements there is a difference between domain and problem requirements
    val problem = Problem(name = "", domainName = domain.name, requirements = Nil, objects = Objects(objects= Nil), state = State(atoms = scala.collection.mutable.Map.empty), goalTaskList = TaskList(ordering = "", tasks = List()))
    (domain.name, domain.requirements, null, Nil, Nil)
  }

  private def buildDomainTypes(domainJson: JsObject): List[DomainType] = { List.empty }

  private def buildDomainPredicates(domainJson: JsObject): List[Predicate] = { List.empty }

  private def buildDomainFunctions(domainJson: JsObject): List[Function] = { List.empty }

  private def buildDomainOperators(domainJson: JsObject): List[DomainOperator] = { List.empty }

  private def buildDomainTasks(domainJson: JsObject): List[DomainTask] = { List.empty }

  private def buildDomainAxioms(domainJson: JsObject): List[Axiom] = { List.empty }

  private def buildProblemRequirements(problemJson: JsObject): List[String] = { List.empty }

  private def buildProblemObjects(problemJson: JsObject): Objects = {Objects(objects= Nil)}

  private def buildProblemInitState(problemJson: JsObject): State = { State(atoms = scala.collection.mutable.Map.empty) }

  private def buildProblemGoalTasks(problemJson: JsObject): TaskList = {TaskList(ordering = "", tasks = List.empty )}
}