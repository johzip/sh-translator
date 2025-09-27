package org.planx.sh.parsing.hpdl

import spray.json._
import DefaultJsonProtocol._

import java.io.File
import scala.io.Source
import org.planx.sh.problem.{Add, Axiom, Constant, Delete, Domain, DomainMethod, DomainOperator, DomainTask, DomainType, EmptyEffect, ForallEffect, Method, NumericAssignment, Objects, Operator, Predicate, Problem, Task, TaskList, Term, Var}
import org.planx.sh.solving.{Bindable, Expression, ExpressionAnd, ExpressionAtomic, ExpressionNil, ExpressionNot, ExpressionOr, Function, InstanceUnifier, State, TaskUnifier}


class FromJSONParser {

  def parseFile(filename: String, domainToCompare: Domain, problemToCompare: Problem): (Domain, Problem) = {
    val source = Source.fromFile(filename)
    val jsonStr = try source.mkString finally source.close()
    val json = jsonStr.parseJson.asJsObject

    val domainName = json.fields.keys.head
    val fullJson = json.fields(domainName).asJsObject

    val domainJson = fullJson.fields("domain").asJsObject
    val problemJson = fullJson.fields("problem").asJsObject

    val domainRequirements = domainJson.fields("requirements").convertTo[List[String]]
    val problemRequirements = problemJson.fields("requirements").convertTo[List[String]]

    val domaintypes = buildDomainTypes(domainJson)
    val domainPredicates = buildDomainPredicates(domainJson)
    val domainFunctions = buildDomainFunctions(domainJson)
    val domainOperators = buildDomainOperators(domainJson.fields("primitive_tasks").convertTo[List[JsObject]])
    val domainTasks = buildDomainTasks(domainJson.fields("compound_tasks").convertTo[List[JsObject]])
    val domainAxioms = buildDomainAxioms(domainJson)

    val problemObjects = buildProblemObjects(problemJson)
    val problemInitState = buildProblemInitState(problemJson.fields("init").convertTo[List[JsObject]])
    val problemGoalTasks = buildProblemGoalTasks(problemJson.fields("goal").convertTo[JsObject])


    val domain = Domain(name = domainName, requirements = domainRequirements, types = Nil, predicates = Nil, functions = Nil, _operators = Nil, uncoupledTasks = Nil, axioms = Nil)
    //TODO: missing ProblemName (is it a Problem?)
    //TODO: missing Objects
    val problem = Problem(name = "problem", domainName = domain.name, requirements = problemRequirements, objects = Objects(objects= Nil), state = problemInitState, goalTaskList = problemGoalTasks)
    testResult(domain, problem, domainToCompare, problemToCompare)
    (domain, problem)
  }

  private def testResult(domain: Domain, problem: Problem, domainToCompare: Domain, problemToCompare: Problem): Boolean = {
    // Implement comparison logic here
    println("goalTaskList NEW: ")
    println(problem.goalTaskList)
    println("goalTaskList ORIGINAL: ")
    println(problemToCompare.goalTaskList)
    true
  }


  private def buildDomainTypes(domainJson: JsObject): List[DomainType] = { List.empty }

  private def buildDomainPredicates(domainJson: JsObject): List[Predicate] = { List.empty }

  private def buildDomainFunctions(domainJson: JsObject): List[Function] = { List.empty }

  private def buildDomainOperators(domainJson: List[JsObject]): List[DomainOperator] = { List.empty }

  private def buildDomainTasks(domainJson: List[JsObject]): List[DomainTask] = { List.empty }

  private def buildDomainAxioms(domainJson: JsObject): List[Axiom] = { List.empty }

  private def buildProblemRequirements(problemJson: JsObject): List[String] = { List.empty }

  private def buildProblemObjects(problemJson: JsObject): Objects = {Objects(objects= Nil)}

  private def buildProblemInitState(problemInitJson: List[JsObject]): State = {
    val state = State(atoms = scala.collection.mutable.Map.empty)
    for (entry <- problemInitJson) {
      val name = entry.fields("name").convertTo[String]
      val parameters = entry.fields("parameters").convertTo[List[String]].toArray
      val typ = entry.fields("type").convertTo[String]
      if (typ == "predicate") {
        state.add(name, parameters)
      }
    }
    state
  }

  private def buildPredicateFromJson(predicateJson: JsObject): Predicate = {
    val name = predicateJson.fields("name").convertTo[String]
    val parametersJson = predicateJson.fields("parameters").convertTo[List[JsObject]]
    val parameters: List[Term] = parametersJson.map { paramJson =>
      val term = paramJson.fields("term").convertTo[String]
      val typ = paramJson.fields("type").convertTo[String]
      typ match {
        case "Constant" => Constant(term)
        case "Var"      => Var(Symbol(term))
        case _          => Constant(term)
      }
    }
    Predicate(name, parameters)

  }

  private def buildProblemGoalTasks(problemGoalJson: JsObject): TaskList = {
    //val ordering = problemGoalJson.fields.get("ordering").map(_.convertTo[String]).getOrElse("")
    val tasksJson = problemGoalJson.fields.get("tasks").map(_.convertTo[List[JsObject]]).getOrElse(Nil)
    val tasks = tasksJson.map { buildPredicateFromJson }

    TaskList(ordering = "unordered", tasks = tasks)
  }
}