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

    //TODO: these fields are missing in the JSON
    val domaintypes = buildDomainTypes(domainJson)
    val domainPredicates = buildDomainPredicates(domainJson)
    val domainFunctions = buildDomainFunctions(domainJson)
    val domainAxioms = buildDomainAxioms(domainJson)

    val domainOperators = buildDomainOperators(domainJson.fields("primitive_tasks").convertTo[List[JsObject]])
    val domainTasks = buildDomainTasks(domainJson.fields("compound_tasks").convertTo[List[JsObject]])

    //TODO: problemObjects is missing in the JSON
    val problemObjects = buildProblemObjects(problemJson)
    val problemInitState = buildProblemInitState(problemJson.fields("init").convertTo[List[JsObject]])
    val problemGoalTasks = buildProblemGoalTasks(problemJson.fields("goal").convertTo[JsObject])


    val domain = Domain(name = domainName, requirements = domainRequirements, types = Nil, predicates = Nil, functions = Nil, _operators = domainOperators, uncoupledTasks = domainTasks, axioms = Nil)
    //TODO: missing ProblemName (is it a Problem?) no pun intended
    val problem = Problem(name = "problem", domainName = domain.name, requirements = problemRequirements, objects = Objects(objects= Nil), state = problemInitState, goalTaskList = problemGoalTasks)
    //testResult(domain, problem, domainToCompare, problemToCompare)
    (domain, problem)
  }

  private def testResult(domain: Domain, problem: Problem, domainToCompare: Domain, problemToCompare: Problem): Boolean = {
    var equal = true

    def compareList[T](name: String, l1: List[T], l2: List[T]): Unit = {
      if (l1 != l2) {
        println(s"Difference in $name:\n  New: $l1\n  Original: $l2")
        equal = false
      }
    }

    if (domain.name != domainToCompare.name) {
      println(s"Difference in Domain name:\n  New: ${domain.name}\n  Original: ${domainToCompare.name}")
      equal = false
    }
    compareList("Domain requirements", domain.requirements, domainToCompare.requirements)
    compareList("Domain types", domain.types, domainToCompare.types)
    compareList("Domain predicates", domain.predicates, domainToCompare.predicates)
    compareList("Domain functions", domain.functions, domainToCompare.functions)
    compareList("Domain operators", domain._operators, domainToCompare._operators)
    compareList("Domain axioms", domain.axioms, domainToCompare.axioms)
    compareList("Domain tasks", domain.uncoupledTasks, domainToCompare.uncoupledTasks)

    if (problem.name != problemToCompare.name) {
      println(s"Difference in Problem name:\n  New: ${problem.name}\n  Original: ${problemToCompare.name}")
      equal = false
    }
    if (problem.domainName != problemToCompare.domainName) {
      println(s"Difference in Problem domainName:\n  New: ${problem.domainName}\n  Original: ${problemToCompare.domainName}")
      equal = false
    }
    compareList("Problem requirements", problem.requirements, problemToCompare.requirements)
    compareList("Problem objects", problem.objects.objects, problemToCompare.objects.objects)
    if (problem.state != problemToCompare.state) {
      println(s"Difference in Problem state:\n  New: ${problem.state}\n  Original: ${problemToCompare.state}")
      equal = false
    }
    if (problem.goalTaskList != problemToCompare.goalTaskList) {
      println(s"Difference in Problem goalTaskList:\n  New: ${problem.goalTaskList}\n  Original: ${problemToCompare.goalTaskList}")
      equal = false
    }

    equal
  }

  // Domain
  private def buildDomainTasks(compoundTasks: List[JsObject]): List[(DomainTask, List[DomainMethod])] = {
    compoundTasks.map { obj =>
      val name = obj.fields("name").convertTo[String]
      val parametersJson = obj.fields("parameters").convertTo[List[JsObject]]
      val parameters: List[Term] = buildParametersFromJSON(parametersJson)
      val domainTask = DomainTask(name, parameters)
      val methodJson = buildMethodsFromJSON(obj.fields.get("methods").map(_.convertTo[List[JsObject]]).getOrElse(Nil))
      (domainTask, methodJson)
    }
  }

  private def buildDomainOperators(domainJson: List[JsObject]): List[DomainOperator] = {
    domainJson.map { obj =>
      val name = obj.fields("name").convertTo[String]
      val parametersJson = obj.fields("parameters").convertTo[List[JsObject]]
      val parameters: List[Term] = buildParametersFromJSON(parametersJson)
      val precondition: Expression = buildPreconditionFromJSON(obj.fields("preconditions").asJsObject())

      val effects = obj.fields.get("effect").map(_.convertTo[List[JsObject]]).getOrElse(Nil)
      val add: List[Add] = effects.collect {
        case eff if eff.fields.get("type").contains(JsString("predicate")) =>
          Add(buildPredicateFromJson(eff))
      }
      val delete: List[Delete] = effects.collect {
        case eff if eff.fields.get("type").contains(JsString("not")) =>
          val exprList = eff.fields("expression").convertTo[List[JsObject]]
          exprList.map { inner =>
            Delete(buildPredicateFromJson(inner))
          }
      }.flatten

      DomainOperator(
        name = name,
        parameters = parameters,
        precondition = precondition,
        add = add,
        delete = delete,
        assignment = List(),
        cost = 0.0
      )
    }
  }

  private def buildMethodsFromJSON(methodsJson: List[JsObject]): List[DomainMethod] = {
    methodsJson.map { obj =>
      val name = obj.fields("name").convertTo[String]
      val precondition: Expression = buildPreconditionFromJSON(obj.fields("preconditions").asJsObject())
      val tasksJson = obj.fields.get("tasks").map(_.convertTo[List[JsObject]]).getOrElse(Nil)
      val tasks = tasksJson.map { buildPredicateFromJson }
      DomainMethod(name = name, precondition = precondition, tasks = TaskList(ordering = "unordered", tasks = tasks))
    }
  }

  private def buildPreconditionFromJSON(preconditions: JsObject): Expression = {
    preconditions.fields.get("type") match {
      case Some(JsString("and")) =>
        val left = preconditions.fields("left").asJsObject
        val right = preconditions.fields("right").asJsObject
        ExpressionAnd(buildPreconditionFromJSON(left), buildPreconditionFromJSON(right))
      case Some(JsString("or")) =>
        val left = preconditions.fields("left").asJsObject
        val right = preconditions.fields("right").asJsObject
        ExpressionOr(buildPreconditionFromJSON(left), buildPreconditionFromJSON(right))
      case Some(JsString("not")) =>
        val expr = preconditions.fields("expression").asJsObject
        ExpressionNot(buildPreconditionFromJSON(expr))
      case Some(JsString("predicate")) =>
        val pred = buildPredicateFromJson(preconditions)
        ExpressionAtomic(pred.name, Bindable(pred.arguments))
      case Some(JsString("nil")) =>
        ExpressionNil()
      case _ =>
        throw new RuntimeException(s"Unknown precondition type: ${preconditions.fields.get("type")}")
    }
  }

  // Problem
  private def buildProblemInitState(problemInitJson: List[JsObject]): State = {
    val state = State(atoms = scala.collection.mutable.Map.empty)
    for (entry <- problemInitJson) {
      val name = entry.fields("name").convertTo[String]
      val parametersJson = entry.fields("parameters").convertTo[List[JsObject]]
      val parameters: List[Term] = buildParametersFromJSON(parametersJson)
      //If init parameters are just List of strings like this "parameters" = ["b2", "b1"]
      //val parameters = entry.fields("parameters").convertTo[List[String]].toArray
      val typ = entry.fields("type").convertTo[String]
      if (typ == "predicate") {
        state.add(name, parameters.toArray)
      }
    }
    state
  }

  private def buildProblemGoalTasks(problemGoalJson: JsObject): TaskList = {
    //TODO: handle ordering properly once ordering is part of the JSON
    //val ordering = problemGoalJson.fields.get("ordering").map(_.convertTo[String]).getOrElse("")
    val tasksJson = problemGoalJson.fields.get("tasks").map(_.convertTo[List[JsObject]]).getOrElse(Nil)
    val tasks = tasksJson.map { buildPredicateFromJson }

    TaskList(ordering = "unordered", tasks = tasks)
    //TaskList(ordering = ordering, tasks = tasks)
  }

  // Helpers
  private def buildPredicateFromJson(predicateJson: JsObject): Predicate = {
    val name = predicateJson.fields("name").convertTo[String]
    val parametersJson = predicateJson.fields("parameters").convertTo[List[JsObject]]
    val parameters: List[Term] = buildParametersFromJSON(parametersJson)
    Predicate(name, parameters)
  }

  private def buildParametersFromJSON(parametersJson: List[JsObject]) = {
    val parameters: List[Term] = parametersJson.map { paramJson =>
      val term = paramJson.fields("name").convertTo[String]
      val typ = paramJson.fields("type").convertTo[String]
      typ match {
        case "Constant" => Constant(term)
        case "Var" => Var(Symbol(term))
        case _ => Constant(term)
      }
    }
    parameters
  }

  //TODO: maby not needed
  private def buildDomainTypes(domainJson: JsObject): List[DomainType] = { List.empty }

  private def buildDomainPredicates(domainJson: JsObject): List[Predicate] = { List.empty }

  private def buildDomainFunctions(domainJson: JsObject): List[Function] = { List.empty }

  private def buildDomainAxioms(domainJson: JsObject): List[Axiom] = { List.empty }

  private def buildProblemObjects(problemJson: JsObject): Objects = {Objects(objects= Nil)}

}