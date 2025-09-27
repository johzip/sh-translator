package org.planx.sh.parsing.hpdl

import spray.json._
import DefaultJsonProtocol._

import java.io.File
import scala.io.Source
import org.planx.sh.problem.{Add, Axiom, Constant, Delete, Domain, DomainMethod, DomainOperator, DomainType, EmptyEffect, ForallEffect, Method, NumericAssignment, Objects, Operator, Predicate, Problem, Task, TaskList, Term, Var}
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

    //TODO: these fields are missing in the JSON ?
    val domaintypes = buildDomainTypes(domainJson)
    val domainPredicates = buildDomainPredicates(domainJson)
    val domainFunctions = buildDomainFunctions(domainJson)
    val domainAxioms = buildDomainAxioms(domainJson)

    //TODO: implement domainTasks
    val domainOperators = buildDomainOperators(domainJson.fields("primitive_tasks").convertTo[List[JsObject]])
    val domainTasks = buildDomainTasks(domainJson.fields("compound_tasks").convertTo[List[JsObject]])

    //TODO: problemObjects is missing in the JSON
    val problemObjects = buildProblemObjects(problemJson)
    val problemInitState = buildProblemInitState(problemJson.fields("init").convertTo[List[JsObject]])
    val problemGoalTasks = buildProblemGoalTasks(problemJson.fields("goal").convertTo[JsObject])


    val domain = Domain(name = domainName, requirements = domainRequirements, types = Nil, predicates = Nil, functions = Nil, _operators = domainOperators, uncoupledTasks = Nil, axioms = Nil)
    //TODO: missing ProblemName (is it a Problem?)
    val problem = Problem(name = "problem", domainName = domain.name, requirements = problemRequirements, objects = Objects(objects= Nil), state = problemInitState, goalTaskList = problemGoalTasks)
    testResult(domain, problem, domainToCompare, problemToCompare)
    (domain, problem)
  }

  private def testResult(domain: Domain, problem: Problem, domainToCompare: Domain, problemToCompare: Problem): Boolean = {
    // Implement comparison logic here
    true
  }

  private def buildDomainOperators(domainJson: List[JsObject]): List[DomainOperator] = {
    domainJson.map { obj =>
      val name = obj.fields("name").convertTo[String]
      val parametersJson = obj.fields("parameters").convertTo[List[JsObject]]
      val parameters: List[Term] = buildParametersFromJSON(parametersJson)
      val precondition: Expression = buildPreconditionFromJSON(obj.fields("precondition").asJsObject())

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

  private def buildDomainTasks(domainJson: List[JsObject]): List[Task] = {
    List.empty
  }

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

  private def buildProblemGoalTasks(problemGoalJson: JsObject): TaskList = {
    //TODO: handle ordering properly once ordering is part of the JSON
    //val ordering = problemGoalJson.fields.get("ordering").map(_.convertTo[String]).getOrElse("")
    val tasksJson = problemGoalJson.fields.get("tasks").map(_.convertTo[List[JsObject]]).getOrElse(Nil)
    val tasks = tasksJson.map { buildPredicateFromJson }

    TaskList(ordering = "unordered", tasks = tasks)
    //TaskList(ordering = ordering, tasks = tasks)
  }

  //TODO: maby not needed
  private def buildDomainTypes(domainJson: JsObject): List[DomainType] = { List.empty }

  private def buildDomainPredicates(domainJson: JsObject): List[Predicate] = { List.empty }

  private def buildDomainFunctions(domainJson: JsObject): List[Function] = { List.empty }

  private def buildDomainAxioms(domainJson: JsObject): List[Axiom] = { List.empty }

  private def buildProblemObjects(problemJson: JsObject): Objects = {Objects(objects= Nil)}

}