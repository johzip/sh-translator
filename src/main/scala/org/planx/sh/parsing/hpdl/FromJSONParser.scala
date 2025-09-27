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

    //TODO: implement domainOperators and domainTasks
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
    println("domain primitive tasks NEW: ")
    domain.operators.foreach { op =>
      println(s"Name: ${op._name}")
      println(s"Parameter: ${op.parameters}")
      println(s"Präcondition: ${op.precondition}")
      println(s"Add-Effekte: ${op.add}")
      println(s"Delete-Effekte: ${op.delete}")
      println(s"Assignment: ${op.assignment}")
      println(s"Kosten: ${op.cost}")
      println("-----")
    }
    println("domain primitive tasks ORIGINAL: ")
    domainToCompare.operators.foreach { op =>
      println(s"Name: ${op._name}")
      println(s"Parameter: ${op.parameters}")
      println(s"Präcondition: ${op.precondition}")
      println(s"Add-Effekte: ${op.add}")
      println(s"Delete-Effekte: ${op.delete}")
      println(s"Assignment: ${op.assignment}")
      println(s"Kosten: ${op.cost}")
      println("-----")
    }
    true
  }

  private def buildDomainOperators(domainJson: List[JsObject]): List[DomainOperator] = {
    domainJson.map { obj =>
      val name = obj.fields("name").convertTo[String]
      val parametersJson = obj.fields("parameters").convertTo[List[JsObject]]
      val parameters: List[Term] = buildParametersFromJSON(parametersJson)
      //TODO: doesn't work yet
      val precondition = obj.fields("precondition") match {
        case jsObj: JsObject if jsObj.fields.get("type").contains(JsString("nil")) => ExpressionNil()
        case _ => ExpressionNil()
      }
      val effects = obj.fields.get("effect").map(_.convertTo[List[JsObject]]).getOrElse(Nil)
      //TODO: doesn't work yet
      val add: List[Add] = effects.collect {
        case eff if eff.fields.get("type").contains(JsString("add")) =>
          Add(buildPredicateFromJson(eff.fields("predicate").asJsObject))
      }
      //TODO: doesn't work yet
      val delete: List[Delete] = effects.collect {
        case eff if eff.fields.get("type").contains(JsString("delete")) =>
          Delete(buildPredicateFromJson(eff.fields("predicate").asJsObject))
      }

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