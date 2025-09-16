package org.planx.sh.parsing.hpdl

import spray.json._
import DefaultJsonProtocol._
import java.io.File
import scala.io.Source

import org.planx.sh.problem.{Add, Delete, EmptyEffect, ForallEffect, NumericAssignment}
import org.planx.sh.problem.{Axiom, Constant, Method, Operator, Predicate, Problem, Task, TaskList, Term, Var}
import org.planx.sh.solving.{State ,Bindable, Expression, ExpressionAnd, ExpressionAtomic, ExpressionNil, ExpressionNot, ExpressionOr, InstanceUnifier, TaskUnifier}


class FromJSONParser {

  def parseFile(filename: String): (String, List[String], Problem, List[Operator], List[Task]) = {
    val source = Source.fromFile(filename)
    val jsonStr = try source.mkString finally source.close()
    val json = jsonStr.parseJson.asJsObject

    val domainName = json.fields.keys.head
    val domainJson = json.fields(domainName).asJsObject

    val requirements = domainJson.fields("requirements").convertTo[List[String]]

    (domainName, requirements, null, Nil, Nil)
  }
}