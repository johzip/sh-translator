package org.planx.sh.parsing.hpdl

import java.io.{File, PrintWriter}
import org.planx.sh.problem.{ Axiom, Constant, Method, Operator, Predicate, Problem, Task, TaskList, Term, Var}
import org.planx.sh.solving.{State ,Bindable, Expression, ExpressionAnd, ExpressionAtomic, ExpressionNil, ExpressionNot, ExpressionOr, InstanceUnifier, TaskUnifier}

class JSONParser(tasks: List[Task], operators: List[Operator], axioms: List[Axiom],domainName: String, problem: Problem) {

  def generateJSON(): String = {
    val primitiveTasks = operators
    val compoundTasks = tasks.filter(t => !operators.exists(_._name == t._name))

    val goalTasks = problem.goalTaskList
    val goalTasksJson = tasksCallToJSON(problem.goalTaskList)
    val initStateJson = generateInitStateJSON(problem.state)

    val primitiveTasksJson = primitiveTasks.map(taskToJSON).mkString(",\n                ")
    val compoundTasksJson = compoundTasks.map(compoundTaskToJSON).mkString(",\n                ")

  s"""{
    "$domainName": {
        "requirements": [
            "strips",
            "typing"
        ],
        "problem": {
            "goal": {
                "tasks": [
                    $goalTasksJson
                ]
            },
            "init": [
                $initStateJson
            ]
        },
        "domain": {
            "name": "$domainName",
            "primitive_tasks": [
                $primitiveTasksJson
            ],
            "compund_tasks": [
                $compoundTasksJson
            ]
        }
    }
  }"""
  }


  private def loadParam(params: List[Any]): String = {
    val paramJSON = new StringBuilder
    for (param <- params) {
      paramJSON.append(paramToJSON(param)).append(",\n")
    }
    paramJSON.toString().stripSuffix(",\n")
  }

  private def taskToJSON(primTask: Operator): String = {
    val correspondingOperator = operators.find(_._name == primTask._name)
    val precondition = expressionToJSON(primTask.precondition)
    val parametersJson = primTask.parameters.map(paramToJSON).mkString(",\n                        ")

    correspondingOperator match {
      case Some(op) =>
        s"""{
            "name": "${primTask._name}",
            "parameters": [
                $parametersJson
            ],
            "precondition": $precondition,
            "effect": {}
        }"""
      case None =>
        s"""{
            "name": "${primTask._name}",
            "parameters": [
                $parametersJson
            ]
        }"""
    }
  }

  private def compoundTaskToJSON(task: Task): String = {
    val parametersJson = task.parameters.map(paramToJSON).mkString(",\n                        ")
    val methodsJson = task.methods.map(methodToJSON).mkString(",\n                        ")

    s"""{
        "name": "${task._name}",
        "parameters": [
            $parametersJson
        ],
        "methods": [
            $methodsJson
        ]
    }"""
  }

  private def methodToJSON(method: Method): String = {
    val method_name = method.name
    val preconditions = expressionToJSON(method.precondition)
    val taskCalls = tasksCallToJSON(method.taskList)
    s"""{
      "name": "$method_name",
      "preconditions": $preconditions,
      "tasks": $taskCalls
    }"""
  }

  private def paramToJSON(param: Any): String = {
    param match {
      case v: Var =>
        s"""{
          "name": "${v.name.name}",
          "type": "${v._type}"
      }"""
      case c: Constant =>
        s"""{
          "name": "${c.c}",
          "type": "constant"
      }"""
      case _ =>
        s"""{
          "name": "unknown",
          "type": "unknown"
      }"""
    }
  }

  def expressionToJSON(expression: Expression): String = {
    expression match {
      case atomic: ExpressionAtomic =>
        val parameters = atomic.blueprint.arguments.map(paramToJSON).mkString(",")
        s"""{
        "type": "atomic",
        "predicate": "${atomic.predicateName}",
        "parameters": [$parameters]
      }"""

      case and: ExpressionAnd =>
        val leftJson = expressionToJSON(and.left)
        val rightJson = expressionToJSON(and.right)
        s"""{
        "type": "and",
        "left": $leftJson,
        "right": $rightJson
      }"""

      case or: ExpressionOr =>
        val leftJson = expressionToJSON(or.left)
        val rightJson = expressionToJSON(or.right)
        s"""{
        "type": "or",
        "left": $leftJson,
        "right": $rightJson
      }"""

      case not: ExpressionNot =>
        val innerJson = expressionToJSON(not.precond)
        s"""{
        "type": "not",
        "expression": $innerJson
      }"""

      case _: ExpressionNil =>
        s"""{
        "type": "nil"
      }"""

      case _ =>
        s"""{
        "type": "unknown",
        "class": "${expression.getClass.getSimpleName}"
      }"""
    }
  }

  private def termToJSON(term: Any): String = {
    s"""{
       |    "term": "${term.toString}",
       |    "type": "${term.getClass.getSimpleName}"
       |}""".stripMargin
  }

  private def tasksCallToJSON(taskCalls: TaskList): String = {
    taskCalls.tasks.map { task =>
      task match {
        case predicate: org.planx.sh.problem.Predicate =>
          val parametersJson = predicate.arguments.map(termToJSON).mkString(",\n" + " " * 16)
          s"""{
             |    "name": "${predicate.name}",
             |    "type": "predicate",
             |    "parameters": [
             |        $parametersJson
             |    ]
             |}""".stripMargin
        case instance: InstanceUnifier =>
          val parametersJson = instance.arguments.map(termToJSON).mkString(",\n" + " " * 16)
          s"""{
             |    "name": "${instance.name}",
             |    "type": "instance",
             |    "parameters": [
             |        $parametersJson
             |    ]
             |}""".stripMargin
        case nestedTaskList: TaskList =>
          tasksCallToJSON(nestedTaskList) // Rekursiver Aufruf für verschachtelte TaskLists
        case _ =>
          s"""{
             |    "name": "unknown",
             |    "type": "${task.getClass.getSimpleName}",
             |    "parameters": []
             |}""".stripMargin
      }
    }.mkString(",\n" + " " * 8)
  }

  def writeToFile(filename: String = "tasks.json"): Unit = {
    val jsonString = generateJSON()

    val file = new File(filename)
    val writer = new PrintWriter(file)
    try {
      writer.write(jsonString)
    } finally {
      writer.close()
    }
  }

  private def generateInitStateJSON(init: State): String = {
    val stateItems = for {
      (atomName, container) <- init.atoms
      arity <- container.byarity.keys
      argumentsList <- container.byarity(arity)
    } yield {
      val parametersJson = argumentsList.map(arg => s""""$arg"""").mkString(", ")
      s"""{
        "predicate": "$atomName",
        "parameters": [$parametersJson]
    }"""
    }

    stateItems.mkString(",\n                ")
  }
}