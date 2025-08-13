package org.planx.sh.parsing.hpdl

import java.io.{File, PrintWriter}
import org.planx.sh.problem.{Operator, Problem, Method, Task, Var, Constant, TaskList}
import org.planx.sh.solving.{Expression, ExpressionAtomic, ExpressionAnd, ExpressionOr, ExpressionNot, ExpressionNil, Bindable}

class JSONParser(tasks: List[Task], operators: List[Operator], domainName: String, problem: Problem) {

  def generateJSON(): String = {
    val primitiveTasks = tasks.filter(t => operators.exists(_._name == t._name))
    val compoundTasks = tasks.filter(t => !operators.exists(_._name == t._name))

    val goalTasks = problem.goalTaskList.tasks
    val goalTasksJson = goalTasks.map(gt => goalTaskToJSON(gt.asInstanceOf[Task])).mkString(",\n                    ")
    val initStateJson = generateInitStateJSON()

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


  private def goalTaskToJSON(goalTask: Task): String = {

    println("goalTask.parameters: "+goalTask.parameters)
    println("goalTask._name: "+goalTask._name)
    val goalTaskName = goalTask._name
    val parameterStringList = loadParam(goalTask.parameters)


    s"""{
        "name": "$goalTaskName",
        "parameters": [ $parameterStringList
        ]
    }"""
  }


  private def loadParam(params: List[Any]): String = {
    val paramJSON = new StringBuilder
    for (param <- params) {
      paramJSON.append(paramToJSON(param)).append(",\n")
    }
    paramJSON.toString().stripSuffix(",\n")
  }

  private def taskToJSON(task: Task): String = {
    val correspondingOperator = operators.find(_._name == task._name)
    val parametersJson = task.parameters.map(paramToJSON).mkString(",\n                        ")

    correspondingOperator match {
      case Some(op) =>
        s"""{
            "name": "${task._name}",
            "parameters": [
                $parametersJson
            ],
            "precondition": [],
            "effect": {}
        }"""
      case None =>
        s"""{
            "name": "${task._name}",
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
        "name": $method_name,
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


  private def tasksCallToJSON(taskCalls: TaskList): String = {
    tasks.map { task =>
      val paramsJson = task.parameters.map { param =>
        s"""{
           |    "name": "${param.asInstanceOf[{def _name: String}]._name}",
           |    "type": "${param.asInstanceOf[{def _type: String}]._type}"
           |}""".stripMargin
        }.mkString(",\n" + " " * 40)
        s"""{
           |    "primitive-task": "${task._name}",
           |    "parameters": [
           |        $paramsJson
           |    ]
           |}""".stripMargin
    }.mkString(",\n" + " " * 32)
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

  private def generateInitStateJSON(): String = {
    s"""{
       |    "name": "kiwi",
       |    "type": "thing",
       |    "var": {
       |        "name": "have",
       |        "value": true
       |    }
       |}""".stripMargin
  }
}