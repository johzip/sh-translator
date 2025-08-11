package org.planx.sh.parsing.hpdl

import java.io.{File, PrintWriter}
import org.planx.sh.problem.{Task, Problem, Operator}

class JSONParser(tasks: List[Task], operators: List[Operator], domainName: String, problem: Problem) {

  def generateJSON(): String = {
    val primitiveTasks = tasks.filter(t => operators.exists(_._name == t._name))
    val compoundTasks = tasks.filter(t => !operators.exists(_._name == t._name))

    val primitiveTasksJson = primitiveTasks.map(taskToJSON).mkString(",\n                ")
    val compoundTasksJson = compoundTasks.map(compoundTaskToJSON).mkString(",\n                ")

    val problemJson = generateProblemJSON()

    s"""{
    "$domainName": {
        "requirements": [
            "strips",
            "typing"
        ],
        $problemJson,
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

  private def generateProblemJSON(): String = {
    val goalTasks = problem.goalTaskList.tasks
    val goalTasksJson = goalTasks.map(goalTaskToJSON).mkString(",\n                    ")
    val initStateJson = generateInitStateJSON()

    s""""problem": {
            "goal": {
                "tasks": [
                    $goalTasksJson
                ]
            },
            "init": [
                $initStateJson
            ]
        }"""
  }

  private def goalTaskToJSON(goalTask: Any): String = {
    // Da goalTask vom Typ TaskUnifier ist, nicht Task
    s"""{
                        "name": "swap",
                        "parameters": $loadParam()
                    }"""
  }


  private def loadParam(params: List[Any]): String = {
    val paramJSON = new StringBuilder
    for (param <- params) {
      paramJSON.append(paramToJSON(param)).append(",\n")
    }
    paramJSON.toString().stripSuffix(",\n")
  }

  private def paramToJSON(param: Any): String = {
    s"""{
                            "name": $name,
                            "type": $type
                        }"""
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
                    "subtasks": [
                        $methodsJson
                    ]
                }"""
  }

  private def methodToJSON(method: Any): String = {
    s"""{
                        "name": $method_name,
                        "preconditions": $loadPreconditions(),
                        "tasks": $loadTasks()
                    }"""
  }

  private def paramToJSON(param: Any): String = {
    s"""{
                            "name": $param_name,
                            "type": $param_type
                        }"""
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
}