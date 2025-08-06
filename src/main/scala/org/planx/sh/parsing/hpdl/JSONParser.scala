package org.planx.sh.parsing.hpdl

import java.io.{File, PrintWriter}
import org.planx.sh.problem.{Task, Operator}

class JSONParser(tasks: List[Task], operators: List[Operator], domainName: String) {

  def generateJSON(): String = {
    val primitiveTasks = tasks.filter(t => operators.exists(_._name == t._name))
    val compoundTasks = tasks.filter(t => !operators.exists(_._name == t._name))

    val primitiveTasksJson = primitiveTasks.map(taskToJSON).mkString(",\n                ")
    val compoundTasksJson = compoundTasks.map(compoundTaskToJSON).mkString(",\n                ")

    s"""{
    "$domainName": {
        "requirements": [
            "strips",
            "typing"
        ],
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
                        "name": "method-name",
                        "preconditions": [],
                        "tasks": []
                    }"""
  }

  private def paramToJSON(param: Any): String = {
    s"""{
                            "name": "param-name",
                            "type": "thing"
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