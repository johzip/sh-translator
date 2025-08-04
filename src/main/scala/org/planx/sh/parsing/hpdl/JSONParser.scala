package org.planx.sh.parsing.hpdl

import java.io.{File, PrintWriter}
import org.planx.sh.problem.Task

class JSONParser(tasks: List[Task]) {
  val taskList = tasks
  val jsonFile = new File("tasks.json")
  val writer = new PrintWriter(jsonFile)
  writer.write("[\n")
  
  

}