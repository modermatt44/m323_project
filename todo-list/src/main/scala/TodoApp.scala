import java.time.LocalDate
import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object TodoApp {

  case class Todo(id: Int, task: String, category: String, deadline: LocalDate, completed: Boolean = false) {
    def complete: Todo = this.copy(completed = true)
  }

  private def addTodo(todos: List[Todo], task: String, category: String, deadline: LocalDate): List[Todo] = {
    val newId = if (todos.isEmpty) 1 else todos.map(_.id).max + 1
    todos :+ Todo(newId, task, category, deadline)
  }

  private def updateTodo(todos: List[Todo], id: Int, task: String, category: String, deadline: LocalDate): List[Todo] = {
    todos.map {
      case todo if todo.id == id => todo.copy(task = task, category = category, deadline = deadline)
      case todo => todo
    }
  }

  private def deleteTodo(todos: List[Todo], id: Int): List[Todo] = {
    todos.filterNot(_.id == id)
  }

  private def completeTodo(todos: List[Todo], id: Int): List[Todo] = {
    todos.map {
      case todo if todo.id == id => todo.complete
      case todo => todo
    }
  }

  private def displayTodos(todos: List[Todo]): Unit = {
    if (todos.isEmpty) {
      println("No todos to display.")
    } else {
      todos.foreach { todo =>
        println(s"ID: ${todo.id}, Task: ${todo.task}, Category: ${todo.category}, Deadline: ${todo.deadline}, Completed: ${todo.completed}")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val initialTodos = List.empty[Todo]
    val finalTodos = mainLoop(initialTodos)
  }

  @tailrec
  private def mainLoop(todos: List[Todo]): List[Todo] = {
    println("\nTo-Do List Management")
    println("1. Add a new to-do")
    println("2. Display all to-dos")
    println("3. Mark to-do as completed")
    println("4. Update to-do")
    println("5. Delete to-do")
    println("6. Exit")

    val choice = readLine("Choose an option: ")

    val manageTodos = choice match {
      case "1" =>
        val task = readLine("Enter task: ")
        val category = readLine("Enter category: ")
        val deadlineStr = readLine("Enter deadline (yyyy-MM-dd): ")
        Try(LocalDate.parse(deadlineStr)) match {
          case Success(deadline) => addTodo(todos, task, category, deadline)
          case Failure(_) =>
            println("Invalid date format. Please enter the date in yyyy-MM-dd format.")
            todos
        }
      case "2" =>
        displayTodos(todos)
        todos
      case "3" =>
        val id = Try(readLine("Enter to-do ID to mark as completed: ").toInt).getOrElse(-1)
        if (todos.exists(_.id == id)) {
          completeTodo(todos, id)
        } else {
          println("To-do ID not found.")
          todos
        }
      case "4" =>
        val id = Try(readLine("Enter to-do ID to update: ").toInt).getOrElse(-1)
        if (todos.exists(_.id == id)) {
          val task = readLine("Enter new task: ")
          val category = readLine("Enter new category: ")
          val deadlineStr = readLine("Enter new deadline (yyyy-MM-dd): ")
          Try(LocalDate.parse(deadlineStr)) match {
            case Success(deadline) => updateTodo(todos, id, task, category, deadline)
            case Failure(_) =>
              println("Invalid date format. Please enter the date in yyyy-MM-dd format.")
              todos
          }
        } else {
          println("To-do ID not found.")
          todos
        }
      case "5" =>
        val id = Try(readLine("Enter to-do ID to delete: ").toInt).getOrElse(-1)
        if (todos.exists(_.id == id)) {
          deleteTodo(todos, id)
        } else {
          println("To-do ID not found.")
          todos
        }
      case "6" =>
        println("Exiting...")
        todos
      case _ =>
        println("Invalid choice. Please try again.")
        todos
    }
    if (choice == "6") manageTodos else mainLoop(manageTodos)
  }
}
