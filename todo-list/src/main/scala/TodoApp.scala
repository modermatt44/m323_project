import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

object TodoApp {

  private object ConsoleColors {
    val RESET = "\u001B[0m"
    val RED = "\u001B[31m"
    val GREEN = "\u001B[32m"
  }

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

  private def filterTodosByCategory(todos: List[Todo], category: String): List[Todo] = {
    todos.filter(_.category == category)
  }

  private def filterTodosByDeadline(todos: List[Todo], deadline: LocalDate): List[Todo] = {
    todos.filter(_.deadline.isEqual(deadline))
  }

  private def searchTodos(todos: List[Todo], query: String): List[Todo] = {
    todos.filter(todo => todo.task.contains(query) || todo.category.contains(query))
  }

  private def displayTodos(todos: List[Todo]): Unit = {
    if (todos.isEmpty) {
      println(ConsoleColors.RED + "No todos to display." + ConsoleColors.RESET)
    } else {
      todos.foreach { todo =>
        val color = if (todo.completed) ConsoleColors.GREEN else ConsoleColors.RED
        println(color + s"ID: ${todo.id}, Task: ${todo.task}, Category: ${todo.category}, Deadline: ${todo.deadline}, Completed: ${todo.completed}" + ConsoleColors.RESET)
      }
    }
  }

  private def saveTodos(todos: List[Todo]): Unit = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val content = todos.map(todo => s"${todo.id},${todo.task},${todo.category},${todo.deadline.format(formatter)},${todo.completed}").mkString("\n")
    Files.write(Paths.get("todos.txt"), content.getBytes, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }

  private def loadTodos(): List[Todo] = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val path = Paths.get("todos.txt")
    if (Files.exists(path)) {
      val lines = Files.readAllLines(path)
      lines.asScala.toList.flatMap { line =>
        val parts = line.split(",")
        if (parts.length == 5) {
          Try {
            val id = parts(0).toInt
            val task = parts(1)
            val category = parts(2)
            val deadline = LocalDate.parse(parts(3), formatter)
            val completed = parts(4).toBoolean
            Some(Todo(id, task, category, deadline, completed))
          }.toOption.flatten.toList
        } else {
          List.empty[Todo]
        }
      }
    } else {
      println("No existing todos file found. Starting with an empty list.")
      List.empty[Todo]
    }
  }

  def main(args: Array[String]): Unit = {
    val initialTodos = loadTodos()
    val finalTodos = mainLoop(initialTodos)
    saveTodos(finalTodos)
  }

  @tailrec
  private def mainLoop(todos: List[Todo]): List[Todo] = {
    println("\nTo-Do List Management")
    println("1. Add a new to-do")
    println("2. Display all to-dos")
    println("3. Mark to-do as completed")
    println("4. Update to-do")
    println("5. Delete to-do")
    println("6. Filter to-dos by category")
    println("7. Filter to-dos by deadline")
    println("8. Search to-dos")
    println("9. Exit")

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
        val category = readLine("Enter category to filter: ")
        val results = filterTodosByCategory(todos, category)
        displayTodos(results)
        todos
      case "7" =>
        val deadlineStr = readLine("Enter deadline to filter (yyyy-MM-dd): ")
        Try(LocalDate.parse(deadlineStr)) match {
          case Success(deadline) =>
            val results = filterTodosByDeadline(todos, deadline)
            displayTodos(results)
          case Failure(_) =>
            println("Invalid date format. Please enter the date in yyyy-MM-dd format.")
        }
        todos
      case "8" =>
        val query = readLine("Enter search query: ")
        val results = searchTodos(todos, query)
        displayTodos(results)
        todos
      case "9" =>
        println("Exiting...")
        todos
      case _ =>
        println("Invalid choice. Please try again.")
        todos
    }
    if (choice == "9") manageTodos else mainLoop(manageTodos)
  }
}