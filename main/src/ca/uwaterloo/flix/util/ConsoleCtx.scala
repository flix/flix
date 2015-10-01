package ca.uwaterloo.flix.util

trait ConsoleCtx {

  def blue(s: String): String

  def green(s: String): String

  def red(s: String): String

}

class WindowsConsole extends ConsoleCtx {
  def blue(s: String): String = s

  def green(s: String): String = s

  def red(s: String): String = s
}

class AnsiConsole extends ConsoleCtx {
  def blue(s: String): String = Console.BLUE + s + Console.RESET

  def green(s: String): String = Console.GREEN + s + Console.RESET

  def red(s: String): String = Console.RED + s + Console.RESET
}
