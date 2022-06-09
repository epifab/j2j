package j2j

case class PrinterContext(indent: Int) {
  def indented: PrinterContext = copy(indent = indent + 1)
}

trait PrettyPrinter[T] {

  def print(t: T)(implicit context: PrinterContext): String

  def printToNewLine(t: T)(implicit context: PrinterContext): String =
    newLine(print(t))

  def newLine(s: String)(implicit context: PrinterContext): String =
    "\n" + ("  " * context.indent) + s

  def indent[U](u: U)(implicit printer: PrettyPrinter[U], context: PrinterContext): String =
    printer.printToNewLine(u)(context.indented)

  def bold(s: String): String  = Console.BOLD + s + Console.RESET
  def blue(s: String): String  = Console.BLUE + s + Console.RESET
  def cyan(s: String): String  = Console.CYAN + s + Console.RESET
  def green(s: String): String = Console.GREEN + s + Console.RESET

}

object PrettyPrinter {
  def apply[T](implicit printer: PrettyPrinter[T]): PrettyPrinter[T] = printer
}
