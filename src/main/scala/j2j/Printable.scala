package j2j

import scala.reflect.runtime.universe.{TypeTag, typeTag}
import j2j.ConsoleOps.*

case class PrinterContext(indent: Int) {
  def indented: PrinterContext = copy(indent = indent + 1)
}

trait Printable {

  def print(implicit context: PrinterContext): String

  override def toString: String = print(PrinterContext(0))

  def printToNewLine(implicit context: PrinterContext): String = newLine(print)

  protected def newLine(s: String)(implicit context: PrinterContext): String =
    "\n" + ("  " * context.indent) + s

  protected def indent(u: Printable)(implicit context: PrinterContext): String =
    u.printToNewLine(context.indented)

}

object ConsoleOps {
  def bold(s: String): String    = Console.BOLD + s + Console.RESET
  def blue(s: String): String    = Console.BLUE + s + Console.RESET
  def cyan(s: String): String    = Console.CYAN + s + Console.RESET
  def green(s: String): String   = Console.GREEN + s + Console.RESET
  def magenta(s: String): String = Console.MAGENTA + s + Console.RESET
  def sqbr(s: String): String    = "[" + s + "]"
}

trait TypeString[-T] {
  def print: String
}

object TypeString {
  implicit def printer[T: TypeTag]: TypeString[T] = new TypeString[T] {
    override def print: String = typeTag[T].tpe.toString
  }

  implicit def vector[T](implicit ts: TypeString[T]): TypeString[Vector[T]] = new TypeString[Vector[T]] {
    override def print: String = s"Vector(${ts.print})"
  }

  implicit def option[T](implicit ts: TypeString[T]): TypeString[Option[T]] = new TypeString[Option[T]] {
    override def print: String = s"Option(${ts.print})"
  }

  def apply[T](implicit printer: TypeString[T]): String = printer.print
}
