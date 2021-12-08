package metalepsis

import cats.syntax.all.*

trait Display:
  def display: String

final case class Label[A](label: String) extends Display:
  override def display = label

final case class Options[A](
  options: List[String],
) extends Display:
  override def display: String =
    options.intercalate(" or ")

final case class MenuItem[A](
  item: Label[A],
  options: Options[A],
) extends Display:
  override def display =
    s"${item.display}: ${options.display}"
