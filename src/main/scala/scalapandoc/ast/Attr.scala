package scalapandoc.ast

/** Pandoc API version compatibility */
case class ApiVersion(major: Int, minor: Int, revision: Int)

object ApiVersion:
  val current = ApiVersion(1, 23, 0)

/** Simple attribute tuple (identifier, classes, key-value pairs) */
case class Attr(identifier: String, classes: List[String], attributes: List[(String, String)])

object Attr:
  val empty = Attr("", List.empty, List.empty)
