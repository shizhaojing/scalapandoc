package scalapandoc.writer

import scalapandoc.ast.*

/** Writes Pandoc AST to Markdown format
 *
 * Simplified implementation for the initial version.
 */
object MarkdownWriter:

  /** Write a Pandoc document to Markdown string */
  def write(doc: Pandoc): String =
    val sb = new StringBuilder

    // Write metadata
    if doc.meta.title.nonEmpty then
      sb.append("title: ")
      sb.append(writeInlines(doc.meta.title))
      sb.append("\n")

    // Write blocks
    for block <- doc.blocks do
      sb.append(writeBlock(block))
      sb.append("\n\n")

    sb.toString.trim

  /** Write a block element */
  private def writeBlock(block: Block): String =
    block match
      case Block.Para(contents) =>
        writeInlines(contents)

      case Block.Plain(contents) =>
        writeInlines(contents)

      case Block.Headline(level, _, contents) =>
        val hashes = "#" * level
        s"$hashes ${writeInlines(contents)}"

      case Block.CodeBlock(_, lines) =>
        val content = lines.mkString("\n")
        s"```\n$content\n```"

      case Block.BlockQuote(contents) =>
        contents.map(b => "> " + writeBlock(b)).mkString("\n")

      case Block.BulletList(items) =>
        items.map { item =>
          item.map(b => "- " + writeBlock(b).trim).mkString("\n")
        }.mkString("\n")

      case Block.OrderedList(start, items) =>
        items.zipWithIndex.map { case (item, idx) =>
          val num = start + idx
          item.map(b => s"$num. " + writeBlock(b).trim).mkString("\n")
        }.mkString("\n")

      case Block.HorizontalRule =>
        "---"

      case Block.Null => ""

  /** Write inline elements */
  private def writeInlines(inlines: List[Inline]): String =
    inlines.map(writeInline).mkString

  /** Write a single inline element */
  private def writeInline(inline: Inline): String =
    inline match
      case Inline.Str(s) => s
      case Inline.Space => " "
      case Inline.SoftBreak => "\n"
      case Inline.LineBreak => "  \n"
      case Inline.Emph(contents) => s"*${writeInlines(contents)}*"
      case Inline.Strong(contents) => s"**${writeInlines(contents)}**"
      case Inline.Strikeout(contents) => s"~~${writeInlines(contents)}~~"
      case Inline.Code(_, text) => s"`$text`"
      case Inline.Link(_, contents, (url, _)) =>
        s"[${writeInlines(contents)}]($url)"
      case Inline.Image(_, contents, (url, _)) =>
        s"![${writeInlines(contents)}]($url)"
      case Inline.RawInline(_, text) => text

  /** Write to a file */
  def writeFile(doc: Pandoc, path: String): Unit =
    import java.io.{File, PrintWriter}
    val pw = new PrintWriter(new File(path))
    try
      pw.write(write(doc))
    finally
      pw.close()
