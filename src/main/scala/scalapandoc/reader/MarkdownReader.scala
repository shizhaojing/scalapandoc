package scalapandoc.reader

import scalapandoc.ast.*
import scalapandoc.ast.TypeTags.*

/** Reads Markdown and converts to Pandoc-compatible AST
 *
 * Simplified implementation using basic string parsing.
 * For production, should use a proper Markdown parser.
 */
object MarkdownReader:

  /** Parse Markdown string into Pandoc document */
  def read(markdown: String): Pandoc =
    val lines = markdown.linesIterator.toList
    val (meta, blocks) = parseDocument(lines)
    Pandoc(meta, blocks)

  /** Simple document parser
   *
   * Extracts title from first # heading and converts blocks.
   */
  private def parseDocument(lines: List[String]): (Meta, List[Block]) =
    var meta = Meta(Nil, Nil, Nil)
    var blocks = List.empty[Block]
    var i = 0

    while i < lines.length do
      val line = lines(i)
      if line.startsWith("#") then
        val level = line.takeWhile(_ == '#').length
        val text = line.drop(level).trim
        if meta.title.isEmpty && level == 1 then
          meta = meta.copy(title = parseInline(text))
        else
          blocks = blocks :+ Block.Headline(level, Attr.empty, parseInline(text))
      else if line.trim.nonEmpty && !line.trim.startsWith("```") then
        // Paragraph (simplified - one line per paragraph)
        blocks = blocks :+ Block.Para(parseInline(line))
      else if line.trim.startsWith("```") then
        // Code block
        val lang = line.trim.drop(3).trim
        i = i + 1
        val codeLines = scala.collection.mutable.ListBuffer[String]()
        while i < lines.length && !lines(i).trim.startsWith("```") do
          codeLines += lines(i)
          i = i + 1
        blocks = blocks :+ Block.CodeBlock(Attr.empty, codeLines.toList)
      else if line.trim.startsWith("---") then
        // Horizontal rule
        blocks = blocks :+ Block.HorizontalRule
      else if line.trim.startsWith("*") || line.trim.startsWith("-") then
        // Bullet list item (simplified)
        val text = line.dropWhile(c => c == '*' || c == '-' || c == ' ').trim
        blocks = blocks :+ Block.BulletList(List(List(Block.Para(parseInline(text)))))
      else if line.nonEmpty && line.head.isDigit then
        // Ordered list item (simplified)
        val text = line.dropWhile(c => c.isDigit || c == '.' || c == ' ').trim
        blocks = blocks :+ Block.OrderedList(1, List(List(Block.Para(parseInline(text)))))
      i = i + 1

    (meta, blocks)

  /** Parse inline elements from a text string
   *
   * Very simplified implementation - handles basic formatting.
   */
  private def parseInline(text: String): List[Inline] =
    // Simple regex-based parsing for common patterns
    var result = List.empty[Inline]
    var remaining = text

    // Handle bold **text**
    val boldPattern = """\*\*(.+?)\*\*""".r
    remaining = boldPattern.replaceAllIn(remaining, { m =>
      val content = m.group(1)
      result = result :+ Inline.Strong(parseInline(content))
      ""
    })

    // Handle italic *text*
    val italicPattern = """\*(.+?)\*""".r
    remaining = italicPattern.replaceAllIn(remaining, { m =>
      val content = m.group(1)
      result = result :+ Inline.Emph(parseInline(content))
      ""
    })

    // Handle code `text`
    val codePattern = """`(.+?)`""".r
    remaining = codePattern.replaceAllIn(remaining, { m =>
      val content = m.group(1)
      result = result :+ Inline.Code(Attr.empty, content)
      ""
    })

    // Handle links [text](url)
    val linkPattern = """\[(.+?)\]\((.+?)\)""".r
    remaining = linkPattern.replaceAllIn(remaining, { m =>
      val text = m.group(1)
      val url = m.group(2)
      result = result :+ Inline.Link(Attr.empty, parseInline(text), (url, ""))
      ""
    })

    // Remaining text
    if remaining.nonEmpty then
      // Split by words and add spaces
      val words = remaining.split("\\s+")
      for (word, idx) <- words.zipWithIndex do
        if word.nonEmpty then
          result = result :+ Inline.Str(word)
        if idx < words.length - 1 then
          result = result :+ Inline.Space

    result

  /** Read markdown from a file */
  def readFile(path: String): Pandoc =
    val source = scala.io.Source.fromFile(path)
    try
      read(source.mkString)
    finally
      source.close()
