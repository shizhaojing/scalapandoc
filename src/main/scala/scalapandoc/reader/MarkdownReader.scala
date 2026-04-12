package scalapandoc.reader

import scalapandoc.ast.Attr
import scalapandoc.ast.Block
import scalapandoc.ast.Inline
import scalapandoc.ast.Meta
import scalapandoc.ast.Pandoc

/** Reads Markdown and converts to Pandoc-compatible AST
 *
 * Simplified implementation using basic string parsing.
 * For production, should use a proper Markdown parser.
 */
object MarkdownReader:

  /** Parse Markdown string into Pandoc document */
  def read(markdown: String): Pandoc =
    val lines: List[String] = markdown.linesIterator.toList
    val (meta: Meta, blocks: List[Block]) = parseDocument(lines)
    Pandoc(meta, blocks)

  /** Simple document parser
   *
   * Extracts title from first # heading and converts blocks.
   */
  private def parseDocument(lines: List[String]): (Meta, List[Block]) =
    var meta: Meta = Meta(Nil, Nil, Nil)
    var blocks: List[Block] = List.empty[Block]
    var i: Int = 0

    while i < lines.length do
      val line: String = lines(i)
      if line.startsWith("#") then
        val level: Int = line.takeWhile(_ == '#').length
        val text: String = line.drop(level).trim
        if meta.title.isEmpty && level == 1 then
          meta = meta.copy(title = parseInline(text))
        else
          blocks = blocks :+ Block.Headline(level, Attr.empty, parseInline(text))
      else if line.trim.nonEmpty && !line.trim.startsWith("```") then
        // Paragraph (simplified - one line per paragraph)
        blocks = blocks :+ Block.Para(parseInline(line))
      else if line.trim.startsWith("```") then
        // Code block
        val lang: String = line.trim.drop(3).trim
        i = i + 1
        val codeLines: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer[String]()
        while i < lines.length && !lines(i).trim.startsWith("```") do
          codeLines += lines(i)
          i = i + 1
        blocks = blocks :+ Block.CodeBlock(Attr.empty, codeLines.toList)
      else if line.trim.startsWith("---") then
        // Horizontal rule
        blocks = blocks :+ Block.HorizontalRule
      else if line.trim.startsWith("*") || line.trim.startsWith("-") then
        // Bullet list item (simplified)
        val text: String = line.dropWhile(c => c == '*' || c == '-' || c == ' ').trim
        blocks = blocks :+ Block.BulletList(List(List(Block.Para(parseInline(text)))))
      else if line.nonEmpty && line.head.isDigit then
        // Ordered list item (simplified)
        val text: String = line.dropWhile(c => c.isDigit || c == '.' || c == ' ').trim
        blocks = blocks :+ Block.OrderedList(1, List(List(Block.Para(parseInline(text)))))
      i = i + 1

    (meta, blocks)

  /** Parse inline elements from a text string
   *
   * Very simplified implementation - handles basic formatting.
   */
  private def parseInline(text: String): List[Inline] =
    // Simple regex-based parsing for common patterns
    var result: List[Inline] = List.empty[Inline]
    var remaining: String = text

    // Handle bold **text**
    val boldPattern: scala.util.matching.Regex = """\*\*(.+?)\*\*""".r
    remaining = boldPattern.replaceAllIn(remaining, { m =>
      val content: String = m.group(1).nn
      result = result :+ Inline.Strong(parseInline(content))
      ""
    })

    // Handle italic *text*
    val italicPattern: scala.util.matching.Regex = """\*(.+?)\*""".r
    remaining = italicPattern.replaceAllIn(remaining, { m =>
      val content: String = m.group(1).nn
      result = result :+ Inline.Emph(parseInline(content))
      ""
    })

    // Handle code `text`
    val codePattern: scala.util.matching.Regex = """`(.+?)`""".r
    remaining = codePattern.replaceAllIn(remaining, { m =>
      val content: String = m.group(1).nn
      result = result :+ Inline.Code(Attr.empty, content)
      ""
    })

    // Handle links [text](url)
    val linkPattern: scala.util.matching.Regex = """\[(.+?)\]\((.+?)\)""".r
    remaining = linkPattern.replaceAllIn(remaining, { m =>
      val text: String = m.group(1).nn
      val url: String = m.group(2).nn
      result = result :+ Inline.Link(Attr.empty, parseInline(text), (url, ""))
      ""
    })

    // Remaining text
    if remaining.nonEmpty then
      // Split by words and add spaces
      val words: Array[String] = remaining.split("\\s+")
      for (word, idx) <- words.zipWithIndex do
        if word.nonEmpty then
          result = result :+ Inline.Str(word)
        if idx < words.length - 1 then
          result = result :+ Inline.Space

    result

  /** Read markdown from a file */
  def readFile(path: String): Pandoc =
    val source: scala.io.Source = scala.io.Source.fromFile(path)
    try
      read(source.mkString)
    finally
      source.close()
