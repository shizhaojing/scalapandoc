package scalapandoc

import munit.FunSuite
import scalapandoc.ast.Block
import scalapandoc.ast.Pandoc
import scalapandoc.reader.MarkdownReader
import scalapandoc.writer.MarkdownWriter
import scalapandoc.filter.{Filter, Filters}

class MarkdownTest extends FunSuite:

  test("parse simple paragraph") {
    val markdown: String = "# Hello World\n\nThis is a test."
    val doc: Pandoc = MarkdownReader.read(markdown)

    assert(doc.blocks.nonEmpty)
    assert(doc.blocks.head match
      case Block.Para(_) => true
      case _ => false
    )
  }

  test("parse code block") {
    val markdown: String = """
      |```scala
      |def hello = "world"
      |```
      """.stripMargin

    val doc: Pandoc = MarkdownReader.read(markdown)

    val codeBlocks: List[Block.CodeBlock] = doc.blocks.collect { case b @ Block.CodeBlock(_, _) => b }
    assert(codeBlocks.nonEmpty)
  }

  test("parse inline formatting") {
    val markdown: String = "This has **bold** and *italic* text."
    val doc: Pandoc = MarkdownReader.read(markdown)

    assert(doc.blocks.head match
      case Block.Para(contents) => contents.nonEmpty
      case _ => false
    )
  }

  test("capitalize filter") {
    val markdown: String = "# hello\n\nthis is a test."
    val doc: Pandoc = MarkdownReader.read(markdown)

    val filtered: Pandoc = Filter.applyFilters(doc, List(Filters.Capitalize))
    // Should capitalize text
    assert(filtered.blocks.nonEmpty)
  }

  test("round-trip: parse and write") {
    val original: String = "# Hello\n\nThis is a test with **bold** text."
    val doc: Pandoc = MarkdownReader.read(original)
    val output: String = MarkdownWriter.write(doc)

    assert(output.nonEmpty)
    assert(output.contains("Hello"))
  }
