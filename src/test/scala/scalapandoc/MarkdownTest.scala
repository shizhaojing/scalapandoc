package scalapandoc

import munit.FunSuite
import scalapandoc.ast.Block
import scalapandoc.ast.Pandoc
import scalapandoc.reader.MarkdownReader
import scalapandoc.writer.MarkdownWriter
import scalapandoc.filter.{Filter, Filters}

class MarkdownTest extends FunSuite:

  test("parse simple paragraph") {
    val markdown = "# Hello World\n\nThis is a test."
    val doc = MarkdownReader.read(markdown)

    assert(doc.blocks.nonEmpty)
    assert(doc.blocks.head match
      case Block.Para(_) => true
      case _ => false
    )
  }

  test("parse code block") {
    val markdown = """
      |```scala
      |def hello = "world"
      |```
      """.stripMargin

    val doc = MarkdownReader.read(markdown)

    val codeBlocks = doc.blocks.collect { case b @ Block.CodeBlock(_, _) => b }
    assert(codeBlocks.nonEmpty)
  }

  test("parse inline formatting") {
    val markdown = "This has **bold** and *italic* text."
    val doc = MarkdownReader.read(markdown)

    assert(doc.blocks.head match
      case Block.Para(contents) => contents.nonEmpty
      case _ => false
    )
  }

  test("capitalize filter") {
    val markdown = "# hello\n\nthis is a test."
    val doc = MarkdownReader.read(markdown)

    val filtered = Filter.applyFilters(doc, List(Filters.Capitalize))
    // Should capitalize text
    assert(filtered.blocks.nonEmpty)
  }

  test("round-trip: parse and write") {
    val original = "# Hello\n\nThis is a test with **bold** text."
    val doc = MarkdownReader.read(original)
    val output = MarkdownWriter.write(doc)

    assert(output.nonEmpty)
    assert(output.contains("Hello"))
  }
