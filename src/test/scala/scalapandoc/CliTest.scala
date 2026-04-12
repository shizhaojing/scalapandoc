package scalapandoc

import munit.FunSuite
import java.io.File
import java.io.PrintWriter
import scala.io.Source
import scalapandoc.ast.Pandoc
import scalapandoc.ast.PandocCodec.given
import scalapandoc.reader.MarkdownReader
import scalapandoc.writer.MarkdownWriter
import scalapandoc.filter.{Filter, Filters}
import io.circe.syntax.EncoderOps
import io.circe.parser.parse

class CliTest extends FunSuite:

  private def writeTemp(name: String, content: String): File =
    val f: File = File.createTempFile(s"scalapandoc-$name-", ".tmp")
    f.deleteOnExit()
    val pw: PrintWriter = new PrintWriter(f)
    try pw.write(content)
    finally pw.close()
    f

  private def readFile(f: File): String =
    val s: Source = Source.fromFile(f)
    try s.mkString
    finally s.close()

  test("markdown to markdown via files") {
    val input = writeTemp("input", "# Hello\n\nThis is **bold** text.")
    val output: File = File.createTempFile("scalapandoc-output-", ".tmp")
    output.deleteOnExit()

    val doc = MarkdownReader.readFile(input.getAbsolutePath)
    MarkdownWriter.writeFile(doc, output.getAbsolutePath)
    val result = readFile(output)
    assert(result.contains("Hello"))
    assert(result.contains("bold"))
  }

  test("markdown to JSON") {
    val input = writeTemp("input", "# Hello\n\nSome text.")

    val doc: Pandoc = MarkdownReader.readFile(input.getAbsolutePath)
    val json: String = doc.asJson.noSpaces
    assert(json.contains("pandoc-api-version"))
    assert(json.contains("blocks"))
    assert(json.contains("Hello"))
  }

  test("JSON to markdown") {
    val original = "# Hello\n\nSome text."
    val input = writeTemp("input", original)

    val doc: Pandoc = MarkdownReader.readFile(input.getAbsolutePath)
    val json: String = doc.asJson.spaces2

    val jsonFile: File = writeTemp("json", json)
    val jsonStr: String = readFile(jsonFile)
    val parsed: io.circe.Json = parse(jsonStr).toTry.get
    val decoded: Pandoc = io.circe.Decoder[Pandoc].decodeJson(parsed).toTry.get

    val md: String = MarkdownWriter.write(decoded)
    assert(md.contains("Hello"))
  }

  test("capitalize built-in filter") {
    val input = writeTemp("input", "# hello\n\nthis is a test.")

    val doc: Pandoc = MarkdownReader.readFile(input.getAbsolutePath)
    val filtered: Pandoc = Filter.applyFilters(doc, List(Filters.Capitalize))
    val md: String = MarkdownWriter.write(filtered)
    assert(md.nonEmpty)
  }

  test("full round-trip: md -> json -> md preserves content") {
    val original = "# Hello\n\nThis is a test with **bold** text."
    val input = writeTemp("input", original)

    // md -> json
    val doc: Pandoc = MarkdownReader.readFile(input.getAbsolutePath)
    val json: String = doc.asJson.spaces2

    // json -> md
    val parsed: io.circe.Json = parse(json).toTry.get
    val decoded: Pandoc = io.circe.Decoder[Pandoc].decodeJson(parsed).toTry.get
    val md: String = MarkdownWriter.write(decoded)

    assert(md.contains("Hello"))
    assert(md.contains("bold"))
  }

  test("pretty JSON output uses spaces2") {
    val input = writeTemp("input", "# Test")
    val doc: Pandoc = MarkdownReader.readFile(input.getAbsolutePath)
    val json: String = doc.asJson.spaces2
    assert(json.contains("\n  "))  // pretty-printed has indentation
  }
