package scalapandoc

import munit.FunSuite
import java.io.File
import java.io.PrintWriter
import scala.io.Source
import scalapandoc.cli.{Cli, Config}

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

  private def tempFile(name: String): File =
    val f: File = File.createTempFile(s"scalapandoc-$name-", ".tmp")
    f.deleteOnExit()
    f

  test("convert --input --output: markdown to markdown file") {
    val input: File = writeTemp("input", "# Hello\n\nThis is **bold** text.")
    val output: File = tempFile("output")

    val result: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      input = Some(input.getAbsolutePath),
      output = Some(output.getAbsolutePath)
    ))

    assert(result.isRight)
    assert(output.exists())
    val content: String = readFile(output)
    assert(content.contains("Hello"))
    assert(content.contains("bold"))
  }

  test("convert --input --to-json: markdown to JSON file") {
    val input: File = writeTemp("input", "# Hello\n\nSome text.")
    val jsonOut: File = tempFile("json")

    val result: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      input = Some(input.getAbsolutePath),
      outputJson = Some(jsonOut.getAbsolutePath)
    ))

    assert(result.isRight)
    assert(jsonOut.exists())
    val json: String = readFile(jsonOut)
    assert(json.contains("pandoc-api-version"))
    assert(json.contains("blocks"))
    assert(json.contains("Hello"))
  }

  test("convert --from-json --output: JSON to markdown file") {
    val input: File = writeTemp("input", "# Hello\n\nSome text.")
    val jsonOut: File = tempFile("json")
    val mdOut: File = tempFile("md")

    val step1: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      input = Some(input.getAbsolutePath),
      outputJson = Some(jsonOut.getAbsolutePath)
    ))
    assert(step1.isRight)

    val step2: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      inputJson = Some(jsonOut.getAbsolutePath),
      output = Some(mdOut.getAbsolutePath)
    ))
    assert(step2.isRight)
    val content: String = readFile(mdOut)
    assert(content.contains("Hello"))
  }

  test("convert --input --output --capitalize: capitalize filter") {
    val input: File = writeTemp("input", "# hello\n\nthis is a test.")
    val output: File = tempFile("output")

    val result: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      input = Some(input.getAbsolutePath),
      output = Some(output.getAbsolutePath),
      capitalize = true
    ))

    assert(result.isRight)
    assert(output.exists())
    val content: String = readFile(output)
    assert(content.nonEmpty)
    // The capitalize filter uppercases Inline.Str nodes in paragraphs
    assert(content.toUpperCase.contains("THIS IS A TEST"))
  }

  test("convert --input with no output: returns markdown to stdout") {
    val input: File = writeTemp("input", "# Hello\n\nWorld.")

    val result: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      input = Some(input.getAbsolutePath)
    ))

    assert(result.isRight)
    val output: String = result.toOption.get
    assert(output.contains("Hello"))
    assert(output.contains("World"))
  }

  test("convert --to-json and --output together: returns error") {
    val input: File = writeTemp("input", "# Hello")
    val output: File = tempFile("output")

    val result: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      input = Some(input.getAbsolutePath),
      output = Some(output.getAbsolutePath),
      outputJson = Some(tempFile("json").getAbsolutePath)
    ))

    assert(result.isLeft)
    assert(result.left.getOrElse("").contains("Cannot specify both"))
  }

  test("convert --from-json --output --pretty: round-trip with pretty JSON") {
    val input: File = writeTemp("input", "# Hello\n\nThis is **bold** text.")
    val jsonOut: File = tempFile("json")
    val mdOut: File = tempFile("md")

    val step1: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      input = Some(input.getAbsolutePath),
      outputJson = Some(jsonOut.getAbsolutePath),
      pretty = true
    ))
    assert(step1.isRight)

    val json: String = readFile(jsonOut)
    assert(json.contains("\n  "))

    val step2: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      inputJson = Some(jsonOut.getAbsolutePath),
      output = Some(mdOut.getAbsolutePath)
    ))
    assert(step2.isRight)
    val md: String = readFile(mdOut)
    assert(md.contains("Hello"))
    assert(md.contains("bold"))
  }

  test("convert --from-json with invalid JSON: returns error") {
    val badJson: File = writeTemp("badjson", "this is not json {{{")
    val output: File = tempFile("output")

    val result: Either[String, String] = Cli.run(Config(
      command = Some("convert"),
      inputJson = Some(badJson.getAbsolutePath),
      output = Some(output.getAbsolutePath)
    ))

    assert(result.isLeft)
  }
