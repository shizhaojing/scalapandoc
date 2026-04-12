package scalapandoc.cli

import scalapandoc.reader.MarkdownReader
import scalapandoc.writer.MarkdownWriter
import scalapandoc.filter.{Filter, Filters, JsonFilter}
import scalapandoc.ast.{Pandoc, PandocCodec}
import scalapandoc.ast.PandocCodec.given
import scopt.OParser
import io.circe.syntax.EncoderOps
import io.circe.parser.parse
import scala.io.Source

/** Command-line interface for scalapandoc */
@main def main(args: String*): Unit =
  val builder = OParser.builder[Config]
  import builder.{opt, note, head, programName, cmd}
  val parser: OParser[Unit, Config] = {
    val children: Seq[OParser[?, Config]] = Seq(
      opt[String]("input")
        .action((x, c) => c.copy(input = Some(x)))
        .text("Input file path (or stdin if not specified)"),
      opt[String]("output")
        .action((x, c) => c.copy(output = Some(x)))
        .text("Output file path (or stdout if not specified)"),
      opt[String]("to-json")
        .action((x, c) => c.copy(outputJson = Some(x)))
        .text("Output AST as JSON to file"),
      opt[String]("from-json")
        .action((x, c) => c.copy(inputJson = Some(x)))
        .text("Read AST as JSON from file"),
      opt[String]("filter")
        .action((x, c) => c.copy(filters = c.filters :+ x))
        .text("External filter command to apply")
        .unbounded(),
      opt[Unit]("capitalize")
        .action((_, c) => c.copy(capitalize = true))
        .text("Apply capitalize filter"),
      opt[Unit]("pretty")
        .action((_, c) => c.copy(pretty = true))
        .text("Pretty print JSON output"),
      note("""
              |Examples:
              |  scalapandoc convert --input README.md --output output.md
              |  scalapandoc convert --input README.md --to-json ast.json
              |  scalapandoc convert --from-json ast.json --output output.md
              |  scalapandoc convert --input README.md --filter ./my-filter.py
              |  scalapandoc convert --input README.md --capitalize
              |""".stripMargin)
    );
    OParser.sequence(
      programName("scalapandoc"),
      head("scalapandoc", "0.1.0"),
      cmd("convert")
        .action((_, c) => c.copy(command = Some("convert")))
        .text("Convert between Markdown and Pandoc JSON AST")
        .children(
          children*
        )
    )
  }

  OParser.parse(parser, args, Config()) match
    case Some(config) if config.help =>
      OParser.usage(parser)
    case Some(config) =>
      run(config)
    case _ =>
      OParser.usage(parser)

  case class Config(
      command: Option[String] = None,
      input: Option[String] = None,
      output: Option[String] = None,
      outputJson: Option[String] = None,
      inputJson: Option[String] = None,
      filters: List[String] = Nil,
      capitalize: Boolean = false,
      pretty: Boolean = false,
      help: Boolean = false
  )

  def run(config: Config): Unit =
    try
      // Step 1: Read input
      val doc: Pandoc = config.inputJson match
        case Some(jsonFile) =>
          // Read from JSON AST
          import scala.io.Source
          val json: String = Source.fromFile(jsonFile).mkString
          import io.circe.parser.parse
          parse(json) match
            case Right(jsonValue: io.circe.Json) =>
              PandocCodec.given_Decoder_Pandoc.decodeJson(jsonValue) match
                case Right(decodedDoc: Pandoc) => decodedDoc
                case Left(err)  =>
                  System.err.println(s"Error decoding JSON: $err")
                  sys.exit(1)
            case Left(err) =>
              System.err.println(s"Error parsing JSON: $err")
              sys.exit(1)
        case None =>
          // Read from Markdown
          val markdown: String = config.input match
            case Some(file) =>
              Source.fromFile(file).mkString
            case None =>
              // Read from stdin
              Source.stdin.getLines().mkString("\n")
          MarkdownReader.read(markdown)

      // Step 2: Apply filters
      val filteredDoc: Pandoc = applyFilters(doc, config)

      // Step 3: Write output
      (config.outputJson, config.output) match
        case (Some(jsonFile), None) =>
          // Output JSON AST
          val json: io.circe.Json = filteredDoc.asJson
          val jsonString: String = if config.pretty then json.spaces2 else json.noSpaces
          import java.io.PrintWriter
          new PrintWriter(jsonFile) { write(jsonString); close() }
          println(s"AST written to $jsonFile")

        case (None, Some(outFile)) =>
          // Output Markdown
          MarkdownWriter.writeFile(filteredDoc, outFile)
          println(s"Output written to $outFile")

        case (None, None) =>
          // Output to stdout
          val markdown: String = MarkdownWriter.write(filteredDoc)
          println(markdown)

        case (Some(_), Some(_)) =>
          System.err.println("Cannot specify both --to-json and --output")
          sys.exit(1)

    catch
      case e: Exception =>
        System.err.println(s"Error: ${e.getMessage}")
        e.printStackTrace()
        sys.exit(1)

  def applyFilters(
      doc: scalapandoc.ast.Pandoc,
      config: Config
  ): scalapandoc.ast.Pandoc =
    import scalapandoc.ast.Pandoc

    val filters: scala.collection.mutable.ListBuffer[Filter] = scala.collection.mutable.ListBuffer.empty[Filter]

    // Add built-in filters
    if config.capitalize then filters += Filters.Capitalize

    // Apply external filters
    val result: Pandoc = config.filters.foldLeft(doc) { (currentDoc: Pandoc, filterCmd: String) =>
      val parts: Array[String] = filterCmd.split(" ")
      JsonFilter.runExternalFilter(currentDoc, parts.toList) match
        case Right(filtered) => filtered
        case Left(err)       =>
          System.err.println(s"Filter error: $err")
          currentDoc
    }

    // Apply internal filters
    Filter.applyFilters(result, filters.toList)
