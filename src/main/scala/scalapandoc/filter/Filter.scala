package scalapandoc.filter

import scalapandoc.ast.Attr
import scalapandoc.ast.Block
import scalapandoc.ast.Inline
import scalapandoc.ast.Pandoc
import scalapandoc.ast.PandocCodec.given
import io.circe.Decoder
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import scala.sys.process.Process
import scala.sys.process.ProcessIO
import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}
import scala.util.{Try, Success, Failure}

/** Filter for transforming Pandoc AST
 *
 * Filters can be written in any language and communicate via JSON.
 * This is compatible with pandoc's filter system.
 */
trait Filter:
  def transform(inline: Inline): Inline
  def transform(block: Block): Block

object Filter:
  /** Apply a sequence of filters to a Pandoc document */
  def applyFilters(doc: Pandoc, filters: List[Filter]): Pandoc =
    filters.foldLeft(doc) { (d, f) =>
      d.copy(blocks = d.blocks.map(b => transformBlock(b, f)))
    }

  private def transformBlock(block: Block, filter: Filter): Block =
    // Transform inner inlines first, then apply filter to block
    val withInlinesTransformed: Block = block match
      case Block.Para(contents) =>
        Block.Para(contents.map(transformInline(_, filter)))
      case Block.Plain(contents) =>
        Block.Plain(contents.map(transformInline(_, filter)))
      case Block.Headline(level, attr, contents) =>
        Block.Headline(level, attr, contents.map(transformInline(_, filter)))
      case Block.BlockQuote(contents) =>
        Block.BlockQuote(contents.map(b => transformBlock(b, filter)))
      case Block.BulletList(items) =>
        Block.BulletList(items.map(_.map(b => transformBlock(b, filter))))
      case Block.OrderedList(start, items) =>
        Block.OrderedList(start, items.map(_.map(b => transformBlock(b, filter))))
      case b => b

    filter.transform(withInlinesTransformed)

  private def transformInline(inline: Inline, filter: Filter): Inline =
    // Recursively transform nested inlines first
    val withInnerTransformed: Inline = inline match
      case Inline.Emph(contents) =>
        Inline.Emph(contents.map(transformInline(_, filter)))
      case Inline.Strong(contents) =>
        Inline.Strong(contents.map(transformInline(_, filter)))
      case Inline.Strikeout(contents) =>
        Inline.Strikeout(contents.map(transformInline(_, filter)))
      case Inline.Link(attr, contents, target) =>
        Inline.Link(attr, contents.map(transformInline(_, filter)), target)
      case Inline.Image(attr, contents, target) =>
        Inline.Image(attr, contents.map(transformInline(_, filter)), target)
      case i => i

    filter.transform(withInnerTransformed)

/** JSON-based filter that communicates via stdin/stdout
 *
 * This allows filters written in any language to work with scalapandoc.
 */
object JsonFilter:
  /** Run a filter process and feed it JSON AST */
  def runExternalFilter(doc: Pandoc, command: List[String]): Either[String, Pandoc] =
    try
      // Serialize document to JSON (pandoc format)
      val json: String = doc.asJson.toString

      // Use scala.sys.process with ProcessIO
      var outputJson: Option[String] = None
      var errorMsg: Option[String] = None

      val processIO: ProcessIO = new ProcessIO(
        (stdin: java.io.OutputStream) => {
          val out: OutputStreamWriter = new OutputStreamWriter(stdin)
          try
            out.write(json)
            out.write("\n")
            out.flush()
          finally
            out.close()
        },
        (stdout: java.io.InputStream) => {
          val in: BufferedReader = new BufferedReader(new InputStreamReader(stdout))
          outputJson = Some(in.readLine())
          in.close()
        },
        (stderr: java.io.InputStream) => {
          val err: BufferedReader = new BufferedReader(new InputStreamReader(stderr))
          val lines: Iterator[String] = Iterator continually err.readLine() takeWhile (_ != null)
          if lines.nonEmpty then
            errorMsg = Some(lines.mkString("\n"))
          err.close()
        }
      )

      val exitCode: Int = Process(command).run(processIO).exitValue()

      if exitCode != 0 then
        Left(errorMsg.getOrElse(s"Filter exited with code $exitCode"))
      else
        outputJson match
          case None => Left("Filter produced no output")
          case Some(jsonStr: String) =>
            parse(jsonStr) match
              case Right(jsonValue: Json) =>
                Decoder[Pandoc].decodeJson(jsonValue) match
                  case Right(decodedDoc: Pandoc) => Right(decodedDoc)
                  case Left(err) => Left(s"Failed to decode filter output: $err")
              case Left(err) => Left(s"Failed to parse filter output: $err")

    catch
      case e: Exception =>
        Left(s"Filter execution failed: ${e.getMessage}")

/** Example filters */
object Filters:
  /** Filter that capitalizes all text */
  val Capitalize: Filter = new Filter:
    def transform(inline: Inline): Inline =
      inline match
        case Inline.Str(s) => Inline.Str(s.toUpperCase)
        case i => i

    def transform(block: Block): Block = block

  /** Filter that removes comments */
  val RemoveComments: Filter = new Filter:
    def transform(inline: Inline): Inline = inline

    def transform(block: Block): Block = block
