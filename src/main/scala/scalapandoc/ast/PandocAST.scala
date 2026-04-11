package scalapandoc.ast

import io.circe.*
import io.circe.generic.semiauto.*

/** Type tag constants for Pandoc JSON format */
object TypeTags:
  // Inline type tags
  final val Str = "Str"
  final val Space = "Space"
  final val SoftBreak = "SoftBreak"
  final val LineBreak = "LineBreak"
  final val Emph = "Emph"
  final val Strong = "Strong"
  final val Strikeout = "Strikeout"
  final val Superscript = "Superscript"
  final val Subscript = "Subscript"
  final val Quoted = "Quoted"
  final val Cite = "Cite"
  final val Code = "Code"
  final val Math = "Math"
  final val RawInline = "RawInline"
  final val Link = "Link"
  final val Image = "Image"
  final val Note = "Note"
  final val Span = "Span"
  final val LineBlock = "LineBlock"

  // Block type tags
  final val Para = "Para"
  final val Plain = "Plain"
  final val CodeBlock = "CodeBlock"
  final val RawBlock = "RawBlock"
  final val BlockQuote = "BlockQuote"
  final val OrderedList = "OrderedList"
  final val BulletList = "BulletList"
  final val DefinitionList = "DefinitionList"
  final val Headline = "Header"
  final val HorizontalRule = "HorizontalRule"
  final val Table = "Table"
  final val Div = "Div"
  final val Null = "Null"

  // Meta value type tags
  final val MetaMap = "MetaMap"
  final val MetaBool = "MetaBool"
  final val MetaString = "MetaString"
  final val MetaInlines = "MetaInlines"
  final val MetaBlocks = "MetaBlocks"
  final val MetaList = "MetaList"

  // JSON field names
  final val TypeField = "t"
  final val ContentField = "c"
  final val ApiVersionField = "pandoc-api-version"
  final val MetaField = "meta"
  final val BlocksField = "blocks"

/** Pandoc API version compatibility */
case class ApiVersion(major: Int, minor: Int, revision: Int)

object ApiVersion:
  val current = ApiVersion(1, 23, 0)

/** Simple attribute tuple */
case class Attr(identifier: String, classes: List[String], attributes: List[(String, String)])

object Attr:
  val empty = Attr("", List.empty, List.empty)

/** Inline elements (simplified for initial version) */
enum Inline:
  case Str(contents: String)
  case Space
  case SoftBreak
  case LineBreak
  case Emph(contents: List[Inline])
  case Strong(contents: List[Inline])
  case Strikeout(contents: List[Inline])
  case Code(attr: Attr, text: String)
  case Link(attr: Attr, contents: List[Inline], target: (String, String))
  case Image(attr: Attr, contents: List[Inline], target: (String, String))
  case RawInline(format: String, text: String)

/** Block elements (simplified for initial version) */
enum Block:
  case Para(contents: List[Inline])
  case Plain(contents: List[Inline])
  case Headline(level: Int, attr: Attr, contents: List[Inline])
  case CodeBlock(attr: Attr, lines: List[String])
  case BlockQuote(contents: List[Block])
  case OrderedList(listStart: Int, items: List[List[Block]])
  case BulletList(items: List[List[Block]])
  case HorizontalRule
  case Null

/** Meta value (simplified) */
enum MetaValue:
  case MetaString(value: String)
  case MetaBool(value: Boolean)
  case MetaInlines(value: List[Inline])
  case MetaBlocks(value: List[Block])
  case MetaMap(value: Map[String, MetaValue])

/** Meta information */
case class Meta(title: List[Inline], authors: List[List[Inline]], date: List[Inline])

/** The root Pandoc document */
case class Pandoc(meta: Meta, blocks: List[Block])

/** JSON encoding for Pandoc AST
 *
 * Uses a simplified tagged union format compatible with Pandoc.
 */
object PandocCodec:
  import TypeTags.*

  // Helper for tagged JSON
  private def tagged(tag: String, content: Json): Json =
    Json.obj((TypeField, Json.fromString(tag)), (ContentField, content))

  private def taggedOnly(tag: String): Json =
    Json.obj((TypeField, Json.fromString(tag)))

  // Inline encoding
  given Encoder[Inline] = Encoder.instance {
    case Inline.Str(s) => tagged(Str, Json.fromString(s))
    case Inline.Space => taggedOnly(Space)
    case Inline.SoftBreak => taggedOnly(SoftBreak)
    case Inline.LineBreak => taggedOnly(LineBreak)
    case Inline.Emph(xs) => tagged(Emph, encodeInlineList(xs))
    case Inline.Strong(xs) => tagged(Strong, encodeInlineList(xs))
    case Inline.Strikeout(xs) => tagged(Strikeout, encodeInlineList(xs))
    case Inline.Code(attr, text) => tagged(Code, Json.arr(
        encodeAttr(attr),
        Json.fromString(text)
      ))
    case Inline.Link(attr, contents, (url, title)) => tagged(Link, Json.arr(
        encodeAttr(attr),
        encodeInlineList(contents),
        Json.fromString(url),
        Json.fromString(title)
      ))
    case Inline.Image(attr, contents, (url, title)) => tagged(Image, Json.arr(
        encodeAttr(attr),
        encodeInlineList(contents),
        Json.fromString(url),
        Json.fromString(title)
      ))
    case Inline.RawInline(fmt, text) => tagged(RawInline, Json.arr(
        Json.fromString(fmt),
        Json.fromString(text)
      ))
  }

  // Block encoding
  given Encoder[Block] = Encoder.instance {
    case Block.Para(xs) => tagged(Para, encodeInlineList(xs))
    case Block.Plain(xs) => tagged(Plain, encodeInlineList(xs))
    case Block.Headline(level, attr, xs) => tagged(Headline, Json.arr(
        Json.fromInt(level),
        encodeAttr(attr),
        encodeInlineList(xs)
      ))
    case Block.CodeBlock(attr, lines) => tagged(CodeBlock, Json.arr(
        encodeAttr(attr),
        Json.fromString(lines.mkString("\n"))
      ))
    case Block.BlockQuote(xs) => tagged(BlockQuote, encodeBlockList(xs))
    case Block.OrderedList(start, xs) => tagged(OrderedList, Json.arr(
        Json.fromInt(start),
        encodeBlockListList(xs)
      ))
    case Block.BulletList(xs) => tagged(BulletList, encodeBlockListList(xs))
    case Block.HorizontalRule => taggedOnly(HorizontalRule)
    case Block.Null => taggedOnly(Null)
  }

  // Meta encoding
  given Encoder[Meta] = Encoder.instance { meta =>
    Json.obj(
      ("title", encodeInlineList(meta.title)),
      ("authors", Json.arr(meta.authors.map(encodeInlineList)*)),
      ("date", encodeInlineList(meta.date))
    )
  }

  // Pandoc document encoding
  given Encoder[Pandoc] = Encoder.instance { pandoc =>
    Json.obj(
      (ApiVersionField, Json.arr(Json.fromInt(1), Json.fromInt(23))),
      (MetaField, Encoder[Meta].apply(pandoc.meta)),
      (BlocksField, Encoder[List[Block]].apply(pandoc.blocks))
    )
  }

  // Inline decoding
  given Decoder[Inline] = Decoder.instance { cursor =>
    cursor.get[String](TypeField).flatMap {
      case Str => cursor.get[String](ContentField).map(Inline.Str(_))
      case Space => Right(Inline.Space)
      case SoftBreak => Right(Inline.SoftBreak)
      case LineBreak => Right(Inline.LineBreak)
      case Emph =>
        cursor.get[Json](ContentField).map { json =>
          Inline.Emph(decodeInlineList(json))
        }
      case Strong =>
        cursor.get[Json](ContentField).map { json =>
          Inline.Strong(decodeInlineList(json))
        }
      case Strikeout =>
        cursor.get[Json](ContentField).map { json =>
          Inline.Strikeout(decodeInlineList(json))
        }
      case Code => cursor.get[List[Json]](ContentField).map { c =>
        Inline.Code(decodeAttr(c(0)), c(1).asString.getOrElse(""))
      }
      case Link => cursor.get[List[Json]](ContentField).map { c =>
        Inline.Link(
          decodeAttr(c(0)),
          decodeInlineList(c(1)),
          (c(2).asString.getOrElse(""), c(3).asString.getOrElse(""))
        )
      }
      case Image => cursor.get[List[Json]](ContentField).map { c =>
        Inline.Image(
          decodeAttr(c(0)),
          decodeInlineList(c(1)),
          (c(2).asString.getOrElse(""), c(3).asString.getOrElse(""))
        )
      }
      case RawInline => cursor.get[List[Json]](ContentField).map { c =>
        Inline.RawInline(c(0).asString.getOrElse(""), c(1).asString.getOrElse(""))
      }
      case t => Left(DecodingFailure(s"Unknown Inline type: $t", cursor.history))
    }
  }

  // Block decoding
  given Decoder[Block] = Decoder.instance { cursor =>
    cursor.get[String](TypeField).flatMap {
      case Para =>
        cursor.get[Json](ContentField).map { json =>
          Block.Para(decodeInlineList(json))
        }
      case Plain =>
        cursor.get[Json](ContentField).map { json =>
          Block.Plain(decodeInlineList(json))
        }
      case Headline => cursor.get[List[Json]](ContentField).map { c =>
        Block.Headline(
          c(0).asNumber.flatMap(_.toInt).getOrElse(1),
          decodeAttr(c(1)),
          decodeInlineList(c(2))
        )
      }
      case CodeBlock => cursor.get[List[Json]](ContentField).map { c =>
        Block.CodeBlock(
          decodeAttr(c(0)),
          c(1).asString.getOrElse("").linesIterator.toList
        )
      }
      case BlockQuote =>
        cursor.get[Json](ContentField).map { json =>
          Block.BlockQuote(decodeBlockList(json))
        }
      case OrderedList => cursor.get[List[Json]](ContentField).map { c =>
        Block.OrderedList(
          c(0).asNumber.flatMap(_.toInt).getOrElse(1),
          decodeBlockListList(c(1))
        )
      }
      case BulletList => cursor.get[List[Json]](ContentField).map { c =>
        Block.BulletList(decodeBlockListList(c(0)))
      }
      case HorizontalRule => Right(Block.HorizontalRule)
      case Null => Right(Block.Null)
      case t => Left(DecodingFailure(s"Unknown Block type: $t", cursor.history))
    }
  }

  // Meta decoding
  given Decoder[Meta] = Decoder.instance { cursor =>
    for
      title <- cursor.get[List[Inline]]("title")
      authors <- cursor.get[List[List[Inline]]]("authors")
      date <- cursor.get[List[Inline]]("date")
    yield Meta(title, authors, date)
  }

  // Pandoc document decoding
  given Decoder[Pandoc] = Decoder.instance { cursor =>
    for
      meta <- cursor.get[Meta](MetaField)
      blocks <- cursor.get[List[Block]](BlocksField)
    yield Pandoc(meta, blocks)
  }

  // Helper encoders/decoders
  private def encodeAttr(attr: Attr): Json =
    Json.arr(
      Json.fromString(attr.identifier),
      Json.arr(attr.classes.map(Json.fromString(_))*),
      Json.fromFields(attr.attributes.map((k, v) => (k, Json.fromString(v))))
    )

  private def decodeAttr(json: Json): Attr =
    json.asArray match
      case Some(arr) if arr.nonEmpty =>
        val id = arr(0).asString.getOrElse("")
        val classes = arr.lift(1).flatMap(_.asArray).getOrElse(Nil).flatMap(_.asString).toList
        val attrs = arr.lift(2).flatMap(_.asObject).getOrElse(JsonObject.empty).toMap
          .flatMap((k, v) => v.asString.map((k, _))).toList
        Attr(id, classes, attrs)
      case _ => Attr.empty

  private def encodeInlineList(xs: List[Inline]): Json =
    Json.fromValues(xs.map(Encoder[Inline].apply))

  private def decodeInlineList(json: Json): List[Inline] =
    json.asArray match
      case Some(arr) => arr.flatMap(Decoder[Inline].decodeJson(_).toOption).toList
      case _ => Nil

  private def encodeBlockList(xs: List[Block]): Json =
    Json.fromValues(xs.map(Encoder[Block].apply))

  private def encodeBlockListList(xss: List[List[Block]]): Json =
    Json.fromValues(xss.map(xs => encodeBlockList(xs)))

  private def decodeBlockList(json: Json): List[Block] =
    json.asArray match
      case Some(arr) => arr.flatMap(Decoder[Block].decodeJson(_).toOption).toList
      case _ => Nil

  private def decodeBlockListList(json: Json): List[List[Block]] =
    json.asArray match
      case Some(arr) => arr.map(decodeBlockList).toList
      case _ => Nil
