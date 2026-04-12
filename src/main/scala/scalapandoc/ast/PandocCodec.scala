package scalapandoc.ast

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import scalapandoc.ast.{TypeTags as tt}

/** JSON encoding/decoding for Pandoc AST
 *
 * Uses a simplified tagged union format compatible with Pandoc.
 */
object PandocCodec:

  // Helper for tagged JSON
  private def tagged(tag: String, content: Json): Json =
    Json.obj((tt.TypeField, Json.fromString(tag)), (tt.ContentField, content))

  private def taggedOnly(tag: String): Json =
    Json.obj((tt.TypeField, Json.fromString(tag)))

  // Inline encoding
  given Encoder[Inline] = Encoder.instance {
    case Inline.Str(s) => tagged(tt.Str, Json.fromString(s))
    case Inline.Space => taggedOnly(tt.Space)
    case Inline.SoftBreak => taggedOnly(tt.SoftBreak)
    case Inline.LineBreak => taggedOnly(tt.LineBreak)
    case Inline.Emph(xs) => tagged(tt.Emph, encodeInlineList(xs))
    case Inline.Strong(xs) => tagged(tt.Strong, encodeInlineList(xs))
    case Inline.Strikeout(xs) => tagged(tt.Strikeout, encodeInlineList(xs))
    case Inline.Code(attr, text) => tagged(tt.Code, Json.arr(
        encodeAttr(attr),
        Json.fromString(text)
      ))
    case Inline.Link(attr, contents, (url, title)) => tagged(tt.Link, Json.arr(
        encodeAttr(attr),
        encodeInlineList(contents),
        Json.fromString(url),
        Json.fromString(title)
      ))
    case Inline.Image(attr, contents, (url, title)) => tagged(tt.Image, Json.arr(
        encodeAttr(attr),
        encodeInlineList(contents),
        Json.fromString(url),
        Json.fromString(title)
      ))
    case Inline.RawInline(fmt, text) => tagged(tt.RawInline, Json.arr(
        Json.fromString(fmt),
        Json.fromString(text)
      ))
  }

  // Block encoding
  given Encoder[Block] = Encoder.instance {
    case Block.Para(xs) => tagged(tt.Para, encodeInlineList(xs))
    case Block.Plain(xs) => tagged(tt.Plain, encodeInlineList(xs))
    case Block.Headline(level, attr, xs) => tagged(tt.Headline, Json.arr(
        Json.fromInt(level),
        encodeAttr(attr),
        encodeInlineList(xs)
      ))
    case Block.CodeBlock(attr, lines) => tagged(tt.CodeBlock, Json.arr(
        encodeAttr(attr),
        Json.fromString(lines.mkString("\n"))
      ))
    case Block.BlockQuote(xs) => tagged(tt.BlockQuote, encodeBlockList(xs))
    case Block.OrderedList(start, xs) => tagged(tt.OrderedList, Json.arr(
        Json.fromInt(start),
        encodeBlockListList(xs)
      ))
    case Block.BulletList(xs) => tagged(tt.BulletList, encodeBlockListList(xs))
    case Block.HorizontalRule => taggedOnly(tt.HorizontalRule)
    case Block.Null => taggedOnly(tt.Null)
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
      (tt.ApiVersionField, Json.arr(Json.fromInt(1), Json.fromInt(23))),
      (tt.MetaField, Encoder[Meta].apply(pandoc.meta)),
      (tt.BlocksField, Encoder[List[Block]].apply(pandoc.blocks))
    )
  }

  // Inline decoding
  given Decoder[Inline] = Decoder.instance { cursor =>
    cursor.get[String](tt.TypeField).flatMap {
      case tt.Str => cursor.get[String](tt.ContentField).map(Inline.Str(_))
      case tt.Space => Right(Inline.Space)
      case tt.SoftBreak => Right(Inline.SoftBreak)
      case tt.LineBreak => Right(Inline.LineBreak)
      case tt.Emph =>
        cursor.get[Json](tt.ContentField).map { json =>
          Inline.Emph(decodeInlineList(json))
        }
      case tt.Strong =>
        cursor.get[Json](tt.ContentField).map { json =>
          Inline.Strong(decodeInlineList(json))
        }
      case tt.Strikeout =>
        cursor.get[Json](tt.ContentField).map { json =>
          Inline.Strikeout(decodeInlineList(json))
        }
      case tt.Code => cursor.get[List[Json]](tt.ContentField).map { c =>
        Inline.Code(decodeAttr(c(0)), c(1).asString.getOrElse(""))
      }
      case tt.Link => cursor.get[List[Json]](tt.ContentField).map { c =>
        Inline.Link(
          decodeAttr(c(0)),
          decodeInlineList(c(1)),
          (c(2).asString.getOrElse(""), c(3).asString.getOrElse(""))
        )
      }
      case tt.Image => cursor.get[List[Json]](tt.ContentField).map { c =>
        Inline.Image(
          decodeAttr(c(0)),
          decodeInlineList(c(1)),
          (c(2).asString.getOrElse(""), c(3).asString.getOrElse(""))
        )
      }
      case tt.RawInline => cursor.get[List[Json]](tt.ContentField).map { c =>
        Inline.RawInline(c(0).asString.getOrElse(""), c(1).asString.getOrElse(""))
      }
      case t => Left(DecodingFailure(s"Unknown Inline type: $t", cursor.history))
    }
  }

  // Block decoding
  given Decoder[Block] = Decoder.instance { cursor =>
    cursor.get[String](tt.TypeField).flatMap {
      case tt.Para =>
        cursor.get[Json](tt.ContentField).map { json =>
          Block.Para(decodeInlineList(json))
        }
      case tt.Plain =>
        cursor.get[Json](tt.ContentField).map { json =>
          Block.Plain(decodeInlineList(json))
        }
      case tt.Headline => cursor.get[List[Json]](tt.ContentField).map { c =>
        Block.Headline(
          c(0).asNumber.flatMap(_.toInt).getOrElse(1),
          decodeAttr(c(1)),
          decodeInlineList(c(2))
        )
      }
      case tt.CodeBlock => cursor.get[List[Json]](tt.ContentField).map { c =>
        Block.CodeBlock(
          decodeAttr(c(0)),
          c(1).asString.getOrElse("").linesIterator.toList
        )
      }
      case tt.BlockQuote =>
        cursor.get[Json](tt.ContentField).map { json =>
          Block.BlockQuote(decodeBlockList(json))
        }
      case tt.OrderedList => cursor.get[List[Json]](tt.ContentField).map { c =>
        Block.OrderedList(
          c(0).asNumber.flatMap(_.toInt).getOrElse(1),
          decodeBlockListList(c(1))
        )
      }
      case tt.BulletList => cursor.get[List[Json]](tt.ContentField).map { c =>
        Block.BulletList(decodeBlockListList(c(0)))
      }
      case tt.HorizontalRule => Right(Block.HorizontalRule)
      case tt.Null => Right(Block.Null)
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
      meta <- cursor.get[Meta](tt.MetaField)
      blocks <- cursor.get[List[Block]](tt.BlocksField)
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
