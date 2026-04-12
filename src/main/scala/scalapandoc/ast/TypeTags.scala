package scalapandoc.ast

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
