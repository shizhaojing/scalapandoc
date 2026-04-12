package scalapandoc.ast

/** Inline elements */
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
