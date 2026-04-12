package scalapandoc.ast

/** Block elements */
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
