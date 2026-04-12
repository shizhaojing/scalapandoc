package scalapandoc.ast

/** The root Pandoc document */
case class Pandoc(meta: Meta, blocks: List[Block])
