# Scalapandoc

A Scala implementation of [pandoc](https://pandoc.org/) - a universal document converter.

## Features

- **Markdown to Markdown conversion** (initial implementation)
- **Pandoc-compatible JSON AST** - exchange documents with pandoc filters
- **Cross-language filter support** - write filters in any language via JSON
- **Pure Scala 3** implementation

## Project Structure

```
scalapandoc/
├── src/main/scala/scalapandoc/
│   ├── ast/          # Pandoc AST definitions with JSON encoding/decoding
│   ├── reader/       # Markdown reader (using flexmark-java)
│   ├── writer/       # Markdown writer
│   ├── filter/       # Filter system for AST transformation
│   └── cli/          # Command-line interface
└── src/test/scala/   # Tests
```

## Building

```bash
sbt compile
```

## Running

```bash
sbt run
```

### Examples

# Convert Markdown to Markdown
sbt "run --input README.md --output output.md"

# Output AST as JSON
sbt "run --input README.md --to-json ast.json"

# Read AST from JSON and output Markdown
sbt "run --from-json ast.json --output output.md"

# Apply a filter
sbt "run --input README.md --filter ./my-filter.py --output output.md"

# Apply built-in capitalize filter
sbt "run --input README.md --capitalize --output output.md"
```

## Pandoc AST Compatibility

The AST format follows pandoc's JSON structure:

```json
{
  "pandoc-api-version": [1, 23],
  "meta": { ... },
  "blocks": [
    {
      "t": "Para",
      "c": [
        { "t": "Str", "c": "Hello" },
        { "t": "Space" },
        { "t": "Str", "c": "World" }
      ]
    }
  ]
}
```

## Writing Filters

Filters read JSON AST from stdin and write to stdout:

```python
#!/usr/bin/env python3
import sys, json

def transform(inline):
    if inline.get('t') == 'Str':
        inline['c'] = inline['c'].upper()
    return inline

ast = json.loads(sys.stdin.read())
# Apply transformations...
print(json.dumps(ast))
```

## License

MIT
