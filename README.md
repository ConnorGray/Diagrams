# Diagram Maker

Diagram Maker is a tool for programmatically constructing diagrams, using high-level
semantically-meaningful expressions for modeling the domain being diagramed.

[**View the Example Gallery**](https://connorgray.com/project/diagrams#example-gallery)

Example:

```wolfram
StringEncodingDiagram[
    "Hey 👋🏻",
    {"Bits", "Bytes", "Codepoints", "Characters", "Graphemes", "String"},
    CharacterEncoding -> "UTF-8",
    ChartLegends -> Automatic
]
```

![Rendered StringEncodingDiagram.](./docs/images/StringEncodingDiagram-1.png)

## Development

See [**Development.md**](./docs/Development.md) for information on contributing to
Diagram Maker.