# Project Structure

```text
chicala
|-- build.sbt
|-- docs/
|-- src/main/scala/chicala/
|   |-- ast/                             : Chicala internal AST
|   |-- convert/
|   |   |-- backend/                     : Emit scala code for stainless or simulation
|   |   |-- frontend/                    : Read Chisel into Chicala AST
|   |   |   |-- astloader/               : Generate a Chicala AST node
|   |   |   |-- treereader/              : Read a Scala AST tree
|   |   |   `-- ...
|   |   |-- pass/                        : Transforms on the Chicala AST
|   |   `-- ChiselToScalaComponent.scala : Process on each Chisel module
|   |-- util/
|   `-- ChicalaPlugin.scala              : Compiler plugin entrance
`-- testcase/                            : Test cases
```
