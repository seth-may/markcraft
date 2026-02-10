# MarkCraft Documentation

> A poetic IDE for the creative soul

## Features

MarkCraft combines **powerful code editing** with *elegant design*,
creating an environment where programming becomes art.

### Syntax Highlighting

Supports `60+ languages` with semantic token coloring:

- **Systems**: C, C++, Rust, Go, Zig, Nim
- **Scripting**: Python, Ruby, Perl, Lua
- **Web**: JavaScript, TypeScript, HTML, CSS
- **Functional**: Haskell, Elixir, Scala, OCaml

### Code Example

```swift
@Observable class Editor {
    var documents: [Document] = []
    var activeTheme: Theme = .midnight
    
    func openFile(_ url: URL) async throws {
        let content = try String(contentsOf: url)
        let doc = Document(url: url, content: content)
        documents.append(doc)
    }
}
```

### Keyboard Shortcuts

| Action | Shortcut | Context |
|--------|----------|---------|
| New File | Cmd+N | Global |
| Save | Cmd+S | Editor |
| Preview | Opt+Cmd+P | Editor |
| Command Palette | Shift+Cmd+P | Global |
| Find & Replace | Cmd+F | Editor |

### Architecture

The app follows a clean **MVVM** pattern:

1. **Model**: `MCDoc`, `SynLang`, `LangDoc`
2. **ViewModel**: `AppState` with `@Published` properties
3. **View**: SwiftUI views with `@EnvironmentObject`

---

*Built with love by MarkCraft — [markcraft.fr](https://markcraft.fr)*
