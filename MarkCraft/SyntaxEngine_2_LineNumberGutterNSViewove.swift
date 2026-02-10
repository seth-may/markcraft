import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog

// ══════════════════════════════════════════════════
// MARK: - Line Number Gutter (NSView overlay)
// ══════════════════════════════════════════════════
class LineNumberGutter: NSView {
    weak var textView: NSTextView?
    var fontSize: CGFloat
    var isDark: Bool
    
    init(textView: NSTextView, fontSize: CGFloat, isDark: Bool) {
        self.textView = textView
        self.fontSize = fontSize
        self.isDark = isDark
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) { fatalError() }
    
    override var isFlipped: Bool { true }
    
    override func draw(_ dirtyRect: NSRect) {
        guard let tv = textView, let lm = tv.layoutManager, let tc = tv.textContainer else { return }
        
        let gutterBg: NSColor
        if isDark {
            gutterBg = NSColor(red:0.035,green:0.031,blue:0.067,alpha:1)
        } else {
            gutterBg = NSColor(red:0.96,green:0.96,blue:0.97,alpha:1)
        }
        gutterBg.setFill()
        dirtyRect.fill()
        
        let visibleRect = tv.enclosingScrollView?.contentView.bounds ?? tv.visibleRect
        let textOffset = tv.textContainerInset.height
        let font = NSFont.monospacedSystemFont(ofSize: fontSize * 0.82, weight: .regular)
        let textColor: NSColor
        if isDark {
            textColor = NSColor(red:0.353,green:0.329,blue:0.439,alpha:1)
        } else {
            textColor = NSColor(red:0.6,green:0.6,blue:0.65,alpha:1)
        }
        let attrs: [NSAttributedString.Key: Any] = [.font: font, .foregroundColor: textColor]
        
        let text = tv.string as NSString
        var lineNum = 1
        var glyphIndex = 0
        let numberOfGlyphs = lm.numberOfGlyphs
        
        while glyphIndex < numberOfGlyphs {
            let charIndex = lm.characterIndexForGlyph(at: glyphIndex)
            let lineRange = text.lineRange(for: NSRange(location: charIndex, length: 0))
            let lineRect = lm.boundingRect(forGlyphRange: lm.glyphRange(forCharacterRange: lineRange, actualCharacterRange: nil), in: tc)
            
            let y = lineRect.origin.y + textOffset - visibleRect.origin.y
            
            if y + lineRect.height > 0 && y < visibleRect.height + 20 {
                let numStr = "\(lineNum)" as NSString
                let strSize = numStr.size(withAttributes: attrs)
                numStr.draw(at: NSPoint(x: 36 - strSize.width, y: y + (lineRect.height - strSize.height) / 2), withAttributes: attrs)
            }
            
            glyphIndex = NSMaxRange(lm.glyphRange(forCharacterRange: lineRange, actualCharacterRange: nil))
            lineNum += 1
            
            if y > visibleRect.height + 50 { break }
        }
    }
}

// ══════════════════════════════════════════════════
// MARK: - Minimap
// ══════════════════════════════════════════════════
struct Minimap: View {
    let source: String; let lang: SynLang?; let isDark: Bool
    var body: some View {
        _minBody()
    }
    @ViewBuilder
    private func _minBody() -> some View {
        Canvas { ctx, size in
            let lines = source.split(separator: "\n", omittingEmptySubsequences: false)
            let lineH: CGFloat = max(1.5, min(3, size.height / CGFloat(max(lines.count, 1))))
            let defColor: Color
            if isDark {
                defColor = Color(red:0.4,green:0.38,blue:0.48)
            } else {
                defColor = Color(red:0.6,green:0.6,blue:0.65)
            }
            let kwColor: Color
            if isDark {
                kwColor = Color(red:0.5,green:0.4,blue:0.75)
            } else {
                kwColor = Color(red:0.35,green:0.25,blue:0.65)
            }
            let strColor: Color
            if isDark {
                strColor = Color(red:0.25,green:0.6,blue:0.3)
            } else {
                strColor = Color(red:0.15,green:0.45,blue:0.2)
            }
            
            for (i, line) in lines.enumerated() {
                let y: CGFloat = CGFloat(i) * lineH
                guard y < size.height else { break }
                let trimmed = line.trimmingCharacters(in: .whitespaces)
                let indent: CGFloat = CGFloat(line.count - trimmed.count) * 1.5
                let w: CGFloat = min(size.width - indent, CGFloat(trimmed.count) * 1.2)
                guard w > 0 else { continue }
                
                var color = defColor
                if let lang = lang, !lang.slc.isEmpty && trimmed.hasPrefix(lang.slc) {
                    color = isDark ? Color(red:0.35,green:0.33,blue:0.42) : Color(red:0.65,green:0.65,blue:0.7)
                } else if trimmed.contains("\"") || trimmed.contains("'") {
                    color = strColor
                } else if let lang = lang {
                    let words = trimmed.split(whereSeparator: { !$0.isLetter && $0 != "_" }).map(String.init)
                    if words.first.map({ lang.kw.contains($0) }) == true { color = kwColor }
                }
                
                ctx.fill(Path(CGRect(x: indent, y: y, width: w, height: max(lineH - 0.5, 1))), with: .color(color.opacity(0.6)))
            }
        }
        .frame(width: 60)
    }
}

// ══════════════════════════════════════════════════
// MARK: - Language Picker View
// ══════════════════════════════════════════════════
struct LanguagePicker: View {
    @EnvironmentObject var st: AppState
    @EnvironmentObject var th: Theme
    @State private var search = ""
    
    var filteredLanguages: [SynLang] {
        search.isEmpty ? Syn.all : Syn.search(search)
    }
    
    var body: some View {
        let c = th.c
        return _lanBody(c: c)
    }
    @ViewBuilder
    private func _lanBody(c: MC) -> some View {
        VStack(spacing: 0) {
            HStack {
                Image(systemName: "magnifyingglass").foregroundColor(c.dim)
                TextField("Search languages…", text: $search)
                    .textFieldStyle(.plain).font(.system(size: 13))
                Text("\(Syn.languageCount) languages")
                    .font(.system(size: 11))
                    .foregroundColor(c.muted)
            }
            .padding(8)
            .background(c.card2)
            
            ScrollView {
                LazyVStack(alignment: .leading, spacing: 0) {
                    if search.isEmpty {
                        let _tArr164 = Array(Syn.byCategory())
                        ForEach(0..<_tArr164.count, id: \.self) { _ti164 in
                            let group = _tArr164[_ti164]
                            Text(group.category.rawValue.uppercased())
                                .font(.system(size: 10, weight: .bold))
                                .foregroundColor(c.muted)
                                .padding(.horizontal, 12).padding(.top, 12).padding(.bottom, 4)
                            
                            ForEach(group.languages) { lang in
                                languageRow(lang, c: c)
                            }
                        }
                    } else {
                        ForEach(filteredLanguages) { lang in
                            languageRow(lang, c: c)
                        }
                    }
                }
            }
        }
    }
    
    func languageRow(_ lang: SynLang, c: MC) -> some View {
        Button(action: {
            st.currentLanguage = lang.name
            if var doc = st.docs.first(where: { $0.id == st.currentDocId }) {
                doc.language = lang.name
            }
        }) {
            HStack(spacing: 8) {
                Image(systemName: lang.icon)
                    .foregroundColor(lang.color)
                    .frame(width: 20)
                Text(lang.name).font(.system(size: 13)).foregroundColor(c.text)
                Spacer()
                Text(".\(lang.ext)")
                    .font(.system(size: 11, design: .monospaced))
                    .foregroundColor(c.muted)
                if st.currentLanguage == lang.name {
                    Image(systemName: "checkmark")
                        .foregroundColor(c.gold)
                        .font(.system(size: 11))
                }
            }
            .padding(.horizontal, 12).padding(.vertical, 6)
            .background(_pc(st.currentLanguage == lang.name, c.card2, Color.clear))
        }
        .buttonStyle(.plain)
    }
}

// ══════════════════════════════════════════════════
// MARK: - Extended Language Definitions
// ══════════════════════════════════════════════════

extension Syn {
    static let extendedLanguages: [(ext: String, name: String, icon: String, keywords: [String])] = [
        ("zig", "Zig", "z.circle.fill", ["const", "var", "fn", "return", "if", "else", "while", "for", "switch", "struct", "enum", "union", "pub", "try", "catch", "defer", "comptime", "inline", "noalias", "threadlocal", "align", "test", "error", "undefined", "null", "true", "false"]),
        ("nim", "Nim", "n.circle.fill", ["proc", "func", "method", "var", "let", "const", "type", "object", "enum", "tuple", "seq", "string", "int", "float", "bool", "char", "if", "elif", "else", "case", "of", "while", "for", "in", "return", "yield", "import", "from", "include", "template", "macro", "iterator"]),
        ("elixir", "Elixir", "drop.fill", ["def", "defp", "defmodule", "defprotocol", "defimpl", "defstruct", "defmacro", "do", "end", "if", "else", "unless", "cond", "case", "when", "fn", "raise", "try", "rescue", "catch", "after", "for", "in", "with", "import", "require", "use", "alias", "pipe_through"]),
        ("haskell", "Haskell", "h.circle.fill", ["module", "where", "import", "qualified", "as", "hiding", "data", "type", "newtype", "class", "instance", "deriving", "if", "then", "else", "case", "of", "let", "in", "where", "do", "return", "forall", "infixl", "infixr", "infix"]),
        ("ocaml", "OCaml", "o.circle.fill", ["let", "in", "val", "fun", "function", "match", "with", "type", "module", "sig", "struct", "end", "open", "include", "if", "then", "else", "begin", "for", "while", "do", "done", "to", "downto", "rec", "and", "or", "not", "true", "false", "ref", "mutable"]),
        ("clojure", "Clojure", "c.circle.fill", ["def", "defn", "defmacro", "defonce", "let", "fn", "if", "when", "cond", "case", "do", "loop", "recur", "for", "doseq", "map", "filter", "reduce", "apply", "partial", "comp", "atom", "deref", "swap!", "reset!", "ns", "require", "use", "import"]),
        ("julia", "Julia", "j.circle.fill", ["function", "end", "return", "if", "elseif", "else", "for", "while", "do", "begin", "let", "const", "local", "global", "struct", "mutable", "abstract", "primitive", "type", "module", "using", "import", "export", "macro", "try", "catch", "finally", "throw"]),
        ("dart", "Dart", "d.circle.fill", ["abstract", "as", "assert", "async", "await", "break", "case", "catch", "class", "const", "continue", "default", "deferred", "do", "dynamic", "else", "enum", "export", "extends", "extension", "external", "factory", "false", "final", "finally", "for", "Function", "get", "if", "implements", "import", "in", "interface", "is", "late", "library", "mixin", "new", "null", "on", "operator", "part", "required", "rethrow", "return", "sealed", "set", "show", "static", "super", "switch", "sync", "this", "throw", "true", "try", "typedef", "var", "void", "while", "with", "yield"]),
        ("v", "V", "v.circle.fill", ["fn", "mut", "struct", "enum", "union", "interface", "import", "module", "pub", "return", "if", "else", "for", "in", "match", "or", "and", "not", "go", "spawn", "defer", "unsafe", "shared", "rlock", "lock", "assert", "as", "is", "none", "true", "false"]),
        ("crystal", "Crystal", "diamond.fill", ["def", "end", "class", "struct", "module", "enum", "lib", "fun", "macro", "if", "elsif", "else", "unless", "case", "when", "while", "until", "for", "in", "do", "begin", "rescue", "ensure", "raise", "return", "yield", "require", "include", "extend", "abstract", "private", "protected"]),
    ]
    
    
    static func allLanguageNames() -> [String] {
        var names = Syn.all.map { $0.name }
        names.append(contentsOf: extendedLanguages.map { $0.name })
        return names.sorted()
    }
}

// ══════════════════════════════════════════════════
// MARK: - Syntax Theme System
// ══════════════════════════════════════════════════

struct SyntaxTheme: Identifiable {
    let id: String; let name: String
    let keyword: NSColor; let string: NSColor; let comment: NSColor
    let number: NSColor; let type: NSColor; let function: NSColor
    let variable: NSColor; let operator_: NSColor; let preprocessor: NSColor
    let background: NSColor; let foreground: NSColor; let selection: NSColor
}

class SyntaxThemeManager: ObservableObject {
    @Published var currentTheme: String = "obsidian"
    
    static let themes: [SyntaxTheme] = [
        SyntaxTheme(id: "obsidian", name: "Obsidian Dark",
            keyword: NSColor(red: 0.8, green: 0.55, blue: 1.0, alpha: 1),
            string: NSColor(red: 0.6, green: 0.9, blue: 0.55, alpha: 1),
            comment: NSColor(red: 0.45, green: 0.43, blue: 0.52, alpha: 1),
            number: NSColor(red: 0.9, green: 0.72, blue: 0.35, alpha: 1),
            type: NSColor(red: 0.4, green: 0.85, blue: 0.95, alpha: 1),
            function: NSColor(red: 0.95, green: 0.8, blue: 0.45, alpha: 1),
            variable: NSColor(red: 0.85, green: 0.85, blue: 0.9, alpha: 1),
            operator_: NSColor(red: 0.9, green: 0.55, blue: 0.65, alpha: 1),
            preprocessor: NSColor(red: 0.75, green: 0.5, blue: 0.3, alpha: 1),
            background: NSColor(red: 0.06, green: 0.05, blue: 0.1, alpha: 1),
            foreground: NSColor(red: 0.85, green: 0.83, blue: 0.9, alpha: 1),
            selection: NSColor(red: 0.25, green: 0.2, blue: 0.4, alpha: 0.5)
        ),
        SyntaxTheme(id: "solarized_dark", name: "Solarized Dark",
            keyword: NSColor(red: 0.52, green: 0.6, blue: 0.0, alpha: 1),
            string: NSColor(red: 0.16, green: 0.63, blue: 0.6, alpha: 1),
            comment: NSColor(red: 0.35, green: 0.43, blue: 0.46, alpha: 1),
            number: NSColor(red: 0.83, green: 0.43, blue: 0.0, alpha: 1),
            type: NSColor(red: 0.15, green: 0.55, blue: 0.82, alpha: 1),
            function: NSColor(red: 0.71, green: 0.54, blue: 0.0, alpha: 1),
            variable: NSColor(red: 0.51, green: 0.58, blue: 0.59, alpha: 1),
            operator_: NSColor(red: 0.86, green: 0.2, blue: 0.18, alpha: 1),
            preprocessor: NSColor(red: 0.83, green: 0.43, blue: 0.0, alpha: 1),
            background: NSColor(red: 0.0, green: 0.17, blue: 0.21, alpha: 1),
            foreground: NSColor(red: 0.51, green: 0.58, blue: 0.59, alpha: 1),
            selection: NSColor(red: 0.07, green: 0.26, blue: 0.3, alpha: 0.6)
        ),
        SyntaxTheme(id: "monokai", name: "Monokai",
            keyword: NSColor(red: 0.97, green: 0.15, blue: 0.47, alpha: 1),
            string: NSColor(red: 0.9, green: 0.86, blue: 0.45, alpha: 1),
            comment: NSColor(red: 0.46, green: 0.44, blue: 0.37, alpha: 1),
            number: NSColor(red: 0.68, green: 0.51, blue: 1.0, alpha: 1),
            type: NSColor(red: 0.4, green: 0.85, blue: 0.94, alpha: 1),
            function: NSColor(red: 0.65, green: 0.89, blue: 0.18, alpha: 1),
            variable: NSColor(red: 0.97, green: 0.97, blue: 0.95, alpha: 1),
            operator_: NSColor(red: 0.97, green: 0.15, blue: 0.47, alpha: 1),
            preprocessor: NSColor(red: 0.68, green: 0.51, blue: 1.0, alpha: 1),
            background: NSColor(red: 0.15, green: 0.16, blue: 0.13, alpha: 1),
            foreground: NSColor(red: 0.97, green: 0.97, blue: 0.95, alpha: 1),
            selection: NSColor(red: 0.27, green: 0.28, blue: 0.21, alpha: 0.6)
        ),
        SyntaxTheme(id: "nord", name: "Nord",
            keyword: NSColor(red: 0.51, green: 0.63, blue: 0.76, alpha: 1),
            string: NSColor(red: 0.65, green: 0.74, blue: 0.55, alpha: 1),
            comment: NSColor(red: 0.38, green: 0.43, blue: 0.5, alpha: 1),
            number: NSColor(red: 0.7, green: 0.56, blue: 0.68, alpha: 1),
            type: NSColor(red: 0.56, green: 0.74, blue: 0.73, alpha: 1),
            function: NSColor(red: 0.53, green: 0.75, blue: 0.82, alpha: 1),
            variable: NSColor(red: 0.85, green: 0.87, blue: 0.91, alpha: 1),
            operator_: NSColor(red: 0.51, green: 0.63, blue: 0.76, alpha: 1),
            preprocessor: NSColor(red: 0.7, green: 0.56, blue: 0.68, alpha: 1),
            background: NSColor(red: 0.18, green: 0.2, blue: 0.25, alpha: 1),
            foreground: NSColor(red: 0.85, green: 0.87, blue: 0.91, alpha: 1),
            selection: NSColor(red: 0.26, green: 0.3, blue: 0.37, alpha: 0.6)
        ),
        SyntaxTheme(id: "light", name: "Light",
            keyword: NSColor(red: 0.0, green: 0.0, blue: 0.8, alpha: 1),
            string: NSColor(red: 0.77, green: 0.1, blue: 0.09, alpha: 1),
            comment: NSColor(red: 0.0, green: 0.5, blue: 0.0, alpha: 1),
            number: NSColor(red: 0.0, green: 0.0, blue: 0.8, alpha: 1),
            type: NSColor(red: 0.17, green: 0.38, blue: 0.53, alpha: 1),
            function: NSColor(red: 0.47, green: 0.13, blue: 0.53, alpha: 1),
            variable: NSColor(red: 0.15, green: 0.15, blue: 0.15, alpha: 1),
            operator_: NSColor(red: 0.0, green: 0.0, blue: 0.0, alpha: 1),
            preprocessor: NSColor(red: 0.5, green: 0.31, blue: 0.0, alpha: 1),
            background: NSColor.white,
            foreground: NSColor(red: 0.15, green: 0.15, blue: 0.15, alpha: 1),
            selection: NSColor(red: 0.73, green: 0.84, blue: 0.95, alpha: 0.5)
        ),
    ]
    
    static func theme(for id: String) -> SyntaxTheme { themes.first { $0.id == id } ?? themes[0] }
}

struct SyntaxThemePickerView: View {
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    @ObservedObject var themeManager: SyntaxThemeManager
    var body: some View {
        let c = th.c
        return _synBody(c: c)
    }
    @ViewBuilder
    private func _synBody(c: MC) -> some View {
        VStack(alignment: .leading, spacing: MCSpacing.md) {
            Text(loc.t("syntax_theme"))
                .font(MCFont.sidebarHeader)
                .foregroundColor(c.muted)
            ForEach(SyntaxThemeManager.themes) { theme in
                Button(action: { themeManager.currentTheme = theme.id }) {
                    HStack(spacing: MCSpacing.sm) {
                        // Color preview
                        HStack(spacing: 2) {
                            Circle().fill(Color(theme.keyword)).frame(width: 8, height: 8)
                            Circle().fill(Color(theme.string)).frame(width: 8, height: 8)
                            Circle().fill(Color(theme.comment)).frame(width: 8, height: 8)
                            Circle().fill(Color(theme.number)).frame(width: 8, height: 8)
                        }
                        Text(theme.name)
                            .font(.system(size: 12))
                            .foregroundColor(_pc(themeManager.currentTheme == theme.id, c.text, c.dim))
                        Spacer()
                        if themeManager.currentTheme == theme.id {
                            Image(systemName: "checkmark")
                                .font(.system(size: 10))
                                .foregroundColor(c.gold)
                        }
                    }.padding(MCSpacing.sm)
                        .background(_pc(themeManager.currentTheme == theme.id, c.card2, Color.clear))
                        .cornerRadius(MCRadius.sm)
                }.buttonStyle(.plain)
            }
        }
    }
}

// Ruby keywords (44)
extension Syn {
    static let rubyKeywords: Set<String> = [
        "module", "class", "def", "end", "if", "elsif",
        "else", "unless", "case", "when", "while", "until",
        "for", "do", "begin", "rescue", "ensure", "raise",
        "return", "yield", "require", "include", "extend", "attr_reader",
        "attr_writer", "attr_accessor", "puts", "print", "gets", "nil",
        "true", "false", "self", "super", "lambda", "proc",
        "and", "or", "not", "in", "then", "retry",
        "break", "next"
    ]
}

// Go keywords (38)
extension Syn {
    static let goKeywords: Set<String> = [
        "package", "import", "func", "return", "var", "const",
        "type", "struct", "interface", "map", "chan", "go",
        "select", "switch", "case", "default", "if", "else",
        "for", "range", "break", "continue", "goto", "defer",
        "fallthrough", "nil", "true", "false", "make", "new",
        "append", "copy", "delete", "len", "cap", "close",
        "panic", "recover"
    ]
}

// Kotlin keywords (42)
extension Syn {
    static let kotlinKeywords: Set<String> = [
        "fun", "val", "var", "class", "object", "interface",
        "abstract", "open", "sealed", "data", "enum", "companion",
        "init", "constructor", "return", "if", "else", "when",
        "for", "while", "do", "try", "catch", "finally",
        "throw", "import", "package", "as", "is", "in",
        "out", "override", "suspend", "inline", "lateinit", "by",
        "lazy", "null", "true", "false", "this", "super"
    ]
}

// Scala keywords (37)
extension Syn {
    static let scalaKeywords: Set<String> = [
        "def", "val", "var", "class", "object", "trait",
        "abstract", "sealed", "case", "extends", "with", "import",
        "package", "return", "if", "else", "match", "for",
        "while", "do", "yield", "try", "catch", "finally",
        "throw", "new", "this", "super", "override", "implicit",
        "lazy", "type", "private", "protected", "null", "true",
        "false"
    ]
}

// Lua keywords (44)
extension Syn {
    static let luaKeywords: Set<String> = [
        "local", "function", "end", "return", "if", "then",
        "else", "elseif", "for", "while", "do", "repeat",
        "until", "break", "in", "and", "or", "not",
        "nil", "true", "false", "goto", "require", "module",
        "setmetatable", "getmetatable", "pairs", "ipairs", "next", "select",
        "unpack", "table", "string", "math", "io", "os",
        "coroutine", "pcall", "error", "assert", "type", "tostring",
        "tonumber", "print"
    ]
}

// PHP keywords (51)
extension Syn {
    static let phpKeywords: Set<String> = [
        "function", "class", "interface", "abstract", "extends", "implements",
        "public", "private", "protected", "static", "final", "const",
        "var", "new", "return", "if", "else", "elseif",
        "switch", "case", "default", "for", "foreach", "while",
        "do", "break", "continue", "try", "catch", "finally",
        "throw", "use", "namespace", "require", "include", "echo",
        "print", "isset", "unset", "empty", "array", "list",
        "null", "true", "false", "self", "parent", "match",
        "enum", "readonly", "fn"
    ]
}

// Perl keywords (42)
extension Syn {
    static let perlKeywords: Set<String> = [
        "sub", "my", "our", "local", "use", "require",
        "package", "return", "if", "elsif", "else", "unless",
        "while", "until", "for", "foreach", "do", "last",
        "next", "die", "warn", "print", "say", "chomp",
        "push", "pop", "shift", "unshift", "splice", "grep",
        "map", "sort", "reverse", "keys", "values", "exists",
        "delete", "defined", "undef", "ref", "bless", "eval"
    ]
}

// R keywords (41)
extension Syn {
    static let rKeywords: Set<String> = [
        "function", "if", "else", "for", "while", "repeat",
        "break", "next", "return", "in", "library", "require",
        "source", "TRUE", "FALSE", "NULL", "NA", "Inf",
        "NaN", "list", "c", "data.frame", "matrix", "array",
        "vector", "factor", "class", "print", "cat", "paste",
        "sprintf", "apply", "sapply", "lapply", "tapply", "which",
        "match", "grep", "gsub", "nchar", "substr"
    ]
}

// Shell keywords (53)
extension Syn {
    static let shellKeywords: Set<String> = [
        "if", "then", "else", "elif", "fi", "case",
        "esac", "for", "while", "until", "do", "done",
        "in", "function", "return", "exit", "break", "continue",
        "local", "export", "readonly", "declare", "unset", "shift",
        "source", "eval", "exec", "trap", "set", "echo",
        "printf", "read", "test", "true", "false", "cd",
        "pwd", "ls", "mkdir", "rm", "cp", "mv",
        "cat", "grep", "sed", "awk", "find", "xargs",
        "sort", "uniq", "wc", "head", "tail"
    ]
}

// MARK: - Token Types
enum SynToken: String {
    case keyword, string, comment, number, type, function, variable
    case operator_, preprocessor, attribute, regex, interpolation
    case docComment, annotation, label, lifetime, macro
}

// MARK: - Language Feature Flags
struct LanguageFeatures {
    let hasClasses: Bool; let hasStructs: Bool; let hasEnums: Bool
    let hasProtocols: Bool; let hasGenerics: Bool; let hasAsync: Bool
    let hasPatternMatching: Bool; let hasMacros: Bool; let hasModules: Bool
    let lineComment: String; let blockCommentStart: String; let blockCommentEnd: String
    let stringDelimiters: [Character]; let supportsInterpolation: Bool
    
    static let swift = LanguageFeatures(hasClasses: true, hasStructs: true, hasEnums: true, hasProtocols: true, hasGenerics: true, hasAsync: true, hasPatternMatching: true, hasMacros: true, hasModules: true, lineComment: "//", blockCommentStart: "/*", blockCommentEnd: "*/", stringDelimiters: ["\""], supportsInterpolation: true)
    static let python = LanguageFeatures(hasClasses: true, hasStructs: false, hasEnums: false, hasProtocols: false, hasGenerics: false, hasAsync: true, hasPatternMatching: true, hasMacros: false, hasModules: true, lineComment: "#", blockCommentStart: "\"\"\"", blockCommentEnd: "\"\"\"", stringDelimiters: ["\"", "'"], supportsInterpolation: true)
    static let rust = LanguageFeatures(hasClasses: false, hasStructs: true, hasEnums: true, hasProtocols: true, hasGenerics: true, hasAsync: true, hasPatternMatching: true, hasMacros: true, hasModules: true, lineComment: "//", blockCommentStart: "/*", blockCommentEnd: "*/", stringDelimiters: ["\""], supportsInterpolation: false)
    static let typescript = LanguageFeatures(hasClasses: true, hasStructs: false, hasEnums: true, hasProtocols: true, hasGenerics: true, hasAsync: true, hasPatternMatching: false, hasMacros: false, hasModules: true, lineComment: "//", blockCommentStart: "/*", blockCommentEnd: "*/", stringDelimiters: ["\"", "'", "`"], supportsInterpolation: true)
}

// ════════════════════════════════════════════════════════════
