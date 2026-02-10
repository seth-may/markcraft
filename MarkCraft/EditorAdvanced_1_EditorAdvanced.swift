import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct BracketMatcher {
    static func findMatch(in text: String, at position: Int) -> Int? { nil }
    static func indentLevel(for line: String) -> Int { line.prefix(while: { $0 == " " || $0 == "\t" }).count / 4 }
}
class FoldingEngine: ObservableObject {
    struct FoldRegion: Identifiable { let id = UUID(); let startLine: Int; let endLine: Int; let label: String; var isFolded: Bool = false }
    @Published var regions: [FoldRegion] = []
}
struct CompletionItem: Identifiable { let id = UUID(); let label: String; let detail: String; let kind: CompletionKind }
enum CompletionKind: String { case keyword, type, function, variable, snippet }
class CompletionEngine: ObservableObject {
    @Published var suggestions: [CompletionItem] = []
    @Published var isActive = false
    func update(source: String, cursor: Int, lang: SynLang?) { }
}
struct CompletionPopupView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
class GhostTextEngine: ObservableObject { @Published var suggestion = "" }
struct GhostTextOverlay: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct BreadcrumbView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct IndentGuidesView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct IndentGuidesOverlay: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct SymbolInfo: Identifiable { let id = UUID(); var name: String; var kind: SymbolKind; var line: Int; var children: [SymbolInfo] = [] }
enum SymbolKind: String { case function, type, variable, constant, property }
class SymbolOutlineProvider: ObservableObject { @Published var symbols: [SymbolInfo] = [] }
struct SymbolOutlineView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }




// MARK: - Markdown Preview (Pure SwiftUI — no WKWebView)
// MARK: - Sidebar Toggle Button (chevron to show/hide sidebar)
struct SidebarToggleButton: View {
    @Binding var isOpen: Bool
    var color: Color
    var accent: Color
    @State private var isHover = false
    
    var body: some View {
        Button(action: {
            withAnimation(.easeInOut(duration: 0.2)) { isOpen.toggle() }
        }) {
            Image(systemName: isOpen ? "chevron.left" : "chevron.right")
                .font(.system(size: 9, weight: .bold))
                .foregroundColor(isHover ? accent : color)
                .frame(width: 16, height: 44)
                .background(
                    RoundedRectangle(cornerRadius: 4)
                        .fill(isHover ? accent.opacity(0.1) : Color.clear)
                )
        }
        .buttonStyle(.plain)
        .onHover { h in withAnimation(.easeInOut(duration: 0.15)) { isHover = h } }
        .help(isOpen ? "Hide sidebar" : "Show sidebar")
    }
}

// MARK: - Split Handle (draggable divider between editor and preview)
struct SplitHandle: View {
    var color: Color
    var accent: Color
    @State private var isHover = false
    
    var body: some View {
        Rectangle()
            .fill(isHover ? accent.opacity(0.5) : color)
            .frame(width: isHover ? 4 : 2)
            .contentShape(Rectangle().inset(by: -4))
            .onHover { h in
                withAnimation(.easeInOut(duration: 0.15)) { isHover = h }
                if h {
                    NSCursor.resizeLeftRight.push()
                } else {
                    NSCursor.pop()
                }
            }
            .padding(.horizontal, 2)
    }
}

// MARK: - ⌘+Scroll Wheel zoom modifier
struct CmdScrollZoom: ViewModifier {
    @Binding var fontSize: Double
    
    func body(content: Content) -> some View {
        content
            .onAppear {
                NSEvent.addLocalMonitorForEvents(matching: .scrollWheel) { event in
                    // ⌘+scroll wheel to zoom
                    if event.modifierFlags.contains(.command) && event.scrollingDeltaY != 0 {
                        let delta = event.scrollingDeltaY > 0 ? 0.5 : -0.5
                        DispatchQueue.main.async {
                            fontSize = min(max(fontSize + delta, 9), 32)
                        }
                        return nil // consume event
                    }
                    return event
                }
            }
    }
}

extension View {
    func cmdScrollZoom(fontSize: Binding<Double>) -> some View {
        modifier(CmdScrollZoom(fontSize: fontSize))
    }
}

struct MarkdownPreviewView: View {
    var source: String
    var isDark: Bool
    var fontScale: CGFloat = 1.0
    
    var body: some View {
        ScrollView(.vertical, showsIndicators: true) {
            VStack(alignment: .leading, spacing: 0) {
                ForEach(Array(parseBlocks(source).enumerated()), id: \.offset) { _, block in
                    renderBlock(block)
                }
            }
            .padding(20)
            .frame(maxWidth: .infinity, alignment: .leading)
            .font(.system(size: 14 * fontScale))
        }
        .background(isDark ? Color(red:0.027,green:0.024,blue:0.055) : Color.white)
    }
    
    // MARK: - Block types
    enum MdBlock {
        case heading(Int, String)
        case paragraph(String)
        case code(String, String)
        case bullet(String)
        case numbered(Int, String)
        case blockquote(String)
        case hr
        case table([[String]])
    }
    
    // MARK: - Parser
    func parseBlocks(_ md: String) -> [MdBlock] {
        var blocks: [MdBlock] = []
        let lines = md.components(separatedBy: "\n")
        var i = 0
        while i < lines.count {
            let line = lines[i]
            let trimmed = line.trimmingCharacters(in: .whitespaces)
            
            // Fenced code
            if trimmed.hasPrefix("```") {
                let lang = String(trimmed.dropFirst(3)).trimmingCharacters(in: .whitespaces)
                var code: [String] = []
                i += 1
                while i < lines.count && !lines[i].trimmingCharacters(in: .whitespaces).hasPrefix("```") {
                    code.append(lines[i])
                    i += 1
                }
                blocks.append(.code(lang, code.joined(separator: "\n")))
                i += 1; continue
            }
            
            // Empty line
            if trimmed.isEmpty { i += 1; continue }
            
            // HR
            if trimmed == "---" || trimmed == "***" || trimmed == "___" {
                blocks.append(.hr); i += 1; continue
            }
            
            // Headings
            if trimmed.hasPrefix("######") { blocks.append(.heading(6, String(trimmed.dropFirst(6).drop(while: { $0 == " " })))); i += 1; continue }
            if trimmed.hasPrefix("#####") { blocks.append(.heading(5, String(trimmed.dropFirst(5).drop(while: { $0 == " " })))); i += 1; continue }
            if trimmed.hasPrefix("####") { blocks.append(.heading(4, String(trimmed.dropFirst(4).drop(while: { $0 == " " })))); i += 1; continue }
            if trimmed.hasPrefix("###") { blocks.append(.heading(3, String(trimmed.dropFirst(3).drop(while: { $0 == " " })))); i += 1; continue }
            if trimmed.hasPrefix("##") { blocks.append(.heading(2, String(trimmed.dropFirst(2).drop(while: { $0 == " " })))); i += 1; continue }
            if trimmed.hasPrefix("# ") { blocks.append(.heading(1, String(trimmed.dropFirst(2)))); i += 1; continue }
            
            // Blockquote
            if trimmed.hasPrefix(">") {
                let content = String(trimmed.dropFirst(1).drop(while: { $0 == " " }))
                blocks.append(.blockquote(content)); i += 1; continue
            }
            
            // Table (pipes)
            if trimmed.contains("|") && !trimmed.hasPrefix("-") {
                var rows: [[String]] = []
                while i < lines.count {
                    let tl = lines[i].trimmingCharacters(in: .whitespaces)
                    guard tl.contains("|") else { break }
                    let cells = tl.split(separator: "|").map { $0.trimmingCharacters(in: .whitespaces) }
                    if !cells.allSatisfy({ $0.allSatisfy({ $0 == "-" || $0 == ":" || $0 == " " }) }) {
                        rows.append(cells)
                    }
                    i += 1
                }
                if !rows.isEmpty { blocks.append(.table(rows)) }
                continue
            }
            
            // Bullet list
            if trimmed.hasPrefix("- ") || trimmed.hasPrefix("* ") || trimmed.hasPrefix("+ ") {
                blocks.append(.bullet(String(trimmed.dropFirst(2)))); i += 1; continue
            }
            
            // Numbered list
            if let dot = trimmed.firstIndex(of: "."),
               trimmed[trimmed.startIndex..<dot].allSatisfy({ $0.isNumber }),
               let num = Int(trimmed[trimmed.startIndex..<dot]) {
                let after = trimmed[trimmed.index(after: dot)...].drop(while: { $0 == " " })
                blocks.append(.numbered(num, String(after))); i += 1; continue
            }
            
            // Paragraph (collect contiguous lines)
            var para = [trimmed]
            i += 1
            while i < lines.count {
                let next = lines[i].trimmingCharacters(in: .whitespaces)
                if next.isEmpty || next.hasPrefix("#") || next.hasPrefix("```") || next.hasPrefix(">")
                    || next.hasPrefix("- ") || next.hasPrefix("* ") || next.hasPrefix("---")
                    || next.contains("|") { break }
                para.append(next); i += 1
            }
            blocks.append(.paragraph(para.joined(separator: " ")))
        }
        return blocks
    }
    
    // MARK: - Renderers
    @ViewBuilder
    func renderBlock(_ block: MdBlock) -> some View {
        switch block {
        case .heading(let level, let text):
            let size: CGFloat = (level == 1 ? 24 : level == 2 ? 20 : level == 3 ? 17 : 14) * fontScale
            let weight: Font.Weight = level <= 2 ? .bold : .semibold
            let accent = isDark ? Color(red:0.655,green:0.545,blue:0.98) : Color(red:0.42,green:0.33,blue:0.82)
            Text(text)
                .font(.system(size: size, weight: weight))
                .foregroundColor(level <= 2 ? accent : textColor)
                .padding(.top, level <= 2 ? 16 : 10)
                .padding(.bottom, 4)
            if level <= 2 {
                Rectangle().fill(borderColor).frame(height: 1).padding(.bottom, 6)
            }
            
        case .paragraph(let text):
            inlineText(text)
                .padding(.vertical, 4)
            
        case .code(_, let code):
            let codeBg = isDark ? Color(red:0.055,green:0.047,blue:0.102) : Color(red:0.96,green:0.96,blue:0.97)
            ScrollView(.horizontal, showsIndicators: false) {
                Text(code)
                    .font(.system(size: 12 * fontScale, design: .monospaced))
                    .foregroundColor(isDark ? Color(red:0.91,green:0.89,blue:0.94) : Color(red:0.11,green:0.11,blue:0.145))
                    .padding(12)
            }
            .background(RoundedRectangle(cornerRadius: 8).fill(codeBg))
            .overlay(RoundedRectangle(cornerRadius: 8).stroke(borderColor, lineWidth: 1))
            .padding(.vertical, 6)
            
        case .bullet(let text):
            HStack(alignment: .top, spacing: 8) {
                Text("•").foregroundColor(accentColor).padding(.top, 2)
                inlineText(text)
            }.padding(.leading, 8).padding(.vertical, 2)
            
        case .numbered(let n, let text):
            HStack(alignment: .top, spacing: 8) {
                Text("\(n).").foregroundColor(accentColor).font(.system(size: 13 * fontScale, weight: .semibold)).frame(width: 20, alignment: .trailing)
                inlineText(text)
            }.padding(.leading, 8).padding(.vertical, 2)
            
        case .blockquote(let text):
            HStack(spacing: 0) {
                Rectangle().fill(accentColor).frame(width: 3)
                inlineText(text)
                    .padding(.horizontal, 12)
                    .padding(.vertical, 6)
                    .opacity(0.85)
            }
            .padding(.vertical, 4)
            
        case .hr:
            Rectangle().fill(borderColor).frame(height: 1).padding(.vertical, 12)
            
        case .table(let rows):
            let codeBg = isDark ? Color(red:0.055,green:0.047,blue:0.102) : Color(red:0.96,green:0.96,blue:0.97)
            VStack(spacing: 0) {
                ForEach(Array(rows.enumerated()), id: \.offset) { rowIdx, cells in
                    HStack(spacing: 0) {
                        ForEach(Array(cells.enumerated()), id: \.offset) { _, cell in
                            Text(cell)
                                .font(.system(size: 12 * fontScale, weight: rowIdx == 0 ? .semibold : .regular))
                                .foregroundColor(textColor)
                                .padding(8)
                                .frame(maxWidth: .infinity, alignment: .leading)
                                .background(rowIdx == 0 ? codeBg : Color.clear)
                                .overlay(Rectangle().stroke(borderColor, lineWidth: 0.5))
                        }
                    }
                }
            }
            .cornerRadius(6)
            .overlay(RoundedRectangle(cornerRadius: 6).stroke(borderColor, lineWidth: 1))
            .padding(.vertical, 6)
        }
    }
    
    func inlineText(_ text: String) -> Text {
        // Simple inline rendering: **bold**, *italic*, `code`, ~~strike~~
        var result = Text("")
        var remaining = text[text.startIndex...]
        
        while !remaining.isEmpty {
            // Inline code `...`
            if remaining.hasPrefix("`"), let endIdx = remaining.dropFirst().firstIndex(of: "`") {
                let code = remaining[remaining.index(after: remaining.startIndex)..<endIdx]
                let codeBg = isDark ? Color(red:0.14,green:0.11,blue:0.26) : Color(red:0.92,green:0.92,blue:0.95)
                result = result + Text(String(code)).font(.system(size: 12 * fontScale, design: .monospaced)).foregroundColor(accentColor)
                remaining = remaining[remaining.index(after: endIdx)...]
                continue
            }
            // Bold **...**
            if remaining.hasPrefix("**"), let r = remaining.dropFirst(2).range(of: "**") {
                let bold = remaining[remaining.index(remaining.startIndex, offsetBy: 2)..<r.lowerBound]
                result = result + Text(String(bold)).bold()
                remaining = remaining[r.upperBound...]
                continue
            }
            // Italic *...*
            if remaining.hasPrefix("*"), let endIdx = remaining.dropFirst().firstIndex(of: "*") {
                let italic = remaining[remaining.index(after: remaining.startIndex)..<endIdx]
                result = result + Text(String(italic)).italic()
                remaining = remaining[remaining.index(after: endIdx)...]
                continue
            }
            // Regular character
            let nextSpecial = remaining.dropFirst().firstIndex(where: { $0 == "`" || $0 == "*" }) ?? remaining.endIndex
            let plain = remaining[remaining.startIndex..<nextSpecial]
            result = result + Text(String(plain))
            remaining = remaining[nextSpecial...]
        }
        
        return result
            .font(.system(size: 14))
            .foregroundColor(textColor)
    }
    
    // MARK: - Colors
    var textColor: Color { isDark ? Color(red:0.91,green:0.89,blue:0.94) : Color(red:0.11,green:0.11,blue:0.145) }
    var accentColor: Color { isDark ? Color(red:0.655,green:0.545,blue:0.98) : Color(red:0.42,green:0.33,blue:0.82) }
    var borderColor: Color { isDark ? Color.white.opacity(0.06) : Color.black.opacity(0.08) }
}

