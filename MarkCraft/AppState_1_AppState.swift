import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog

// MARK: - Diagnostics
struct Diagnostic: Identifiable {
    let id = UUID()
    let line: Int
    let col: Int
    let message: String
    let severity: DiagSeverity
}

enum DiagSeverity: String { case error, warning, info, hint }

// MARK: - App State
final class AppState: ObservableObject {
    @Published var docs: [MCDoc] = []
    @Published var currentDocId: UUID?
    @Published var currentFile = "Untitled.swift"
    @Published var currentFilePath = ""
    @Published var currentLanguage = "Swift"
    @Published var recentFiles: [String] = {
        if let data = UserDefaults.standard.data(forKey: "mc_recentFiles"),
           let files = try? JSONDecoder().decode([String].self, from: data) {
            return files
        }
        return []
    }()
    @Published var source: String = [
        "// MarkCraft V150 — IDE Poétique",
        "import SwiftUI",
        "",
        "struct ContentView: View {",
        "    @State private var message = \"Hello, MarkCraft!\"",
        "    ",
        "    var body: some View {",
        "        VStack(spacing: 20) {",
        "            Text(message)",
        "                .font(.largeTitle)",
        "            Button(\"Tap Me\") {",
        "                message = \"Welcome to V150!\"",
        "            }",
        "            .buttonStyle(.borderedProminent)",
        "        }",
        "        .padding()",
        "    }",
        "}",
    ].joined(separator: "\n")
    @Published var source2 = ""
    
    // Subscription State
    @AppStorage("mc_subscribed") var isSubscribed: Bool = false
    @AppStorage("mc_sub_tier") var subscriptionTier: String = ""
    @Published var showPaywall: Bool = false
    @Published var showDocPreview: Bool = false
    @Published var showMarkdownPreview: Bool = true
    @Published var previewLangDoc: LangDoc?
    
    // UI State
    @Published var sidebarTab = 0
    @Published var showSidebar = false
    @Published var bottomTab = 0
    @Published var showBottomPanel = false
    @Published var splitView = false
    @Published var focusMode = false
    @Published var showNewFile = false
    @Published var showExport = false
    @Published var showSettings = false
    @Published var showAbout = false
    @Published var showSupportReminder = false
    @Published var showCommandPalette = false
    @Published var showFindReplace = false
    @Published var showGoToLine = false
    @Published var showLanguagePicker = false
    @Published var showSnippets = false
    @Published var showTetris = false
    @Published var showHelp = false
    
    // Find & Replace
    @Published var findText = ""
    @Published var replaceText = ""
    
    // Notifications
    @Published var notifications: [MCNotification] = []
    
    // Settings
    @AppStorage("mc_font_size") var fontSize: Double = 13.5
    @AppStorage("mc_tab_size") var tabSize: Int = 4
    @AppStorage("mc_word_wrap") var wordWrap = true
    @AppStorage("mc_show_minimap") var showMinimap = true
    @AppStorage("mc_show_line_numbers") var showLineNumbers = true
    @AppStorage("mc_auto_save") var autoSave = false
    @AppStorage("mc_auto_indent") var autoIndent = true
    @AppStorage("mc_bracket_match") var bracketMatch = true
    @AppStorage("mc_show_ink_toolbar") var showInkToolbar = true
    @AppStorage("mc_audio_quality") var audioQuality = "high"
    @AppStorage("mc_journal_reminders") var journalReminders = true
    @AppStorage("mc_streak_notifications") var streakNotifications = true
    @AppStorage("mc_on_this_day") var onThisDayEnabled = true
    @AppStorage("mc_smart_notes") var smartNotesEnabled = true
    @AppStorage("mc_ghost_text") var ghostTextEnabled = true
    @AppStorage("mc_ambient_sounds") var ambientSoundsEnabled = false
    @AppStorage("mc_particle_effects") var particleEffectsEnabled = true
    @AppStorage("mc_claude_api_key") var claudeApiKey = ""
    @AppStorage("mc_claude_model") var claudeModel = "claude-sonnet-4-20250514"
    @AppStorage("mc_privacy_encryption") var privacyEncryption = true
    @AppStorage("mc_biometric_lock") var biometricLock = false
    
    // Diagnostics & AI
    @Published var diagnostics: [Diagnostic] = []
    @Published var chatMessages: [ChatMessage] = []
    @Published var chatInput = ""
    @Published var isAIThinking = false
    
    // Ink & Audio
    @Published var inkMode = false
    @Published var inkAnnotations: [InkAnnotation] = []
    
    // Session Tracking
    @Published var currentSessionStart = Date()
    @Published var currentSessionLinesAdded = 0
    @Published var currentSessionLinesRemoved = 0
    @Published var currentSessionFilesModified: Set<String> = []
    @Published var currentMood = ""
    
    // Computed
    var syn: SynLang? {
        Syn.detect(currentFile) ?? Syn.all.first { $0.name == currentLanguage }
    }
    var lineCount: Int { source.components(separatedBy: "\n").count }
    var charCount: Int { source.count }
    var wordCount: Int {
        source.split(whereSeparator: { !$0.isLetter && !$0.isNumber }).count
    }
    var findCount: Int {
        findText.isEmpty ? 0 : source.components(separatedBy: findText).count - 1
    }
    
    // Symbols Outline
    var symbols: [(icon: String, name: String, line: Int)] {
        let lines = source.components(separatedBy: "\n")
        var result: [(String, String, Int)] = []
        let prefixes: [(String, String)] = [
            ("func ","f.square"),("def ","f.square"),("class ","c.square"),
            ("struct ","s.square"),("enum ","e.square"),("protocol ","p.square"),
            ("interface ","i.square"),("fn ","f.square"),("pub fn ","f.square"),
            ("const ","k.square"),("type ","t.square"),
            ("// MARK: -","bookmark"),("# ","number"),("## ","number"),
        ]
        for (i, line) in lines.enumerated() {
            let t = line.trimmingCharacters(in: .whitespaces)
            for (pfx, icon) in prefixes {
                let hasPrefix = t.hasPrefix(pfx)
                let hasPublicPrefix = t.hasPrefix("public \(pfx)")
                let hasPrivatePrefix = t.hasPrefix("private \(pfx)")
                if hasPrefix || hasPublicPrefix || hasPrivatePrefix {
                    let nm = t.replacingOccurrences(of: pfx, with: "")
                        .components(separatedBy: CharacterSet(charactersIn: "({:< \n")).first ?? ""
                    if !nm.isEmpty {
                        result.append((icon, String(nm.prefix(40)), i + 1))
                    }
                    break
                }
            }
        }
        return result
    }
    
    // Document Management
    func switchTo(_ doc: MCDoc) {
        if let idx = docs.firstIndex(where: { $0.id == currentDocId }) {
            docs[idx].content = source
            docs[idx].modified = Date()
        }
        currentDocId = doc.id
        currentFile = doc.title
        source = doc.content
        if doc.language.isEmpty {
            currentLanguage = Syn.detect(doc.title)?.name ?? "Swift"
        } else {
            currentLanguage = doc.language
        }
    }
    
    func createNewDoc(title: String = "Untitled.swift", content: String = "", emoji: String = "📄") {
        let lang = Syn.detect(title)?.name ?? "Swift"
        let doc = MCDoc(t: title, e: emoji, content: content, lang: lang)
        docs.append(doc)
        switchTo(doc)
        notify("Created \(title)", icon: "doc.badge.plus")
    }
    
    func loadAllExamples() {
        let emojis: [String: String] = [
            "Swift":"🍊","Python":"🐍","JavaScript":"🌐","TypeScript":"🔷","JSX":"⚛️","TSX":"⚛️",
            "HTML":"🌐","CSS":"🎨","Java":"☕️","C":"⚙️","C++":"⚙️","C#":"💚","Rust":"🦀",
            "Go":"🐹","Ruby":"💎","PHP":"🐘","Kotlin":"🟣","Dart":"🎯","Shell":"🐚",
            "SQL":"🗄️","Lua":"🌙","R":"📊","Scala":"🔴","Haskell":"λ","Elixir":"💧",
            "Clojure":"🟢","Nim":"👑","Perl":"🐪","Zig":"⚡","Markdown":"📝","LaTeX":"📐"
        ]
        var count = 0
        for langDoc in LangDocLibrary.docs {
            let exists = docs.contains { $0.title == "Example.\(langDoc.ext)" }
            if !exists {
                let emoji = emojis[langDoc.name] ?? "📄"
                let doc = MCDoc(t: "Example.\(langDoc.ext)", e: emoji, content: langDoc.example, lang: langDoc.name)
                docs.append(doc)
                count += 1
            }
        }
        if let first = docs.last { switchTo(first) }
        showSidebar = false
        notify("Loaded \(count) examples", icon: "tray.full")
    }
    
    func closeDoc(_ id: UUID) {
        docs.removeAll { $0.id == id }
        if currentDocId == id {
            if let first = docs.first { switchTo(first) }
            else {
                currentDocId = nil
                source = ""
                currentFile = "Untitled"
            }
        }
    }
    
    func saveCurrentDoc() {
        if let idx = docs.firstIndex(where: { $0.id == currentDocId }) {
            docs[idx].content = source
            docs[idx].modified = Date()
        }
    }
    
    func addRecent(_ path: String) {
        recentFiles.removeAll { $0 == path }
        recentFiles.insert(path, at: 0)
        if recentFiles.count > 20 { recentFiles = Array(recentFiles.prefix(20)) }
        if let data = try? JSONEncoder().encode(recentFiles) {
            UserDefaults.standard.set(data, forKey: "mc_recentFiles")
        }
    }
    
    func duplicateDoc(_ id: UUID) {
        guard let doc = docs.first(where: { $0.id == id }) else { return }
        let copy = MCDoc(t: "Copy of \(doc.title)", e: doc.emoji, content: doc.content, lang: doc.language)
        docs.append(copy)
        switchTo(copy)
    }
    
    // Find & Replace
    func replaceNext() {
        guard !findText.isEmpty else { return }
        guard let range = source.range(of: findText) else { return }
        source.replaceSubrange(range, with: replaceText)
    }
    
    func replaceAll() {
        guard !findText.isEmpty else { return }
        source = source.replacingOccurrences(of: findText, with: replaceText)
    }
    
    // Notifications
    func notify(_ text: String, icon: String = "info.circle", tint: Color = .green) {
        let n = MCNotification(icon: icon, title: text, message: "", color: tint, time: Date())
        notifications.append(n)
        DispatchQueue.main.asyncAfter(deadline: .now() + 3) { [weak self] in
            self?.notifications.removeAll { $0.id == n.id }
        }
    }
    
    // Command Palette
    func paletteCommands() -> [PaletteCmd] {
        [
            PaletteCmd(title:"New File", icon:"doc.badge.plus", shortcut:"⌘N") { [weak self] in self?.showNewFile = true },
            PaletteCmd(title:"Save", icon:"square.and.arrow.down", shortcut:"⌘S") { [weak self] in self?.saveCurrentDoc() },
            PaletteCmd(title:"Find & Replace", icon:"magnifyingglass", shortcut:"⌘F") { [weak self] in self?.showFindReplace = true },
            PaletteCmd(title:"Go to Line", icon:"arrow.right.to.line", shortcut:"⌘G") { [weak self] in self?.showGoToLine = true },
            PaletteCmd(title:"Settings", icon:"gearshape", shortcut:"⌘,") { [weak self] in self?.showSettings = true },
        ]
    }
    
    func sessionSummary() -> String {
        let mins = Int(Date().timeIntervalSince(currentSessionStart) / 60)
        return "Session: \(mins) min, \(currentLanguage)"
    }
    
    func resetSession() {
        currentSessionStart = Date()
        currentSessionLinesAdded = 0
        currentSessionLinesRemoved = 0
        currentSessionFilesModified.removeAll()
    }
}

// MARK: - Runner
final class Runner: ObservableObject {
    @Published var output = ""
    @Published var isRunning = false
    @Published var issues: Int = 0
    @Published var exitCode: Int32? = nil
    @Published var startTime: Date? = nil
    @Published var executionTime: TimeInterval? = nil
    
    let supported = [
        "Swift","Python","JavaScript","Shell","Ruby","Lua","Go","Rust",
        "TypeScript","C","C++","Java","Kotlin","Perl","R","PHP",
        "Haskell","Elixir","Scala","Dart","Nim","Zig","Julia","OCaml"
    ]
    
    private let interpreters: [String: (cmd: String, args: [String], ext: String)] = [
        "Swift": ("swift", [], "swift"),
        "Python": ("python3", [], "py"),
        "JavaScript": ("node", [], "js"),
        "Shell": ("bash", [], "sh"),
        "Ruby": ("ruby", [], "rb"),
        "Lua": ("lua", [], "lua"),
        "Go": ("go", ["run"], "go"),
        "Perl": ("perl", [], "pl"),
        "R": ("Rscript", [], "r"),
        "PHP": ("php", [], "php"),
    ]
    
    func run(_ code: String, lang: String) {
        guard !isRunning else { return }
        guard let interp = interpreters[lang] else {
            output = "⚠️ No interpreter for \(lang)"
            return
        }
        isRunning = true
        startTime = Date()
        output = "▶ Running \(lang)...\n"
        
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            do {
                let tmp = FileManager.default.temporaryDirectory
                    .appendingPathComponent("mc_run.\(interp.ext)")
                try code.write(to: tmp, atomically: true, encoding: .utf8)
                
                let proc = Process()
                proc.executableURL = URL(fileURLWithPath: "/usr/bin/env")
                proc.arguments = [interp.cmd] + interp.args + [tmp.path]
                proc.currentDirectoryURL = FileManager.default.temporaryDirectory
                
                let pipe = Pipe()
                let errPipe = Pipe()
                proc.standardOutput = pipe
                proc.standardError = errPipe
                proc.environment = ProcessInfo.processInfo.environment
                
                try proc.run()
                proc.waitUntilExit()
                
                let stdout = String(data: pipe.fileHandleForReading.readDataToEndOfFile(), encoding: .utf8) ?? ""
                let stderr = String(data: errPipe.fileHandleForReading.readDataToEndOfFile(), encoding: .utf8) ?? ""
                let elapsed = Date().timeIntervalSince(self?.startTime ?? Date())
                
                DispatchQueue.main.async {
                    var out = ""
                    if !stdout.isEmpty { out += stdout }
                    if !stderr.isEmpty { out += "\n⚠️ STDERR:\n\(stderr)" }
                    if stdout.isEmpty && stderr.isEmpty { out = "(No output)" }
                    out += "\n\n───── Finished ─────\n"
                    out += "Exit code: \(proc.terminationStatus)\n"
                    out += String(format: "Time: %.3fs", elapsed)
                    self?.output = out
                    self?.exitCode = proc.terminationStatus
                    self?.executionTime = elapsed
                    self?.isRunning = false
                    try? FileManager.default.removeItem(at: tmp)
                }
            } catch {
                DispatchQueue.main.async {
                    self?.output = "❌ Error: \(error.localizedDescription)"
                    self?.isRunning = false
                }
            }
        }
    }
    
    func stop() { isRunning = false; output += "\n⛔ Stopped." }
    func clear() { output = ""; exitCode = nil; executionTime = nil }
}

// MARK: - Export
enum ExportFormat: String, CaseIterable, Identifiable {
    case plainText = "Plain Text"
    case markdown = "Markdown"
    case html = "HTML"
    case rtf = "RTF"
    case latexExport = "LaTeX"
    case pdf = "PDF"
    var id: String { rawValue }
}

struct ExportManager {
    static func exportCode(_ code: String, format: ExportFormat, filename: String, lang: SynLang?) -> Data? {
        switch format {
        case .plainText: return code.data(using: .utf8)
        case .markdown:
            let md = "# \(filename)\n\n```\(lang?.ext ?? "")\n\(code)\n```\n"
            return md.data(using: .utf8)
        case .html:
            let esc = code.replacingOccurrences(of: "&", with: "&amp;")
                .replacingOccurrences(of: "<", with: "&lt;")
                .replacingOccurrences(of: ">", with: "&gt;")
            let h = "<html><body><pre><code>\(esc)</code></pre></body></html>"
            return h.data(using: .utf8)
        case .rtf:
            return try? NSAttributedString(string: code).data(
                from: NSRange(location: 0, length: code.count),
                documentAttributes: [.documentType: NSAttributedString.DocumentType.rtf]
            )
        case .latexExport:
            return "\\begin{lstlisting}\n\(code)\n\\end{lstlisting}".data(using: .utf8)
        case .pdf: return nil
        }
    }
}

// MARK: - Undo/Redo
class UndoRedoStack<T> {
    private var undoStack: [T] = []
    private var redoStack: [T] = []
    private let maxSize: Int
    
    init(maxSize: Int = 100) { self.maxSize = maxSize }
    
    func push(_ state: T) {
        undoStack.append(state)
        if undoStack.count > maxSize { undoStack.removeFirst() }
        redoStack.removeAll()
    }
    
    func undo() -> T? {
        guard let state = undoStack.popLast() else { return nil }
        redoStack.append(state)
        return undoStack.last
    }
    
    func redo() -> T? {
        guard let state = redoStack.popLast() else { return nil }
        undoStack.append(state)
        return state
    }
    
    var canUndo: Bool { undoStack.count > 1 }
    var canRedo: Bool { !redoStack.isEmpty }
}

// MARK: - File History
struct FileSnapshot: Codable {
    let timestamp: Date
    let content: String
    let linesChanged: Int
    let description: String
}

class FileHistoryTracker: ObservableObject {
    @Published var snapshots: [UUID: [FileSnapshot]] = [:]
    
    func recordChange(docId: UUID, content: String, description: String = "Edit") {
        let snap = FileSnapshot(timestamp: Date(), content: content, linesChanged: 0, description: description)
        snapshots[docId, default: []].append(snap)
        if snapshots[docId]?.count ?? 0 > 50 {
            snapshots[docId]?.removeFirst()
        }
    }
    
    func history(for docId: UUID) -> [FileSnapshot] { snapshots[docId] ?? [] }
}

// MARK: - Notification Center
class NotifCenter: ObservableObject {
    @Published var notifications: [MCNotification] = []
    @Published var unread = 0
    
    func push(_ icon: String, _ title: String, _ msg: String, _ color: Color) {
        let n = MCNotification(icon: icon, title: title, message: msg, color: color, time: Date())
        notifications.insert(n, at: 0)
        unread += 1
        if notifications.count > 50 { notifications.removeLast() }
    }
    func markRead() { unread = 0 }
    func clear() { notifications.removeAll(); unread = 0 }
}
