import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog

// MARK: - Website Palette
typealias MCColors = MC

struct MC {
    let bg, card, card2, editor, border: Color
    let text, dim, muted: Color
    let accent, gold, copper, rose, cyan, lavender, violet: Color
    let green, red, orange, yellow, pink: Color
}

enum ThemeMode: String, CaseIterable {
    case light, dark, system
    var icon: String {
        switch self {
        case .light: return "sun.max.fill"
        case .dark: return "moon.fill"
        case .system: return "circle.lefthalf.filled"
        }
    }
}

final class Theme: ObservableObject {
    @AppStorage("mc_theme") var mode = ThemeMode.dark
    var isDark: Bool {
        if mode == .dark { return true }
        if mode == .system {
            return NSApp.effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        }
        return false
    }
    var scheme: ColorScheme? {
        switch mode {
        case .system: return nil
        case .dark: return .dark
        case .light: return .light
        }
    }
    static let darkColors = MC(
        bg: Color(red:0.027,green:0.024,blue:0.055),
        card: Color(red:0.055,green:0.047,blue:0.102),
        card2: Color(red:0.078,green:0.067,blue:0.149),
        editor: Color(red:0.035,green:0.031,blue:0.067),
        border: Color.white.opacity(0.06),
        text: Color(red:0.91,green:0.894,blue:0.941),
        dim: Color(red:0.545,green:0.522,blue:0.627),
        muted: Color(red:0.353,green:0.329,blue:0.439),
        accent: Color(red:0.655,green:0.545,blue:0.98),
        gold: Color(red:0.96,green:0.651,blue:0.137),
        copper: Color(red:0.91,green:0.529,blue:0.357),
        rose: Color(red:0.831,green:0.392,blue:0.541),
        cyan: Color(red:0.404,green:0.91,blue:0.976),
        lavender: Color(red:0.655,green:0.545,blue:0.98),
        violet: Color(red:0.176,green:0.106,blue:0.412),
        green: Color(red:0.298,green:0.851,blue:0.392),
        red: Color(red:0.957,green:0.361,blue:0.388),
        orange: Color(red:0.957,green:0.612,blue:0.282),
        yellow: Color(red:0.976,green:0.859,blue:0.369),
        pink: Color(red:0.929,green:0.494,blue:0.651)
    )
    static let lightColors = MC(
        bg: Color(red:0.976,green:0.973,blue:0.980),
        card: Color(red:0.996,green:0.996,blue:1.0),
        card2: Color(red:0.945,green:0.941,blue:0.957),
        editor: Color.white,
        border: Color.black.opacity(0.08),
        text: Color(red:0.11,green:0.11,blue:0.145),
        dim: Color(red:0.42,green:0.42,blue:0.475),
        muted: Color(red:0.56,green:0.56,blue:0.62),
        accent: Color(red:0.42,green:0.33,blue:0.82),
        gold: Color(red:0.82,green:0.545,blue:0.08),
        copper: Color(red:0.78,green:0.42,blue:0.22),
        rose: Color(red:0.71,green:0.29,blue:0.42),
        cyan: Color(red:0.15,green:0.58,blue:0.67),
        lavender: Color(red:0.42,green:0.33,blue:0.82),
        violet: Color(red:0.35,green:0.22,blue:0.62),
        green: Color(red:0.16,green:0.59,blue:0.24),
        red: Color(red:0.82,green:0.18,blue:0.22),
        orange: Color(red:0.82,green:0.49,blue:0.12),
        yellow: Color(red:0.71,green:0.59,blue:0.08),
        pink: Color(red:0.78,green:0.29,blue:0.42)
    )
    var c: MC { isDark ? Theme.darkColors : Theme.lightColors }
}

// MARK: - Core Types
struct MCDoc: Identifiable, Codable {
    var id = UUID()
    var title: String
    var emoji: String
    var isFavorite: Bool
    var created = Date()
    var modified = Date()
    var content: String
    var language: String
    var annotations: [InkAnnotation]
    var audioSessions: [UUID]
    var tags: [String]
    var mood: String
    var weatherInfo: String
    var location: String
    
    init(t: String, e: String = "📄", f: Bool = false, content: String = "", lang: String = "") {
        self.title = t; self.emoji = e; self.isFavorite = f
        self.content = content; self.language = lang
        self.annotations = []; self.audioSessions = []
        self.tags = []; self.mood = ""; self.weatherInfo = ""; self.location = ""
    }
    var t: String { get { title } set { title = newValue } }
    var e: String { get { emoji } set { emoji = newValue } }
    var f: Bool { get { isFavorite } set { isFavorite = newValue } }
    var d: Date { get { modified } set { modified = newValue } }
    var source: String { get { content } set { content = newValue } }
    var lineCount: Int { content.components(separatedBy: "\n").count }
    var wordCount: Int { content.split(separator: " ").count }
    var charCount: Int { content.count }
    var isModified: Bool { modified > created }
    var detectedLanguage: SynLang? { Syn.detect(title) }
    var sizeDescription: String { charCount < 1000 ? "\(charCount) chars" : "\(charCount / 1000)K" }
}

struct InkAnnotation: Identifiable, Codable {
    var id = UUID()
    var points: [[Double]]
    var color: String
    var thickness: Double
    var tool: InkTool
    var timestamp: Date
    var lineNumber: Int
}

enum InkTool: String, Codable, CaseIterable {
    case pen, pencil, highlighter, eraser, lasso, shape
}

struct AudioSession: Identifiable, Codable {
    var id = UUID()
    var filename: String
    var duration: Double
    var created: Date
    var transcript: String
    var codeTimestamps: [CodeTimestamp]
    var smartNotes: String
}

struct CodeTimestamp: Codable {
    var time: Double
    var lineNumber: Int
    var action: String
}

struct JournalEntry: Identifiable, Codable {
    var id = UUID()
    var date: Date
    var title: String
    var body: String
    var codeSnippet: String
    var language: String
    var mood: String
    var weather: String
    var location: String
    var tags: [String]
    var photos: [String]
    var audioSessionId: UUID?
    var projectName: String
    var linesAdded: Int
    var linesRemoved: Int
    var filesModified: [String]
    var duration: TimeInterval
    var streakDay: Int
}

struct Streak: Codable {
    var currentStreak: Int
    var longestStreak: Int
    var lastDate: Date?
    var totalDays: Int
    var startDate: Date
    mutating func recordDay() {
        let cal = Calendar.current
        let today = cal.startOfDay(for: Date())
        if let last = lastDate {
            let lastDay = cal.startOfDay(for: last)
            let diff = cal.dateComponents([.day], from: lastDay, to: today).day ?? 0
            if diff == 1 { currentStreak += 1 }
            else if diff > 1 { currentStreak = 1 }
        } else { currentStreak = 1 }
        longestStreak = max(longestStreak, currentStreak)
        lastDate = today; totalDays += 1
    }
}

struct PromptPack: Identifiable {
    let id = UUID()
    let name: String
    let icon: String
    let prompts: [String]
    let category: String
}

struct PaletteCmd: Identifiable {
    let id = UUID()
    let title: String
    let icon: String
    let shortcut: String
    let action: () -> Void
}

enum ChatRole { case user, assistant, system }
struct ChatMessage: Identifiable {
    let id = UUID()
    let role: ChatRole
    let content: String
}
struct CodeContext {
    let source: String
    let filename: String
    let language: String
}
struct QuickAction: Identifiable {
    let id = UUID()
    let icon: String
    let label: String
    let prompt: String
}

struct FlashCard: Identifiable {
    var id = UUID()
    var question: String
    var answer: String
    var category: String
    var difficulty: Int
    var lastReviewed: Date?
}

struct QuizQuestion: Identifiable {
    var id = UUID()
    var question: String
    var options: [String]
    var correctIndex: Int
    var explanation: String
}

struct MCNotification: Identifiable {
    let id = UUID()
    let icon: String
    let title: String
    let message: String
    let color: Color
    let time: Date
}

// MARK: - Prompt Packs
enum PromptPacks {
    static let all: [PromptPack] = [
        PromptPack(name: "Daily Dev Reflection", icon: "brain.head.profile", prompts: [
            "What was the most interesting thing you coded today?",
            "Describe a bug you solved and how it felt.",
            "What did you learn that you didn't know yesterday?",
        ], category: "reflection"),
        PromptPack(name: "Architecture Thinking", icon: "building.2", prompts: [
            "Draw the data flow of your current feature.",
            "Explain your architecture in one sentence.",
        ], category: "architecture"),
        PromptPack(name: "Code Review Journal", icon: "eye.circle", prompts: [
            "What feedback did you give or receive in code review today?",
        ], category: "review"),
        PromptPack(name: "Career Growth", icon: "arrow.up.right.circle", prompts: [
            "What skill are you actively trying to improve?",
        ], category: "career"),
        PromptPack(name: "Debugging Stories", icon: "ant.circle", prompts: [
            "Describe the weirdest bug you have ever encountered.",
        ], category: "debugging"),
        PromptPack(name: "Gratitude & Joy", icon: "heart.circle", prompts: [
            "What part of coding brought you joy today?",
        ], category: "gratitude"),
    ]
}

struct ThemeModeToggle: View {
    @EnvironmentObject var th: Theme
    var body: some View {
        let c = th.c
        HStack(spacing: 2) {
            ForEach(ThemeMode.allCases, id: \.self) { mode in
                let isActive: Bool = th.mode == mode
                let fg: Color = _pc(isActive, c.gold, c.muted)
                let bg: Color = _pc(isActive, c.card2, Color.clear)
                Button(action: { withAnimation(.easeOut(duration: 0.2)) { th.mode = mode } }) {
                    Image(systemName: mode.icon)
                        .font(.system(size: 10))
                        .foregroundColor(fg)
                        .frame(width: 26, height: 22)
                        .background(bg)
                        .cornerRadius(4)
                }.buttonStyle(.plain)
            }
        }
        .padding(2)
        .background(c.card)
        .cornerRadius(6)
    }
}

struct AppConfig {
    static let appName = "MarkCraft"
    static let bundle = "com.markcraft.app"
    static let version = "1.5.0"
    static let build = "150"
    static let supportedExtensions = [
        "swift","py","ts","js","rs","go","rb","kt","scala","lua",
        "php","pl","r","sh","bash","zsh","css","html","json","yaml",
        "toml","xml","sql","md","txt","c","cpp","h","hpp","java",
        "cs","fs","hs","ml","ex","exs","clj","jl","dart","v","cr","zig","nim"
    ]
    static var extensions: [String] { supportedExtensions }
    static var fullVersion: String { "\(appName) V\(build)" }
}

class AppLifecycle: ObservableObject {
    @Published var isFirstLaunch: Bool
    @Published var launchCount: Int
    @Published var totalEditingTime: TimeInterval = 0
    init() {
        let d = UserDefaults.standard
        let count = d.integer(forKey: "mc_launch_count") + 1
        launchCount = count; isFirstLaunch = count == 1
        d.set(count, forKey: "mc_launch_count")
    }
    func recordSession(duration: TimeInterval) {
        totalEditingTime += duration
        UserDefaults.standard.set(totalEditingTime, forKey: "mc_total_time")
    }
}

struct MCMenuCommands: Commands {
    @Binding var showSettings: Bool
    @Binding var showHelp: Bool
    var loc: Loc
    var body: some Commands {
        CommandGroup(replacing: .newItem) {
            Button(loc.t("new_file")) { }.keyboardShortcut("n", modifiers: .command)
        }
        CommandGroup(replacing: .help) {
            Button(loc.t("markcraft_help")) { showHelp = true }.keyboardShortcut("?", modifiers: .command)
            Divider()
            Button("🧱 Tetris") { }
        }
    }
}

// MARK: - @main App Entry
@main
struct MarkCraftApp: App {
    @StateObject var st = AppState()
    @StateObject var loc = Loc()
    @StateObject var th = Theme()
    @StateObject var run = Runner()
    @StateObject var journal = JournalManager()
    @StateObject var ink = InkManager()
    @StateObject var audio = AudioManager()
    
    var body: some Scene {
        WindowGroup {
            RootView()
                .environmentObject(st)
                .environmentObject(loc)
                .environmentObject(th)
                .environmentObject(run)
                .environmentObject(journal)
                .environmentObject(ink)
                .environmentObject(audio)
                .frame(minWidth: 1000, minHeight: 680)
                .preferredColorScheme(th.scheme)
                .overlay {
                    if st.showPaywall {
                        PaywallView()
                            .environmentObject(st)
                            .environmentObject(th)
                            .environmentObject(loc)
                            .transition(.opacity)
                    }
                }
                .onAppear {
                    if !st.isSubscribed { st.showPaywall = true }
                }
        }
        .defaultSize(width: 1600, height: 1000)
        .commands {
            CommandGroup(replacing: .newItem) {
                Button(loc.t("new_file")) { st.showNewFile = true }
                    .keyboardShortcut("N", modifiers: .command)
                
                Divider()
                
                Button(loc.t("open")) { openFile() }
                    .keyboardShortcut("O", modifiers: .command)
                
                // Open Recent (20 derniers)
                Menu(loc.t("open_recent")) {
                    if st.recentFiles.isEmpty {
                        Text(loc.t("no_recent_files")).foregroundColor(.secondary)
                    } else {
                        ForEach(st.recentFiles.prefix(20), id: \.self) { path in
                            Button(URL(fileURLWithPath: path).lastPathComponent) {
                                openRecentFile(path)
                            }
                        }
                        Divider()
                        Button(loc.t("clear_menu")) { st.recentFiles.removeAll() }
                    }
                }
                
                Button(loc.t("load_examples")) { st.loadAllExamples() }
                    .keyboardShortcut("E", modifiers: [.command, .shift])
                
                Divider()
                
                Button(loc.t("save")) { saveFile() }
                    .keyboardShortcut("S", modifiers: .command)
                
                Button(loc.t("save_as")) { saveFileAs() }
                    .keyboardShortcut("S", modifiers: [.command, .shift])
                
                Divider()
                
                Button(loc.t("print")) { printDocument() }
                    .keyboardShortcut("P", modifiers: .command)
            }
            CommandGroup(after: .textEditing) {
                Button(loc.t("find_replace")) { st.showFindReplace = true }
                    .keyboardShortcut("F", modifiers: .command)
                Button(loc.t("go_to_line")) { st.showGoToLine = true }
                    .keyboardShortcut("G", modifiers: .command)
                Button(loc.t("command_palette")) { st.showCommandPalette = true }
                    .keyboardShortcut("P", modifiers: [.command, .shift])
            }
            CommandMenu(loc.t("run")) {
                Button(loc.t("run_code")) {
                    if let syn = st.syn, run.supported.contains(syn.name) {
                        st.showBottomPanel = true
                        st.bottomTab = 1
                        run.run(st.source, lang: syn.name)
                    }
                }.keyboardShortcut("R", modifiers: .command)
            }
            CommandMenu(loc.t("view")) {
                // Panels
                Button(st.showMarkdownPreview ? loc.t("hide_preview") : loc.t("show_preview")) {
                    withAnimation { st.showMarkdownPreview.toggle() }
                }.keyboardShortcut("P", modifiers: [.command, .option])
                Button(st.showSidebar ? loc.t("hide_sidebar") : loc.t("show_sidebar")) {
                    withAnimation { st.showSidebar.toggle() }
                }.keyboardShortcut("B", modifiers: .command)
                Button(st.showBottomPanel ? loc.t("hide_terminal") : loc.t("show_terminal")) {
                    withAnimation { st.showBottomPanel.toggle() }
                }.keyboardShortcut("J", modifiers: .command)
                Button(st.showDocPreview ? loc.t("hide_doc") : loc.t("show_doc")) {
                    withAnimation { st.showDocPreview.toggle() }
                }.keyboardShortcut("D", modifiers: [.command, .option])
                Button(st.splitView ? loc.t("hide_split") : loc.t("show_split")) {
                    withAnimation { st.splitView.toggle() }
                }.keyboardShortcut("\\", modifiers: .command)
                
                Divider()
                
                // Editor display
                Button(st.showLineNumbers ? loc.t("hide_line_numbers") : loc.t("show_line_numbers")) {
                    st.showLineNumbers.toggle()
                }.keyboardShortcut("L", modifiers: [.command, .shift])
                Button(st.showMinimap ? loc.t("hide_minimap") : loc.t("show_minimap")) {
                    st.showMinimap.toggle()
                }.keyboardShortcut("M", modifiers: [.command, .option])
                Button(st.wordWrap ? loc.t("disable_word_wrap") : loc.t("enable_word_wrap")) {
                    st.wordWrap.toggle()
                }.keyboardShortcut("W", modifiers: [.command, .option])
                
                Divider()
                
                // Modes
                Button(st.focusMode ? loc.t("exit_focus_mode") : loc.t("enter_focus_mode")) {
                    withAnimation { st.focusMode.toggle() }
                }.keyboardShortcut("F", modifiers: [.command, .shift])
                Button(st.inkMode ? loc.t("exit_ink_mode") : loc.t("enter_ink_mode")) {
                    withAnimation { st.inkMode.toggle() }
                }.keyboardShortcut("I", modifiers: [.command, .option])
                
                Divider()
                
                // Appearance
                Menu(loc.t("appearance")) {
                    ForEach(ThemeMode.allCases, id: \.self) { mode in
                        Button(action: { withAnimation { th.mode = mode } }) {
                            let check = th.mode == mode ? "✓ " : "   "
                            Text("\(check)\(mode.rawValue.capitalized)")
                        }
                    }
                }
                
                Divider()
                
                // Zoom
                Button(loc.t("zoom_in")) { st.fontSize = min(st.fontSize + 1, 32) }
                    .keyboardShortcut("+", modifiers: .command)
                Button(loc.t("zoom_out")) { st.fontSize = max(st.fontSize - 1, 9) }
                    .keyboardShortcut("-", modifiers: .command)
                Button(loc.t("reset_zoom")) { st.fontSize = 13.5 }
                    .keyboardShortcut("0", modifiers: .command)
            }
            CommandGroup(replacing: .appSettings) {
                Button(loc.t("settings_dots")) { st.showSettings = true }
                    .keyboardShortcut(",", modifiers: .command)
                Button(loc.t("about_markcraft")) { st.showAbout = true }
            }
            CommandGroup(replacing: .help) {
                Button(loc.t("markcraft_help")) { st.showHelp = true }
                    .keyboardShortcut("?", modifiers: .command)
                Divider()
                Button("🧱 Tetris") { st.showTetris = true }
                    .keyboardShortcut("T", modifiers: [.command, .option, .shift])
            }
        }
    }
    
    func openFile() {
        let panel = NSOpenPanel()
        panel.allowedContentTypes = [.plainText, .sourceCode, .data]
        panel.allowsMultipleSelection = false
        if panel.runModal() == .OK, let url = panel.url {
            loadFile(url)
        }
    }
    
    func openRecentFile(_ path: String) {
        let url = URL(fileURLWithPath: path)
        guard FileManager.default.fileExists(atPath: path) else {
            st.recentFiles.removeAll { $0 == path }
            st.notify("File not found", icon: "xmark.circle", tint: .red)
            return
        }
        loadFile(url)
    }
    
    func loadFile(_ url: URL) {
        do {
            let content = try String(contentsOf: url, encoding: .utf8)
            let name = url.lastPathComponent
            let lang = Syn.detect(name)?.name ?? ""
            let doc = MCDoc(t: name, content: content, lang: lang)
            st.docs.append(doc)
            st.switchTo(doc)
            st.currentFilePath = url.path
            st.addRecent(url.path)
            st.showSidebar = true
            st.sidebarTab = 0  // Switch to Files tab
            st.notify("Opened \(name)", icon: "doc.fill")
        } catch {
            st.notify("Error: \(error.localizedDescription)", icon: "xmark.circle", tint: .red)
        }
    }
    
    func saveFile() {
        st.saveCurrentDoc()
        if st.currentFilePath.isEmpty {
            saveFileAs()
        } else {
            let url = URL(fileURLWithPath: st.currentFilePath)
            do {
                try st.source.write(to: url, atomically: true, encoding: .utf8)
                st.notify("Saved \(url.lastPathComponent)", icon: "checkmark.circle", tint: .green)
            } catch {
                st.notify("Save failed: \(error.localizedDescription)", icon: "xmark.circle", tint: .red)
            }
        }
    }
    
    func saveFileAs() {
        st.saveCurrentDoc()
        let panel = NSSavePanel()
        panel.allowedContentTypes = [.plainText, .sourceCode]
        panel.nameFieldStringValue = st.currentFile
        panel.canCreateDirectories = true
        if panel.runModal() == .OK, let url = panel.url {
            do {
                try st.source.write(to: url, atomically: true, encoding: .utf8)
                st.currentFilePath = url.path
                st.currentFile = url.lastPathComponent
                if let idx = st.docs.firstIndex(where: { $0.id == st.currentDocId }) {
                    st.docs[idx].title = url.lastPathComponent
                }
                st.addRecent(url.path)
                st.notify("Saved as \(url.lastPathComponent)", icon: "checkmark.circle", tint: .green)
            } catch {
                st.notify("Save failed: \(error.localizedDescription)", icon: "xmark.circle", tint: .red)
            }
        }
    }
    
    func printDocument() {
        let printInfo = NSPrintInfo.shared
        printInfo.horizontalPagination = .fit
        printInfo.verticalPagination = .automatic
        printInfo.isHorizontallyCentered = false
        printInfo.isVerticallyCentered = false
        printInfo.topMargin = 36
        printInfo.bottomMargin = 36
        printInfo.leftMargin = 36
        printInfo.rightMargin = 36
        
        let pageW = printInfo.paperSize.width - 72
        let textView = NSTextView(frame: NSRect(x: 0, y: 0, width: pageW, height: 0))
        textView.isEditable = false
        
        if st.showMarkdownPreview {
            // Print rendered Markdown preview
            let rendered = markdownToAttributedString(st.source, pageWidth: pageW)
            textView.textStorage?.setAttributedString(rendered)
        } else {
            // Print raw source with syntax highlighting
            let header = "\(st.currentFile) — \(st.currentLanguage) — Printed from MarkCraft\n\n"
            textView.font = NSFont.monospacedSystemFont(ofSize: 10, weight: .regular)
            if let lang = st.syn {
                let hl = Syn.highlight(st.source, lang: lang, isDark: false)
                let full = NSMutableAttributedString(string: header, attributes: [
                    .font: NSFont.systemFont(ofSize: 10, weight: .bold),
                    .foregroundColor: NSColor.darkGray
                ])
                full.append(hl)
                full.addAttribute(.font, value: NSFont.monospacedSystemFont(ofSize: 10, weight: .regular),
                                  range: NSRange(location: header.count, length: full.length - header.count))
                textView.textStorage?.setAttributedString(full)
            } else {
                textView.string = header + st.source
            }
        }
        textView.sizeToFit()
        
        let printOp = NSPrintOperation(view: textView, printInfo: printInfo)
        printOp.showsPrintPanel = true
        printOp.showsProgressPanel = true
        printOp.run()
    }
    
    func markdownToAttributedString(_ md: String, pageWidth: CGFloat) -> NSAttributedString {
        let result = NSMutableAttributedString()
        let lines = md.components(separatedBy: "\n")
        
        let bodyFont = NSFont.systemFont(ofSize: 11, weight: .regular)
        let monoFont = NSFont.monospacedSystemFont(ofSize: 10, weight: .regular)
        let h1Font = NSFont.systemFont(ofSize: 22, weight: .bold)
        let h2Font = NSFont.systemFont(ofSize: 18, weight: .bold)
        let h3Font = NSFont.systemFont(ofSize: 14, weight: .semibold)
        let accentColor = NSColor(red:0.42, green:0.33, blue:0.82, alpha:1)
        let codeBlockBg = NSColor(red:0.95, green:0.95, blue:0.97, alpha:1)
        
        let bodyPara = NSMutableParagraphStyle()
        bodyPara.lineSpacing = 3
        bodyPara.paragraphSpacing = 6
        
        let codePara = NSMutableParagraphStyle()
        codePara.lineSpacing = 2
        codePara.paragraphSpacing = 8
        codePara.headIndent = 12
        codePara.firstLineHeadIndent = 12
        
        let bulletPara = NSMutableParagraphStyle()
        bulletPara.lineSpacing = 2
        bulletPara.headIndent = 20
        bulletPara.firstLineHeadIndent = 8
        
        let quotePara = NSMutableParagraphStyle()
        quotePara.lineSpacing = 2
        quotePara.headIndent = 16
        quotePara.firstLineHeadIndent = 16
        
        // Header
        let header = NSAttributedString(string: "\(st.currentFile) — Printed from MarkCraft\n\n",
                                        attributes: [.font: NSFont.systemFont(ofSize: 9, weight: .medium),
                                                     .foregroundColor: NSColor.gray])
        result.append(header)
        
        var inCode = false
        var codeBlock: [String] = []
        
        for line in lines {
            let trimmed = line.trimmingCharacters(in: .whitespaces)
            
            // Code block
            if trimmed.hasPrefix("```") {
                if inCode {
                    let code = codeBlock.joined(separator: "\n") + "\n"
                    result.append(NSAttributedString(string: code, attributes: [
                        .font: monoFont, .foregroundColor: NSColor.black,
                        .backgroundColor: codeBlockBg, .paragraphStyle: codePara
                    ]))
                    codeBlock = []; inCode = false
                } else { inCode = true }
                continue
            }
            if inCode { codeBlock.append(line); continue }
            
            if trimmed.isEmpty {
                result.append(NSAttributedString(string: "\n"))
                continue
            }
            
            // HR
            if trimmed == "---" || trimmed == "***" {
                let hrPara = NSMutableParagraphStyle(); hrPara.paragraphSpacing = 12
                result.append(NSAttributedString(string: "————————————————————————————\n",
                    attributes: [.font: bodyFont, .foregroundColor: NSColor.lightGray, .paragraphStyle: hrPara]))
                continue
            }
            
            // Headings
            if trimmed.hasPrefix("### ") {
                let text = String(trimmed.dropFirst(4)) + "\n"
                result.append(NSAttributedString(string: text, attributes: [.font: h3Font, .foregroundColor: NSColor.black, .paragraphStyle: bodyPara]))
                continue
            }
            if trimmed.hasPrefix("## ") {
                let text = String(trimmed.dropFirst(3)) + "\n"
                let h2Para = NSMutableParagraphStyle(); h2Para.paragraphSpacingBefore = 12; h2Para.paragraphSpacing = 4
                result.append(NSAttributedString(string: text, attributes: [.font: h2Font, .foregroundColor: accentColor, .paragraphStyle: h2Para]))
                continue
            }
            if trimmed.hasPrefix("# ") {
                let text = String(trimmed.dropFirst(2)) + "\n"
                let h1Para = NSMutableParagraphStyle(); h1Para.paragraphSpacingBefore = 16; h1Para.paragraphSpacing = 6
                result.append(NSAttributedString(string: text, attributes: [.font: h1Font, .foregroundColor: accentColor, .paragraphStyle: h1Para]))
                continue
            }
            
            // Blockquote
            if trimmed.hasPrefix(">") {
                let content = "▎ " + String(trimmed.dropFirst(1).drop(while: { $0 == " " })) + "\n"
                result.append(NSAttributedString(string: content, attributes: [
                    .font: NSFont.systemFont(ofSize: 11, weight: .regular), .foregroundColor: NSColor.darkGray, .paragraphStyle: quotePara
                ]))
                continue
            }
            
            // Bullet
            if trimmed.hasPrefix("- ") || trimmed.hasPrefix("* ") {
                let content = "  •  " + String(trimmed.dropFirst(2)) + "\n"
                result.append(NSAttributedString(string: content, attributes: [.font: bodyFont, .foregroundColor: NSColor.black, .paragraphStyle: bulletPara]))
                continue
            }
            
            // Paragraph
            result.append(NSAttributedString(string: trimmed + "\n", attributes: [.font: bodyFont, .foregroundColor: NSColor.black, .paragraphStyle: bodyPara]))
        }
        
        // Flush code block if unclosed
        if inCode && !codeBlock.isEmpty {
            result.append(NSAttributedString(string: codeBlock.joined(separator: "\n") + "\n",
                attributes: [.font: monoFont, .foregroundColor: NSColor.black, .backgroundColor: codeBlockBg, .paragraphStyle: codePara]))
        }
        
        return result
    }
}
