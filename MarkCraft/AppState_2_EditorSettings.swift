import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


// MARK: - Editor Settings
class EditorSettings: ObservableObject {
    @AppStorage("mc_theme_name") var themeName = "obsidian"
    @AppStorage("mc_cursor_style") var cursorStyle = "line"
    @AppStorage("mc_scroll_speed") var scrollSpeed = 1.0
    @Published var customColors: [String: Color] = [:]
}

struct EditorSettingsView: View {
    @EnvironmentObject var st: AppState
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    var body: some View {
        let c = th.c
        ScrollView {
            VStack(alignment: .leading, spacing: 20) {
                Text("Settings").font(.title2).foregroundColor(c.text)
                
                // Appearance
                VStack(alignment: .leading, spacing: 10) {
                    Label("Appearance", systemImage: "paintbrush")
                        .font(.headline).foregroundColor(c.accent)
                    
                    HStack(spacing: 12) {
                        ForEach(ThemeMode.allCases, id: \.self) { mode in
                            Button(action: { withAnimation(.easeInOut(duration: 0.25)) { th.mode = mode } }) {
                                VStack(spacing: 6) {
                                    Image(systemName: mode.icon)
                                        .font(.system(size: 20))
                                    Text(mode.rawValue.capitalized)
                                        .font(.system(size: 11, weight: .medium))
                                }
                                .frame(width: 80, height: 60)
                                .foregroundColor(th.mode == mode ? .white : c.dim)
                                .background(
                                    RoundedRectangle(cornerRadius: 10)
                                        .fill(th.mode == mode ? c.accent : c.card2)
                                )
                                .overlay(
                                    RoundedRectangle(cornerRadius: 10)
                                        .stroke(th.mode == mode ? c.accent : c.border, lineWidth: th.mode == mode ? 2 : 1)
                                )
                            }
                            .buttonStyle(.plain)
                        }
                    }
                }
                
                Divider().background(c.border)
                
                // Editor
                VStack(alignment: .leading, spacing: 10) {
                    Label("Editor", systemImage: "doc.text")
                        .font(.headline).foregroundColor(c.accent)
                    
                    HStack {
                        Text(loc.t("font_size_label")).foregroundColor(c.dim)
                        Slider(value: $st.fontSize, in: 9...32, step: 0.5)
                        Text(String(format: "%.0f", st.fontSize))
                            .foregroundColor(c.text)
                            .frame(width: 30)
                    }
                    Toggle("Word Wrap", isOn: $st.wordWrap)
                        .foregroundColor(c.text)
                    Toggle("Show Minimap", isOn: $st.showMinimap)
                        .foregroundColor(c.text)
                    Toggle("Show Line Numbers", isOn: $st.showLineNumbers)
                        .foregroundColor(c.text)
                    Toggle("Auto Save", isOn: $st.autoSave)
                        .foregroundColor(c.text)
                    Toggle("Bracket Matching", isOn: $st.bracketMatch)
                        .foregroundColor(c.text)
                }
            }.padding()
        }.background(c.bg)
    }
}

// MARK: - AutoSave
class AutoSaveManager: ObservableObject {
    private var timer: Timer?
    func start(interval: TimeInterval = 30, action: @escaping () -> Void) {
        timer = Timer.scheduledTimer(withTimeInterval: interval, repeats: true) { _ in action() }
    }
    func stop() { timer?.invalidate(); timer = nil }
}

// MARK: - Workspace
struct Workspace: Identifiable, Codable {
    var id = UUID()
    var name: String
    var files: [String]
    var created: Date
}

class WorkspaceManager: ObservableObject {
    @Published var workspaces: [Workspace] = []
    @Published var activeWorkspace: UUID?
    
    func create(name: String) {
        let ws = Workspace(name: name, files: [], created: Date())
        workspaces.append(ws)
        activeWorkspace = ws.id
    }
}

struct WorkspaceSwitcher: View {
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    @ObservedObject var manager: WorkspaceManager
    var body: some View {
        let c = th.c
        VStack(spacing: 8) {
            Text(loc.t("workspaces")).font(.headline).foregroundColor(c.text)
            ForEach(manager.workspaces) { ws in
                let isActive: Bool = manager.activeWorkspace == ws.id
                HStack {
                    Image(systemName: "folder")
                        .foregroundColor(_pc(isActive, c.gold, c.dim))
                    Text(ws.name).foregroundColor(c.text)
                    Spacer()
                }
                .padding(8)
                .background(_pc(isActive, c.card2, Color.clear))
                .cornerRadius(6)
                .onTapGesture { manager.activeWorkspace = ws.id }
            }
        }.padding()
    }
}

// MARK: - Session Restore
class SessionRestore: ObservableObject {
    @Published var savedSessions: [SavedSession] = []
    func save(files: [String], activeFile: String) {
        let s = SavedSession(files: files, activeFile: activeFile, timestamp: Date())
        savedSessions.append(s)
    }
}

struct SavedSession: Codable, Identifiable {
    var id = UUID()
    var files: [String]
    var activeFile: String
    var timestamp: Date
}

// MARK: - File Watcher
class FileWatcher: ObservableObject {
    @Published var watchedPaths: [String] = []
    func watch(_ path: String) { watchedPaths.append(path) }
    func unwatch(_ path: String) { watchedPaths.removeAll { $0 == path } }
}

// MARK: - Clipboard History
class ClipboardHistory: ObservableObject {
    @Published var items: [String] = []
    func add(_ text: String) {
        items.insert(text, at: 0)
        if items.count > 25 { items.removeLast() }
    }
}

struct ClipboardHistoryView: View {
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    @ObservedObject var clipboard: ClipboardHistory
    var body: some View {
        let c = th.c
        VStack(spacing: 8) {
            Text(loc.t("clipboard_history")).font(.headline).foregroundColor(c.text)
            ForEach(0..<clipboard.items.count, id: \.self) { i in
                let item = clipboard.items[i]
                Text(item.prefix(60))
                    .font(.system(.caption, design: .monospaced))
                    .foregroundColor(c.dim)
                    .padding(6)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .background(c.card)
                    .cornerRadius(4)
            }
        }.padding()
    }
}
