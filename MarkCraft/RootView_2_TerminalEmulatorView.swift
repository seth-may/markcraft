import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct TerminalView: View {
    @EnvironmentObject var th: Theme
    @State private var input = ""
    @State private var output: [String] = ["$ Welcome to MarkCraft Terminal"]
    var body: some View {
        let c = th.c
        VStack(spacing: 0) {
            ScrollView {
                VStack(alignment: .leading, spacing: 2) {
                    ForEach(0..<output.count, id: \.self) { i in
                        Text(output[i])
                            .font(.system(size: 12, design: .monospaced))
                            .foregroundColor(c.text)
                    }
                }
                .frame(maxWidth: .infinity, alignment: .leading)
                .padding(8)
            }
            HStack(spacing: 4) {
                Text("$").foregroundColor(c.green).font(.system(size: 12, design: .monospaced))
                TextField("", text: $input)
                    .textFieldStyle(.plain)
                    .font(.system(size: 12, design: .monospaced))
                    .foregroundColor(c.text)
                    .onSubmit { runCommand() }
            }
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            .background(c.card)
        }
        .background(c.editor)
    }
    private func runCommand() {
        let cmd = input.trimmingCharacters(in: .whitespaces)
        guard !cmd.isEmpty else { return }
        output.append("$ \(cmd)")
        switch cmd {
        case "clear": output = []
        case "help": output.append("Commands: clear, help, date, echo, pwd")
        case "date": output.append(Date().formatted())
        case "pwd": output.append(FileManager.default.currentDirectoryPath)
        default:
            if cmd.hasPrefix("echo ") {
                output.append(String(cmd.dropFirst(5)))
            } else {
                output.append("\(cmd): command not found")
            }
        }
        input = ""
    }
}

struct EnhancedTabBar: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}

enum BottomPanelTab: String, CaseIterable { case problems, output, terminal, journal }

struct BottomPanelManager: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}

struct OutputView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}

struct ProblemsView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}

struct ActivityBarView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}

struct EditorTabContextMenu: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}

struct IndentGuideOverlay: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}

struct DiffGutterView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
