import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct MCFileType: Identifiable { let id = UUID(); let name: String; let ext: [String]; let mime: String; let icon: String }
enum FileTypeRegistry { static let types: [MCFileType] = [] }
enum MCA11y { static let editor = "Code Editor" }
struct AccessibleIcon: View { let name: String; let label: String; var body: some View { Image(systemName: name).accessibilityLabel(label) } }
class PerformanceMonitor: ObservableObject { @Published var fps: Double = 60; @Published var memoryMB: Double = 0 }
struct PerformanceOverlay: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
enum MCCrashReporter { static func report(_ error: Error) { print("Crash: \(error)") } }
enum MCLogger { static func log(_ message: String) { print("[MC] \(message)") } }
enum FeatureFlags {
    @AppStorage("ff_ai_chat") static var aiChat = true
    @AppStorage("ff_ink_mode") static var inkMode = true
    @AppStorage("ff_audio") static var audio = true
    static var allFlags: [(name: String, key: String)] { [("AI Chat", "ff_ai_chat"), ("Ink Mode", "ff_ink_mode"), ("Audio", "ff_audio")] }
}
struct FeatureFlagsView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct DebugConsoleView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct Msg: Identifiable { let id = UUID(); let level: Lvl; let text: String; let timestamp = Date() }
enum Lvl: String { case info, warn, error, debug }
struct MemoryProfilerView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct BuildLogView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct LogEntry: Identifiable { let id = UUID(); let message: String; let status: LogStatus; let timestamp = Date() }
enum LogStatus: String { case success, failure, pending, running }
struct CPUProfilerView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct PerformanceDashboard: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct Breakpoint: Identifiable { let id = UUID(); var line: Int; var file: String; var isEnabled: Bool = true; var condition: String = "" }
class BreakpointManager: ObservableObject { @Published var breakpoints: [Breakpoint] = [] }
struct BreakpointListView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct VariableInspectorView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct Variable: Identifiable { let id = UUID(); var name: String; var value: String; var type: String }
