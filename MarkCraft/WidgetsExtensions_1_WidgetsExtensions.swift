import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct WidgetConfig {
    enum WidgetKind: String, CaseIterable, Identifiable { case streak, activity, stats, quote; var id: String { rawValue } }
}
struct StreakWidgetPreview: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct ActivityWidgetPreview: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct StatsWidgetPreview: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct QuoteWidgetPreview: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct WidgetGalleryView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
enum MCURLScheme { static func handle(_ url: URL) {} }
struct ShareSheet: View { var body: some View { EmptyView() } }
struct KeyboardShortcutManager { static var shortcuts: [Shortcut] = [] }
struct Shortcut: Identifiable { let id = UUID(); var key: String; var modifiers: String; var action: String; var category: String }
struct KeyboardShortcutsView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct ThemeCustomizer: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
