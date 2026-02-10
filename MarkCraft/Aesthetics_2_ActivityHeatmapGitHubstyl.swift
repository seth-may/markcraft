import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct ActivityHeatmap: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct RadialMenuItem: Identifiable { let id = UUID(); let icon: String; let label: String; let action: () -> Void }
struct RadialMenuView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct SyntaxThemePreview: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct ScrollProgressIndicator: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
