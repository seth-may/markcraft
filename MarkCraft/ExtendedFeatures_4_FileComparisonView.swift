import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct FileComparisonView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
class TagManager: ObservableObject { @Published var tags: [String] = [] }
struct TagSidebarView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct TypewriterModeSettings: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
class WikiLinkParser { func parse(_ text: String) -> [(range: Range<String.Index>, target: String)] { [] } }
struct WikiLinkView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct BacklinksView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct NoteLinkGraphView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
class PinArchiveManager: ObservableObject { @Published var pinnedIds: Set<UUID> = []; @Published var archivedIds: Set<UUID> = [] }
