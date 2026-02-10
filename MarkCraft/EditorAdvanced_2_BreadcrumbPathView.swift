import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct BreadcrumbBarView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
enum DiffLineKind: String { case unchanged, added, removed, modified }
class DiffEngine: ObservableObject { @Published var diffs: [DiffLine] = [] }
struct DiffLine: Identifiable { let id = UUID(); let kind: DiffLineKind; let oldLineNum: Int?; let newLineNum: Int?; let content: String }
struct DiffView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct SearchMatch: Identifiable { let id = UUID(); let docId: UUID; let docTitle: String; let lineNumber: Int; let column: Int; let length: Int; let lineContent: String }
class SearchEngine: ObservableObject { @Published var matches: [SearchMatch] = [] }
struct FindInFilesView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct CompletionPopup: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
class MultiCursorEngine: ObservableObject {
    struct Cursor: Identifiable { let id = UUID(); var line: Int; var col: Int }
    @Published var cursors: [Cursor] = []
}
class CodeLensProvider: ObservableObject {
    struct Lens: Identifiable { let id = UUID(); var line: Int; var title: String }
    @Published var lenses: [Lens] = []
}
