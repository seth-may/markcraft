import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct BookPage: Identifiable { let id = UUID(); var type: BookPageType; var content: BookContent }
enum BookPageType: String { case cover, toc, chapter, section, appendix, colophon }
struct BookContent { var title: String = ""; var body: String = ""; var code: String = ""; var stats: [String: String] = [:] }
class BookLayoutEngine: ObservableObject { @Published var pages: [BookPage] = [] }
struct BookPreviewView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
enum BookExportFormat: String { case pdf, epub, html }
struct BookExportView: View { @EnvironmentObject var th: Theme; @EnvironmentObject var journal: JournalManager; var body: some View { EmptyView() } }
