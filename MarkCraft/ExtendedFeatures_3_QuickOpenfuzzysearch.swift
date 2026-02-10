import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct QuickOpenView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
enum MDNode { case heading(Int, [MDInline]); case paragraph([MDInline]); case codeBlock(String, String) }
enum MDInline { case text(String); case bold(String); case italic(String); case code(String); case link(String, String) }
class MarkdownParser { func parse(_ input: String) -> [MDNode] { [] } }
struct MDRenderer: View { let nodes: [MDNode]; var body: some View { EmptyView() } }
