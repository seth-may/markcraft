import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct ParticleEffectsView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct Particle: Identifiable { let id = UUID(); var x: Double; var y: Double; var vx: Double; var vy: Double; var life: Double; var color: Color }
enum AmbientSound: String, CaseIterable, Identifiable { case rain, ocean, forest, cafe, fire, wind; var id: String { rawValue } }
struct AmbientSoundMixer: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct CodeStatsView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct YearRecapView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
struct AIChatMessage: Identifiable { let id = UUID(); let role: ChatRole; let content: String; let timestamp = Date() }
class AIChatManager: ObservableObject { @Published var messages: [AIChatMessage] = [] }
struct AIChatPanelView: View { @EnvironmentObject var th: Theme; @EnvironmentObject var journal: JournalManager; var body: some View { EmptyView() } }

enum MDBlock { case heading(Int, String); case paragraph(String); case code(String, String); case list([String]) }
struct WritingStatsView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
