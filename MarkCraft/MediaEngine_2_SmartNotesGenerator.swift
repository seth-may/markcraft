import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


class SmartNotesEngine { var notes: [SmartNote] = [] }
struct SmartNote: Identifiable { let id = UUID(); var text: String; var type: NoteType }
enum NoteType: String { case summary, insight, todo, question }
struct AudioSessionCard: View {
    let session: AudioSession
    @EnvironmentObject var th: Theme
    var body: some View { Text(session.filename).foregroundColor(th.c.text).padding(8) }
}
struct AudioSessionListView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
struct AudioTranscriptView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
enum AudioEffect: String, CaseIterable { case reverb, echo, pitch, speed }
struct AudioEffectsPanel: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}

final class InkManager: ObservableObject {
    @Published var inkMode = false
    @Published var annotations: [InkAnnotation] = []
    @Published var currentTool: InkTool = .pen
    @Published var currentColor = "gold"
    @Published var currentThickness: Double = 2.0
    @Published var currentOpacity: Double = 1.0
    @Published var showToolbox = true
    @Published var toolboxPosition = CGPoint(x: 100, y: 100)
    @Published var isDrawing = false
    @Published var currentStroke: [CGPoint] = []
    @Published var undoStack: [[InkAnnotation]] = []
    @Published var redoStack: [[InkAnnotation]] = []
    @Published var selectedAnnotations: Set<UUID> = []
    
    static let inkColors: [(name: String, color: Color)] = [
        ("gold", Color(red: 0.96, green: 0.651, blue: 0.137)),
        ("copper", Color(red: 0.91, green: 0.529, blue: 0.357)),
        ("rose", Color(red: 0.831, green: 0.392, blue: 0.541)),
        ("cyan", Color(red: 0.404, green: 0.91, blue: 0.976)),
        ("lavender", Color(red: 0.655, green: 0.545, blue: 0.98)),
        ("green", Color(red: 0.298, green: 0.851, blue: 0.392)),
    ]
    
    func addAnnotation(_ points: [[Double]], color: String, tool: InkTool) {
        let a = InkAnnotation(points: points, color: color, thickness: currentThickness, tool: tool, timestamp: Date(), lineNumber: 0)
        annotations.append(a)
    }
    func undo() { guard let last = undoStack.popLast() else { return }; redoStack.append(annotations); annotations = last }
    func redo() { guard let last = redoStack.popLast() else { return }; undoStack.append(annotations); annotations = last }
    func clear() { undoStack.append(annotations); annotations.removeAll(); redoStack.removeAll() }
}

struct InkToolbarView: View {
    @EnvironmentObject var th: Theme
    @EnvironmentObject var ink: InkManager
    var body: some View {
        let c = th.c
        HStack(spacing: 8) {
            ForEach(InkTool.allCases, id: \.self) { tool in
                let isActive: Bool = ink.currentTool == tool
                Button(action: { ink.currentTool = tool }) {
                    Text(tool.rawValue.prefix(3))
                        .font(.system(size: 10))
                        .foregroundColor(_pc(isActive, c.gold, c.muted))
                        .padding(4)
                        .background(_pc(isActive, c.card2, Color.clear))
                        .cornerRadius(4)
                }.buttonStyle(.plain)
            }
        }.padding(6).background(c.card).cornerRadius(8)
    }
}
struct InkCanvasView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
struct InkAnnotationsListView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
