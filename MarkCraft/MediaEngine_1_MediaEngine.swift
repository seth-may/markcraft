import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


final class AudioManager: ObservableObject {
    @Published var isRecording = false
    @Published var isPaused = false
    @Published var isPlaying = false
    @Published var currentTime: TimeInterval = 0
    @Published var duration: TimeInterval = 0
    @Published var sessions: [AudioSession] = []
    @Published var currentSessionId: UUID?
    @Published var transcript = ""
    @Published var smartNotes = ""
    @Published var waveformData: [Float] = []
    @Published var codeTimestamps: [CodeTimestamp] = []
    @Published var recordingLevel: Float = 0
    
    private var audioRecorder: AVAudioRecorder?
    private var audioPlayer: AVAudioPlayer?
    
    func startRecording() {
        guard !isRecording else { return }
        isRecording = true
        let id = UUID()
        currentSessionId = id
    }
    
    func stopRecording() {
        guard isRecording else { return }
        audioRecorder?.stop()
        isRecording = false
        if let id = currentSessionId {
            let session = AudioSession(id: id, filename: "session_\(id).m4a)", duration: currentTime, created: Date(), transcript: "", codeTimestamps: [], smartNotes: "")
            sessions.append(session)
        }
    }
    
    func toggleRecording() {
        if isRecording { stopRecording() } else { startRecording() }
    }
    
    func play(session: AudioSession) { isPlaying = true }
    func pause() { isPaused = true; isPlaying = false }
    func stop() { isPlaying = false; isPaused = false; currentTime = 0 }
}

struct AudioRecorderView: View {
    @EnvironmentObject var th: Theme
    @EnvironmentObject var audio: AudioManager
    var body: some View {
        let c = th.c
        VStack(spacing: 12) {
            Circle()
                .fill(_pc(audio.isRecording, c.red, c.muted))
                .frame(width: 60, height: 60)
                .onTapGesture { audio.toggleRecording() }
            Text(_pc(audio.isRecording, c.red, c.dim) == c.red ? "Recording..." : "Tap to Record")
                .foregroundColor(c.dim)
        }.padding()
    }
}
struct AudioSessionsListView: View {
    @EnvironmentObject var th: Theme
    @EnvironmentObject var audio: AudioManager
    var body: some View {
        let c = th.c
        VStack {
            ForEach(audio.sessions) { s in
                HStack { Text(s.filename).foregroundColor(c.text); Spacer() }
                    .padding(8)
            }
        }
    }
}
struct AudioPlaybackBar: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
struct SmartNotesView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
struct SessionReplayView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
struct WaveformView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
