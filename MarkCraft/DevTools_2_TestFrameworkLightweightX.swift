import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct TestResult: Identifiable { let id = UUID(); let name: String; let passed: Bool; let duration: TimeInterval; let message: String }
class TestRunner: ObservableObject {
    @Published var results: [TestResult] = []
    @Published var isRunning = false
    func runAllTests() { isRunning = true; DispatchQueue.main.asyncAfter(deadline: .now() + 1) { self.isRunning = false } }
}
struct TestReportView: View { @EnvironmentObject var th: Theme; var body: some View { EmptyView() } }
