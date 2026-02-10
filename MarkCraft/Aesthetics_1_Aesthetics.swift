import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct AnimatedGradientBG: View { var body: some View { LinearGradient(colors: [.purple, .blue], startPoint: .topLeading, endPoint: .bottomTrailing) } }
struct GlowingBorder: ViewModifier { var color: Color = Color(red: 0.96, green: 0.651, blue: 0.137); var radius: CGFloat = 10; func body(content: Content) -> some View { content.overlay(RoundedRectangle(cornerRadius: radius).stroke(color, lineWidth: 1)) } }
struct ShimmerEffect: ViewModifier { func body(content: Content) -> some View { content } }
struct TypewriterText: View { let text: String; var body: some View { Text(text) } }
struct MorphingNumber: View { let value: Int; var body: some View { Text("\(value)") } }
struct PulseRing: View { var color: Color = .blue; var body: some View { Circle().stroke(color, lineWidth: 2).frame(width: 40, height: 40) } }
struct GlassCard<Content: View>: View { let content: Content; init(@ViewBuilder content: () -> Content) { self.content = content() }; var body: some View { content.padding().background(.ultraThinMaterial).cornerRadius(12) } }
struct AnimatedProgressBar: View { var progress: Double; var color: Color = .blue; var body: some View { GeometryReader { g in let w = g.size.width * CGFloat(progress); Rectangle().fill(color).frame(width: w, height: 4).cornerRadius(2) } .frame(height: 4) } }
struct ConfettiBurst: View { var body: some View { EmptyView() } }
struct ConfettiParticle: Identifiable { let id = UUID(); var x: Double; var y: Double; var color: Color }
struct StreakFireView: View { var streak: Int = 0; var body: some View { Text("🔥 \(streak)") } }
struct MoodRingView: View { var mood: String = ""; var body: some View { Text(mood.isEmpty ? "😊" : mood) } }
struct LanguageBadge: View { var name: String; var color: Color; var body: some View { Text(name).font(.caption2).foregroundColor(.white).padding(.horizontal, 6).padding(.vertical, 2).background(color).cornerRadius(4) } }
