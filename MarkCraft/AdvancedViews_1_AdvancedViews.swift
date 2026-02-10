import SwiftUI
import AppKit
import Foundation
import Combine
import AVFoundation
import UniformTypeIdentifiers
import OSLog


struct OnboardingTourView: View {
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    var body: some View { Text(loc.t("welcome_tour_title")).foregroundColor(th.c.text).padding() }
}
struct ShortcutsCheatSheet: View {
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    var body: some View { Text(loc.t("shortcuts_title")).foregroundColor(th.c.text).padding() }
}
struct GoToLineOverlay: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
struct DevQuoteBar: View {
    @EnvironmentObject var th: Theme
    var body: some View {
        let quotes = ["Code is poetry.", "Ship it.", "Make it work, make it right, make it fast."]
        let q = quotes[Int.random(in: 0..<quotes.count)]
        Text(q).font(.system(size: 10, design: .serif)).foregroundColor(th.c.muted).padding(.horizontal, 12)
    }
}
struct NewFileDialog: View {
    @EnvironmentObject var st: AppState
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    @State private var filename = "Untitled.swift"
    var body: some View {
        let c = th.c
        ZStack {
            Color.black.opacity(0.4).onTapGesture { st.showNewFile = false }
            VStack(spacing: 12) {
                Text(loc.t("new_file")).font(.headline).foregroundColor(c.text)
                TextField("Filename", text: $filename).textFieldStyle(.roundedBorder)
                HStack {
                    Button("Cancel") { st.showNewFile = false }.foregroundColor(c.muted)
                    Spacer()
                    Button("Create") { st.createNewDoc(title: filename); st.showNewFile = false }
                        .foregroundColor(c.gold)
                }
            }
            .padding(20).frame(width: 350).background(c.card).cornerRadius(12).shadow(radius: 20)
        }
    }
}
struct MCContentView: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}

// MARK: - App Icon (Original M Logo)
struct MCLogoView: View {
    var size: CGFloat = 120
    var body: some View {
        Canvas { ctx, sz in
            let w = sz.width; let h = sz.height
            let pad = w * 0.032
            let rr = CGRect(x: pad, y: pad, width: w - pad * 2, height: h - pad * 2)
            let cr = w * 0.216
            // Background gradient
            let bgGrad = Gradient(colors: [Color(red: 0.176, green: 0.106, blue: 0.412), Color(red: 0.051, green: 0.039, blue: 0.165)])
            ctx.fill(Path(roundedRect: rr, cornerRadius: cr), with: .linearGradient(bgGrad, startPoint: .zero, endPoint: CGPoint(x: w, y: h)))
            // Glow ellipse
            let ew: CGFloat = w * 0.625; let eh: CGFloat = h * 0.547
            let er = CGRect(x: (w - ew) / 2, y: h * 0.15, width: ew, height: eh)
            ctx.opacity = 0.2
            ctx.fill(Path(ellipseIn: er), with: .linearGradient(Gradient(colors: [Color(red: 0.655, green: 0.545, blue: 0.98).opacity(0.3), Color(red: 0.404, green: 0.91, blue: 0.976).opacity(0)]), startPoint: .zero, endPoint: CGPoint(x: w, y: h)))
            ctx.opacity = 1.0
            // M letter
            var m = Path()
            let sx = w / 1024; let sy = h / 1024
            m.move(to: CGPoint(x: 236 * sx, y: 760 * sy))
            m.addLine(to: CGPoint(x: 236 * sx, y: 310 * sy))
            m.addLine(to: CGPoint(x: 336 * sx, y: 310 * sy))
            m.addLine(to: CGPoint(x: 512 * sx, y: 560 * sy))
            m.addLine(to: CGPoint(x: 688 * sx, y: 310 * sy))
            m.addLine(to: CGPoint(x: 788 * sx, y: 310 * sy))
            m.addLine(to: CGPoint(x: 788 * sx, y: 760 * sy))
            m.addLine(to: CGPoint(x: 696 * sx, y: 760 * sy))
            m.addLine(to: CGPoint(x: 696 * sx, y: 456 * sy))
            m.addLine(to: CGPoint(x: 540 * sx, y: 680 * sy))
            m.addLine(to: CGPoint(x: 484 * sx, y: 680 * sy))
            m.addLine(to: CGPoint(x: 328 * sx, y: 456 * sy))
            m.addLine(to: CGPoint(x: 328 * sx, y: 760 * sy))
            m.closeSubpath()
            let mGrad = Gradient(colors: [Color(red: 0.96, green: 0.651, blue: 0.137), Color(red: 0.91, green: 0.529, blue: 0.357), Color(red: 0.831, green: 0.392, blue: 0.541)])
            ctx.fill(m, with: .linearGradient(mGrad, startPoint: .zero, endPoint: CGPoint(x: w, y: h)))
            // Cursor diamond
            var d = Path()
            d.move(to: CGPoint(x: 740 * sx, y: 800 * sy))
            d.addLine(to: CGPoint(x: 760 * sx, y: 760 * sy))
            d.addLine(to: CGPoint(x: 780 * sx, y: 800 * sy))
            d.addLine(to: CGPoint(x: 760 * sx, y: 810 * sy))
            d.closeSubpath()
            ctx.opacity = 0.6
            ctx.fill(d, with: .color(Color(red: 0.404, green: 0.91, blue: 0.976)))
            // Stars
            ctx.opacity = 0.7
            ctx.fill(Path(ellipseIn: CGRect(x: 800 * sx - 4, y: 250 * sy - 4, width: 8, height: 8)), with: .color(Color(red: 0.96, green: 0.651, blue: 0.137)))
            ctx.opacity = 0.5
            ctx.fill(Path(ellipseIn: CGRect(x: 820 * sx - 2.5, y: 270 * sy - 2.5, width: 5, height: 5)), with: .color(Color(red: 0.655, green: 0.545, blue: 0.98)))
            ctx.opacity = 0.4
            ctx.fill(Path(ellipseIn: CGRect(x: 790 * sx - 1.8, y: 280 * sy - 1.8, width: 3.6, height: 3.6)), with: .color(Color(red: 0.404, green: 0.91, blue: 0.976)))
        }
        .frame(width: size, height: size)
        .clipShape(RoundedRectangle(cornerRadius: size * 0.22))
        .shadow(color: Color(red: 0.655, green: 0.545, blue: 0.98).opacity(0.4), radius: 20)
    }
}

// MARK: - Paywall / Subscription View
struct PaywallView: View {
    @EnvironmentObject var st: AppState
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    @State private var selectedTier: Int = 1
    @State private var animateIn = false
    @State private var pulseGlow = false
    
    private let tiers: [(name: String, nameEN: String, price: String, priceNote: String, features: [String], badge: String)] = [
        ("Essentiel", "Essential", "0,99 €", "/mois", [
            "60 langages", "Terminal intégré", "Thème sombre/clair", "5 fichiers max"
        ], ""),
        ("Pro", "Pro", "1,99 €", "/mois", [
            "Tout Essentiel +", "Fichiers illimités", "Journal développeur", "Annotations encre", "Export PDF"
        ], "POPULAIRE"),
        ("Premium", "Premium", "2,99 €", "/mois", [
            "Tout Pro +", "Assistant IA Claude", "Sessions audio", "Mode Zen", "Mises à jour prioritaires"
        ], "COMPLET"),
    ]
    
    var body: some View {
        let c = th.c
        ZStack {
            // Backdrop
            Color.black.opacity(0.85).ignoresSafeArea()
            
            // Animated gradient background
            RoundedRectangle(cornerRadius: 24)
                .fill(
                    LinearGradient(
                        colors: [c.bg, Color(red: 0.08, green: 0.06, blue: 0.18)],
                        startPoint: .top, endPoint: .bottom
                    )
                )
                .frame(maxWidth: 680, maxHeight: 720)
                .overlay(
                    RoundedRectangle(cornerRadius: 24)
                        .stroke(c.lavender.opacity(0.3), lineWidth: 1)
                )
                .shadow(color: c.lavender.opacity(pulseGlow ? 0.3 : 0.1), radius: pulseGlow ? 30 : 10)
            
            VStack(spacing: 0) {
                // Header with logo
                VStack(spacing: 12) {
                    MCLogoView(size: 80)
                        .scaleEffect(animateIn ? 1.0 : 0.5)
                        .opacity(animateIn ? 1 : 0)
                    
                    Text("MarkCraft")
                        .font(.system(size: 28, weight: .bold))
                        .foregroundStyle(
                            LinearGradient(colors: [c.lavender, c.gold], startPoint: .leading, endPoint: .trailing)
                        )
                    
                    Text(loc.t("unlock_full"))
                        .font(.system(size: 14))
                        .foregroundColor(c.muted)
                }
                .padding(.top, 28)
                .padding(.bottom, 20)
                
                // Tier cards
                HStack(spacing: 12) {
                    ForEach(0..<3, id: \.self) { i in
                        tierCard(index: i, selected: selectedTier == i)
                            .onTapGesture { withAnimation(.spring(response: 0.3)) { selectedTier = i } }
                            .scaleEffect(animateIn ? 1.0 : 0.8)
                            .opacity(animateIn ? 1 : 0)
                            .animation(.spring(response: 0.5).delay(Double(i) * 0.1), value: animateIn)
                    }
                }
                .padding(.horizontal, 24)
                
                Spacer().frame(height: 24)
                
                // Subscribe button
                Button(action: subscribe) {
                    HStack(spacing: 8) {
                        Image(systemName: "crown.fill")
                            .font(.system(size: 14))
                        Text(loc.t("subscribe_now"))
                            .font(.system(size: 15, weight: .bold))
                    }
                    .foregroundColor(.white)
                    .frame(maxWidth: 320, minHeight: 48)
                    .background(
                        LinearGradient(colors: [c.lavender, Color(red: 0.55, green: 0.35, blue: 0.95)], startPoint: .leading, endPoint: .trailing)
                    )
                    .cornerRadius(14)
                    .shadow(color: c.lavender.opacity(0.4), radius: 12)
                }
                .buttonStyle(.plain)
                .padding(.bottom, 8)
                
                // Restore + Later + legal
                VStack(spacing: 6) {
                    HStack(spacing: 20) {
                        Button(loc.t("restore_purchases")) { restorePurchases() }
                            .font(.system(size: 12))
                            .foregroundColor(c.muted)
                            .buttonStyle(.plain)
                        
                        Button(loc.t("later")) { st.showPaywall = false }
                            .font(.system(size: 12))
                            .foregroundColor(c.dim)
                            .buttonStyle(.plain)
                    }
                    
                    Text(loc.t("sub_legal"))
                        .font(.system(size: 9))
                        .foregroundColor(c.dim)
                        .multilineTextAlignment(.center)
                        .padding(.horizontal, 40)
                }
                .padding(.bottom, 20)
            }
            .frame(maxWidth: 680, maxHeight: 720)
        }
        .onAppear {
            withAnimation(.spring(response: 0.6)) { animateIn = true }
            withAnimation(.easeInOut(duration: 2).repeatForever(autoreverses: true)) { pulseGlow = true }
        }
    }
    
    @ViewBuilder
    private func tierCard(index: Int, selected: Bool) -> some View {
        let c = th.c
        let tier = tiers[index]
        VStack(spacing: 8) {
            // Badge
            if !tier.badge.isEmpty {
                Text(tier.badge)
                    .font(.system(size: 8, weight: .heavy))
                    .foregroundColor(.white)
                    .padding(.horizontal, 8)
                    .padding(.vertical, 3)
                    .background(Capsule().fill(c.gold))
            } else {
                Spacer().frame(height: 16)
            }
            
            // Name
            Text(tier.name)
                .font(.system(size: 16, weight: .bold))
                .foregroundColor(selected ? c.text : c.muted)
            
            // Price
            Text(tier.price)
                .font(.system(size: 28, weight: .heavy))
                .foregroundColor(selected ? c.gold : c.text)
            Text(tier.priceNote)
                .font(.system(size: 11))
                .foregroundColor(c.dim)
            
            Divider().background(c.border)
            
            // Features
            VStack(alignment: .leading, spacing: 4) {
                ForEach(0..<tier.features.count, id: \.self) { j in
                    HStack(spacing: 6) {
                        Image(systemName: "checkmark.circle.fill")
                            .font(.system(size: 10))
                            .foregroundColor(selected ? c.green : c.muted)
                        Text(tier.features[j])
                            .font(.system(size: 11))
                            .foregroundColor(selected ? c.text : c.muted)
                    }
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.horizontal, 8)
            
            Spacer()
        }
        .padding(.vertical, 16)
        .padding(.horizontal, 8)
        .frame(maxWidth: .infinity, minHeight: 300)
        .background(
            RoundedRectangle(cornerRadius: 16)
                .fill(selected ? c.card : c.bg)
                .overlay(
                    RoundedRectangle(cornerRadius: 16)
                        .stroke(selected ? c.lavender : c.border, lineWidth: selected ? 2 : 1)
                )
        )
        .scaleEffect(selected ? 1.03 : 1.0)
        .animation(.spring(response: 0.3), value: selected)
    }
    
    private func subscribe() {
        // StoreKit integration placeholder
        // In production: use StoreKit 2 Product.purchase()
        let tier = tiers[selectedTier]
        st.isSubscribed = true
        st.subscriptionTier = tier.nameEN
        st.showPaywall = false
        st.notify("Abonnement \(tier.name) activé!", icon: "crown.fill", tint: .orange)
    }
    
    private func restorePurchases() {
        // StoreKit: Transaction.currentEntitlements
        st.notify("Vérification en cours...", icon: "arrow.clockwise")
    }
}

struct ExportDialog: View {
    @EnvironmentObject var th: Theme
    var body: some View { EmptyView() }
}
struct SplashView: View {
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    var body: some View {
        VStack(spacing: 16) { MCLogoView(size: 100); Text("MarkCraft").font(.system(size: 24, weight: .bold)).foregroundColor(th.c.text); Text(loc.t("poetic_ide")).font(.system(size: 14)).foregroundColor(th.c.gold) }.padding(60)
    }
}
