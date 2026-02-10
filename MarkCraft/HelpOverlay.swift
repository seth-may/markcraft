// HelpOverlay.swift — MarkCraft V150
// Localized help overlay accessible from Help → MarkCraft Help (⌘?)
import SwiftUI

struct HelpOverlay: View {
    @EnvironmentObject var st: AppState
    @EnvironmentObject var th: Theme
    @EnvironmentObject var loc: Loc
    @State private var tab = 0
    
    var body: some View {
        let c = th.c
        ZStack {
            Color.black.opacity(0.6)
                .ignoresSafeArea()
                .onTapGesture { st.showHelp = false }
            
            VStack(spacing: 0) {
                // Title bar
                HStack {
                    Image(systemName: "questionmark.circle.fill")
                        .foregroundColor(c.accent)
                    Text(loc.t("markcraft_help"))
                        .font(.system(size: 16, weight: .bold))
                        .foregroundColor(c.text)
                    Spacer()
                    // Tab picker
                    Picker("", selection: $tab) {
                        Text(h("tab_shortcuts")).tag(0)
                        Text(h("tab_features")).tag(1)
                        Text(h("tab_guide")).tag(2)
                    }
                    .pickerStyle(.segmented)
                    .frame(width: 360)
                    Spacer()
                    Button(action: { st.showHelp = false }) {
                        Image(systemName: "xmark.circle.fill")
                            .font(.system(size: 18))
                            .foregroundColor(c.muted)
                    }.buttonStyle(.plain)
                }
                .padding(.horizontal, 20)
                .padding(.vertical, 12)
                .background(c.card)
                
                Divider().overlay(c.border)
                
                // Content
                ScrollView {
                    VStack(alignment: .leading, spacing: 16) {
                        switch tab {
                        case 0: shortcutsTab(c)
                        case 1: featuresTab(c)
                        case 2: guideTab(c)
                        default: EmptyView()
                        }
                    }
                    .padding(24)
                }
                .background(c.bg)
            }
            .frame(width: 700, height: 520)
            .cornerRadius(12)
            .shadow(color: .black.opacity(0.4), radius: 20)
        }
    }
    
    // MARK: - Shortcuts Tab
    func shortcutsTab(_ c: MC) -> some View {
        VStack(alignment: .leading, spacing: 20) {
            sectionTitle(h("sc_file"), c)
            shortcutGrid([
                ("⌘N", h("sc_new")),
                ("⌘O", h("sc_open")),
                ("⌘S", h("sc_save")),
                ("⇧⌘S", h("sc_save_as")),
                ("⌘P", h("sc_print")),
                ("⇧⌘E", h("sc_examples")),
            ], c)
            
            sectionTitle(h("sc_edit"), c)
            shortcutGrid([
                ("⌘F", h("sc_find")),
                ("⌘G", h("sc_goto")),
                ("⇧⌘P", h("sc_palette")),
                ("⌘Z", h("sc_undo")),
                ("⇧⌘Z", h("sc_redo")),
            ], c)
            
            sectionTitle(h("sc_view"), c)
            shortcutGrid([
                ("⌥⌘P", h("sc_preview")),
                ("⌘\\", h("sc_split")),
                ("⌘B", h("sc_sidebar")),
                ("⌥⌘D", h("sc_doc")),
                ("⌘+", h("sc_zoomin")),
                ("⌘-", h("sc_zoomout")),
            ], c)
            
            sectionTitle(h("sc_run"), c)
            shortcutGrid([
                ("⌘R", h("sc_execute")),
                ("⌘,", h("sc_settings")),
                ("⌘?", h("sc_help")),
                ("⇧⌥⌘T", "Tetris 🧱"),
            ], c)
        }
    }
    
    func shortcutGrid(_ items: [(String, String)], _ c: MC) -> some View {
        LazyVGrid(columns: [
            GridItem(.flexible()), GridItem(.flexible())
        ], spacing: 6) {
            ForEach(items, id: \.0) { key, desc in
                HStack(spacing: 10) {
                    Text(key)
                        .font(.system(size: 12, weight: .bold, design: .monospaced))
                        .foregroundColor(c.accent)
                        .frame(width: 60, alignment: .trailing)
                    Text(desc)
                        .font(.system(size: 13))
                        .foregroundColor(c.text)
                    Spacer()
                }
                .padding(.vertical, 4)
                .padding(.horizontal, 8)
                .background(c.card.opacity(0.5))
                .cornerRadius(6)
            }
        }
    }
    
    // MARK: - Features Tab
    func featuresTab(_ c: MC) -> some View {
        VStack(alignment: .leading, spacing: 16) {
            featureRow("paintbrush", h("ft_syntax"), h("ft_syntax_d"), c)
            featureRow("doc.richtext", h("ft_preview"), h("ft_preview_d"), c)
            featureRow("globe", h("ft_html"), h("ft_html_d"), c)
            featureRow("terminal", h("ft_terminal"), h("ft_terminal_d"), c)
            featureRow("book", h("ft_journal"), h("ft_journal_d"), c)
            featureRow("text.magnifyingglass", h("ft_palette"), h("ft_palette_d"), c)
            featureRow("moon.stars", h("ft_themes"), h("ft_themes_d"), c)
            featureRow("globe.europe.africa", h("ft_i18n"), h("ft_i18n_d"), c)
            featureRow("printer", h("ft_print"), h("ft_print_d"), c)
            featureRow("doc.text", h("ft_docs"), h("ft_docs_d"), c)
        }
    }
    
    func featureRow(_ icon: String, _ title: String, _ desc: String, _ c: MC) -> some View {
        HStack(alignment: .top, spacing: 12) {
            Image(systemName: icon)
                .font(.system(size: 18))
                .foregroundColor(c.accent)
                .frame(width: 28)
            VStack(alignment: .leading, spacing: 3) {
                Text(title)
                    .font(.system(size: 14, weight: .semibold))
                    .foregroundColor(c.text)
                Text(desc)
                    .font(.system(size: 12))
                    .foregroundColor(c.dim)
                    .fixedSize(horizontal: false, vertical: true)
            }
        }
        .padding(10)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(c.card)
        .cornerRadius(8)
    }
    
    // MARK: - Guide Tab
    func guideTab(_ c: MC) -> some View {
        VStack(alignment: .leading, spacing: 16) {
            Text(h("guide_welcome"))
                .font(.system(size: 18, weight: .bold))
                .foregroundColor(c.text)
            
            Text(h("guide_intro"))
                .font(.system(size: 13))
                .foregroundColor(c.dim)
                .fixedSize(horizontal: false, vertical: true)
            
            Divider().overlay(c.border)
            
            guideStep("1", h("guide_s1"), h("guide_s1_d"), c)
            guideStep("2", h("guide_s2"), h("guide_s2_d"), c)
            guideStep("3", h("guide_s3"), h("guide_s3_d"), c)
            guideStep("4", h("guide_s4"), h("guide_s4_d"), c)
            guideStep("5", h("guide_s5"), h("guide_s5_d"), c)
            
            Divider().overlay(c.border)
            
            HStack(spacing: 8) {
                Text("MarkCraft V150")
                    .font(.system(size: 11, weight: .bold, design: .monospaced))
                    .foregroundColor(c.accent)
                Text("•")
                    .foregroundColor(c.muted)
                Text(h("guide_footer"))
                    .font(.system(size: 11))
                    .foregroundColor(c.muted)
            }
        }
    }
    
    func guideStep(_ num: String, _ title: String, _ desc: String, _ c: MC) -> some View {
        HStack(alignment: .top, spacing: 12) {
            Text(num)
                .font(.system(size: 14, weight: .black, design: .rounded))
                .foregroundColor(.white)
                .frame(width: 26, height: 26)
                .background(c.accent)
                .cornerRadius(13)
            VStack(alignment: .leading, spacing: 3) {
                Text(title)
                    .font(.system(size: 14, weight: .semibold))
                    .foregroundColor(c.text)
                Text(desc)
                    .font(.system(size: 12))
                    .foregroundColor(c.dim)
                    .fixedSize(horizontal: false, vertical: true)
            }
        }
    }
    
    // MARK: - Section title
    func sectionTitle(_ text: String, _ c: MC) -> some View {
        Text(text)
            .font(.system(size: 14, weight: .bold))
            .foregroundColor(c.accent)
            .padding(.top, 4)
    }
    
    // MARK: - Localized help string
    func h(_ key: String) -> String {
        loc.t("help_\(key)")
    }
}
