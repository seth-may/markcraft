// WebPreviewView.swift — MarkCraft V150
// Live HTML/CSS preview using WKWebView
import SwiftUI
import WebKit

// MARK: - Web Preview (HTML/CSS live rendering)
struct WebPreviewView: NSViewRepresentable {
    var source: String
    var language: String // "HTML" or "CSS"
    var isDark: Bool
    
    func makeNSView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.preferences.setValue(true, forKey: "developerExtrasEnabled")
        let wv = WKWebView(frame: .zero, configuration: config)
        wv.setValue(false, forKey: "drawsBackground")
        return wv
    }
    
    func updateNSView(_ wv: WKWebView, context: Context) {
        let html = buildHTML()
        wv.loadHTMLString(html, baseURL: nil)
    }
    
    private func buildHTML() -> String {
        if language == "CSS" {
            // Wrap CSS in a demo HTML page
            return """
            <!DOCTYPE html>
            <html>
            <head><meta charset="UTF-8">
            <style>
            \(source)
            </style>
            </head>
            <body style="margin:20px;font-family:-apple-system,system-ui,sans-serif;background:\(isDark ? "#0d0b1a" : "#ffffff");color:\(isDark ? "#e8e6f0" : "#1c1c24");">
            <h1>Heading 1</h1>
            <h2>Heading 2</h2>
            <h3>Heading 3</h3>
            <p>This is a paragraph with <strong>bold</strong>, <em>italic</em> and <code>inline code</code>.</p>
            <p>Ceci est un paragraphe avec du <a href="#">lien</a> et du texte.</p>
            <ul><li>Item 1</li><li>Item 2</li><li>Item 3</li></ul>
            <ol><li>First</li><li>Second</li><li>Third</li></ol>
            <blockquote>A blockquote element for styling.</blockquote>
            <pre><code>let x = 42
            print("Hello")</code></pre>
            <table border="1" cellpadding="8" cellspacing="0">
            <tr><th>Name</th><th>Value</th></tr>
            <tr><td>Alpha</td><td>100</td></tr>
            <tr><td>Beta</td><td>200</td></tr>
            </table>
            <button>Button</button>
            <input type="text" placeholder="Input field" style="margin:4px">
            <div class="card" style="padding:16px;margin:12px 0;border:1px solid \(isDark ? "#333" : "#ddd");border-radius:8px;">
            <h4>Card Component</h4>
            <p>A sample card for testing your CSS styles.</p>
            </div>
            <footer style="margin-top:20px;padding-top:12px;border-top:1px solid \(isDark ? "#333" : "#ddd");font-size:12px;opacity:0.6;">
            MarkCraft CSS Preview — edit your stylesheet and see changes live
            </footer>
            </body></html>
            """
        } else {
            // HTML: render as-is, inject base styles if no <style> or <link>
            var html = source
            let hasDoctype = html.lowercased().contains("<!doctype") || html.lowercased().contains("<html")
            
            if !hasDoctype {
                // Wrap fragment in a full HTML document
                let bg = isDark ? "#0d0b1a" : "#ffffff"
                let fg = isDark ? "#e8e6f0" : "#1c1c24"
                html = """
                <!DOCTYPE html>
                <html>
                <head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0">
                <style>
                body { margin: 20px; font-family: -apple-system, system-ui, sans-serif; background: \(bg); color: \(fg); }
                img { max-width: 100%; height: auto; }
                a { color: #a78bfa; }
                </style>
                </head>
                <body>
                \(source)
                </body>
                </html>
                """
            }
            return html
        }
    }
}

// MARK: - Smart Preview (auto-selects Markdown or HTML/CSS)
struct SmartPreviewView: View {
    var source: String
    var language: String
    var isDark: Bool
    var fontScale: CGFloat
    
    var body: some View {
        VStack(spacing: 0) {
            // Preview header
            HStack(spacing: 6) {
                Image(systemName: previewIcon)
                    .font(.system(size: 10))
                    .foregroundColor(.gray)
                Text(previewLabel)
                    .font(.system(size: 10, weight: .medium, design: .monospaced))
                    .foregroundColor(.gray)
                Spacer()
                if isWebPreview {
                    Circle()
                        .fill(Color.green)
                        .frame(width: 6, height: 6)
                    Text("Live")
                        .font(.system(size: 9, weight: .medium))
                        .foregroundColor(.green)
                }
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 6)
            .background(isDark ? Color(red:0.05,green:0.04,blue:0.08) : Color(white: 0.95))
            
            // Preview content
            if isWebPreview {
                WebPreviewView(source: source, language: language, isDark: isDark)
            } else {
                MarkdownPreviewView(source: source, isDark: isDark, fontScale: fontScale)
            }
        }
    }
    
    var isWebPreview: Bool {
        let lang = language.lowercased()
        return lang == "html" || lang == "css" || lang == "scss" || lang == "svelte" || lang == "vue" || lang == "jsx" || lang == "tsx"
    }
    
    var previewIcon: String {
        isWebPreview ? "globe" : "doc.richtext"
    }
    
    var previewLabel: String {
        if isWebPreview {
            return language.uppercased() == "CSS" || language.uppercased() == "SCSS" ? "CSS Preview" : "HTML Preview"
        }
        return "Markdown Preview"
    }
}
