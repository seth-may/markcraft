import SwiftUI
import Combine

// MARK: - Generic Repository Pattern
protocol Repository {
    associatedtype Entity: Identifiable & Codable
    func fetchAll() async throws -> [Entity]
    func save(_ entity: Entity) async throws
    func delete(id: Entity.ID) async throws
}

actor DataStore<T: Identifiable & Codable>: Repository {
    typealias Entity = T
    private var cache: [T.ID: T] = [:]
    private let encoder = JSONEncoder()
    private let decoder = JSONDecoder()
    
    func fetchAll() async throws -> [Entity] {
        Array(cache.values)
    }
    
    func save(_ entity: T) async throws {
        cache[entity.id] = entity
        let data = try encoder.encode(entity)
        try data.write(to: fileURL(for: entity.id))
    }
    
    func delete(id: T.ID) async throws {
        cache.removeValue(forKey: id)
        try FileManager.default.removeItem(at: fileURL(for: id))
    }
    
    private func fileURL(for id: T.ID) -> URL {
        FileManager.default.temporaryDirectory
            .appendingPathComponent("\(id).json")
    }
}

// MARK: - SwiftUI View with Animation
struct PulseButton: View {
    let title: String
    let action: () -> Void
    @State private var isPressed = false
    
    var body: some View {
        Button(action: {
            withAnimation(.spring(response: 0.3, dampingFraction: 0.5)) {
                isPressed = true
            }
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
                isPressed = false
                action()
            }
        }) {
            Text(title)
                .font(.headline)
                .foregroundColor(.white)
                .padding(.horizontal, 32)
                .padding(.vertical, 14)
                .background(
                    RoundedRectangle(cornerRadius: 16)
                        .fill(.linearGradient(
                            colors: [.purple, .blue],
                            startPoint: .leading,
                            endPoint: .trailing
                        ))
                )
                .scaleEffect(isPressed ? 0.92 : 1.0)
                .shadow(color: .purple.opacity(0.4), radius: isPressed ? 4 : 12)
        }
        .buttonStyle(.plain)
    }
}

// MARK: - Async Stream Processing
func processStream<T: Sendable>(
    _ stream: AsyncStream<T>,
    transform: @Sendable @escaping (T) async -> T?
) -> AsyncStream<T> {
    AsyncStream { continuation in
        Task {
            for await item in stream {
