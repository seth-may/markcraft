import java.util.*;
import java.util.concurrent.*;
import java.util.stream.*;
import java.util.function.*;

// --- Generic Event-Driven Architecture ---
public sealed interface Event permits UserEvent, SystemEvent {
    String id();
    long timestamp();
}

record UserEvent(String id, long timestamp, String userId, String action) implements Event {}
record SystemEvent(String id, long timestamp, String component, String level) implements Event {}

// --- Reactive Stream Processor ---
class EventProcessor<T extends Event> {
    private final ConcurrentHashMap<String, List<Consumer<T>>> handlers = new ConcurrentHashMap<>();
    private final ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();
    private final BlockingQueue<T> queue = new LinkedBlockingQueue<>();
    
    public void subscribe(String eventType, Consumer<T> handler) {
        handlers.computeIfAbsent(eventType, k -> new CopyOnWriteArrayList<>()).add(handler);
    }
    
    public CompletableFuture<Void> publish(T event) {
        return CompletableFuture.runAsync(() -> {
            queue.offer(event);
            String type = event.getClass().getSimpleName();
            handlers.getOrDefault(type, List.of())
                .forEach(h -> executor.submit(() -> h.accept(event)));
        }, executor);
    }
    
    public <R> CompletableFuture<R> process(
        List<T> events,
        Function<T, R> mapper,
        BinaryOperator<R> reducer,
        R identity
    ) {
        return CompletableFuture.supplyAsync(() ->
            events.parallelStream()
                .map(mapper)
                .reduce(identity, reducer)
        );
    }
}

// --- Builder with Validation ---
class ServerConfig {
    private final String host;
    private final int port;
    private final int maxConnections;
    private final Duration timeout;
    
    private ServerConfig(Builder b) {
        this.host = Objects.requireNonNull(b.host);
        this.port = b.port;
        this.maxConnections = b.maxConnections;
        this.timeout = b.timeout;
    }
    
    static class Builder {
        private String host;
        private int port = 8080;
        private int maxConnections = 100;
        private Duration timeout = Duration.ofSeconds(30);
        
        Builder host(String h) { this.host = h; return this; }
        Builder port(int p) { this.port = p; return this; }
        Builder maxConnections(int m) { this.maxConnections = m; return this; }
        Builder timeout(Duration t
