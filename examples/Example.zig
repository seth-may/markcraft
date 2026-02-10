const std = @import("std");
const Allocator = std.mem.Allocator;

// --- Generic HashMap with Open Addressing ---
pub fn AutoHashMap(comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();
        const Entry = struct { key: K, value: V, used: bool = false };
        
        entries: []Entry,
        count: usize = 0,
        allocator: Allocator,
        
        pub fn init(allocator: Allocator, capacity: usize) !Self {
            const entries = try allocator.alloc(Entry, capacity);
            @memset(entries, Entry{ .key = undefined, .value = undefined, .used = false });
            return Self{ .entries = entries, .count = 0, .allocator = allocator };
        }
        
        pub fn put(self: *Self, key: K, value: V) !void {
            if (self.count * 4 >= self.entries.len * 3) try self.resize();
            const idx = self.findSlot(key);
            if (!self.entries[idx].used) self.count += 1;
            self.entries[idx] = Entry{ .key = key, .value = value, .used = true };
        }
        
        pub fn get(self: *const Self, key: K) ?V {
            const idx = self.findSlot(key);
            return if (self.entries[idx].used) self.entries[idx].value else null;
        }
        
        fn findSlot(self: *const Self, key: K) usize {
            var idx = std.hash.uint32(@bitCast(key)) % self.entries.len;
            while (self.entries[idx].used and self.entries[idx].key != key) {
                idx = (idx + 1) % self.entries.len;
            }
            return idx;
        }
        
        fn resize(self: *Self) !void {
            const old = self.entries;
            self.entries = try self.allocator.alloc(Entry, old.len * 2);
            @memset(self.entries, Entry{ .key = undefined, .value = undefined, .used = false });
            self.count = 0;
            for (old) |entry| if (entry.used) try self.put(entry.key, entry.value);
            self.allocator.free(old);
        }
        
        pub fn deinit(self: *Self) void {
            self.allocator.free(self.entries);
        }
    };
}

// --- Thread Pool ---
pub fn ThreadPool(comptime max_threads: usize) type {
    return struct {
        threads: [max_threads]std.Thread = undefined,
        active: usize = 0,
        
        pub fn spawn(self: *@This(), func: anytype, args: anytype) !void {
            if (self.active >= max_threads) return error.PoolFull;
            self.threads[self.active] = try std.Thread.spawn(.{}, func, ar
