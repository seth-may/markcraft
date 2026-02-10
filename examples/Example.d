// D Language: Lock-free concurrent hash map
import core.atomic;
import std.algorithm : map, filter, each;
import std.range : iota;
import std.conv : to;
import std.stdio : writefln, writeln;
import std.parallelism : parallel, taskPool;
import core.thread : Thread;
import core.time : dur;

/// Lock-free hash map using open addressing with linear probing
struct ConcurrentMap(K, V) if (is(K == string) || is(K : long)) {
    private enum State : ubyte { empty = 0, occupied = 1, deleted = 2 }
    
    private struct Slot {
        shared State state = State.empty;
        K key;
        shared V value;
    }

    private shared(Slot)[] slots;
    private shared size_t count_ = 0;
    
    this(size_t capacity) {
        slots = new shared(Slot)[capacity];
    }

    bool insert(K key, V value) {
        auto idx = hashOf(key) % slots.length;
        foreach (_; 0 .. slots.length) {
            auto state = atomicLoad(slots[idx].state);
            if (state == State.empty || state == State.deleted) {
                if (cas(&slots[idx].state, state, State.occupied)) {
                    slots[idx].key = key;
                    atomicStore(slots[idx].value, value);
                    atomicOp!"+="(count_, 1);
                    return true;
                }
            }
            idx = (idx + 1) % slots.length;
        }
        return false; // full
    }

    V* get(K key) {
        auto idx = hashOf(key) % slots.length;
        foreach (_; 0 .. slots.length) {
            auto state = atomicLoad(slots[idx].state);
            if (state == State.empty) return null;
            if (state == State.occupied && slots[idx].key == key) {
                return cast(V*)&slots[idx].value;
            }
            idx = (idx + 1) % slots.length;
        }
        return null;
    }

    bool remove(K key) {
        auto idx = hashOf(key) % slots.length;
        foreach (_; 0 .. slots.length) {
            if (atomicLoad(slots[idx].state) == State.empty) return false;
            if (slots[idx].key == key) {
                atomicStore(slots[idx].state, State.deleted);
                atomicOp!"-="(count_, 1);
                return true;
            }
            idx = (idx + 1) % slots.length;
        }
        return false;
    }

    @property size_t length() { return atomicLoad(count_); }
    
    private size_t hashOf(K key) {
        static if (is(K == string)) {
            size_t h = 5381;
            foreach (c; key) h = ((h << 5) + h) + c;
            return h;
        } else {
            return cast(size_t)(key * 2654435761);
        }
    }
}

void main() {
    auto hmap = ConcurrentMap!(string, int)(1024);
    
    // Parallel inserts
    foreach (i; taskPool.parallel(iota(100))) {
        auto key = "key_" ~ i.to!string;
        hmap.insert(key, cast(int)i * 42);
    }
    
    writefln("Inserted %d entries", hmap.length);
    
    // Concurrent reads
    foreach (i; taskPool.parallel(iota(100))) {
        auto key = "key_" ~ i.to!string;
        if (auto val = hmap.get(key))
            assert(*val == cast(int)i * 42);
    }
    
    writeln("All concurrent reads verified");
    
    // Stats
    writefln("Final size: %d", hmap.length);
}
