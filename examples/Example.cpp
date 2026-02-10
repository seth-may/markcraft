#include <concepts>
#include <coroutine>
#include <memory>
#include <functional>
#include <unordered_map>
#include <vector>
#include <optional>
#include <ranges>

// --- Concepts & Constraints ---
template<typename T>
concept Serializable = requires(T t) {
    { t.serialize() } -> std::convertible_to<std::string>;
    { T::deserialize(std::string{}) } -> std::same_as<T>;
};

// --- CRTP Observer Pattern ---
template<typename Derived>
class Observable {
    std::vector<std::function<void(const Derived&)>> observers_;
public:
    void subscribe(std::function<void(const Derived&)> fn) {
        observers_.push_back(std::move(fn));
    }
    void notify() {
        for (auto& fn : observers_) fn(static_cast<const Derived&>(*this));
    }
};

// --- Smart Pointer with Custom Deleter ---
template<typename T>
using SharedPool = std::shared_ptr<T>;

template<typename T>
class ObjectPool {
    std::vector<std::unique_ptr<T>> pool_;
    std::vector<T*> available_;
    
public:
    SharedPool<T> acquire() {
        T* ptr;
        if (!available_.empty()) {
            ptr = available_.back();
            available_.pop_back();
        } else {
            pool_.push_back(std::make_unique<T>());
            ptr = pool_.back().get();
        }
        return SharedPool<T>(ptr, [this](T* p) { available_.push_back(p); });
    }
};

// --- Compile-Time Map ---
template<typename Key, typename Value, std::size_t N>
struct ConstexprMap {
    std::array<std::pair<Key, Value>, N> data;
    
    [[nodiscard]] constexpr Value at(const Key& key) const {
        for (const auto& [k, v] : data) {
            if (k == key) return v;
        }
        throw std::out_of_range("Key not found");
    }
};

// --- Ranges Pipeline ---
auto transform_pipeline(const std::vector<int>& input) {
    return input
        | std::views::filter([](int n) { return n > 0; })
        | std::views::transform([](int n) { return n * n; })
        | std::views::take(10);
}
