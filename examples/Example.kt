import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

// --- Sealed Class Hierarchy ---
sealed interface ViewState<out T> {
    data object Loading : ViewState<Nothing>
    data class Success<T>(val data: T) : ViewState<T>
    data class Error(val message: String, val retry: suspend () -> Unit) : ViewState<Nothing>
}

// --- Flow-based Repository ---
class UserRepository(private val api: ApiService, private val cache: UserCache) {
    fun observeUsers(): Flow<ViewState<List<User>>> = flow {
        emit(ViewState.Loading)
        try {
            val cached = cache.getAll()
            if (cached.isNotEmpty()) emit(ViewState.Success(cached))
            val fresh = api.fetchUsers()
            cache.saveAll(fresh)
            emit(ViewState.Success(fresh))
        } catch (e: Exception) {
            emit(ViewState.Error(e.message ?: "Unknown error") { observeUsers() })
        }
    }.flowOn(Dispatchers.IO)
     .distinctUntilChanged()
     .catch { emit(ViewState.Error(it.message ?: "Flow error") {}) }
}

// --- DSL Builder ---
@DslMarker annotation class HtmlDsl

@HtmlDsl
class HtmlBuilder {
    private val elements = mutableListOf<String>()
    
    fun head(block: HeadBuilder.() -> Unit) {
        elements += "<head>${HeadBuilder().apply(block).build()}</head>"
    }
    
    fun body(block: BodyBuilder.() -> Unit) {
        elements += "<body>${BodyBuilder().apply(block).build()}</body>"
    }
    
    fun build() = "<!DOCTYPE html><html>${elements.joinToString("")}</html>"
}

fun html(block: HtmlBuilder.() -> Unit) = HtmlBuilder().apply(block).build()

// --- Extension Functions + Inline ---
inline fun <T> List<T>.partitionBy(predicate: (T) -> Boolean): Pair<List<T>, List<T>> {
    val (left, right) = mutableListOf<T>() to mutableListOf<T>()
    forEach { if (predicate(it)) left.add(it) else right.add(it) }
    return left to right
}

// --- Delegation Pattern ---
interface Logger { fun log(msg: String) }
class ConsoleLogger : Logger { override fun log(msg: String) = println("[LOG] $msg") }

class Service(logger: Logger) : Logger by logger {
    suspend fun process(items: List<String>) = coroutineScope {
        items.map { async { log("Processing: $it"); it.uppercase() } }.awaitAll()
    }
}
