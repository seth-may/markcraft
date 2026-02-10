import asyncio
from dataclasses import dataclass, field
from typing import Generic, TypeVar, Protocol, AsyncGenerator
from collections import defaultdict
import json

T = TypeVar('T')

# --- Decorator Pattern ---
def retry(max_attempts: int = 3, delay: float = 1.0):
    def decorator(func):
        async def wrapper(*args, **kwargs):
            for attempt in range(max_attempts):
                try:
                    return await func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_attempts - 1:
                        raise
                    await asyncio.sleep(delay * (2 ** attempt))
        return wrapper
    return decorator

# --- Generic Event System ---
class EventBus(Generic[T]):
    def __init__(self):
        self._handlers: dict[str, list] = defaultdict(list)
        self._history: list[tuple[str, T]] = []
    
    def on(self, event: str):
        def decorator(handler):
            self._handlers[event].append(handler)
            return handler
        return decorator
    
    async def emit(self, event: str, data: T) -> None:
        self._history.append((event, data))
        tasks = [h(data) for h in self._handlers.get(event, [])]
        await asyncio.gather(*tasks)

# --- Data Pipeline ---
@dataclass
class Pipeline:
    steps: list = field(default_factory=list)
    
    def pipe(self, func):
        self.steps.append(func)
        return self
    
    async def execute(self, data):
        result = data
        for step in self.steps:
            if asyncio.iscoroutinefunction(step):
                result = await step(result)
            else:
                result = step(result)
        return result

# --- Context Manager + Generator ---
class DatabasePool:
    def __init__(self, size: int = 5):
        self._pool: asyncio.Queue = asyncio.Queue(maxsize=size)
        self._size = size
    
    async def __aenter__(self):
        for _ in range(self._size):
            await self._pool.put(await self._create_conn())
        return self
    
    async def __aexit__(self, *exc):
        while not self._pool.empty():
            conn = await self._pool.get()
            await conn.close()
    
    async def _create_conn(self):
        return {"connected": True, "id": id(self)}
    
    async def stream_query(self, sql: str) -> AsyncGenerator:
        conn = await self._pool.get()
        try:
            for row in range(100):
                yield {"row": row, "sql": sql}
