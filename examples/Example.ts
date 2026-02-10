// Advanced TypeScript: Conditional Types, Template Literals, Decorators

// --- Branded Types ---
type Brand<T, B extends string> = T & { readonly __brand: B };
type UserId = Brand<string, 'UserId'>;
type Email = Brand<string, 'Email'>;
type Currency = Brand<number, 'Currency'>;

// --- Builder Pattern with Fluent API ---
type BuilderMethods<T> = {
  [K in keyof T as `set${Capitalize<string & K>}`]: (value: T[K]) => Builder<T>;
};

class Builder<T extends Record<string, unknown>> {
  private data: Partial<T> = {};
  
  constructor(private validator?: (data: T) => boolean) {}
  
  set<K extends keyof T>(key: K, value: T[K]): this {
    this.data[key] = value;
    return this;
  }
  
  build(): T {
    const result = this.data as T;
    if (this.validator && !this.validator(result)) {
      throw new Error('Validation failed');
    }
    return result;
  }
}

// --- Type-safe Event Emitter ---
type EventMap = Record<string, unknown>;

class TypedEmitter<Events extends EventMap> {
  private handlers = new Map<keyof Events, Set<Function>>();
  
  on<K extends keyof Events>(
    event: K,
    handler: (data: Events[K]) => void | Promise<void>
  ): () => void {
    if (!this.handlers.has(event)) this.handlers.set(event, new Set());
    this.handlers.get(event)!.add(handler);
    return () => this.handlers.get(event)?.delete(handler);
  }
  
  async emit<K extends keyof Events>(event: K, data: Events[K]): Promise<void> {
    const fns = this.handlers.get(event) ?? new Set();
    await Promise.all([...fns].map(fn => fn(data)));
  }
}

// --- Recursive Deep Readonly ---
type DeepReadonly<T> = T extends Function ? T
  : T extends Map<infer K, infer V> ? ReadonlyMap<DeepReadonly<K>, DeepReadonly<V>>
  : T extends Set<infer U> ? ReadonlySet<DeepReadonly<U>>
  : T extends object ? { readonly [P in keyof T]: DeepReadonly<T[P]> }
  : T;

// --- Result Type (Rust-inspired) ---
type Result<T, E = Error> =
  | { ok: true; value: T }
  | { ok: false; error: E };

function tryCatch<T>(fn: () => T): Result<T> {
  try { return { ok: true, value: fn() }; }
  catch (e) { return { ok: false, error: e as Error }; }
}

// --- Middleware Pipeline ---
type Middleware<Ctx> = (ctx: Ctx, next: () => Promise<void>) => Promise<void>;

class Pipeline<Ctx> {
  private stack: Middleware<Ctx>[] = [];
  
  use(mw: Middleware<Ctx>): this {
    this.stack.push(mw);
    return this;
  }
  
  async execute(ctx: Ctx): Promise<void> {
    const run = (i: number): Promise<void> =>
      i < this.stac
