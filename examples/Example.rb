# frozen_string_literal: true

# --- DSL Builder Pattern ---
class RouteBuilder
  attr_reader :routes

  def initialize(&block)
    @routes = []
    @middlewares = []
    instance_eval(&block) if block_given?
  end

  def middleware(&block) = @middlewares << block

  %w[get post put patch delete].each do |method|
    define_method(method) do |path, **opts, &handler|
      @routes << {
        method: method.upcase, path: path,
        handler: handler, middlewares: @middlewares.dup,
        **opts
      }
    end
  end

  def namespace(prefix, &block)
    nested = self.class.new(&block)
    nested.routes.each do |route|
      route[:path] = "\#{prefix}#{route[:path]}"
      @routes << route
    end
  end
end

# --- Lazy Enumerator Pipeline ---
module Pipeline
  refine Enumerator::Lazy do
    def through(*transforms)
      transforms.reduce(self) { |enum, fn| enum.map(&fn) }
    end
  end
end

# --- Concurrent Worker Pool ---
class WorkerPool
  def initialize(size: 4)
    @queue = Queue.new
    @workers = size.times.map { spawn_worker }
    @results = Concurrent::Hash.new
  end

  def submit(id, &task)
    @queue << [id, task]
    self
  end

  def await_all
    @queue.close
    @workers.each(&:join)
    @results.to_h
  end

  private

  def spawn_worker
    Thread.new do
      while (job = @queue.pop)
        id, task = job
        @results[id] = begin
          { ok: task.call }
        rescue => e
          { error: e.message }
        end
      end
    end
  end
end

# --- Memoization with Fiber ---
def fibonacci
  Fiber.new do
    a, b = 0, 1
    loop do
      Fiber.yield a
      a, b = b, a + b
    end
  end
end

# --- Usage ---
app = RouteBuilder.new do
  middleware { |req| puts "Logger: #{req[:path]}" }

  get '/users' do |req|
    { users: User.all.map(&:to_h) }
  end

  namespace '/api/v2' do
    post '/events' do |req|
      Event.create!(req[:body])
    end
  end
end
