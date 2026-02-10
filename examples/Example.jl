# Neural network from scratch with multiple dispatch
module SimpleNN

using LinearAlgebra, Random, Printf

abstract type Layer end
abstract type Activation <: Layer end

struct Dense <: Layer
    weights::Matrix{Float64}
    bias::Vector{Float64}
end

struct ReLU <: Activation end
struct Sigmoid <: Activation end
struct Softmax <: Activation end

# Constructors
Dense(in::Int, out::Int) = Dense(
    randn(out, in) * sqrt(2.0 / in),
    zeros(out)
)

# Forward pass — multiple dispatch
forward(l::Dense, x::Vector) = l.weights * x .+ l.bias
forward(::ReLU, x::Vector) = max.(0, x)
forward(::Sigmoid, x::Vector) = 1.0 ./ (1.0 .+ exp.(-x))
function forward(::Softmax, x::Vector)
    ex = exp.(x .- maximum(x))
    ex ./ sum(ex)
end

# Network
struct Network
    layers::Vector{Layer}
end

function forward(net::Network, x::Vector)
    for layer in net.layers
        x = forward(layer, x)
    end
    x
end

# Loss functions
cross_entropy(y_pred, y_true) = -sum(y_true .* log.(y_pred .+ 1e-8))

function accuracy(net::Network, X::Matrix, Y::Matrix)
    correct = 0
    for i in 1:size(X, 2)
        pred = forward(net, X[:, i])
        if argmax(pred) == argmax(Y[:, i])
            correct += 1
        end
    end
    correct / size(X, 2)
end

# Training
function train!(net::Network, X, Y; epochs=100, lr=0.01)
    for epoch in 1:epochs
        total_loss = 0.0
        for i in 1:size(X, 2)
            pred = forward(net, X[:, i])
            total_loss += cross_entropy(pred, Y[:, i])
        end
        if epoch % 10 == 0
            acc = accuracy(net, X, Y)
            @printf "Epoch %3d | Loss: %.4f | Acc: %.2f%%\n" epoch total_loss/size(X,2) acc*100
        end
    end
end

# Demo
function demo()
    Random.seed!(42)
    net = Network([
        Dense(4, 16), ReLU(),
        Dense(16, 8), ReLU(),
        Dense(8, 3), Softmax()
    ])
    X = randn(4, 100)
    Y = zeros(3, 100)
    for i in 1:100; Y[rand(1:3), i] = 1.0; end
    @printf "Network: %d layers, %d parameters\n" length(net.layers) sum(
        l isa Dense ? length(l.weights) + length(l.bias) : 0
        for l in net.layers)
    train!(net, X, Y, epochs=50, lr=0.01)
end

end # module
SimpleNN.demo()
