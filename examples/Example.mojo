# Mojo: High-performance matrix library with SIMD
from math import sqrt, abs
from memory import memset_zero, memcpy
from sys.info import simdwidthof
from algorithm import vectorize, parallelize
from random import rand

alias F64 = DType.float64
alias simd_width = simdwidthof[F64]()

struct Matrix:
    var data: DTypePointer[F64]
    var rows: Int
    var cols: Int

    fn __init__(inout self, rows: Int, cols: Int):
        self.rows = rows
        self.cols = cols
        self.data = DTypePointer[F64].alloc(rows * cols)
        memset_zero(self.data, rows * cols)

    fn __del__(owned self):
        self.data.free()

    fn __getitem__(self, row: Int, col: Int) -> Float64:
        return self.data.load(row * self.cols + col)

    fn __setitem__(inout self, row: Int, col: Int, val: Float64):
        self.data.store(row * self.cols + col, val)

    fn fill_random(inout self):
        rand(self.data, self.rows * self.cols)

    @staticmethod
    fn zeros(rows: Int, cols: Int) -> Matrix:
        return Matrix(rows, cols)

    @staticmethod
    fn identity(n: Int) -> Matrix:
        var m = Matrix(n, n)
        for i in range(n):
            m[i, i] = 1.0
        return m

    fn transpose(self) -> Matrix:
        var result = Matrix(self.cols, self.rows)
        for i in range(self.rows):
            for j in range(self.cols):
                result[j, i] = self[i, j]
        return result

    # SIMD-accelerated matrix multiply
    fn matmul(self, other: Matrix) -> Matrix:
        var result = Matrix(self.rows, other.cols)
        
        @parameter
        fn calc_row(i: Int):
            for k in range(self.cols):
                @parameter
                fn dot[width: Int](j: Int):
                    var a = SIMD[F64, width](self[i, k])
                    var b = other.data.load[width=width](k * other.cols + j)
                    var c = result.data.load[width=width](i * other.cols + j)
                    result.data.store(i * other.cols + j, c + a * b)
                vectorize[dot, simd_width](other.cols)
        parallelize[calc_row](self.rows)
        return result

    fn frobenius_norm(self) -> Float64:
        var sum: Float64 = 0.0
        @parameter
        fn accum[width: Int](i: Int):
            var v = self.data.load[width=width](i)
            sum += (v * v).reduce_add()
        vectorize[accum, simd_width](self.rows * self.cols)
        return sqrt(sum)

    fn print_summary(self):
        print("Matrix", self.rows, "x", self.cols,
              "norm:", self.frobenius_norm())

fn benchmark():
    alias N = 512
    print("Matrix multiply:", N, "x", N)
    var a = Matrix(N, N)
    var b = Matrix(N, N)
    a.fill_random()
    b.fill_random()
    
    var c = a.matmul(b)
    c.print_summary()
    print("Done - SIMD width:", simd_width)

fn main():
    benchmark()
