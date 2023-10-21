using BTree
using Test

@testset "BTree.jl" begin
    # Write your tests here.
    @test BTree.hello() == "hello"
    @test BTree.hello() != "Hello world!"
end
