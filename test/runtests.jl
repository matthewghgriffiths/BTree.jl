using BTree
using Test

@testset "BTree.jl" begin
    node = BTree.B⁺Node{Int, Int}(5)
    @show(node)
    @test BTree.order(node) == 5
    @test length(node) == 0 

    tree = BTree.B⁺Tree{Int, Int}(10)
    @test BTree.order(tree) == 10
    @test length(tree) == 0 

    insert!(node, 1, 2)
    @show node
    insert!(node, 1, 3)
    @show node
    insert!(node, 0, 4)
    @show node
    insert!(node, 2, 1)
    @show node
    @show BTree.split(node)
end

