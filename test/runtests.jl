using BTree
using Test
using AbstractTrees

@testset "BTree.jl" begin
	tree =  BTree.B⁺Tree{Int, Int}(5)
	data = Dict{Int, Int}()
	for i in 1:200
		k, v = rand(1:1000), rand(1:100)
		tree[k] = v
		data[k] = v

		@test BTree.test_b⁺tree(tree, data)
    end
end

