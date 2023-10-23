using BTree
using Test
using AbstractTrees

@testset "BTree.jl" begin
	tree =  BTree.B⁺Tree{Int, Int}(5)
	for i in 1:200
		k, v = rand(1:1000), rand(1:100)
		tree[k] = v
		leaf_keys = AbstractTrees.Leaves(tree.root) .|> keys .|> collect |> x -> reduce(vcat, x)
		@test issorted(leaf_keys)
    end
end

