using BTree
using Test
using AbstractTrees
using Random


@testset "B⁺Tree" begin
	rng = MersenneTwister(1337)
	n_keys = 500
	kvs = Pair.(rand(rng, 1:n_keys, n_keys), rand(rng, 1:n_keys, n_keys))

	tree_sizes = [2, 3, 11, 32]
	for N in tree_sizes
		tree =  BTree.B⁺Tree{Int, Int}(10)
		data = Dict{Int, Int}()
		for (k, v) in kvs
			tree[k] = v
			data[k] = v
			@test BTree.test_b⁺tree(tree, data)
		end

		ks = keys(tree) |> collect
		ks = ks[randperm(rng, length(ks))]

		for k in ks
			delete!(data, k)
			delete!(tree, k)
			@test BTree.test_b⁺tree(tree, data)
		end
	end
end

