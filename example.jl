### A Pluto.jl notebook ###
# v0.19.41

using Markdown
using InteractiveUtils

# ╔═╡ af17526e-7180-11ee-18f4-17737fc4e8c4
begin
	using Pkg	
	using BenchmarkTools
	using Random 
	using DataStructures
	using SparseArrays
	using Profile
	using ProfileCanvas
	using Test

	Pkg.activate(".")
	
	using Base.Order
	using AbstractTrees
	using Revise
	using BTree
end

# ╔═╡ ce6373a5-0ae8-49ef-97a1-c26a13fde5c6
begin
	N = 128
	n = 2_000_000
	
	rng = MersenneTwister(1337)
	ks, vs = rand(rng, 1:n, n), rand(rng, 1:100, n)
	kvs = Pair.(ks, vs)
	ks = ks[randperm(rng, n)]

	
	data = Dict(kvs)
	sorted_data = SortedDict(kvs)

	spv = sparsevec(collect(keys(data)), collect(values(data)))

	k = ks[1]
	
	tree = BTree.B⁺Tree{N}(kvs)
	root = tree.root;
end

# ╔═╡ 520d7ac0-8b82-4aff-a209-052de45ccbdf
@benchmark $data[$k]

# ╔═╡ fb8ce847-c461-497f-8137-1ecafcffd75c
@benchmark $spv[$k]

# ╔═╡ 18154a17-462f-4d27-9bc0-5fdab2a1d0e8
@benchmark $(tree.root)[$k]

# ╔═╡ 3fa67dc6-c0bf-4e71-adc3-6184ec023ad4
@benchmark $tree[$k]

# ╔═╡ 9a1c12aa-c5ba-4b46-8e65-57bcd2b7f5be
@benchmark BTree.search($(tree.root), $k)

# ╔═╡ c89a7564-1d35-4744-a2de-76b85a4ee038
begin
	@btime BTree.search($(tree.root), x) setup=(x=rand(1:1000))
	@btime BTree.index($(tree.root), x) setup=(x=rand(1:1000))
	@btime BTree.get($tree, x) setup=(x=rand(1:1000))
end

# ╔═╡ a8c01879-6cde-4bba-a717-b8cce343a772
let
	tree =  BTree.B⁺Tree{Int, Int, N}()
	@benchmark $tree[i] = j setup=(i=rand(1:n); j=rand(1:n))
end

# ╔═╡ 5e5d6624-5b69-47d4-b199-b99750ac8124
let
	sorted_data = SortedDict{Int, Int}()
	@benchmark $sorted_data[i] = j setup=(i=rand(1:n); j=rand(1:n))
end

# ╔═╡ a530c0de-448a-48f5-a205-f4fbf34015b0
let
	sorted_data = SortedDict{Int, Int}()
	@benchmark $sorted_data[i] = j setup=(i=rand(1:n); j=rand(1:n))
end

# ╔═╡ 173b4e60-37b5-46be-b681-7a8f586aa816
let
	data =  Dict{Int, Int}()
	@benchmark $data[i] = j setup=(i=rand(1:n); j=rand(1:n))
end

# ╔═╡ fd917f69-2886-416f-b603-d374cfdb6e0a


# ╔═╡ Cell order:
# ╠═af17526e-7180-11ee-18f4-17737fc4e8c4
# ╠═ce6373a5-0ae8-49ef-97a1-c26a13fde5c6
# ╠═520d7ac0-8b82-4aff-a209-052de45ccbdf
# ╠═fb8ce847-c461-497f-8137-1ecafcffd75c
# ╠═18154a17-462f-4d27-9bc0-5fdab2a1d0e8
# ╠═3fa67dc6-c0bf-4e71-adc3-6184ec023ad4
# ╠═9a1c12aa-c5ba-4b46-8e65-57bcd2b7f5be
# ╠═c89a7564-1d35-4744-a2de-76b85a4ee038
# ╠═a8c01879-6cde-4bba-a717-b8cce343a772
# ╠═5e5d6624-5b69-47d4-b199-b99750ac8124
# ╠═a530c0de-448a-48f5-a205-f4fbf34015b0
# ╠═173b4e60-37b5-46be-b681-7a8f586aa816
# ╠═fd917f69-2886-416f-b603-d374cfdb6e0a
