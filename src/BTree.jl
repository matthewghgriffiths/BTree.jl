module BTree

import Base: insert!, keys, values, pairs, length, isempty, setindex!, eltype
# using DataStructures: MutableLinkedList
import AbstractTrees
using IterTools

# Write your package code here.

export B⁺Tree, B⁺Node

abstract type AbstractBNode{K, V, N} end

abstract type B⁺Node{K, V, N} <: AbstractBNode{K, V, N} end

abstract type AbstractBTree{K, V, N} end

keys(node::AbstractBNode) = node.keys 
values(node::AbstractBNode) = node.values

struct B⁺Internal{K, V, N} <: B⁺Node{K, V, N}
    keys::Vector{K}
    values::Vector{B⁺Node{K, V, N}}
    # B⁺Internal(keys::Vector{V}, nodes::Vector{B⁺Node{K, V, N}}) where {K, V, N}
    #     # nodes = convert(Vector{B⁺Node{K, V, N}}, nodes)
    #     new{K, V, N}(keys, nodes)
    # end
end

function B⁺Internal(keys::Vector, nodes::Vector{<:B⁺Node{K, V, N}}) where {K, V, N}
    keys = convert(Vector{K}, keys)
    nodes = convert(Vector{B⁺Node{K, V, N}}, nodes)
    B⁺Internal(keys, nodes)
end
# B⁺Internal(keys::Vector{V}, nodes::Vector{B⁺Node{K, V, N}}) where {K, V, N} = B⁺Internal{K, V, N}(keys, nodes)
B⁺Internal(nodes::Vector{<:B⁺Node{K, V, N}}) where {K, V, N} = B⁺Internal(first.(keys.(nodes)), nodes)
B⁺Internal{N}(keys::Vector, nodes::Vector{<:B⁺Node{K, V, N}}) where {K, V, N} = B⁺Internal(keys, nodes)


struct B⁺Child{K, V, N} <: B⁺Node{K, V, N}
    keys::Vector{K}
    values::Vector{V}
    B⁺Child{K, V, N}(k::Vector{K}, v::Vector{V}) where {K, V, N} = new{K, V, N}(k, v)
    B⁺Child{N}(k::Vector{K}, v::Vector{V}) where {K, V, N} = new{K, V, N}(k, v)
end
B⁺Child{K, V, N}() where {K, V, N} = B⁺Child{N}(K[], V[])

B⁺Node{K, V, N}() where {K, V, N} = B⁺Child{K, V, N}()
B⁺Node(::Type{K}, ::Type{V}, order::Integer) where {K, V} = B⁺Node{K, V}(order)
B⁺Node{K, V}(order::Integer) where {K, V} = B⁺Child{K, V, order}()

B⁺Node{K, V, N}(keys::Vector{K}, values::Vector{V}) where {K, V, N} = B⁺Child{N}(keys, values)
B⁺Node{N}(keys::Vector{K}, values::Vector{V}) where {K, V, N} = B⁺Child{N}(keys, values)
B⁺Node{N}(keys::Vector{K}, values::Vector{V}) where {K, V <: B⁺Node, N} = B⁺Internal{N}(keys, values)

B⁺Node(keys::Vector{K}, values::Vector{V}, order::Int) where {K, V} = B⁺Node{order}(keys, values) 
# B⁺Node(keys::Vector, nodes::Vector{<:B⁺Node}, ::Int) = B⁺Internal(keys, nodes)

B⁺Node(nodes::Vector{<:B⁺Node}) = B⁺Internal(nodes)
B⁺Node(nodes::Vararg{B⁺Node{K, V, N}, M}) where {K, V, N, M} = B⁺Node(collect(nodes))

# struct B⁺Node{K, V, N} <: AbstractB⁺Node{K, V, N}
#     keys::Vector{K}
#     values::Union{Vector{B⁺Node{K, V, N}}, Vector{V}}
# end
# B⁺Node(pair::Pair) = B⁺Node(pair...)
# B⁺Node(left::B⁺Node, ::Nothing) = left
# function B⁺Node(left::B⁺Node{K, V, N}, right::B⁺Node{K, V, N}) where {K, V, N}
#     B⁺Node{K, V, N}(
#         [first(keys(left)), first(keys(right))], 
#         [left, right]
#     )
# end

Base.sizehint!(node::AbstractBNode) = begin
    sizehint!(keys(node))
    sizehint!(values(node))
end

isinternal(::Any) = false
ischild(::Any) = false
isinternal(node::B⁺Internal) = true
isinternal(node::B⁺Child) = false
ischild(node::B⁺Internal) = false
ischild(node::B⁺Child) = true
# isinternal(node::B⁺Node) = eltype(node) <: B⁺Node
# ischild(node::B⁺Node) = !isinternal(node)

AbstractTrees.children(node::AbstractBNode) = ischild(node) ? () : values(node)
AbstractTrees.nodevalue(node::AbstractBNode{K, V, N}) where {K, V, N} = ischild(node) ? collect(pairs(node)) : keys(node)
# AbstractTrees.NodeType(::Type{<:AbstractBNode}) = AbstractTrees.HasNodeType()
# AbstractTrees.nodetype(::Type{<:AbstractBNode}) = AbstractBNode
# AbstractTrees.NodeType(::Type{<:B⁺Node}) = AbstractTrees.HasNodeType()
AbstractTrees.nodetype(::Type{<:B⁺Node{K, V, N}}) where {K, V, N} = B⁺Node{K, V, N}

order(::B⁺Node{K, V, N}) where {K, V, N} = N
# eltype(::B⁺Node{K, V, N}) where {K, V, N} = V

for f in [:length, :isempty, :iterate, :eltype]
    @eval $f(node::B⁺Node) = $f(values(node))
end

function insert!(node::AbstractBNode, i, key, value)
    insert!(keys(node), i, key)
    insert!(values(node), i, value)
    return node 
end

mutable struct B⁺Tree{K, V, N}
    root::B⁺Node{K, V, N}
    # leaves::MutableLinkedList{B⁺Node{K, V, N}}
end

function B⁺Tree{K, V}(order::Integer) where {K, V} 
    root = B⁺Node{K, V, order}(K[], V[])
    # leaves = MutableLinkedList{B⁺Node{K, V, order}}(root)
    B⁺Tree{K, V, order}(root)
end
order(::B⁺Tree{K, V, N}) where {K, V, N} = N
length(tree::B⁺Tree) = sum(length, leaves(tree))

leaves(tree::B⁺Tree) = AbstractTrees.Leaves(tree.root)
Base.keys(tree::B⁺Tree)   = Iterators.flatten(keys.(leaves(tree)))
Base.values(tree::B⁺Tree) = Iterators.flatten(values.(leaves(tree)))
Base.pairs(tree::B⁺Tree)  = Iterators.flatten(pairs.(leaves(tree)))

function update(node::B⁺Node, value, key)
    ks = keys(node)
    vs = values(node)
    if !ischild(node)
        i = searchsortedlast(ks, key)
        i0 = firstindex(ks)
        if i < i0
            ks[i0] = key 
            i = i0
        end 
        left, nodes... = update(vs[i], value, key)
        vs[i] = left
        for (j, next) in enumerate(nodes)
            insert!(node, i + j, first(keys(next)),  next)
        end
    else
        i = searchsortedfirst(ks, key)
        if (isempty(ks) || i > lastindex(ks) || ks[i] != key)
            insert!(node, i, key, value)
        else
            vs[i] = value 
        end
    end
    split(node)
end

function split(node::B⁺Node{K, V, N}, minsize::Integer, maxsize::Integer) where {K, V, N}
    ks = keys(node)
    vs = values(node)
    inds = eachindex(ks)
    stop_index = lastindex(inds)
    stop = stop_index - maxsize + 1
    i = 1 
    nodes = B⁺Node{K, V, N}[]
    while i < stop
        sel = i:i + minsize - 1
        push!(nodes, B⁺Node{N}(ks[sel], vs[sel]))
        i += minsize
    end
    if i < stop_index
        sel = i:stop_index
        push!(nodes, B⁺Node{N}(ks[sel], vs[sel]))
    end
    return nodes 
end

function split(node::B⁺Node{K, V, N}) where {K, V, N}
    length(node) > order(node) || return B⁺Node{K, V, N}[node]
    split(node, cld(N, 2), N)
end

function setindex!(tree::B⁺Tree, value, key)
    nodes = update(tree.root, value, key)
    if length(nodes) == 1
        tree.root = nodes[1]
    else
        tree.root = B⁺Node(nodes)
    end
    tree
end

function test_b⁺tree_key_order(tree)
    leaf_keys = keys(tree) |> collect
    @assert all(leaf_keys[2:end] .> leaf_keys[1:end-1])
end

function test_b⁺tree(tree, data)
    test_b⁺tree_key_order(tree)
    @assert all((data[k] == v) for (k, v) in pairs(tree))
    @assert length(data) == length(tree)
    true
end

end