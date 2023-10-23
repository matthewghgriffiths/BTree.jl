module BTree

import Base: insert!, keys, values, length, isempty, setindex!, eltype
# using DataStructures: MutableLinkedList
import AbstractTrees

# Write your package code here.

export B⁺Tree, B⁺Node

abstract type AbstractBNode{K, V, N} end

abstract type AbstractB⁺Node{K, V, N} <: AbstractBNode{K, V, N} end

abstract type AbstractBTree{K, V, N} end

keys(node::AbstractBNode) = node.keys 
values(node::AbstractBNode) = node.values

mutable struct B⁺Node{K, V, N} <: AbstractB⁺Node{K, V, N}
    keys::Vector{K}
    values::Union{Vector{B⁺Node{K, V, N}}, Vector{V}}
end

B⁺Node{K, V}(order::Integer) where {K, V} = B⁺Node{K, V, order}(K[], V[]) 
B⁺Node(pair::Pair) = B⁺Node(pair...)
B⁺Node(left::B⁺Node, right::Nothing) = left 
function B⁺Node(left::B⁺Node{K, V, N}, right::B⁺Node{K, V, N}) where {K, V, N}
    B⁺Node{K, V, N}(
        [first(keys(left)), first(keys(right))], 
        [left, right]
    )
end

Base.sizehint!(node::AbstractBNode) = begin
    sizehint!(keys(node))
    sizehint!(values(node))
end

isinternal(::Any) = false
ischild(::Any) = false
isinternal(node::AbstractB⁺Node) = eltype(node) <: AbstractB⁺Node
ischild(node::AbstractB⁺Node) = !isinternal(node)

AbstractTrees.children(node::AbstractBNode) = ischild(node) ? () : values(node)
AbstractTrees.nodevalue(node::AbstractBNode{K, V, N}) where {K, V, N} = ischild(node) ? collect(pairs(node)) : keys(node)

order(::B⁺Node{K, V, N}) where {K, V, N} = N

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
length(tree::B⁺Tree) = sum(length, tree.leaves)

function update(node::AbstractB⁺Node, value, key)
    ks = keys(node)
    vs = values(node)
    if !ischild(node)
        i = searchsortedlast(ks, key)
        i0 = firstindex(ks)
        if i < i0
            ks[i0] = key 
            i = i0
        end 
        left, right = update(vs[i], value, key)
        vs[i] = left
        isnothing(right) || insert!(node, i + 1, first(keys(right)),  right)
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

function split(node::B⁺Node{K, V, N}, mid::Integer) where {K, V, N}
    ks = keys(node)
    vs = values(node)
    inds = eachindex(ks)
    left = inds[1:mid]
    right = inds[mid + 1:end]
    left_node = B⁺Node{K, V, N}(ks[left], vs[left])
    right_node = B⁺Node{K, V, N}(ks[right], vs[right])
    Pair(left_node, right_node)
end

function split(node::B⁺Node{K, V, N}) where {K, V, N}
    length(node) > order(node) || return Pair(node, nothing)
    split(node, cld(N, 2))
end

function setindex!(tree::B⁺Tree, value, key)
    tree.root = B⁺Node(update(tree.root, value, key))
    tree
end

end