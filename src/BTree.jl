module BTree

import Base: length, insert!, keys, values
using DataStructures: MutableLinkedList

# Write your package code here.

# export B⁺tree, B⁺node, order

abstract type AbstractBNode{K, V, N} end

keys(node::AbstractBNode) = node.keys 
values(node::AbstractBNode) = node.values

struct B⁺Node{K, V, N} <: AbstractBNode{K, V, N}
    keys::Vector{K}
    values::Union{Vector{B⁺Node{K, V, N}}, Vector{V}}
end

B⁺Node{K, V}(order::Integer) where {K, V} = B⁺Node{K, V, order}(K[], V[]) 

struct B⁺Tree{K, V, N}
    root::B⁺Node{K, V, N}
    leaves::MutableLinkedList{B⁺Node{K, V, N}}
end

function B⁺Tree{K, V}(order::Integer) where {K, V} 
    root = B⁺Node{K, V, order}(K[], V[])
    leaves = MutableLinkedList{B⁺Node{K, V, order}}(root)
    B⁺Tree{K, V, order}(root, leaves)
end

order(::B⁺Node{K, V, N}) where {K, V, N} = N
order(::B⁺Tree{K, V, N}) where {K, V, N} = N
length(node::B⁺Node) = length(node.values)
length(tree::B⁺Tree) = sum(length, tree.leaves)

function insert!(node::AbstractBNode{K, V, N}, key::K, value::V) where {K, V, N}
    ks = keys(node)
    vs = values(node)
    if length(node) == 0
        push!(ks, key)
        push!(vs, value)
    else

        for i in eachindex(ks)
            ki = ks[i]
            if key == ki
                vs[i] = value 
                @goto cleanup 
            elseif key < ki 
                insert!(ks, i, key)
                insert!(vs, i, value)
                @goto cleanup 
            end
        end
        push!(ks, key)
        push!(vs, value)
        @label cleanup
    end

    node
end

function split(node::B⁺Node{K, V, N}) where {K, V, N}
    mid = fld(N, 2) 

    ks = keys(node)
    vs = values(node)
    inds = eachindex(ks)
    left = inds[1:mid]
    right = inds[mid + 1:end]

    left_node = B⁺Node{K, V, N}(ks[left], vs[left])
    right_node = B⁺Node{K, V, N}(ks[right], vs[right])
    parent_keys = K[ks[left[1]], ks[right[1]]]
    parent_values = B⁺Node{K, V, N}[left_node, right_node]
    
    B⁺Node{K, V, N}(parent_keys, parent_values)
end

end