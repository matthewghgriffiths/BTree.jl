

abstract type abstractB⁺Node{K, V, N} <: AbstractBNode{K, V, N} end
abstract type abstractB⁺Tree{K, V, N} <: AbstractBTree{K, V, N} end

struct B⁺Child{K, V, N} <: abstractB⁺Node{K, V, N}
    keys::Vector{K}
    values::Vector{V}
    B⁺Child{K, V, N}(k::Vector{K}, v::Vector{V}) where {K, V, N} = new{K, V, N}(k, v)
    B⁺Child{N}(k::Vector{K}, v::Vector{V}) where {K, V, N} = new{K, V, N}(k, v)
end
B⁺Child{K, V, N}() where {K, V, N} = B⁺Child{N}(K[], V[])

struct B⁺Internal{K, V, N} <: abstractB⁺Node{K, V, N}
    keys::Vector{K}
    values::Vector{Union{B⁺Internal{K, V, N}, B⁺Child{K, V, N}}}
    B⁺Internal{K, V, N}(keys::Vector{V}, nodes::Vector{Union{B⁺Internal{K, V, N}, B⁺Child{K, V, N}}}) where 
        {K, V, N} = sizehint!(new{K, V, N}(keys, nodes), N + 1)
end

const B⁺Node{K, V, N} = Union{B⁺Internal{K, V, N}, B⁺Child{K, V, N}}

function B⁺Internal(keys::Vector, nodes::Vector{<:B⁺Node{K, V, N}}) where {K, V, N}
    keys = convert(Vector{K}, keys)
    nodes = convert(Vector{B⁺Node{K, V, N}}, nodes)
    B⁺Internal{K, V, N}(keys, nodes)
end
B⁺Internal(nodes::Vector{<:B⁺Node}) = B⁺Internal(first.(keys.(nodes)), nodes)
B⁺Internal{N}(keys::Vector, nodes::Vector{<:B⁺Node{K, V, N}}) where {K, V, N} = B⁺Internal(keys, nodes)


B⁺Node{K, V, N}() where {K, V, N} = B⁺Child{K, V, N}()
B⁺Node(::Type{K}, ::Type{V}, order::Integer) where {K, V} = B⁺Node{K, V, order}()
B⁺Node{K, V}(order::Integer) where {K, V} = B⁺Child{K, V, order}()

B⁺Node(keys::Vector{K}, values::Vector{V}, order::Int) where {K, V} = B⁺Node{order}(keys, values) 

B⁺Node{K, V, N}(keys::Vector{K}, values::Vector{V}) where {K, V, N} = B⁺Child{N}(keys, values)
B⁺Node{N}(keys::Vector{K}, values::Vector{V}) where {K, V, N} = B⁺Child{N}(keys, values)

B⁺Node{K, V, N}(keys::Vector{K}, values::Vector{<:B⁺Node{K, V, N}}) where {K, V, N} = B⁺Internal{N}(keys, values)
B⁺Node{N}(keys::Vector{K}, values::Vector{V}) where {K, V <: B⁺Node, N} = B⁺Internal{N}(keys, values)
B⁺Node(nodes::Vector{<:B⁺Node}) = B⁺Internal(nodes)
B⁺Node(nodes::Vararg{B⁺Node{K, V, N}, M}) where {K, V, N, M} = B⁺Node(collect(nodes))


isinternal(node::B⁺Internal) = true
isinternal(node::B⁺Child) = false
ischild(node::B⁺Internal) = false
ischild(node::B⁺Child) = true

# eltype(::B⁺Node{K, V, N}) where {K, V, N} = V
# AbstractTrees.NodeType(::Type{<:AbstractBNode}) = AbstractTrees.HasNodeType()
# AbstractTrees.nodetype(::Type{<:AbstractBNode}) = AbstractBNode
# AbstractTrees.NodeType(::Type{<:B⁺Node}) = AbstractTrees.HasNodeType()
AbstractTrees.nodetype(::Type{<:B⁺Node{K, V, N}}) where {K, V, N} = B⁺Node{K, V, N}

for f in [:length, :isempty, :iterate, :eltype]
    @eval $f(node::B⁺Node) = $f(values(node))
end


mutable struct B⁺Tree{K, V, N} <: abstractB⁺Tree{K, V, N}
    root::B⁺Node{K, V, N}
    # leaves::MutableLinkedList{B⁺Node{K, V, N}}
end

function B⁺Tree{K, V}(order::Integer) where {K, V} 
    root = B⁺Node{K, V, order}(K[], V[])
    # leaves = MutableLinkedList{B⁺Node{K, V, order}}(root)
    B⁺Tree{K, V, order}(root)
end
length(tree::B⁺Tree) = sum(length, leaves(tree))

leaves(tree::B⁺Tree) = AbstractTrees.Leaves(tree.root)
Base.keys(tree::B⁺Tree)   = Iterators.flatten(keys.(leaves(tree)))
Base.values(tree::B⁺Tree) = Iterators.flatten(values.(leaves(tree)))
Base.pairs(tree::B⁺Tree)  = Iterators.flatten(pairs.(leaves(tree)))

function setindex!(node::B⁺Internal{K, V, N}, value, key) where {K, V, N} 
    ks = keys(node)
    nodes = values(node)
    i = index(ks, key)
    (key < ks[i]) && (ks[i] = key)
    nodes[i], children... = split(setindex!(nodes[i], value, key))
    for (j, childⱼ) in enumerate(children)
        insert!(node, i + j, childⱼ, first(keys(childⱼ)))
    end
    node
end

function setindex!(node::B⁺Child{K, V, N}, value, key) where {K, V, N} 
    isempty(node) && return push!(node, value, key)
    ks = keys(node)
    vs = values(node)
    i = index(ks, key)
    k = ks[i]
    if k == key 
        vs[i] = value 
    else
        insert!(node, i + (k < key), value, key)
    end
    node
end

function split(node::B⁺Node{K, V, N}, splitsize::Integer, maxsize::Integer) where {K, V, N}
    ks = keys(node)
    vs = values(node)
    inds = eachindex(ks)
    stop_index = lastindex(inds)
    stop = stop_index - maxsize + 1
    nsplits = cld(length(node), minsize(node)) - 1
    i = 1 
    nodes = B⁺Node{K, V, N}[]
    sizehint!(node, nsplits)
    while i < stop
        sel = i:i + splitsize - 1
        push!(nodes, node[sel])
        i += splitsize
    end
    if i < stop_index
        sel = i:stop_index
        push!(nodes, node[sel])
    end
    return nodes 
end

function split(node::B⁺Node{K, V, N}) where {K, V, N}
    isover(node) || return B⁺Node{K, V, N}[node]
    split(node, minsize(node), order(node))
end

function setindex!(tree::B⁺Tree, value, key)
    node = setindex!(tree.root, value, key)
    nodes = split(node)
    if length(nodes) == 1
        tree.root, = nodes
    else
        tree.root = B⁺Node(nodes)
    end
    tree
end

function _search_nodes(key, node::AbstractBNode{K, V, N}) where {K, V, N}
    nodes = ()
    next_key = key 
    while !isa(node, V)
        nodes = (nodes..., node)
        ks = keys(node)
        i = index(ks, key)
        next_key = ks[i]
        node = values(node)[i]
    end
    (next_key => node), nodes
    # isa(next, V) ? ((keys(node)[i] => next), nodes) : search_nodes(key, nodes..., next)
end

function search_nodes(key, nodes::Vararg{AbstractBNode{K, V, N}, M}) where {K, V, N, M}
    node = last(nodes)
    ks = keys(node)
    i = index(ks, key)
    next = values(node)[i]
    if isa(next, V)
        val_key = ks[i]
        return (val_key => next, nodes)
    else
        return search_nodes(key, nodes..., next)
    end
    # isa(next, V) ? ((keys(node)[i] => next), nodes) : search_nodes(key, nodes..., next)
end

function search(node::AbstractBNode{K, V, N}, key) where {K, V, N}
    ks = keys(node)
    i = index(ks, key)
    next = values(node)[i]
    isa(next, V) ? (ks[i] => next) : search(next, key)
end

# function search(node::AbstractBNode, key)
#     ks = keys(node)
#     i = searchsortedlast(@view(ks[2:end]), key) + 1
#     i, values(node)[i]
# end

function Base.getindex(node::AbstractBNode, key)
    i = index(node, key)
    keys(node)[i] == key || throw(KeyError(key))
    values(node)[i]
end

# function drop(node::AbstractBNode{K, V, N}, key)::Pair{K, V} where {K, V, N}
#     k, next = search(node, key)
#     isa(next, V) ? (k => next) : drop(next, key)
# end

function Base.getindex(tree::B⁺Tree{K, V, N}, key) where {K, V, N}
    k, val = search(tree.root, key)
    k == key || throw(KeyError(key))
    val 
end

function Base.get(tree::B⁺Tree{K, V, N}, key, default) where {K, V, N}
    k, val = search(tree.root, key)
    k == key ? val : default
end

Base.get(tree::B⁺Tree{K, V, N}, key)  where {K, V, N} = get(tree, key, nothing)

function test_b⁺tree_key_order(tree)
    leaf_keys = keys(tree) |> collect
    @assert all(leaf_keys[2:end] .> leaf_keys[1:end-1])
end

function test_b⁺tree(tree, data)
    test_b⁺tree_key_order(tree)
    @assert all((data[k] == v) for (k, v) in pairs(tree))
    @assert all((tree[k] == v) for (k, v) in pairs(data))
    @assert length(data) == length(tree)

    root, nodes... = AbstractTrees.PreOrderDFS(tree.root)
    @assert all(isvalid, nodes)
    true
end