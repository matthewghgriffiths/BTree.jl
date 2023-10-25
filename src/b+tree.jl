

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
    i, k = indexkey(node, key)
    (key < k) && (ks[i] = key)
    nodes = values(node)
    child = setindex!(nodes[i], value, key)
    nodes[i], children = split(child)
    for (j, childⱼ) in enumerate(children)
        insert!(node, i + j, childⱼ, first(keys(childⱼ)))
    end
    node
end

function setindex!(node::B⁺Child{K, V, N}, value, key) where {K, V, N} 
    isempty(node) && return push!(node, value, key)
    vs = values(node)
    i, k = indexkey(node, key)
    if k == key 
        vs[i] = value 
    else
        insert!(node, i + (k < key), value, key)
    end
    node
end

function split(node::B, splitsize::Integer, maxsize::Integer) where {B <: B⁺Node}
    stop_index = lastindex(keys(node))
    stop = stop_index - maxsize + 1
    nsplits = fld(length(node), minsize(node))
    # nodes = B[]
    # sizehint!(node, nsplits)
    nodes = Vector{B}(undef, nsplits - 1)

    i = firstindex(keys(node))
    left = node[i:i + splitsize - 1]
    i += splitsize
    j = 1
    while i < stop
        nodes[j] = node[i:i + splitsize - 1]
        i += splitsize
        j += 1
    end
    if i < stop_index
        nodes[j] = node[i:stop_index]
    end
    return left, nodes 
end

function split(node::B) where {B <: B⁺Node}
    # isover(node) || return B⁺Node{K, V, N}[node]
    isover(node) || return node, B[]
    split(node, minsize(node), order(node))
end

function setindex!(tree::B⁺Tree, value, key)
    node= setindex!(tree.root, value, key)
    node, children = split(node)
    if isempty(children)
        tree.root = node
    else
        tree.root = B⁺Node(vcat(node, children))
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

function Base.getindex(node::AbstractBNode, key)
    i = index(node, key)
    keys(node)[i] == key || throw(KeyError(key))
    values(node)[i]
end

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

function Base.delete!(node::B⁺Internal{K, V, N}, key) where {K, V, N}
    ks = keys(node)
    i, k = indexkey(node, key)
    (key < k) && (ks[i] = key)
    nodes = values(node)
    child = delete!(nodes[i], key)
    (isvalid(child) || (length(node) == 1)) && return node

    if (i == lastindex(ks))
        if merge!(nodes[i - 1], child)
            deleteat!(node, i)
        else
            ks[i] = first(keys(child))
        end
    else
        if merge!(child, nodes[i + 1])
            deleteat!(node, i + 1)
        else
            ks[i + 1] = first(keys(nodes[i + 1]))
        end
        ks[i] = first(keys(child))
    end
    node 
end

function Base.delete!(node::B⁺Child{K, V, N}, key) where {K, V, N} 
    i, k = indexkey(node, key)
    k == key && deleteat!(node, i)
    node
end

function Base.delete!(tree::B⁺Tree{K, V, N}, key) where {K, V, N}
    root = delete!(tree.root, key)
    if length(root) == 1
        child, = values(root)
        isa(child, B⁺Node) && (tree.root = child)
    end
    tree 
end

function merge!(left, right)
    left_diff = length(left) - minsize(left)
    right_diff = length(right) - minsize(right)
    if (left_diff == -1) && (right_diff > 0)
        k, v = splice!(right, firstindex(keys(right)))
        push!(left, v, k)
        return false
    elseif (right_diff == -1) && (left_diff > 0)
        k, v = splice!(left, lastindex(keys(left)))
        insert!(right, firstindex(keys(right)), v, k)
        return false
    end
    append!(left, right)
    return true
end 

function test_b⁺tree(tree)
    leaf_keys = keys(tree) |> collect
    @assert all(leaf_keys[2:end] .> leaf_keys[1:end-1])
end

function test_b⁺tree(tree, data)
    test_b⁺tree(tree)
    root, children... = AbstractTrees.PreOrderDFS(tree.root)
    @assert all(isvalid, children)
    @assert order(root) >= length(root) >= min(2, length(data))


    @assert all((data[k] == v) for (k, v) in pairs(tree))
    @assert all((tree[k] == v) for (k, v) in pairs(data))
    @assert length(data) == length(tree)
    true
end