

abstract type abstractB⁺Node{K, V, N} <: AbstractBNode{K, V, N} end
abstract type abstractB⁺Tree{K, V, N} <: AbstractBTree{K, V, N} end

struct B⁺Child{K, V, N} <: abstractB⁺Node{K, V, N}
    keys::Vector{K}
    values::Vector{V}
    B⁺Child{K, V, N}(k::Vector{K}, v::Vector{V}) where {K, V, N} = new{K, V, N}(k, v)
    B⁺Child{N}(k::Vector{K}, v::Vector{V}) where {K, V, N} = new{K, V, N}(k, v)
end
B⁺Child{K, V, N}() where {K, V, N} = B⁺Child{N}(K[], V[])

function B⁺Child{K, V, N}(k::AbstractVector{K}, v::AbstractVector{V}) where {K, V, N}
    k = convert(Vector{K}, k)
    v = convert(Vector{V}, v)
    B⁺Child{K, V, N}(k, v)
end

struct B⁺Internal{K, V, N} <: abstractB⁺Node{K, V, N}
    keys::Vector{K}
    values::Union{Vector{B⁺Internal{K, V, N}}, Vector{B⁺Child{K, V, N}}}
    B⁺Internal{K, V, N}(keys::Vector{V}, nodes::Union{Vector{B⁺Internal{K, V, N}}, Vector{B⁺Child{K, V, N}}}) where 
        {K, V, N} = sizehint!(new{K, V, N}(keys, nodes), N + 1)
end

const B⁺Node{K, V, N} = Union{B⁺Internal{K, V, N}, B⁺Child{K, V, N}}
const B⁺Nodes{K, V, N} = Union{AbstractVector{B⁺Internal{K, V, N}}, AbstractVector{B⁺Child{K, V, N}}, AbstractVector{<:B⁺Node{K, V, N}}}

function B⁺Internal(keys::AbstractVector, nodes::B⁺Nodes{K, V, N}) where {K, V, N}
    keys = convert(Vector{K}, keys)
    nodes = convert(Vector{typeof(first(nodes))}, nodes)
    B⁺Internal{K, V, N}(keys, nodes)
end
B⁺Internal(nodes::AbstractVector{<:B⁺Node}) = B⁺Internal(first.(keys.(nodes)), nodes)
B⁺Internal{N}(keys::AbstractVector, nodes::B⁺Nodes{K, V, N}) where {K, V, N} = B⁺Internal(keys, nodes)


B⁺Node{K, V, N}() where {K, V, N} = B⁺Child{K, V, N}()
B⁺Node(::Type{K}, ::Type{V}, order::Integer) where {K, V} = B⁺Node{K, V, order}()
B⁺Node{K, V}(order::Integer) where {K, V} = B⁺Child{K, V, order}()

B⁺Node(keys::Vector{K}, values::Vector{V}, order::Int) where {K, V} = B⁺Node{order}(keys, values) 
B⁺Node{K, V, N}(keys::Vector{K}, values::Vector{V}) where {K, V, N} = B⁺Child{N}(keys, values)
B⁺Node{N}(keys::Vector{K}, values::Vector{V}) where {K, V, N} = B⁺Child{N}(keys, values)
B⁺Node{K, V, N}(keys::AbstractVector{K}, values::AbstractVector{<:B⁺Node{K, V, N}}) where {K, V, N} = B⁺Internal{N}(keys, values)
B⁺Node{N}(keys::AbstractVector{K}, values::AbstractVector{V}) where {K, V <: B⁺Node, N} = B⁺Internal{N}(keys, values)
B⁺Node(nodes::AbstractVector{<:B⁺Node}) = B⁺Internal(nodes)
B⁺Node(nodes::Vararg{B⁺Node{K, V, N}, M}) where {K, V, N, M} = B⁺Node(collect(nodes))


isinternal(node::B⁺Internal) = true
isinternal(node::B⁺Child) = false
ischild(node::B⁺Internal) = false
ischild(node::B⁺Child) = true

# eltype(::B⁺Node{K, V, N}) where {K, V, N} = V
# AbstractTrees.NodeType(::Type{<:AbstractBNode}) = AbstractTrees.HasNodeType()
# AbstractTrees.nodetype(::Type{<:AbstractBNode}) = AbstractBNode
# AbstractTrees.NodeType(::Type{<:B⁺Node}) = AbstractTrees.HasNodeType()
AbstractTrees.childrentype(::Type{<:AbstractBNode{K, V, N}}) where {K, V, N} = Vector{<:Union{AbstractBNode{K, V, N}, V}}
AbstractTrees.childrentype(::Type{<:B⁺Internal{K, V, N}}) where {K, V, N} = Vector{B⁺Node{K, V, N}}
AbstractTrees.nodetype(::Type{<:B⁺Node{K, V, N}}) where {K, V, N} = B⁺Node{K, V, N}

for f in [:length, :isempty, :iterate, :eltype]
    @eval $f(node::B⁺Node) = $f(values(node))
end


mutable struct B⁺Tree{K, V, N} <: abstractB⁺Tree{K, V, N}
    root::B⁺Node{K, V, N}
    # leaves::MutableLinkedList{B⁺Node{K, V, N}}
end

B⁺Tree{K, V}(order::Integer) where {K, V} = B⁺Tree{K, V, order}()
function B⁺Tree{K, V, N}() where {K, V, N} 
    root = B⁺Node{K, V, N}(K[], V[])
    # leaves = MutableLinkedList{B⁺Node{K, V, order}}(root)
    B⁺Tree{K, V, N}(root)
end

function B⁺Tree{N}(data::AbstractVector{Pair{K, V}}) where {K, V, N}
    tree = B⁺Tree{K, V, N}()
    for (k, v) in data
        tree[k] = v 
    end
    tree 
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

function split(node::B, splitsize::Integer) where {B <: B⁺Node}
    i0 = firstindex(keys(node))
    iend = lastindex(keys(node))
    nsplits = div(iend - i0 + 1, splitsize) - 1
    nodes = Vector{B}(undef, nsplits)

    i1 = i0 + splitsize - 1
    left = indexat(node, i0:i1)
    for j in 1:nsplits - 1
        shift = j * splitsize
        nodes[j] = indexat(node, i0 + shift: i1 + shift)
    end
    nodes[nsplits] = indexat(node, i0 + nsplits * splitsize:iend)
    return left, nodes 
end

function split(node::B) where {B <: B⁺Node}
    isover(node) || return node, B[]
    split(node, minsize(node))
end

function setindex!(tree::B, value, key) where B <: abstractB⁺Tree
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

function index(node::B⁺Child, start, stop)
    i0, k0 = indexkey(node, start)
    i1 = index(node, stop)
    return i0 + (start > k0):i1 - (stop < k0)
end
function index(node::B⁺Child, start, ::Nothing)
    i0, k0 = indexkey(node, start)
    return i0 + (start > k0):lastindex(node)
end
function index(node::B⁺Child, ::Nothing, stop)
    i1 = index(node, stop)
    return firstindex(node):i1
end
index(::B⁺Child, ::Nothing, ::Nothing) = Colon()

Base.getindex(node::B⁺Child, range::UnitRange) = node[index(node, range)]
Base.getindex(node::B⁺Child, start, stop) = values(node)[index(node, start, stop)]
Base.view(node::B⁺Child, range::UnitRange) = view(node, first(range), last(range))
Base.view(node::B⁺Child, start, stop) = view(values(node), index(node, start, stop))
Base.view(node::B⁺Child, ::Colon) = values(node)

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
    isempty(tree) && return tree 

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


struct ChainedIterables{I<:NTuple}
    iterables::I
end

ChainedIterables(iterables...) = ChainedIterables(iterables)





slice(f::Function, node::B⁺Child, start, stop) = Ref(@view f(node)[index(node, start, stop)])
function slice(f::Function, node, start, stop)
    children = @view values(node)[index(node, start, stop)]
    i0, i1 = firstindex(children), lastindex(children)
    [
        child2 for (i, child) in pairs(children) 
        for child2 in slice(f, child, i==i0 ? start : nothing, i==i1 ? stop : nothing)
    ]
end
slice(node, start, stop) = slice(values, node, start, stop)


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