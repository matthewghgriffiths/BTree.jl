
abstract type AbstractBNode{K,V,N,W} end

@inline order(::AbstractBNode{K,V,N,W}) where {K,V,N,W} = N
@inline minsize(::AbstractBNode{K,V,N,W}) where {K,V,N,W} = cld(N, 2)
@inline isfull(node::AbstractBNode) = length(node) == order(node)
@inline isover(node::AbstractBNode) = length(node) > order(node)
@inline isunder(node::AbstractBNode) = length(node) < minsize(node)
@inline isvalid(node::AbstractBNode) = !isover(node) && !isunder(node) && issorted(node)

Base.keys(node::AbstractBNode) = node.keys
Base.values(node::AbstractBNode) = node.values
Base.issorted(node::AbstractBNode) = issorted(keys(node))
Base.firstindex(node::AbstractBNode) = firstindex(keys(node))
Base.lastindex(node::AbstractBNode) = lastindex(keys(node))

Base.length(node::AbstractBNode) = length(values(node))
Base.isempty(node::AbstractBNode) = isempty(values(node))


Base.eltype(::AbstractBNode{K,V,N,W}) where {K,V,N,W} = Pair{K,W}
Base.pairs(node::AbstractBNode) = Iterators.map(Pair, keys(node), values(node))
Base.first(node::AbstractBNode) = Pair(first(keys(node)), first(values(node)))
Base.last(node::AbstractBNode) = Pair(last(keys(node)), last(values(node)))
function Base.iterate(node::AbstractBNode)
    kiter = iterate(keys(node))
    isnothing(kiter) && return
    viter = iterate(values(node))
    Pair.(kiter, viter)
end
function Base.iterate(node::AbstractBNode, (kstate, vstate))
    kiter = iterate(keys(node), kstate)
    isnothing(kiter) && return
    viter = iterate(values(node), vstate)
    Pair.(kiter, viter)
end

nodekey(node::AbstractBNode) = first(keys(node))

ChildType(node::AbstractBNode) = ChildType(typeof(node))
ChildType(::Type{<:AbstractBNode{K,V,N,W}}) where {K,V,N,W} = W
ChildType(node, D) = ChildType(typeof(node), D)
ChildType(::Type{T}, ::Val{1}) where {K,V,N,W,T<:AbstractBNode{K,V,N,W}} = ChildType(T)
ChildType(::Type{<:AbstractBNode{K,V,N,W}}, ::Val{D}) where {K,V,N,W,D} =
    D > 0 ? ChildType(W, Val(D - 1)) : ChildType(W, Val(depth(W) + D))
ChildType(::Type{T}, D::Int) where {K,V,N,W,T<:AbstractBNode{K,V,N,W}} = ChildType(T, Val(D))

@inline index(node::AbstractBNode, range::UnitRange) = index(keys(node), first(range), last(range))
@inline index(node::AbstractBNode, ks...) = index(keys(node), ks...)
@inline index(ks::AbstractVector, range::AbstractRange) = index(ks, first(range), last(range))
@inline index(ks::AbstractVector, key1, key2) = index(ks, key1):index(ks, key2)
@inline index(ks::AbstractVector, key1, ::Nothing) = index(ks, key1):lastindex(ks)
@inline index(ks::AbstractVector, ::Nothing, key2) = firstindex(ks):index(ks, key2)
@inline index(::AbstractVector, ::Nothing, ::Nothing) = Colon()
@inline index(ks::AbstractVector, key) = max(searchsortedlast(ks, key), 1)

@inline indexkey(node::AbstractBNode, ks...) = indexkey(keys(node), ks...)
@inline function indexkey(ks::AbstractVector, key)
    i = index(ks, key)
    i => ks[i]
end

@inline indexvalue(node::AbstractBNode, ks...) = indexvalue(keys(node), values(node), ks...)
@inline function indexvalue(ks::AbstractVector, vs::AbstractVector, key)
    i, k = indexkey(ks, key)
    v = vs[i]
    k => v
end

function search(node::AbstractBNode{K,V,N,B}, key)::Pair{K,V} where {K,V,N,B}
    ks = keys(node)
    i = index(ks, key)
    next = values(node)[i]
    isa(next, V) ? (ks[i] => next) : search(next, key)
end

search_(node, key) = search(node, key)

Base.getindex(node::AbstractBNode, range::UnitRange) = getindex(node, first(range), last(range))
function Base.getindex(node::AbstractBNode, key)
    k, val = search(node, key)
    k == key || throw(KeyError(key))
    val
end
# Base.getindex(node::AbstractBNode, range::UnitRange) = node[index(node, range)]
Base.getindex(node::AbstractBNode, start, stop) = values(node)[index(node, start, stop)]
Base.view(node::AbstractBNode, inds...) = view(values(node), index(node, inds...))
Base.view(node::AbstractBNode, range::UnitRange) = view(node, first(range), last(range))
Base.view(node::AbstractBNode, ::Colon) = values(node)

function Base.push!(node::AbstractBNode, value, key)
    push!(keys(node), key)
    push!(values(node), value)
    return node
end

function insert!(node::AbstractBNode, i, value, key)
    insert!(keys(node), i, key)
    insert!(values(node), i, value)
    return node
end

AbstractTrees.children(node::AbstractBNode) = ischild(node) ? () : values(node)
AbstractTrees.nodevalue(node::AbstractBNode) = nodekey(node) # ischild(node) ? collect(pairs(node)) : keys(node)
AbstractTrees.childrentype(::Type{<:AbstractBNode{K,V,N,B}}) where {K,V,N,B} = Vector{B}
AbstractTrees.childtype(::Type{<:AbstractBNode{K,V,N,B}}) where {K,V,N,B} = B
# AbstractTrees.nodetype(::Type{B}) where {K,V,N, B <: AbstractBNode{K,V,N}} = K
# AbstractTrees.NodeType(::Type{<:AbstractBNode}) = AbstractTrees.HasNodeType()


function Base.sizehint!(node::AbstractBNode, sz::Integer)
    sizehint!(keys(node), sz)
    sizehint!(values(node), sz)
    node
end

indexat(node::B, ind) where {B<:AbstractBNode} = keys(node)[ind] => values(node)[ind]
indexat(node::B, slice::UnitRange{<:Integer}) where {B<:AbstractBNode} = B(keys(node)[slice], values(node)[slice])

getnested(node) = node
getnested(node::AbstractBNode, i, inds::Vararg{Any, N}) where N = getnested(values(node)[i], inds...)

function Base.deleteat!(node::B, i) where {B<:AbstractBNode}
    deleteat!(keys(node), i)
    deleteat!(values(node), i)
end

function Base.append!(node::B, ks, vs) where {B<:AbstractBNode}
    append!(keys(node), ks)
    append!(values(node), vs)
    node
end

Base.append!(left::B, right::B) where {B<:AbstractBNode} = append!(left, keys(right), values(right))
Base.splice!(node::AbstractBNode, i) = splice!(keys(node), i) => splice!(values(node), i)

function mergenodes!(left::AbstractBNode, right::AbstractBNode)
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


@generated function depth(::B) where {B<:AbstractBNode}
    h = 0
    C = B
    while C <: AbstractBNode
        h = h + 1
        C = ChildType(C)
    end
    h
end
@generated function depth(Node::Type{<:AbstractBNode})
    B = Node.parameters[1]
    h = 0
    while B <: AbstractBNode
        h = h + 1
        B = B.parameters[4]
    end
    h
end

@inline leftinds(node) = ntuple(one, Val(depth(node)))

@generated function descend(node::AbstractBNode, inds::Vararg{Integer,N}=leftinds(node)...) where {N}
    next = :node
    D = :($next,)
    for i in 1:N
        next = :(values($next)[inds[$i]])
        D = :($D..., $next)
    end
    D
end





abstract type AbstractBTree{K,V,N,B} end

order(::AbstractBTree{K,V,N}) where {K,V,N} = N
root(tree::AbstractBTree) = tree.root
Base.isempty(tree::AbstractBTree) = isempty(root(tree))

length(tree::AbstractBTree) = sum(length, leaves(tree))
leaves(tree::AbstractBTree) = AbstractTrees.Leaves(root(tree))
Base.keys(tree::AbstractBTree) = Iterators.flatten(keys.(leaves(tree)))
Base.values(tree::AbstractBTree) = Iterators.flatten(values.(leaves(tree)))
Base.pairs(tree::AbstractBTree) = Iterators.flatten(pairs.(leaves(tree)))
search(tree::AbstractBTree, key) = search(root(tree), key)

function Base.getindex(tree::AbstractBTree{K,V,N}, key) where {K,V,N}
    k, val = search(tree.root, key)
    k == key || throw(KeyError(key))
    val
end

function Base.get(tree::AbstractBTree{K,V,N}, key, default) where {K,V,N}
    k, val = search(tree.root, key)
    k == key ? val : default
end

Base.get(tree::AbstractBTree{K,V,N}, key) where {K,V,N} = get(tree, key, nothing)

function Base.merge!(tree::AbstractBTree, data::Union{AbstractDict,AbstractBTree})
    for (i, v) in pairs(data)
        tree[i] = v
    end
    tree
end
function Base.merge!(tree::AbstractBTree, data::AbstractVector)
    for (i, v) in data
        tree[i] = v
    end
    tree
end
function Base.merge!(tree::AbstractBTree, data::Vararg{Pair, N}) where N
    for (i, v) in data
        tree[i] = v
    end
    tree
end

function Base.delete!(tree::AbstractBTree{K,V,N}, key) where {K,V,N}
    isempty(tree) && return tree

    root = delete!(tree.root, key)
    if length(root) == 1
        child, = values(root)
        isa(child, B⁺Node) && (tree.root = child)
    end
    tree
end


struct SplitNode{N<:AbstractBNode}
    node::N
    splitsize::Integer
    maxsize::Integer
    nsplits::Integer
    stop::Integer
    stop_index::Integer
end

Base.iterate(split::SplitNode) = length(split) == 1 ? (split.node, 1) : Base.iterate(split, 1)
function Base.iterate(split::SplitNode{B}, state) where {B}
    if state < split.stop
        next = state + split.splitsize - 1
    elseif state < split.stop_index
        next = split.stop_index
    else
        return nothing
    end
    split[state:next], next + 1
end
Base.getindex(split::SplitNode{B}, slice) where {B} = split.node[slice]
Base.length(split::SplitNode) = split.nsplits
Base.eltype(::SplitNode{B}) where {B} = B
