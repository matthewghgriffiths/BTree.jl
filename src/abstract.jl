

abstract type AbstractBNode{K, V, N} end

@inline order(::AbstractBNode{K, V, N}) where {K, V, N} = N
@inline minsize(::AbstractBNode{K, V, N}) where {K, V, N} = cld(N, 2)
@inline isfull(node::AbstractBNode{K, V, N}) where {K, V, N} = length(node) == order(node)
@inline isover(node::AbstractBNode{K, V, N}) where {K, V, N} = length(node) > order(node)
@inline isunder(node::AbstractBNode{K, V, N}) where {K, V, N} = length(node) < minsize(node)
@inline isvalid(node::AbstractBNode) = !isover(node) && !isunder(node) && issorted(node)

Base.keys(node::AbstractBNode) = node.keys 
Base.values(node::AbstractBNode) = node.values
Base.length(node::AbstractBNode) = length(values(node))
Base.issorted(node::AbstractBNode) = issorted(keys(node))
Base.isempty(node::AbstractBNode) = isempty(values(node))
Base.iterate(node::AbstractBNode) = iterate(values(node))
Base.eltype(node::AbstractBNode) = eltype(values(node))
Base.first(node::AbstractBNode) = first(values(node))
Base.last(node::AbstractBNode) = last(values(node))

Base.firstindex(node::AbstractBNode) = firstindex(keys(node))
Base.lastindex(node::AbstractBNode) = lastindex(keys(node))

@inline index(node::AbstractBNode, range::UnitRange) = index(keys(node), first(range), last(range))
@inline index(node::AbstractBNode, ks...) = index(keys(node), ks...)
@inline index(ks::AbstractVector, range::AbstractRange) = index(ks, first(range), last(range))
@inline index(ks::AbstractVector, key1, key2) = index(ks, key1):index(ks, key2)
@inline index(ks::AbstractVector, key1, ::Nothing) = index(ks, key1):lastindex(ks)
@inline index(ks::AbstractVector, ::Nothing, key2) = firstindex(ks):index(ks, key2)
@inline index(ks::AbstractVector, ::Nothing, ::Nothing) = Colon()
@inline index(ks::AbstractVector, key) = searchsortedlast(@view(ks[2:end]), key) + 1

@inline indexkey(node::AbstractBNode, ks...) = indexkey(keys(node), ks...)
@inline function indexkey(ks::AbstractVector{K}, key::K1) where {K, K1 <: K} 
    i = index(ks, key)
    i => ks[i]
end

function search(node::AbstractBNode{K, V, N}, key) where {K, V, N}
    ks = keys(node)
    i = index(ks, key)
    next = values(node)[i]
    isa(next, V) ? (ks[i] => next) : search(next, key)
end

Base.getindex(node::AbstractBNode, range::UnitRange) = getindex(node, first(range), last(range))
function Base.getindex(node::AbstractBNode, key)
    k, val = search(node, key)
    k == key || throw(KeyError(key))
    val 
end

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

isinternal(::Any) = false
ischild(::Any) = false
AbstractTrees.children(node::AbstractBNode) = ischild(node) ? () : values(node)
AbstractTrees.nodevalue(node::AbstractBNode{K, V, N}) where {K, V, N} = ischild(node) ? collect(pairs(node)) : keys(node)

abstract type AbstractBTree{K, V, N} end

order(::AbstractBTree{K, V, N}) where {K, V, N} = N
root(tree::AbstractBTree) = tree.root
Base.isempty(tree::AbstractBTree) = isempty(root(tree))

function Base.sizehint!(node::AbstractBNode, sz::Integer)
    sizehint!(keys(node), sz)
    sizehint!(values(node), sz)
    node
end

indexat(node::B, ind) where {B <: AbstractBNode} = keys(node)[ind] => values(node)[ind]
indexat(node::B, slice::UnitRange{<:Integer}) where {B <: AbstractBNode} = B(keys(node)[slice], values(node)[slice])

getnested(node) = node
getnested(node::AbstractBNode, i, inds...) = getnested(values(node)[i], inds...)

function Base.deleteat!(node::B, i) where {B <: AbstractBNode}
    deleteat!(keys(node), i)
    deleteat!(values(node), i)
end

function Base.append!(node::B, ks, vs) where {B <: AbstractBNode}
    append!(keys(node), ks)
    append!(values(node), vs)
    node
end
Base.append!(left::B, right::B) where {B <: AbstractBNode} = append!(left, keys(right), values(right))
Base.splice!(node::B, i) where {B <: AbstractBNode} = splice!(keys(node), i) => splice!(values(node), i)


struct SplitNode{N<:AbstractBNode}
    node::N
    splitsize::Integer 
    maxsize::Integer 
    nsplits::Integer 
    stop::Integer 
    stop_index::Integer 
end

Base.iterate(split::SplitNode) = length(split) == 1 ? (split.node, 1) : Base.iterate(split, 1)
function Base.iterate(split::SplitNode{B}, state) where B
    if state < split.stop
        next = state + split.splitsize - 1
    elseif state < split.stop_index 
        next = split.stop_index
    else
        return nothing
    end
    split[state:next], next + 1
end
Base.getindex(split::SplitNode{B}, slice) where B = split.node[slice]
Base.length(split::SplitNode) = split.nsplits 
Base.eltype(::SplitNode{B}) where B = B
function split_node(node::AbstractBNode, splitsize::Integer, maxsize::Integer)
    stop_index = lastindex(keys(node))
    stop = stop_index - maxsize + 1
    nsplits = fld(length(node), minsize(node))
    SplitNode(node, splitsize, maxsize, nsplits, stop, stop_index)
end
function split_node(node::B) where B <:  AbstractBNode
    isover(node) || return SplitNode(node, order(node), order(node), 1, 1, 1)
    split_node(node, minsize(node), order(node))
end