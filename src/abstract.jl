

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

index(node::AbstractBNode, key) = index(keys(node), key)
index(ks::Vector{K}, key::K) where K = searchsortedlast(@view(ks[2:end]), key) + 1

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


function Base.sizehint!(node::AbstractBNode, sz::Integer)
    sizehint!(keys(node), sz)
    sizehint!(values(node), sz)
    node
end
Base.getindex(node::B, slice::UnitRange{<:Integer}) where {B <: AbstractBNode} = B(keys(node)[slice], values(node)[slice])

struct SplitNode{N<:AbstractBNode}
    node::N
    splitsize::Integer 
    maxsize::Integer 
    nsplits::Integer 
    stop::Integer 
    stop_index::Integer 
end

Base.getindex(split::SplitNode{B}, slice) where B = split.node[slice]
Base.length(split::SplitNode) = split.nsplits 
Base.eltype(::SplitNode{B}) where B = B

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

function split_node(node::AbstractBNode, splitsize::Integer, maxsize::Integer)
    stop_index = lastindex(keys(node))
    stop = stop_index - maxsize + 1
    nsplits = cld(length(node), minsize(node)) - 1
    SplitNode(node, splitsize, maxsize, nsplits, stop, stop_index)
end

function split_node(node::B) where B <:  AbstractBNode
    isover(node) || return SplitNode(node, order(node), order(node), 1, 1, 1)
    split_node(node, minsize(node), order(node))
end