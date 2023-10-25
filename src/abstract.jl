

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

function Base.push!(node::AbstractBNode, key, value)
    push!(keys(node), key)
    push!(values(node), value)
    return node 
end

function insert!(node::AbstractBNode, i, key, value)
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


Base.sizehint!(node::AbstractBNode, sz::Integer) = begin
    sizehint!(keys(node), sz)
    sizehint!(values(node), sz)
    node
end
