
abstract type abstractB⁺Node{K,V,N,B} <: AbstractBNode{K,V,N,B} end
abstract type abstractB⁺Tree{K,V,N,B} <: AbstractBTree{K,V,N,B} end

const InternalBNode{K,V,N,B} = abstractB⁺Node{K,V,N,<:B} where {K,V,N,B<:abstractB⁺Node}
const ChildBNode{K,V,N,B} = abstractB⁺Node{K,V,N,<:B} where {K,V<:B,N,B<:V}

isinternal(::abstractB⁺Node{K,V,N,W}) where {K,V,N,W} = W <: abstractB⁺Node{K,V,N}
ischild(node) = !isinternal(node)


Base.setindex!(node::abstractB⁺Node, value, key) = isinternal(node) ?
                                                   setinternalindex!(node, value, key) : setchildindex!(node, value, key)

function setinternalindex!(node::abstractB⁺Node, value, key)
    ks = keys(node)
    i, k = indexkey(node, key)
    (key < k) && (ks[i] = key)
    nodes = values(node)
    child = setindex!(nodes[i], value, key)
    nodes[i], children = split!(child)
    if !isnothing(children)
        for (j, childⱼ) in enumerate(children)
            insert!(node, i + j, childⱼ, nodekey(childⱼ))
        end
    end
    node
end



function setchildindex!(node::abstractB⁺Node, value, key)
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


Base.delete!(node::abstractB⁺Node, key) = isinternal(node) ?
                                          deleteinternal!(node, key) : deletechild!(node, key)

function deleteinternal!(node::abstractB⁺Node, key)
    ks = keys(node)
    i, k = indexkey(node, key)
    (key < k) && (ks[i] = key)
    nodes = values(node)
    child = delete!(nodes[i], key)
    (isvalid(child) || (length(node) == 1)) && return node

    if (i == lastindex(ks))
        if mergenodes!(nodes[i-1], child)
            deleteat!(node, i)
        else
            ks[i] = nodekey(child)
        end
    else
        if mergenodes!(child, nodes[i+1])
            deleteat!(node, i + 1)
        else
            ks[i+1] = nodekey(nodes[i+1])
        end
        ks[i] = nodekey(child)
    end
    node
end


function deletechild!(node::abstractB⁺Node, key)
    i, k = indexkey(node, key)
    k == key && deleteat!(node, i)
    node
end




function index(node::ChildBNode, start, stop)
    i0, k0 = indexkey(node, start)
    i1 = index(node, stop)
    return i0+(start>k0):i1-(stop<k0)
end
function index(node::ChildBNode, start, ::Nothing)
    i0, k0 = indexkey(node, start)
    return i0+(start>k0):lastindex(node)
end
function index(node::ChildBNode, ::Nothing, stop)
    i1 = index(node, stop)
    return firstindex(node):i1
end
index(::ChildBNode, ::Nothing, ::Nothing) = Colon()

Base.getindex(node::ChildBNode, range::UnitRange) = node[index(node, range)]
Base.getindex(node::ChildBNode, start, stop) = values(node)[index(node, start, stop)]
Base.view(node::ChildBNode, range::UnitRange) = view(node, first(range), last(range))
Base.view(node::ChildBNode, start, stop) = view(values(node), index(node, start, stop))
Base.view(node::ChildBNode, ::Colon) = values(node)

aftersplit!(left, right) = right

function split!(node::B, splitsize::Integer) where {B<:abstractB⁺Node}
    i0 = firstindex(keys(node))
    i1 = i0 + splitsize - 1
    iend = lastindex(keys(node))
    nsplits = div(iend - i0 + 1, splitsize) - 1

    nsplits == 0 && return node, nothing

    nodes = Vector{B}(undef, nsplits)
    i1 = i0 + splitsize - 1
    left = indexat(node, i0:i1)
    for j in 1:nsplits-1
        shift = j * splitsize
        left = nodes[j] = aftersplit!(left, indexat(node, i0+shift:i1+shift))
    end
    nodes[nsplits] = aftersplit!(left, indexat(node, i0+nsplits*splitsize:iend))
    return left, nodes
end


function split!(node::abstractB⁺Node)
    isover(node) || return node, nothing
    split!(node, minsize(node))
end

@generated function drop(node, key)
    n = depth(node)
    next = :node
    k = :key
    for _ in 1:n
        ks = :(keys($next))
        vs = :(values($next))
        j = :(max(searchsortedlast($ks, key), 1))
        next = :($vs[$j])
        k = :($ks[$j])
    end
    :($k => $next)
end

# @generated function descend(node::abstractB⁺Node) where B
#     C = B
# 	next = :node
# 	D = :($next,)
#     while C <: abstractB⁺Node
#         C = BTree.ChildType(C)
# 		next = :(values($next)[1])
# 		D = :($D..., $next)
#     end
#     D
# end

struct B⁺Node{K,V,N,B} <: abstractB⁺Node{K,V,N,B}
    keys::Vector{K}
    values::Vector{B}
    function B⁺Node{K,V,N,B}(k::Vector{K}, v::Vector{B}) where {K,V,N,B}
        N < 5 && throw(ArgumentError("N=$N must be greater than 5"))
        new{K,V,N,B}(k, v)
    end
end

B⁺Node{K,V,N}(k::Vector{K}, v::Vector{B}) where {K,V,N,B} = B⁺Node{K,V,N,B}(k, v)
B⁺Node(nodes::AbstractVector{<:B⁺Node}) = B⁺Node(first.(keys.(nodes)), nodes)
B⁺Node{K,V,N}(nodes::AbstractVector{<:B⁺Node}) where {K,V,N} = B⁺Node(first.(keys.(nodes)), nodes)

const B⁺Child{K,V,N,B} = B⁺Node{K,V,N,B} where {K,V<:B,N,B<:V}
const B⁺Internal{K,V,N,B} = B⁺Node{K,V,N,B} where {K,V,N,B<:abstractB⁺Node}

function B⁺Node(keys::AbstractVector{K}, nodes::AbstractVector{<:B⁺Node{K,V,N}}) where {K,V,N}
    B = eltype(nodes)
    keys = convert(Vector{K}, keys)
    nodes = convert(Vector{B}, nodes)
    B⁺Node{K,V,N,B}(keys, nodes)
end

B⁺Node{K,V,N}() where {K,V,N} = B⁺Node{K,V,N,V}(K[], V[])
B⁺Node(::Type{K}, ::Type{V}, order::Integer) where {K,V} = B⁺Node{K,V,order}()
B⁺Node{K,V}(N::Integer) where {K,V} = B⁺Node{K,V,N}()
B⁺Node(keys::Vector{K}, values::Vector{V}, N::Integer) where {K,V} = B⁺Node{K,V,N}(keys, values)
B⁺Node(nodes::Vararg{B}) where {B<:B⁺Node} = B⁺Node(collect(nodes))


@generated function search(node, key)
    next = :node 
    k = :key
    D = depth(node)
    for i in 1:D
        ks = :(keys($next))
        i = :(index($ks, key))
        next = :(values($next)[$i])
        k = :(ks[$i])
    end
    :($k => $next)
    next
end

struct StableB⁺Node{K,V,N,B} <: abstractB⁺Node{K,V,N,B}
    keys::Vector{K}
    values::Union{Vector{V},Vector{StableB⁺Node{K,V,N}}}

    function StableB⁺Node{K,V,N,B}(k::Vector{K}, v::Vector{C}) where {K,V,N,B,C<:Union{V,StableB⁺Node{K,V,N}}}
        N < 1 && throw(ArgumentError("N=$N must be greater than 1"))
        new{K,V,N,Union{V,StableB⁺Node{K,V,N}}}(k, v)
    end
end
const StableB⁺ChildNode{K,V,N} = Union{V,StableB⁺Node{K,V,N}}

# StableB⁺Node{K,V,N,B}(args...) where {K,V,N,B} = StableB⁺Node{K,V,N}(args...)
StableB⁺Node{K,V,N}(k::Vector{K}, v::Vector{V}) where {K,V,N} = StableB⁺Node{K,V,N,StableB⁺ChildNode{K,V,N}}(k, v)
StableB⁺Node{K,V,N}(k::Vector{K}, v::Vector{B}) where {K,V,N,B} = StableB⁺Node{K,V,N,StableB⁺ChildNode{K,V,N}}(k, convert(Vector{StableB⁺Node{K,V,N}}, v))

function StableB⁺Node(keys::AbstractVector{K}, nodes::AbstractVector{<:abstractB⁺Node{K,V,N}}) where {K,V,N}
    keys = convert(Vector{K}, keys)
    nodes = convert(Vector{StableB⁺Node{K,V,N}}, nodes)
    StableB⁺Node{K,V,N,StableB⁺ChildNode{K,V,N}}(keys, nodes)
end


StableB⁺Node(node::B) where {K,V,N,B<:abstractB⁺Node{K,V,N}} = StableB⁺Node{K,V,N}(keys(node), values(node))
StableB⁺Node{K,V,N}() where {K,V,N} = StableB⁺Node{K,V,N}(K[], V[])
StableB⁺Node{K,V}(N::Integer) where {K,V} = StableB⁺Node{K,V,N}()
StableB⁺Node(keys::Vector{K}, values::Vector{V}, order::Integer) where {K,V} = StableB⁺Node{K,V,order}(keys, values)

StableB⁺Node(nodes::AbstractVector{B}) where {K,V,N,B<:StableB⁺Node{K,V,N}} = StableB⁺Node{K,V,N}(nodes)
StableB⁺Node{K,V,N,B}(nodes::AbstractVector{<:StableB⁺Node}) where {K,V,N,B} = StableB⁺Node{K,V,N}(nodes)
StableB⁺Node{K,V,N}(nodes::AbstractVector{<:StableB⁺Node}) where {K,V,N} = StableB⁺Node(first.(keys.(nodes)), nodes)

isinternal(node::StableB⁺Node) = eltype(values(node)) <: abstractB⁺Node

# isinternal(node::B⁺Internal) = true
# isinternal(node::B⁺Child) = false
# ischild(node::B⁺Internal) = false
# ischild(node::B⁺Child) = true
# eltype(::B⁺Node{K, V, N}) where {K, V, N} = V
# AbstractTrees.NodeType(::Type{<:AbstractBNode}) = AbstractTrees.HasNodeType()
# AbstractTrees.nodetype(::Type{<:AbstractBNode}) = AbstractBNode
# AbstractTrees.NodeType(::Type{<:B⁺Node}) = AbstractTrees.HasNodeType()
# AbstractTrees.nodetype(::Type{<:B⁺Node{K, V, N}}) where {K, V, N} = B⁺Node{K, V, N, Any}



mutable struct B⁺Tree{K,V,N,B<:abstractB⁺Node{K,V,N}} <: abstractB⁺Tree{K,V,N,B}
    root::B
    # leaves::MutableLinkedList{B⁺Node{K, V, N}}
    B⁺Tree{K,V,N,B}(root) where {K,V,N,B} = B⁺Tree(root)
    function B⁺Tree(root::B) where {K,V,N,B<:abstractB⁺Node{K,V,N}}
        W = B.name.wrapper{K,V,N}
        new{K,V,N,W}(root)
    end
end

B⁺Tree{K,V}(order::Integer) where {K,V} = B⁺Tree{K,V,order}()
function B⁺Tree{K,V,N}() where {K,V,N}
    root = B⁺Node{K,V,N,V}(K[], V[])
    # leaves = MutableLinkedList{B⁺Node{K, V, order}}(root)
    B⁺Tree{K,V,N,B⁺Node{K,V,N}}(root)
end

function B⁺Tree{N}(data::AbstractVector{Pair{K,V}}) where {K,V,N}
    tree = B⁺Tree{K,V,N}()
    for (k, v) in data
        tree[k] = v
    end
    tree
end

function setindex!(tree::B, value, key) where {K,V,N,W,B<:abstractB⁺Tree{K,V,N,W}}
    node = setindex!(tree.root, value, key)
    node, children = split!(node)
    if !isnothing(children)
        tree.root = W(vcat(node, children))
    end
    tree
end

slice(f::Function, node::B⁺Child, start, stop) = Ref(@view f(node)[index(node, start, stop)])
function slice(f::Function, node, start, stop)
    children = @view values(node)[index(node, start, stop)]
    i0, i1 = firstindex(children), lastindex(children)
    [
        child2 for (i, child) in pairs(children)
        for child2 in slice(f, child, i == i0 ? start : nothing, i == i1 ? stop : nothing)
    ]
end
slice(node, start, stop) = slice(values, node, start, stop)


struct DepthIterator{D,V}
    node::V
end
DepthIterator{D}(node::V) where {D,V<:AbstractBNode} = DepthIterator{D > 0 ? D : depth(V) + D,V}(node)
DepthIterator(node::V) where {V<:AbstractBNode} = DepthIterator{depth(node),V}(node)
Base.IteratorSize(::DepthIterator) = Base.SizeUnknown()
Base.eltype(::DepthIterator{D,V}) where {D,V} = ChildType(V, Val(D))
Base.IteratorEltype(::DepthIterator) = Base.HasEltype()

@inline function splitfirst(iter)
    isnothing(iter) && return
    val, state... = iter
    return val, state
end

Base.iterate(iter::DepthIterator{D,V}) where {D,V} = splitfirst(iteratestart(iter.node, Val(D)))
@inline Base.iterate(::DepthIterator, state) = splitfirst(next_value(state...))


@generated function iteratestart(node, ::Val{D}=Val(depth(node))) where {D}
    next = :node
    state = :($next,)
    for i in 1:D
        nextstate = :(iterate(values($next)))
        state = :($nextstate..., $state...)
        next = :($nextstate[1])
    end
    :($state)
end
iteratestart(iter::DepthIterator{D,V}) where {D,V} = iteratestart(iter.node, Val(D))

@inline function next_value(state, node, rest::Vararg{Any, N}) where {N}
    val_state = iterate(values(node), state)
    if !isnothing(val_state)
        (val_state..., node, rest...)
    elseif !isempty(rest)
        node_rest = next_value(rest...)
        isnothing(node_rest) ? nothing : next_value(one(state), node_rest...)
    else
        nothing
    end
end