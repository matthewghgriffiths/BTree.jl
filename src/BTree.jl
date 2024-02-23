module BTree

import Base: insert!, keys, values, pairs, length, isempty, setindex!, 
    eltype, getindex, iterate
# using DataStructures: MutableLinkedList
import AbstractTrees
using IterTools
using Accessors

export B⁺Tree, B⁺Node

include("abstract.jl")
include("b+tree.jl")
include("tests.jl")

end