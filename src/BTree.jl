module BTree

import Base: insert!, keys, values, pairs, length, isempty, setindex!, eltype, getindex
# using DataStructures: MutableLinkedList
import AbstractTrees
using IterTools

export B⁺Tree, B⁺Node

include("abstract.jl")
include("b+tree.jl")

end