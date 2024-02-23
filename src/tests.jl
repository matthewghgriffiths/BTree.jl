

test_b⁺tree(tree) = issorted(keys(tree) |> collect)

function test_b⁺tree(tree, data)
    test_b⁺tree(tree)
    root, children... = AbstractTrees.PreOrderDFS(tree.root)
    
    all(isvalid, children) || return false
    order(root) >= length(root) >= min(2, length(data))  || return false
    all((data[k] == v) for (k, v) in pairs(tree)) || return false
    all((tree[k] == v) for (k, v) in pairs(data)) || return false
    length(data) == length(tree) || return false
    true
end
