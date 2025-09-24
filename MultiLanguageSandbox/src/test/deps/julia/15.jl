struct BinaryTreeNode
    val::Int
    left::Union{BinaryTreeNode, Nothing}
    right::Union{BinaryTreeNode, Nothing}

    # Constructor for a node with a value and optional left and right children.
    BinaryTreeNode(val::Int, left::Union{BinaryTreeNode, Nothing}=nothing, right::Union{BinaryTreeNode, Nothing}=nothing) = new(val, left, right)
end
    

function inorder_traversal(root::Union{BinaryTreeNode, Nothing}) :: Vector{Int}
    """
    Perform an inorder traversal of a binary tree and return the values in a list.
    
    This function traverses a binary tree in an inorder manner (left node, root, right node) and collects the values of the nodes in a list. For a binary tree, this results in values being returned in a non-decreasing order.
    
    Examples:
    >>> inorder_traversal(BinaryTreeNode(1, nothing, BinaryTreeNode(2, BinaryTreeNode(3), nothing)))
    [1, 3, 2]
    
    >>> inorder_traversal(nothing)
    []
    
    >>> inorder_traversal(BinaryTreeNode(1))
    [1]
    """
    result = Int[]
    stack = Union{BinaryTreeNode, Nothing}[]
    current = root
    
    while !isempty(stack) || current !== nothing
        # Traverse to the leftmost node
        while current !== nothing
            push!(stack, current)
            current = current.left
        end
        
        # Process the node
        current = pop!(stack)
        push!(result, current.val)
        
        # Move to the right subtree
        current = current.right
    end
    
    return result
end
@assert inorder_traversal(BinaryTreeNode(1, nothing, BinaryTreeNode(2, BinaryTreeNode(3), nothing))) == [1, 3, 2]
@assert inorder_traversal(nothing) == []
@assert inorder_traversal(BinaryTreeNode(1)) == [1]
@assert inorder_traversal(BinaryTreeNode(1, BinaryTreeNode(2), BinaryTreeNode(3))) == [2, 1, 3]