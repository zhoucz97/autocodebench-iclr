function total_smoked_cigarettes(cigarettes::Int, conversion_rate::Int) :: Int

    """
    Calculate the total number of cigarettes Peter can smoke. Peter starts with a certain number of cigarettes, 
    and for every `conversion_rate` number of cigarette butts, he can get a new cigarette.

    # Arguments
    - `cigarettes`: Initial number of cigarettes Peter has.
    - `conversion_rate`: Number of cigarette butts needed to get a new cigarette.

    # Examples
    julia> total_smoked_cigarettes(4, 3)
    5
    """
    total = 0
    butts = 0
    
    while cigarettes > 0
        # Smoke all available cigarettes
        total += cigarettes
        butts += cigarettes
        
        # Convert butts to new cigarettes
        new_cigs = butts ÷ conversion_rate
        butts -= new_cigs * conversion_rate
        
        # Update the number of cigarettes
        cigarettes = new_cigs
    end
    
    return total
end
@assert total_smoked_cigarettes(4, 3) == 5
@assert total_smoked_cigarettes(10, 3) == 14
@assert total_smoked_cigarettes(100, 4) == 133
@assert total_smoked_cigarettes(1, 2) == 1  # Edge case: Not enough cigarettes to exchange for a new one.