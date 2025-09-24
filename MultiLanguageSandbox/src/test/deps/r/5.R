


# This function 'teach_cooking_skills' is designed to calculate the minimum number of members who need to be taught a specific skill (e.g., cooking a dish) directly by an expert, so that the skill can be disseminated to all members of a group through peer teaching. 
# The input is a list where each element represents a member and contains the members they can teach. 
# The function returns the minimum number of direct teachings required for all members to learn the skill.
# Examples:
# teach_cooking_skills(list(c(2, 4, 3), c(4, 5), c(), c(), c(1))): This would return 1, as teaching member 1 is sufficient for all members to learn the skill.

teach_cooking_skills <- function(member_teaching_preferences) {
    n <- length(member_teaching_preferences)
    if (n == 0) return(0)
    adj_list <- vector("list", n)
    for (i in 1:n) {
        adj_list[[i]] <- member_teaching_preferences[[i]]
    }
    reachable_from <- function(start) {
        visited <- logical(n)
        queue <- c(start)
        visited[start] <- TRUE
        while (length(queue) > 0) {
            current <- queue[1]
            queue <- queue[-1]
            neighbors <- adj_list[[current]]
            for (neighbor in neighbors) {
                if (!visited[neighbor]) {
                    visited[neighbor] <- TRUE
                    queue <- c(queue, neighbor)
                }
            }
        }
        which(visited)
    }
    uncovered <- 1:n
    count <- 0
    
    while (length(uncovered) > 0) {
        max_cover <- -1
        best_member <- -1
        for (member in 1:n) {
            reachable <- reachable_from(member)
            covered <- intersect(reachable, uncovered)
            if (length(covered) > max_cover) {
                max_cover <- length(covered)
                best_member <- member
            }
        }
        if (best_member == -1) {



            break
        }
        count <- count + 1
        reachable <- reachable_from(best_member)
        uncovered <- setdiff(uncovered, reachable)
    }
    
    return(count)
}
# Test cases
main <- function() {

    stopifnot(teach_cooking_skills(list(c(2, 4, 3), c(4, 5), c(), c(), c(1))) == 1)
    stopifnot(teach_cooking_skills(list(c(2), c(3), c(), c(), c())) == 3)
    stopifnot(teach_cooking_skills(list(c(), c(), c(), c(), c())) == 5)
    stopifnot(teach_cooking_skills(list(c(2, 3, 4, 5), c(3, 4, 5), c(4, 5), c(5), c())) == 1)
    stopifnot(teach_cooking_skills(list(c(2, 3), c(3), c())) == 1)


}

# Run tests
main()