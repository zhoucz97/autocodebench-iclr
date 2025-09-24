import java.util.*;
import java.util.*;

class Solution {
    /**
     * Given the size of the n-teresting space, the starting and ending n-tersection coordinates, and the paths between adjacent n-tersections, determine if it is possible to travel from the starting n-tersection to the ending n-tersection.
     * 
     * Parameters:
     * - n: the size of the n-teresting space
     * - start: the starting n-tersection coordinates
     * - end: the ending n-tersection coordinates
     * - paths: paths between adjacent n-tersections in the n-teresting space
     * 
     * Output:
     * - If travel is possible, return "can be travelled"
     * - If travel is not possible, return "cannot be travelled"
     */
    public static String canTravelMaze(int n, int[] start, int[] end, int[][] paths) 
{
        // Create a map to represent the adjacency list of the graph
        Map<String, List<String>> graph = new HashMap<>();
        
        // Populate the graph with the given paths
        for (int[] path : paths) {
            String from = path[0] + "," + path[1];
            String to = path[2] + "," + path[3];
            
            graph.computeIfAbsent(from, k -> new ArrayList<>()).add(to);
            graph.computeIfAbsent(to, k -> new ArrayList<>()).add(from);
        }
        
        // Convert start and end coordinates to string keys
        String startKey = start[0] + "," + start[1];
        String endKey = end[0] + "," + end[1];
        
        // If start or end is not in the graph, check if they are adjacent via paths
        // But according to the problem, paths are between adjacent intersections, so start and end must be in the graph or adjacent.
        // However, the problem statement might imply that all intersections are part of the graph via paths.
        // So proceed under the assumption that start and end are valid nodes.
        
        // Perform BFS to check reachability
        Queue<String> queue = new LinkedList<>();
        Set<String> visited = new HashSet<>();
        
        queue.add(startKey);
        visited.add(startKey);
        
        while (!queue.isEmpty()) {
            String current = queue.poll();
            if (current.equals(endKey)) {
                return "can be travelled";
            }
            
            List<String> neighbors = graph.getOrDefault(current, new ArrayList<>());
            for (String neighbor : neighbors) {
                if (!visited.contains(neighbor)) {
                    visited.add(neighbor);
                    queue.add(neighbor);
                }
            }
        }
        
        return "cannot be travelled";
    };
        System.out.println(canTravelMaze(n, start, end, paths)); // Output: can be travelled
    }
    public static void main(String[] args) {
        // Test the function with different test cases
        assert canTravelMaze(2, new int[]{0, 0}, new int[]{2, 2}, new int[][]{{0, 0, 0, 1}, {0, 1, 1, 1}, {1, 1, 1, 2}, {1, 2, 2, 2}}).equals("can be travelled");
        assert canTravelMaze(2, new int[]{0, 0}, new int[]{2, 2}, new int[][]{{0, 0, 0, 1}, {0, 1, 1, 1}, {1, 1, 2, 1}, {2, 1, 2, 2}}).equals("can be travelled");
        assert canTravelMaze(2, new int[]{0, 0}, new int[]{2, 2}, new int[][]{{0, 0, 0, 1}, {0, 1, 1, 1}, {1, 1, 2, 1}}).equals("cannot be travelled");
        System.out.println("All tests passed");
    }
}