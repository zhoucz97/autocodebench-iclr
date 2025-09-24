import java.util.ArrayList;
import java.util.List;
import java.util.*;

class Solution {

  /**
   * Given a tree with n nodes and edges, and a boolean array hasApple indicating if each node has an apple.
   * Starting from node 0, return the minimum time required to collect all the apples and return to node 0.
   *
   * Example:
   * Input: n = 7, edges = [[0,1],[0,2],[1,4],[1,5],[2,3],[2,6]], hasApple = [false,false,true,false,true,true,false]
   * Output: 8
   *
   * Input: n = 7, edges = [[0,1],[0,2],[1,4],[1,5],[2,3],[2,6]], hasApple = [false,false,true,false,false,true,false]
   * Output: 6
   *
   * Input: n = 7, edges = [[0,1],[0,2],[1,4],[1,5],[2,3],[2,6]], hasApple = [false,false,false,false,false,false,false]
   * Output: 0
   */
  List<Set<Integer>> tree;
  int[] res;

  public int minTime(int n, int[][] edges, boolean[] hasApple) 
{
    public int minTime(int n, int[][] edges, boolean[] hasApple) {
        // Build the adjacency list
        List<List<Integer>> adj = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            adj.add(new ArrayList<>());
        }
        for (int[] edge : edges) {
            int u = edge[0];
            int v = edge[1];
            adj.get(u).add(v);
            adj.get(v).add(u);
        }
        
        return dfs(0, -1, adj, hasApple);
    }
    
    private int dfs(int node, int parent, List<List<Integer>> adj, boolean[] hasApple) {
        int totalTime = 0;
        for (int child : adj.get(node)) {
            if (child == parent) continue;
            int childTime = dfs(child, node, adj, hasApple);
            if (childTime > 0 || hasApple[child]) {
                totalTime += childTime + 2;
            }
        }
        return totalTime;
    }
}
  public static void main(String[] args) {
    Solution solution = new Solution();

    // Test the function with different test cases
    assert solution.minTime(
      7,
      new int[][] {
        { 0, 1 },
        { 0, 2 },
        { 1, 4 },
        { 1, 5 },
        { 2, 3 },
        { 2, 6 },
      },
      new boolean[] { true, false, true, false, true, true, false }
    ) ==
    8;
    assert solution.minTime(
      7,
      new int[][] {
        { 0, 1 },
        { 0, 2 },
        { 1, 4 },
        { 1, 5 },
        { 2, 3 },
        { 2, 6 },
      },
      new boolean[] { true, true, true, true, true, true, true }
    ) ==
    12;
    assert solution.minTime(
      7,
      new int[][] {
        { 0, 1 },
        { 0, 2 },
        { 1, 4 },
        { 1, 5 },
        { 2, 3 },
        { 2, 6 },
      },
      new boolean[] { false,false,true,false,false,true,false }
    ) ==
    6;

    assert solution.minTime(
      7,
      new int[][] {
        { 0, 1 },
        { 0, 2 },
        { 1, 4 },
        { 1, 5 },
        { 2, 3 },
        { 2, 6 },
      },
      new boolean[] { false,false,false,false,false,false,false }
    ) ==
    0;
    System.out.println("All tests passed");
  }
}