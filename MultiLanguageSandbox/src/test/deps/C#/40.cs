using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{


/* Calculates the maximum total value of purchases within a budget, considering main items and their accessories.
    - `totalMoney` is the total budget.
    - `items` is a list of tuples, where each tuple represents an item with its price, importance, and category (0 for main items, item number of the main item if an accessory).
    For example:
    >>> MaxValueOfPurchases(1000, new List<(int, int, int)> { (800, 2, 0), (400, 5, 1), (300, 5, 1), (400, 3, 0), (500, 2, 0) })
    2200
    >>> MaxValueOfPurchases(500, new List<(int, int, int)> { (300, 3, 0), (200, 4, 1), (100, 2, 0) })
    1700
*/
public static int MaxValueOfPurchases(int totalMoney, List<(int price, int importance, int category)> items)
{
        // Group items by main category (0 is main, others are accessories of the main item's index)
        Dictionary<int, List<(int price, int importance)>> mainItems = new Dictionary<int, List<(int, int)>>();
        
        foreach (var item in items)
        {
            if (item.category == 0)
            {
                // Main item
                int mainIndex = items.IndexOf(item); // Using the index in the list as the main item's identifier
                mainItems[mainIndex] = new List<(int, int)> { (item.price, item.importance) };
            }
            else
            {
                // Accessory item, find the main item it belongs to
                int mainIndex = item.category; // The category field for accessories is the main item's index
                if (mainItems.ContainsKey(mainIndex))
                {
                    mainItems[mainIndex].Add((item.price, item.importance));
                }
            }
        }

        // Now, process each main item and its accessories
        // We'll use dynamic programming where dp[i][j] is the max importance with first i main items and budget j
        // But since the number of main items can be up to the number of items, we need to handle it efficiently
        
        // Collect all main items (their indices in the original list)
        List<int> mainItemIndices = new List<int>(mainItems.Keys);
        int n = mainItemIndices.Count;
        
        // Initialize DP: dp[i][j] is max importance with first i main items and budget j
        // We can optimize space by using a 1D array since we process items one by one
        int[] dp = new int[totalMoney + 1];
        
        foreach (int mainIndex in mainItemIndices)
        {
            var mainAndAccessories = mainItems[mainIndex];
            int mainPrice = mainAndAccessories[0].price;
            int mainImportance = mainAndAccessories[0].importance;
            
            // For each main item, we need to consider all possible subsets of its accessories
            // Generate all possible combinations of accessories (price and importance sums)
            List<(int price, int importance)> accessoryCombinations = new List<(int, int)>();
            accessoryCombinations.Add((0, 0)); // option to take no accessories
            
            for (int i = 1; i < mainAndAccessories.Count; i++)
            {
                int accPrice = mainAndAccessories[i].price;
                int accImportance = mainAndAccessories[i].importance;
                
                // For each existing combination, add the new accessory
                int count = accessoryCombinations.Count;
                for (int j = 0; j < count; j++)
                {
                    var existing = accessoryCombinations[j];
                    int newPrice = existing.price + accPrice;
                    int newImportance = existing.importance + accImportance;
                    accessoryCombinations.Add((newPrice, newImportance));
                }
            }
            
            // Now, for each possible accessory combination, update the DP in reverse order
            // We process the DP array from high to low to avoid overwriting values we need
            for (int j = totalMoney; j >= 0; j--)
            {
                if (dp[j] > 0 || j == 0)
                {
                    foreach (var acc in accessoryCombinations)
                    {
                        int totalPrice = mainPrice + acc.price;
                        if (j + totalPrice <= totalMoney)
                        {
                            if (dp[j + totalPrice] < dp[j] + mainImportance + acc.importance)
                            {
                                dp[j + totalPrice] = dp[j] + mainImportance + acc.importance;
                            }
                        }
                    }
                }
            }
        }
        
        int maxImportance = 0;
        for (int j = 0; j <= totalMoney; j++)
        {
            if (dp[j] > maxImportance)
            {
                maxImportance = dp[j];
            }
        }
        
        return maxImportance;
    }
    static void Main()
    {

        Debug.Assert(MaxValueOfPurchases(1000, new List<(int, int, int)> { (800, 2, 0), (400, 5, 1), (300, 5, 1), (400, 3, 0), (500, 2, 0) }) == 2200);
        Debug.Assert(MaxValueOfPurchases(500, new List<(int, int, int)> { (300, 3, 0), (200, 4, 1), (100, 2, 0) }) == 1700);

    }
}
