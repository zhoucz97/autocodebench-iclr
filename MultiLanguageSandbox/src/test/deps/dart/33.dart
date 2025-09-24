
int calculateSavings(List<int> budgets)
/*
BB has always managed her pocket money by herself. At the beginning of each month, her mother gives BB 300 yuan, from which BB budgets her monthly expenses and consistently matches her actual expenses with her budget.
To teach BB how to save money, her mother proposed that BB could deposit any whole hundreds of yuan with her at any time, and at the end of the year, she would return the money to BB with an additional 20% interest. Therefore, BB devised a savings plan: at the beginning of each month, after receiving her pocket money from her mother, if she anticipates having more than 100 yuan or exactly 100 yuan left at the end of the month, she would deposit the whole hundreds with her mother and keep the remainder with herself.
BB realized that the main risk of this savings plan is that the money deposited with her mother cannot be withdrawn before the end of the year. It is possible that at the beginning of some month, the money BB has plus the money given by her mother for that month is not enough for the month's budget. If this situation arises, BB will have to economize and cut back on her budget for that month.
Now, according to BB's budget from January to December 2004, determine whether this situation will occur. If not, calculate how much money BB will have at the end of 2004 after her mother returns her savings with an additional 20% interest.

## Input Format

12 lines of data, representing BB's budget from January to December.

## Output Format

A single integer. If there is a month during the implementation of the savings plan where the money is insufficient, output -X, where X indicates the first month when this situation occurs; otherwise, output how much money BB will have at the end of 2004.

## example
calculateSavings([290, 230, 280, 200, 300, 170, 340, 50, 90, 80, 200, 60]) => -7
*/
{
  int currentMoney = 0;
  int savings = 0;
  
  for (int month = 0; month < 12; month++) {
    // Receive monthly allowance
    currentMoney += 300;
    
    // Check if budget can be met
    if (currentMoney < budgets[month]) {
      return -(month + 1); // Months are 1-based in output
    }
    
    // Apply budget
    currentMoney -= budgets[month];
    
    // Save whole hundreds if possible
    if (currentMoney >= 100) {
      int wholeHundreds = (currentMoney ~/ 100) * 100;
      savings += wholeHundreds;
      currentMoney -= wholeHundreds;
    }
  }
  
  // Calculate total after adding 20% interest to savings
  int totalSavingsWithInterest = (savings * 120) ~/ 100;
  int finalAmount = currentMoney + totalSavingsWithInterest;
  
  return finalAmount;
}
void main() {
  assert(calculateSavings([290, 230, 280, 200, 300, 170, 340, 50, 90, 80, 200, 60]) == -7);
  assert(calculateSavings([290, 230, 280, 200, 300, 170, 330, 50, 90, 80, 200, 60]) == 1580);
  assert(calculateSavings([300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300]) == 0);
}