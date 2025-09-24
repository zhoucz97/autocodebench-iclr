<?php

/**
    * Calculates the monthly repayment amount for an equal principal repayment loan.
    * In this repayment model, each month's repayment amount consists of two parts:
    * - A constant principal payment, which is the total loan amount divided by the total number of months.
    * - The interest payment, which is the outstanding loan amount multiplied by the monthly interest rate.
    * Input: 
    * - $loanAmount (int): The total loan amount in ten-thousands.
    * - $monthlyInterestRate (float): The monthly interest rate.
    * - $totalMonths (int): The total number of months for loan repayment.
    * Output: Returns the first month's repayment amount as an integer (in Yuan). Discard the decimal point and do not round
    * Example: calculateMonthlyRepayment(500, 0.004, 360) should return 33889.
*/

function calculateMonthlyRepayment($loanAmount, $monthlyInterestRate, $totalMonths): int {

    // Calculate the principal payment per month (in ten-thousands)
    $principalPayment = $loanAmount / $totalMonths;
    
    // Calculate the interest payment for the first month (in ten-thousands)
    $interestPayment = $loanAmount * $monthlyInterestRate;
    
    // Total repayment for the first month in ten-thousands
    $totalRepaymentTenThousand = $principalPayment + $interestPayment;
    
    // Convert to Yuan (multiply by 10,000) and discard decimal part
    $totalRepaymentYuan = (int) ($totalRepaymentTenThousand * 10000);
    
    return $totalRepaymentYuan;
}
function main(){
    assert(calculateMonthlyRepayment(500, 0.004, 360) === 33888);
    assert(calculateMonthlyRepayment(300, 0.005, 180) === 31666); // Example test case

}

main();
?>