

/**
 * Time passed quickly, and Wang Wang had reached the age of months for vaccination. Wang Wang came to the hospital and wanted to give himself a shot of rabies vaccine, but he couldn't resist the doctor's temptation. He needs a total of 4 injections. He first received three shots of Miao Sanduo, and then the rabies vaccine. A shot of Miao Sanduo costs 120 yuan, and a shot of rabies vaccine costs 100 yuan. However, Wang Wang only has a yuan of money and cannot take all the shots at once. At this time, the doctor said: "How can we complete the injection at one time? There needs to be a month's interval between every two injections..." So Wang Wang decided to save money while giving himself injections (because Wang Wang is a meow who loves injections), Wang Wang If I can save B yuan every month, how many vaccinations can Wangwang get on time?
 * Example:
 * >>> calculateMaxVaccinations(10, 200)
 *     0
 * >>> calculateMaxVaccinations(150, 100)
 *     2
 * >>> calculateMaxVaccinations(500, 100)
 *     4
 */

fun calculateMaxVaccinations(
    initialAmount: Int, 
    monthlySavings: Int, 
): Int 
{
    val vaccinationCosts = listOf(120, 120, 120, 100)
    var currentMoney = initialAmount
    var vaccinationsGiven = 0
    
    for (cost in vaccinationCosts) {
        if (currentMoney >= cost) {
            currentMoney -= cost
            vaccinationsGiven++
            // After giving the vaccination, wait a month to save money
            currentMoney += monthlySavings
        } else {
            break
        }
    }
    
    return vaccinationsGiven
}
fun main(){
    check(calculateMaxVaccinations(10, 200) == 0)
    check(calculateMaxVaccinations(150, 100) == 2)
    check(calculateMaxVaccinations(500, 100) == 4)
    check(calculateMaxVaccinations(300, 50) == 3)
    check(calculateMaxVaccinations(0, 200) == 0)
    check(calculateMaxVaccinations(100, 20) == 0)
    check(calculateMaxVaccinations(120, 100) == 1)
    check(calculateMaxVaccinations(400, 0) == 3)

}
main()