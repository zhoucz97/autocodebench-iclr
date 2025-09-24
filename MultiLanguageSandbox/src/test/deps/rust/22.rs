

/*
  Calculates the number of patients who cannot be served based on the available medicine stock and their requests.

  Arguments:
  - total_medicine: The total amount of medicine available at the beginning of the day.
  - num_patients: The number of patients coming to get medicine.
  - patient_requests: An array of usize, each representing the amount of medicine requested by a patient, in the order they arrive.

  Returns:
  - The number of patients who cannot be served because the medicine stock is insufficient.

  Example:
  - count_unserved_patients(30, 6, &[10, 5, 20, 6, 7, 8]) returns 2
  - count_unserved_patients(50, 4, &[10, 15, 10, 20]) returns 1
  - count_unserved_patients(10, 3, &[5, 10, 5]) returns 1
*/

fn count_unserved_patients(total_medicine: usize, num_patients: usize, patient_requests: &[usize]) -> usize {
    let mut remaining_medicine = total_medicine;
    let mut unserved_count = 0;

    for &request in patient_requests.iter().take(num_patients) {
        if request <= remaining_medicine {
            remaining_medicine -= request;
        } else {
            unserved_count += 1;
        }
    }

    unserved_count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example1() {
        assert_eq!(count_unserved_patients(30, 6, &[10, 5, 20, 6, 7, 8]), 2);
    }

    #[test]
    fn test_example2() {
        assert_eq!(count_unserved_patients(50, 4, &[10, 15, 10, 20]), 1);
    }

    #[test]
    fn test_example3() {
        assert_eq!(count_unserved_patients(10, 3, &[5, 10, 5]), 1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(count_unserved_patients(30, 6, &[10, 5, 20, 6, 7, 8]), 2);
        assert_eq!(count_unserved_patients(50, 4, &[10, 15, 10, 20]), 1);
        assert_eq!(count_unserved_patients(10, 3, &[5, 10, 5]), 1);
        assert_eq!(count_unserved_patients(20, 5, &[4, 5, 7, 3, 2]), 1);
        
    }
    

}