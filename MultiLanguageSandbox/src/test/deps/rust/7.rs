
/*
  Calculates the highest score, lowest score, and average score from a list of student scores.
  
  The function takes a vector of integer scores, each ranging from 0 to 100, representing the scores of students in an exam. It returns a tuple containing the highest score, lowest score, and the average score calculated from these scores.

  Arguments:
  - scores: A Vec<i32> containing the exam scores of students.

  Returns:
  - A tuple (i32, i32, f64), where the first element is the highest score, the second is the lowest score, and the third is the average score rounded to two decimal places.

  Example:
  - exam_statistics(vec![80, 92, 56, 74, 88, 99, 10]) returns (99, 10, 71.29)
  - exam_statistics(vec![100, 85, 76, 90]) returns (100, 76, 87.75)
  - exam_statistics(vec![50, 50, 50, 50]) returns (50, 50, 50.00)
*/
fn exam_statistics(scores: Vec<i32>) -> (i32, i32, f64){
    if scores.is_empty() {
        return (0, 0, 0.0);
    }

    let mut max_score = scores[0];
    let mut min_score = scores[0];
    let mut sum = 0;

    for &score in &scores {
        if score > max_score {
            max_score = score;
        }
        if score < min_score {
            min_score = score;
        }
        sum += score;
    }

    let average = sum as f64 / scores.len() as f64;
    let rounded_average = (average * 100.0).round() / 100.0;

    (max_score, min_score, rounded_average)
}



#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn main() {
        assert_eq!(exam_statistics(vec![80, 92, 56, 74, 88, 99, 10]), (99, 10, 71.29));
        assert_eq!(exam_statistics(vec![100, 85, 76, 90]), (100, 76, 87.75));
        assert_eq!(exam_statistics(vec![50, 50, 50, 50]), (50, 50, 50.00));
    }
}