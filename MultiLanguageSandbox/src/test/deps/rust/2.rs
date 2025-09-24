

 /*
  Input to this function is a string containing multiple groups of nested parentheses. Your goal is to
  separate those group into separate strings and return the list of those.
  Separate groups are balanced (each open brace is properly closed) and not nested within each other
  Ignore any spaces in the input string.
 
 */
 
 use std::{slice::Iter, cmp::{max, self}, mem::replace, collections::{HashSet, HashMap}, ops::Index, ascii::AsciiExt};
 use rand::Rng;
 use regex::Regex;
 use md5;
 use std::any::{Any, TypeId};
 
 fn separate_paren_groups(paren_string: String) -> Vec<String>{
 
    let mut result = Vec::new();
    let mut current_group = String::new();
    let mut depth = 0;
    
    for c in paren_string.chars().filter(|&c| c != ' ') {
        match c {
            '(' => {
                if depth == 0 && !current_group.is_empty() {
                    // Start of a new group when we weren't in one before
                    result.push(current_group);
                    current_group = String::new();
                }
                current_group.push(c);
                depth += 1;
            },
            ')' => {
                current_group.push(c);
                depth -= 1;
                if depth == 0 && !current_group.is_empty() {
                    // End of a complete group
                    result.push(current_group);
                    current_group = String::new();
                }
            },
            _ => {} // Ignore other characters (though problem says input is just parentheses)
        }
    }
    
    // Add any remaining group if we ended in the middle of one
    if !current_group.is_empty() {
        result.push(current_group);
    }
    
    result
}


 #[cfg(test)]
 mod tests {
  use super::*;
 
 #[test]
  fn test_separate_paren_groups() {
  assert_eq!(
  separate_paren_groups(String::from("(()()) ((())) () ((())()())")),
  vec!["(()())", "((()))", "()", "((())()())"]
  );
  assert_eq!(
  separate_paren_groups(String::from("() (()) ((())) (((())))")),
  vec!["()", "(())", "((()))", "(((())))"]
  );
  assert_eq!(
  separate_paren_groups(String::from("(()(())((())))")),
  vec!["(()(())((())))"]
  );
  assert_eq!(
  separate_paren_groups(String::from("( ) (( )) (( )( ))")),
  vec!["()", "(())", "(()())"]
  );
  }
 
 }
 