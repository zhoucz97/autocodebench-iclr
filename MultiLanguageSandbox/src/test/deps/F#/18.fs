
let rearrangeString (s: string) (indices: int list) =
    let chars = s.ToCharArray()
    let result = Array.zeroCreate<char> chars.Length
    for i in 0 .. chars.Length - 1 do
        result.[indices.[i]] <- chars.[i]
    System.String(result)


let test () =
    if rearrangeString "codeleet" [4; 5; 6; 7; 0; 2; 1; 3] <> "leetcode" then
        failwith "Test Case 1 failed"
    if rearrangeString "abc" [0; 1; 2] <> "abc" then
        failwith "Test Case 2 failed"
    if rearrangeString "aiohn" [3; 1; 4; 2; 0] <> "nihao" then
        failwith "Test Case 3 failed"
    if rearrangeString "aaiougrt" [4; 0; 2; 6; 7; 3; 1; 5] <> "arigatou" then
        failwith "Test Case 4 failed"
    if rearrangeString "art" [1; 0; 2] <> "rat" then
        failwith "Test Case 5 failed"

test ()