
let multiply (a: int) (b: int): int =
    if a < 1 || a > 50000 || b < 1 || b > 50000 then
        failwith "Inputs must be between 1 and 50,000"
    a * b
```

This function:
1. Takes two integer parameters `a` and `b`
2. Checks if either input is outside the valid range (1-50,000)
3. If invalid, throws an exception with an error message
4. Otherwise, returns the product of `a` and `b`

The `int` type in F# (and .NET) is a 32-bit signed integer with a maximum value of 2,147,483,647. Since the maximum possible product (50,000 * 50,000 = 2,500,000,000) exceeds this limit, this implementation would actually fail for the maximum values. 

To properly handle the full range without overflow, you should use the `int64` type instead:

```fsharp
let multiply (a: int) (b: int): int64 =
    if a < 1 || a > 50000 || b < 1 || b > 50000 then
        failwith "Inputs must be between 1 and 50,000"
    int64 a * int64 b


let check () =
    if multiply 2 3 <> 6 then
        failwith "Test Case 1 failed"
    if multiply 100 200 <> 20000 then
        failwith "Test Case 2 failed"
    if multiply 1 50000 <> 50000 then
        failwith "Test Case 3 failed"
    if multiply 250 200 <> 50000 then
        failwith "Test Case 4 failed"
    if multiply 123 456 <> 56088 then
        failwith "Test Case 5 failed"
    if multiply 500 100 <> 50000 then
        failwith "Test Case 6 failed"

check ()