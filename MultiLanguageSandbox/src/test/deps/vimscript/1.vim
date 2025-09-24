
" return "Hello, MMCODEEVAL: Masssively Multilingual Code Evaluation"

function Hello_mmcodeeval() abort
    return "Hello, MMCODEEVAL: Massively Multilingual Code Evaluation"
endfunction

if assert_equal("Hello, MMCODEEVAL: Masssively Multilingual Code Evaluation", Hello_mmcodeeval()) | cq | endif
exit(0)