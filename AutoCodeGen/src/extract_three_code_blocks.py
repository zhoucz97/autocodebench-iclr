import argparse
from utils import *
import re

def extract_code_blocks(text):
    # 正则表达式匹配以 ```{language} 和 ``` 包裹的代码块
    pattern = r'```(\w+)\s([\s\S]*?)```'
    matches = re.findall(pattern, text)
    code_blocks = []
    try:
        for match in matches:
            language, code = match
            code_blocks.append((language, code.strip()))
        assert len(code_blocks) == 3
        return code_blocks
    except:
        return None


if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', type=str, default='python.jsonl')
    parser.add_argument('--output', type=str, default='python_solution_msg.jsonl')
    args = parser.parse_args()

    dt = read_jsonl(args.input)

    for i, item in enumerate(dt):
        code_blocks = extract_code_blocks(item['output'])
        if code_blocks is None:
            continue
        (lang, solution), (_, demo_test_input), (_, full_test_input) = code_blocks
        item['canonical_solution'] = f"```{lang}\n" + solution + "\n```"
        item['demo_test_func'] = demo_test_input
        item['full_test_func'] = full_test_input
        item['language'] = item['lang']
        del item['messages']
        del item['output']
        del item['lang']
        write_jsonl([item], args.output, mode='a')
    
