import argparse
from utils import *
import re

def extract_code_blocks(text):
    pattern = r'```(\w+)\s([\s\S]*?)```'
    matches = re.findall(pattern, text)
    code_blocks = []
    try:
        for match in matches:
            language, code = match
            code_blocks.append((language, code.strip()))
        assert len(code_blocks) == 2
        return code_blocks
    except:
        return None


if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', type=str, default='python.jsonl')
    parser.add_argument('--output', type=str, default='python_solution_msg.jsonl')
    args = parser.parse_args()

    dt = read_jsonl(args.input)

    datas = []
    for i, item in enumerate(dt):
        code_blocks = extract_code_blocks(item['output'])
        if code_blocks is None:
            continue
        (lang, demo_test_func), (_, full_test_func) = code_blocks
        item['demo_test_func'] = demo_test_func
        item['full_test_func'] = full_test_func
        del item['messages']
        del item['output']
        del item['demo_test_input']
        del item['demo_test_output']
        del item['full_test_input']
        del item['full_test_output']
        datas.append(item)
    write_jsonl(datas, args.output, mode='w')

