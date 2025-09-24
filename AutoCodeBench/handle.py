import json


with open("bench_20.jsonl", "r") as f, open("bench_20_prompt.jsonl", "w") as f_out:
    for line in f:
        data = json.loads(line)
        prompt = "You are an expert programmer. Your task is to provide a code solution within a single Markdown code block for the given programming problem. Do not include any direct execution commands, test cases, or usage examples within the code block."
        question = prompt + "\n\n" + data["question"].strip()
        f_out.write(json.dumps({"prompt": question}, ensure_ascii=False) + "\n")