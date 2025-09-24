import json

wf = open('bench_lite_20.clean.jsonl', 'w', encoding='utf-8')

with open('bench_lite_20.jsonl') as infile:
    results = []
    for line in infile:
        j = json.loads(line)
        # import pdb
        # pdb.set_trace()
        dic = {
            "question": j['question'], 
            "canonical_solution": j['main_test_func'],
            "demo_test_func": j['demo_test_func'],
            'full_test_func':j['full_test_func'],
            'language': j['language'], 
            'dscoder-v2-lite_pass_cnt': j['dsv2_successed_cnt']
        }
        wf.write(json.dumps(dic, ensure_ascii=False)+'\n')

wf.close()