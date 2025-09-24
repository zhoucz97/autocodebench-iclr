# -*- coding: utf-8 -*-

# -*- coding: utf-8 -*-

import os
import copy
import time
import uuid
import json
import requests
import sseclient
import argparse
from tqdm import tqdm
from concurrent.futures import ThreadPoolExecutor
import concurrent.futures
import random
from argparse import ArgumentParser
# from openai import OpenAI


stream = True
Hunyuan = True
ss_url = "http://stream-server-online-openapi.turbotke.production.polaris:1081/openapi/chat/completions"
# model = "ds_0324_64k_quant_nj_rayne"
model = "ds_r1_0528_teg_0.5.1_w4a8_tiantianzhu_nj"
# model = "7b_dense_v1_epoch3-longctx_merge_v1-32k-v0710-sft-longctx"
headers = {
        "Content-Type": "application/json",
        "Authorization": "Bearer 7auGXNATFSKl7dF",
        "Wsid": "10103",
        }



def requests_api(entry):
    # yuanbao_prompt = """你是一个由腾讯开发的有用的人工智能助手，你的名字是“腾讯元宝”，简称“元宝”，你的英文名是“Tencent Yuanbao”，你乐于帮助大家解答问题。\n现在的时间是2025-03-14 00:50:00 周五"""
    messages = entry['messages']
    # json_data = {
    #     "model": model,
    #     "query_id": "test_query_id_" + str(uuid.uuid4()),
    #     "messages": messages,
    #     "temperature": 0.7,
    #     "top_p": 0.8,
    #     "top_k": 20,
    #     "output_seq_len": 16384,
    #     "max_input_seq_len": 8192,
    #     "stream": stream
    # }
    # # # r1推荐参数
    # json_data = {
    #     "model": model,
    #     "query_id": "test_query_id_" + str(uuid.uuid4()),
    #     "messages": messages,
    #     "temperature": 0.6,
    #     "top_p": 0.8,
    #     "top_k": 20,
    #     "output_seq_len": 32768,
    #     "max_input_seq_len": 16384,
    #     "stream": stream
    # }

    # # qwen3推荐参数
    # json_data = {
    #     "model": model,
    #     "query_id": "test_query_id_" + str(uuid.uuid4()),
    #     "messages": messages,
    #     "temperature": 0.6,
    #     "top_p": 0.95,
    #     "top_k": 20,
    #     "output_seq_len": 32768,
    #     "max_input_seq_len": 8192,
    #     "stream": stream
    # }
    # greedy
    json_data = {
        "model": model,
        "query_id": "test_query_id_" + str(uuid.uuid4()),
        "messages": messages,
        "temperature": 0.1,
        "top_p": 1,
        "top_k": 1,
        "output_seq_len": 8192,
        "max_input_seq_len": 8192,
        "stream": stream
    }
        

    resp = requests.post(ss_url, headers=headers, json=json_data, stream=stream)

    if stream == True:
        client = sseclient.SSEClient(resp)
        stream_text = []
        for event in client.events():
            if event.data not in ['', None, '[DONE]']:
                data_js = json.loads(event.data)
                if "error" in data_js:
                    print(data_js["error"])
                    raise Exception(str(data_js["error"]))
                try:
                    # print(data_js)
                    chunk = data_js['choices'][0]['delta']['content']
                    stream_text.append(chunk)
                except Exception as e:
                    if 'event' in data_js:
                        if data_js['event'].get('name', '') == 'thinking' and data_js['event'].get('state', -1) == 0:
                            chunk = '<think>\n'
                            stream_text.append(chunk)
                            continue
                        elif data_js['event'].get('name', '') == 'thinking' and data_js['event'].get('state', -1) == 2:
                            chunk = '</think>\n'
                            stream_text.append(chunk)
                            continue
            # print(stream_text)
        model_output = ''.join(stream_text)
    else:
        model_output = resp.json()["choices"][0]["message"]["content"]
    output = model_output.split("</think>")[-1].strip()

    # import pdb
    # pdb.set_trace()
    # print(model_output)
    # entry['generations'] = [model_output]
    entry['output'] = output
    # if '<think>' in model_output:
    #     entry['output_with_think'] = model_output
    return entry


if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument("--input",  default="xx", type=str)
    parser.add_argument("--output", default="xx", type=str)

    args = parser.parse_args()
    input_file = args.input
    output_file = args.output
    
    print(output_file)

    data = []
    with open(input_file, 'r',encoding='utf-8') as file:
        for line in file:
            d = json.loads(line)
            data.append(d)
        # data = [json.loads(line) for line in file]
    data = data#[:1] # * 10
    # data = random.sample(data, 3)

    print(len(data))


    output_file = open(output_file, "a", encoding='utf8')
    with ThreadPoolExecutor(max_workers=1) as executor:
        future_to_entry = {executor.submit(requests_api, copy.deepcopy(entry)): entry for entry in tqdm(data)}
        for future in tqdm(concurrent.futures.as_completed(future_to_entry)):
            entry = future_to_entry[future]
            try:
                updated_entry = future.result()
                if updated_entry['output'] != "":
                    output_file.write(json.dumps(updated_entry, ensure_ascii=False) + "\n")
            except Exception as e:
                print(repr(e))
    output_file.close()