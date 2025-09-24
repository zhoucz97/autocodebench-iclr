import json
import csv
import os
import random

def jsonl_to_csv(jsonl_filepath, csv_filepath):
    fields = ['task_id', 'index', 'tag', 'question', 'solution', 'test_function',  'code', 'critic', 'exec_result', 'output']
    
    data_to_write = []
    
    try:
        with open(jsonl_filepath, 'r', encoding='utf-8') as infile:
            for line_num, line in enumerate(infile, 1):
                try:
                    record = json.loads(line.strip())
                    row = {}
                    for field in fields:
                        row[field] = record.get(field, None)  # 使用.get()方法，如果字段不存在则返回None
                    data_to_write.append(row)
                except json.JSONDecodeError as e:
                    print(f"Error decoding JSON on line {line_num} in {jsonl_filepath}: {e}")
                except Exception as e:
                    print(f"An unexpected error occurred on line {line_num} in {jsonl_filepath}: {e}")

        if not data_to_write:
            print(f"No data was successfully processed from {jsonl_filepath}. CSV file will not be created.")
            return

        random.shuffle(data_to_write)
        with open(csv_filepath, 'w', newline='', encoding='utf-8') as outfile:
            writer = csv.DictWriter(outfile, fieldnames=fields)
            
            writer.writeheader()  # 写入CSV文件头
            writer.writerows(data_to_write) # 写入数据
        
        print(f"Successfully converted '{jsonl_filepath}' to '{csv_filepath}'")

    except FileNotFoundError:
        print(f"Error: The file '{jsonl_filepath}' was not found.")
    except Exception as e:
        print(f"An error occurred: {e}")


# --- 使用示例 ---
if __name__ == "__main__":
    # 确保你的JSONL文件存在，并替换为你的文件路径
    input_jsonl_file = '/apdcephfs_qy4/share_302593112/wxyeszhou/autoCodeBench/autoCodeBench/data/pyedu0605/critic/pyedu200-v3critic-250609.jsonl' 
    output_csv_file = '/apdcephfs_qy4/share_302593112/wxyeszhou/autoCodeBench/autoCodeBench/data/pyedu0605/critic/pyedu200-v3critic-250609.csv'

    jsonl_to_csv(input_jsonl_file, output_csv_file)

    # 验证CSV文件是否成功创建
    if os.path.exists(output_csv_file):
        print(f"\nContent of '{output_csv_file}':")
        with open(output_csv_file, 'r', encoding='utf-8') as f:
            print(f.read())