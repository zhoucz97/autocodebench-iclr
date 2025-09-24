#!/usr/bin/env python3

import json
import os
import sys
import requests
import threading
import concurrent.futures
import time
from language import get_language, get_file_extension
import argparse

test_cases = {}

erlang_test_cases = [
    "has_close_elements.erl",
    "arithmetic_sequence.erl",
    "array_intersection.erl",
    "automorphic_numbers.erl",
    "average_without_extremes.erl",
    "binary_to_hex.erl",
    "bit_shift.erl",
    "convert_and_concat.erl",
    "cow_conference.erl",
    "extract_numbers.erl",
    "fibonacci.erl",
    "filter_odds.erl",
    "find_duplicates.erl",
    "insert_element.erl",
    "is_prefix.erl",
    "leap_year_sum.erl",
    "list_occurrence.erl",
    "merge_lists.erl",
    "multiply_evens.erl",
    "prime_numbers.erl",
    "quick_sort.erl",
    "ranking_comparison.erl",
    "remove_duplicates.erl",
    "replace_element.erl",
    "reverse_after_position.erl",
    "split_string.erl",
    "sum_odds.erl",
    "sum_values_by_key.erl",
    "third_largest.erl",
    "two_sum.erl",
    "absolute_value.erl",
    "arithmetic_sum.erl",
    "ascii_to_char.erl",
    "binary_search.erl",
    "char_to_ascii.erl",
    "expression_calc.erl",
    "fish_danger.erl",
    "fish_numbers_game.erl",
    "int_bool_conversion.erl",
    "median_without_extremes.erl",
    "minimum_n.erl",
    "monotonic_list.erl",
    "multiply.erl",
    "number_sign.erl",
    "pen_purchase.erl",
    "power_of_two.erl",
    "reverse_number.erl",
    "swimming_distance.erl",
    "swimming_steps.erl",
    "swimming_time.erl",
]

def generate_test_filename(language):
    if language not in test_cases:
        test_cases[language] = 0
    test_cases[language] += 1
    extension = get_file_extension(language)
    return f"test/code/test_{test_cases[language]}{extension}"

def test_language(server_ip, server_port, lang_dir_name, execution_results, task_results, awk_test_cases, debug=False, timeout=5):
    """测试单个语言的函数"""
    normalized_lang = get_language(lang_dir_name)

    # 初始化语言的测试结果存储
    if normalized_lang not in execution_results:
        execution_results[normalized_lang] = {}

    lang_dir_path = os.path.join("test/deps", lang_dir_name)
    if not os.path.isdir(lang_dir_path):
        return

    print(f"\n开始测试 {normalized_lang} 语言...")

    # 处理当前语言目录下的所有测试文件
    file_extension = get_file_extension(normalized_lang)
    for code_file in os.listdir(lang_dir_path):
        # 筛选有效的测试文件
        if not (code_file.endswith(file_extension) or code_file.endswith(".txt")):
            continue

        # 根据不同语言处理测试用例
        if normalized_lang == "erlang":
            # Erlang测试用例特殊处理
            try:
                task_index = str(erlang_test_cases.index(code_file) + 1)
            except ValueError:
                continue  # 如果不在预定义列表中，跳过

            code_path = os.path.join(lang_dir_path, code_file)
            print(f"测试Erlang用例 {task_index}: {code_path}")

            with open(code_path, "r") as f:
                source_code = f.read()

            full_task_id = f"{normalized_lang}/{task_index}"

        elif normalized_lang == "awk":
            # AWK测试用例特殊处理
            if not code_file.startswith("awk"):
                continue

            task_index = code_file.split(".")[0].split('k')[1]
            source_code = awk_test_cases[task_index]["code"]
            full_task_id = awk_test_cases[task_index]["task_id"]

        else:
            # 其他语言的通用处理
            task_index = code_file.split(".")[0]
            code_path = os.path.join(lang_dir_path, code_file)

            with open(code_path, "r") as f:
                source_code = f.read()

            full_task_id = f"{normalized_lang}/{task_index}"

        # 构建HTTP请求负载
        payload = {
            "src_uid": full_task_id,
            "lang": normalized_lang,
            "source_code": source_code,
            "show_log": True,
            "request_extensions": {
                "timeout": timeout,
                "debug": debug
            }
        }

        # 发送HTTP请求并处理结果
        try:
            url = f"http://{server_ip}:{server_port}/submit"
            response = requests.post(url, json=payload, timeout=30)
            response_data = response.json()
            print(json.dumps(response_data))

            # 检查执行结果
            success = response_data.get("exec_outcome") == "PASSED"
            execution_results[normalized_lang][task_index] = success
        except Exception as e:
            print(f"请求失败: {e}")
            execution_results[normalized_lang][task_index] = False

    print(f"{normalized_lang} 语言测试完成")
    return execution_results

def run_test(server_ip, server_port, debug, language, timeout):
    """
    通过HTTP请求运行测试用例并验证结果

    参数:
        server_ip: 服务器IP地址
        language: 指定要测试的编程语言，默认为None（测试所有语言）
    """
    start_time = time.time()

    # 如果指定了语言，进行标准化处理
    target_language = get_language(language) if language is not None else None

    # 获取所有可用语言目录
    available_languages = os.listdir("test/deps")

    # 筛选目标语言目录
    if target_language is not None:
        available_languages = [lang for lang in available_languages
                              if get_language(lang) == target_language]

    # 读取标准测试结果数据
    standard_results = {}
    task_results = {}
    with open("test/deps/_detail.jsonl", "r") as f:
        for line in f:
            lang_name = get_language(line.split("\t")[0])
            test_data = json.loads(line.split("\t")[1])
            standard_results[lang_name] = test_data

            if lang_name not in task_results:
                task_results[lang_name] = {}

            for item in test_data:
                task_id = item["task_id"].split("/")[1]
                task_results[lang_name][task_id] = item["pass"]

    # 存储测试执行结果，使用线程安全的字典
    execution_results = {}
    results_lock = threading.Lock()  # 用于保护共享的execution_results

    # 特殊处理AWK语言的测试用例
    awk_test_cases = {}
    if os.path.exists("test/AWK.jsonl"):
        with open("test/AWK.jsonl", "r") as f:
            for line in f:
                data = json.loads(line)
                task_id = data["task_id"].split("/")[1]
                awk_test_cases[task_id] = {
                    "code": data["canonical_solution"],
                    "task_id": data["task_id"]
                }

    # 修改test_language函数的包装器，使其适配多线程调用
    def thread_safe_test_language(lang_dir_name):
        # 使用线程安全的方式更新结果
        result = test_language(server_ip, server_port, lang_dir_name, {}, task_results, awk_test_cases, debug, timeout)
        if result is None:
            return
        with results_lock:
            for lang, tests in result.items():
                if lang not in execution_results:
                    execution_results[lang] = {}
                for test_id, test_result in tests.items():
                    execution_results[lang][test_id] = test_result

    print(f"开始使用多线程测试 {len(available_languages)} 种语言...")

    # 创建并启动线程
    threads = []
    for lang_dir_name in available_languages:
        thread = threading.Thread(target=thread_safe_test_language,
                                args=(lang_dir_name,),
                                name=f"Thread-{get_language(lang_dir_name)}")
        threads.append(thread)
        thread.start()

    # 等待所有线程完成
    for thread in threads:
        thread.join()

    end_time = time.time()
    total_time = end_time - start_time
    print(f"\n所有语言测试完成，总耗时: {total_time:.2f}秒")

    # 输出结果比较
    GREEN = "\033[32m"
    YELLOW = "\033[33m"
    RESET = "\033[0m"

    # 获取需要输出结果的语言列表
    languages_to_report = [target_language] if target_language is not None else list(execution_results.keys())

    for lang in languages_to_report:
        # 检查该语言是否有执行结果
        if lang not in execution_results:
            print(f"\n{lang}语言未测试或没有测试结果")
            continue

        print(f"\n{lang}语言测试结果比较:")
        print("=" * 50)
        print("任务ID | 标准结果 | 执行结果 | 一致性")
        print("-" * 50)

        # 检查该语言是否有标准对比结果
        if lang not in task_results:
            print(f"没有{lang}语言的标准测试结果用于对比")
            continue

        for task_index in task_results[lang]:
            standard_result = task_results[lang][task_index]

            if task_index in execution_results[lang]:
                actual_result = execution_results[lang][task_index]
                is_consistent = standard_result == actual_result

                color = GREEN if is_consistent else YELLOW
                consistency_text = "一致" if is_consistent else "不一致"

                print(f"{task_index} | {standard_result} | {actual_result} | {color}{consistency_text}{RESET}")
            else:
                print(f"{task_index} | {standard_result} | 未执行 | {YELLOW}未测试{RESET}")

        print("=" * 50)

    # 打印统计信息
    total_tests = 0
    total_passed = 0
    total_consistent = 0

    print("\n测试统计信息:")
    print("=" * 60)
    print("语言 | 测试数 | 通过数 | 一致性")
    print("-" * 60)

    for lang in sorted(execution_results.keys()):
        if lang not in task_results:
            continue

        lang_tests = 0
        lang_passed = 0
        lang_consistent = 0

        for test_id, result in execution_results[lang].items():
            if test_id in task_results[lang]:
                lang_tests += 1
                total_tests += 1

                if result:
                    lang_passed += 1
                    total_passed += 1

                if result == task_results[lang][test_id]:
                    lang_consistent += 1
                    total_consistent += 1

        if lang_tests > 0:
            lang_pass_rate = lang_passed / lang_tests * 100
            lang_consistent_rate = lang_consistent / lang_tests * 100
            print(f"{lang.ljust(10)} | {lang_tests:3d} | {lang_passed:3d} ({lang_pass_rate:5.1f}%) | {lang_consistent:3d} ({lang_consistent_rate:5.1f}%)")

    print("-" * 60)
    if total_tests > 0:
        total_pass_rate = total_passed / total_tests * 100
        total_consistent_rate = total_consistent / total_tests * 100
        print(f"总计 | {total_tests:3d} | {total_passed:3d} ({total_pass_rate:5.1f}%) | {total_consistent:3d} ({total_consistent_rate:5.1f}%)")

    print("=" * 60)
    print(f"总耗时: {total_time:.2f}秒")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='运行测试用例')
    parser.add_argument('--server_ip', type=str, default="21.86.44.198", help='服务器IP地址, 默认21.86.44.198')
    parser.add_argument('--server_port', type=int, default=8080, help='服务器端口, 默认8080')
    parser.add_argument('--lang', type=str, default=None, help='编程语言, 默认None代表全部语言')
    parser.add_argument('--timeout', type=int, default=5, help='测试用例超时时间, 默认5秒')
    parser.add_argument('--debug', action='store_true', help='调试模式, 默认False代表不保留运行环境')

    args = parser.parse_args()
    run_test(args.server_ip, args.server_port, args.debug, args.lang, args.timeout)
