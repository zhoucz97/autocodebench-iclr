#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Unified JSONL Processor
"""

import json
import requests
import time
import argparse
import os
import logging
import re
import uuid
from typing import Dict, Any, List, Set
from multiprocessing import Pool, Manager, Queue
import multiprocessing as mp
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm
from prettytable import PrettyTable

# 设置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('unified_processor.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

# 全局处理器实例，避免重复创建
_global_processor = None


def init_worker(server_ip, server_port):
    """初始化工作进程"""
    global _global_processor
    _global_processor = UnifiedProcessor(server_ip, server_port)


def process_single_data_worker(args):
    """多进程工作函数，处理单条数据"""
    data, index, debug = args
    global _global_processor

    # 使用全局处理器实例
    result = _global_processor.process_data(data, debug)
    result["index"] = index
    result["original_data"] = data

    return result


def process_single_data_worker(data, index, debug, print_code=False):
    """单任务工作函数，处理一条数据"""
    global _global_processor

    result = _global_processor.process_data(data, debug, print_code)
    result["index"] = index
    result["original_data"] = data

    time.sleep(0.2)  # 适当延迟避免API过载
    return result


class UnifiedProcessor:
    def __init__(self, server_ip: str = "127.0.0.1", server_port: int = 8080):
        self.server_ip = server_ip
        self.server_port = server_port
        self.submit_url = f"http://{server_ip}:{server_port}/submit"
        self.headers = {
            "Content-Type": "application/json"
        }
        # 需要特殊处理的语言
        self.special_languages = []

    
    def read_jsonl_file(self, file_path: str, line_number: int = None, target_language: str = None) -> List[Dict[str, Any]]:
        """读取JSONL文件并返回数据列表"""
        data_list = []
        total_count = 0
        filtered_count = 0
    
        try:
            # 如果指定了行号，需要两遍处理：第一遍统计，第二遍选择
            if line_number is not None:
                # 第一遍：统计总数据和匹配数据
                with open(file_path, 'r', encoding='utf-8') as file:
                    for line_num, line in enumerate(file, 1):
                        line = line.strip()
                        if line:
                            try:
                                data = json.loads(line)
                                total_count += 1

                                # 语言过滤统计
                                if target_language:
                                    data_language = data.get("language", "").lower()
                                    if data_language == target_language.lower():
                                        filtered_count += 1
                                else:
                                    filtered_count += 1
                            except json.JSONDecodeError as e:
                                logger.error(f"第{line_num}行JSON解析错误: {e}")
                                continue

                # 第二遍：选择指定行号的数据
                current_filtered = 0
                with open(file_path, 'r', encoding='utf-8') as file:
                    for line_num, line in enumerate(file, 1):
                        line = line.strip()
                        if line:
                            try:
                                data = json.loads(line)

                                # 语言过滤
                                if target_language:
                                    data_language = data.get("language", "").lower()
                                    if data_language != target_language.lower():
                                        continue

                                current_filtered += 1
                                if current_filtered == line_number:
                                    # 添加绝对行号信息
                                    data['_absolute_line_number'] = line_num
                                    data['_relative_line_number'] = current_filtered
                                    data_list.append(data)
                                    break
                            except json.JSONDecodeError as e:
                                continue
            else:
                # 没有指定行号，需要两遍处理来正确计算相对行号
                # 第一遍：为每种语言建立行号映射
                language_line_mapping = {}  # {language: [(absolute_line, data), ...]}

                with open(file_path, 'r', encoding='utf-8') as file:
                    for line_num, line in enumerate(file, 1):
                        line = line.strip()
                        if line:
                            try:
                                data = json.loads(line)
                                total_count += 1

                                data_language = data.get("language", "").lower()

                                # 语言过滤
                                if target_language:
                                    if data_language != target_language.lower():
                                        continue

                                # 为每种语言建立映射
                                if data_language not in language_line_mapping:
                                    language_line_mapping[data_language] = []

                                language_line_mapping[data_language].append((line_num, data))
                                filtered_count += 1

                            except json.JSONDecodeError as e:
                                logger.error(f"第{line_num}行JSON解析错误: {e}")
                                continue

                # 第二遍：为每种语言的数据分配相对行号
                for language, lang_data_list in language_line_mapping.items():
                    for relative_line_num, (absolute_line_num, data) in enumerate(lang_data_list, 1):
                        # 添加绝对行号和相对行号信息（相对行号是在该语言内的行号）
                        data['_absolute_line_number'] = absolute_line_num
                        data['_relative_line_number'] = relative_line_num
                        data_list.append(data)

                # 按绝对行号排序，保持原始文件顺序
                data_list.sort(key=lambda x: x['_absolute_line_number'])

            if target_language:
                logger.info(f"语言过滤: {target_language} - 总数据{total_count}条，匹配{filtered_count}条，最终读取{len(data_list)}条")
            else:
                logger.info(f"成功读取{len(data_list)}条数据")
            return data_list
        except FileNotFoundError:
            logger.error(f"文件不存在: {file_path}")
            return []
        except Exception as e:
            logger.error(f"读取文件时发生错误: {e}")
            return []


    def extract_fields(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """提取需要的字段，如果直接提取失败则尝试从original_data字段获取"""
        def get_field_with_fallback(field_name: str, default_value: str = "") -> str:
            """从data或original_data中获取字段值"""
            # 首先尝试直接从data中获取
            value = data.get(field_name, "")
            if value:  # 如果有值就直接返回
                return value
            return default_value

        def get_solution_with_fallback() -> str:
            """从output字段提取代码块"""
            output = data.get("output", "")
            value = self._extract_code_from_output(output)
            if value:
                return value
            return ""

        return {
            "language": get_field_with_fallback("language", "").lower(),
            "full_test_func": get_field_with_fallback("full_test_func", ""),
            "demo_test_func": get_field_with_fallback("demo_test_func", ""),
            "main_test_func": get_solution_with_fallback()
        }

    def _extract_code_from_output(self, output: str) -> str:
        """从output字段中提取代码块，格式为 ```{language}\n{code}```"""
        if not output:
            return ""

        # 使用正则表达式匹配代码块
        matches = re.finditer(r'```(\w+)\n(.*?)```', output, flags=re.DOTALL)

        for match in matches:
            language = match.group(1)
            code = match.group(2).strip()
            if code:  # 如果提取到了代码，返回第一个非空的代码块
                return code

        # 如果没有匹配到标准格式，尝试简单的去掉首行处理
        # 先去掉开始和结尾的```符号
        cleaned_output = output.strip()
        if cleaned_output.startswith('```'):
            cleaned_output = cleaned_output[3:]
        if cleaned_output.endswith('```'):
            cleaned_output = cleaned_output[:-3]

        lines = cleaned_output.strip().split('\n')
        if len(lines) > 1:
            # 去掉第一行，返回剩余内容
            return '\n'.join(lines[1:]).strip()

        return cleaned_output.strip()


    def call_submit_api(self, data: Dict[str, Any], test_type: str = "full", debug: bool = False, print_code: bool = False) -> Dict[str, Any]:
        """调用submit接口"""
        try:
            language = data["language"]
            is_special_language = language in self.special_languages
            
            # 根据测试类型选择测试代码
            if test_type == "full":
                test_code = data["full_test_func"]
            elif test_type == "demo":
                test_code = data["demo_test_func"]
            else:
                raise ValueError(f"不支持的测试类型: {test_type}")
            
            if not is_special_language:
                # 普通语言：使用传统的func_code和main_code方式
                payload = {
                    "src_uid": f"bench_test_{test_type}_{int(time.time())}",
                    "func_code": data["main_test_func"],
                    "main_code": test_code,
                    "lang": language,
                    "show_log": "true",
                    "request_extensions": {"timeout": 30, "debug": str(debug).lower()}
                }
            
            # Debug模式下输出请求内容
            if debug:
                print(f"\n{'='*80}")
                print(f"🚀 DEBUG - REQUEST 请求内容:")
                print(f"{'='*80}")
                print(f"URL: {self.submit_url}")
                print(f"Headers: {json.dumps(self.headers, indent=2, ensure_ascii=False)}")
                print(f"Payload:")
                # 为了可读性，对payload进行格式化输出
                payload_copy = payload.copy()
                if 'source_code' in payload_copy and len(payload_copy['source_code']) > 500:
                    # 如果source_code太长，只显示前后部分
                    code = payload_copy['source_code']
                    payload_copy['source_code'] = f"{code[:200]}...\n[代码太长，省略中间部分，总长度: {len(code)} 字符]\n...{code[-200:]}"
                print(json.dumps(payload_copy, indent=2, ensure_ascii=False))
                print(f"{'='*80}\n")

            # logger.info(f"调用submit接口，测试类型: {test_type}, 语言: {language}, 特殊处理: {is_special_language}, debug: {debug}")
            response = requests.post(self.submit_url, headers=self.headers, json=payload, timeout=60)
            
            # Debug模式下输出响应内容
            if debug:
                print(f"\n{'='*80}")
                print(f"📥 DEBUG - RESPONSE 响应内容:")
                print(f"{'='*80}")
                print(f"Status Code: {response.status_code}")
                print(f"Response Headers: {dict(response.headers)}")
                try:
                    response_json = response.json()
                    print(f"Response Body:")
                    print(json.dumps(response_json, indent=2, ensure_ascii=False))
                except:
                    print(f"Response Text: {response.text}")
                print(f"{'='*80}\n")

            if response.status_code == 200:
                result = response.json()
                # logger.info(f"API调用成功，{test_type}，结果: {result.get('exec_outcome', 'unknown')}")
                return {
                    "success": True,
                    "response": result,
                    "status_code": response.status_code
                }
            else:
                logger.error(f"API调用失败，状态码: {response.status_code}, 响应: {response.text}")
                return {
                    "success": False,
                    "error": f"HTTP {response.status_code}: {response.text}",
                    "status_code": response.status_code
                }
                
        except requests.exceptions.Timeout:
            logger.error("API调用超时")
            return {
                "success": False,
                "error": "请求超时",
                "status_code": None
            }
        except requests.exceptions.RequestException as e:
            logger.error(f"API调用异常: {e}")
            return {
                "success": False,
                "error": str(e),
                "status_code": None
            }
        except Exception as e:
            logger.error(f"处理数据时发生错误: {e}")
            return {
                "success": False,
                "error": str(e),
                "status_code": None
            }
    
    def process_data(self, data: Dict[str, Any], debug: bool = False, print_code: bool = False) -> Dict[str, Any]:
        """处理单条数据，调用两次submit接口"""
        extracted_data = self.extract_fields(data)
        
        # 检查必要字段是否存在，提供更详细的错误信息
        missing_fields = []
        for field_name, field_value in extracted_data.items():
            if not field_value or field_value.strip() == "":
                missing_fields.append(field_name)

        if missing_fields:
            error_msg = f"缺少必要字段: {', '.join(missing_fields)}"
            logger.warning(f"{error_msg}，原始数据键: {list(data.keys())}")

            # 如果有original_data，也打印其键
            if "original_data" in data and isinstance(data["original_data"], dict):
                logger.warning(f"original_data键: {list(data['original_data'].keys())}")

            return {
                "success": False,
                "error": error_msg,
                "full_test_result": None,
                "demo_test_result": None,
                "language": extracted_data.get("language", "unknown"),
                "missing_fields": missing_fields
            }
        
        # 调用full_test_func
        full_test_result = self.call_submit_api(extracted_data, "full", debug, print_code)

        # 等待一秒再调用demo_test_func
        time.sleep(1)
        
        # 调用demo_test_func
        demo_test_result = self.call_submit_api(extracted_data, "demo", debug, print_code)

        # 判断整体是否成功（两个API调用都成功且代码执行都通过才算成功）
        full_api_success = full_test_result.get("success", False)
        demo_api_success = demo_test_result.get("success", False)
        
        # 检查代码执行结果
        full_exec_passed = (full_api_success and 
                           full_test_result.get("response", {}).get("exec_outcome") == "PASSED")
        demo_exec_passed = (demo_api_success and 
                           demo_test_result.get("response", {}).get("exec_outcome") == "PASSED")
        
        # 整体成功：两个API调用都成功且代码执行都通过
        overall_success = full_exec_passed and demo_exec_passed
        
        # 在指定行模式下，如果测试失败，打印完整的response信息
        if print_code and not overall_success:
            print(f"\n{'='*80}")
            print(f"❌ 测试失败详情 - {extracted_data['language'].upper()}")
            print(f"{'='*80}")

            # 打印FULL测试结果
            print(f"\n🔍 FULL 测试结果:")
            print(f"   API调用成功: {full_api_success}")
            print(f"   执行结果: {full_test_result.get('response', {}).get('exec_outcome', 'unknown')}")
            if full_test_result.get("response"):
                print(f"   完整响应:")
                print(json.dumps(full_test_result["response"], indent=2, ensure_ascii=False))
            elif full_test_result.get("error"):
                print(f"   错误信息: {full_test_result['error']}")

            # 打印DEMO测试结果
            print(f"\n🔍 DEMO 测试结果:")
            print(f"   API调用成功: {demo_api_success}")
            print(f"   执行结果: {demo_test_result.get('response', {}).get('exec_outcome', 'unknown')}")
            if demo_test_result.get("response"):
                print(f"   完整响应:")
                print(json.dumps(demo_test_result["response"], indent=2, ensure_ascii=False))
            elif demo_test_result.get("error"):
                print(f"   错误信息: {demo_test_result['error']}")

            print(f"{'='*80}\n")

        return {
            "success": overall_success,
            "full_test_result": full_test_result,
            "demo_test_result": demo_test_result,
            "language": extracted_data["language"]
        }
    
    def process_file(self, file_path: str, max_items: int = None, line_number: int = None,
                     debug: bool = False, concurrency: int = 5, target_language: str = None) -> List[Dict[str, Any]]:
        """处理整个JSONL文件"""
        logger.info(f"开始处理文件: {file_path}")
        if target_language:
            logger.info(f"语言过滤: 只处理 {target_language} 语言的数据")

        # 读取数据
        data_list = self.read_jsonl_file(file_path, line_number, target_language)
        if not data_list:
            return []
        
        # 限制处理数量（只有在没有指定行号时才生效）
        if max_items and line_number is None:
            data_list = data_list[:max_items]
            logger.info(f"限制处理数量为: {max_items}")
        
        # 如果只有一条数据或指定了行号，使用串行处理
        if len(data_list) == 1 or line_number is not None:
            logger.info("使用串行处理模式")
            return self._process_file_serial(data_list, line_number, debug)

        # 使用多进程处理
        logger.info(f"使用多进程处理模式，并发数: {concurrency}")
        return self._process_file_multiprocess(data_list, debug, concurrency)

    def _process_file_serial(self, data_list: List[Dict[str, Any]], line_number: int = None,
                           debug: bool = False) -> List[Dict[str, Any]]:
        """串行处理文件"""
        results = []

        # 判断是否为指定行模式（用于打印代码）
        is_single_line_mode = line_number is not None

        # 使用tqdm显示进度
        desc = f"处理第{line_number}行数据" if line_number else "串行处理"
        with tqdm(total=len(data_list), desc=desc, unit="条") as pbar:
            for i, data in enumerate(data_list, 1):
                result = self.process_data(data, debug, print_code=is_single_line_mode)
                result["index"] = i
                result["original_data"] = data
                results.append(result)

                # 更新进度条
                pbar.update(1)
                pbar.set_postfix({
                    "成功": sum(1 for r in results if r.get("success", False)),
                    "失败": sum(1 for r in results if not r.get("success", False))
                })

                # 在每次处理之间稍作等待，避免过于频繁的请求
                if i < len(data_list):
                    time.sleep(0.5)

        logger.info(f"串行处理完成，共处理{len(results)}条数据")
        return results
    
    def _process_file_multiprocess(self, data_list: List[Dict[str, Any]], debug: bool = False,
                                 concurrency: int = 5) -> List[Dict[str, Any]]:
        """多进程处理文件 - 简化版本"""
        total_items = len(data_list)

        # 如果数据量小，直接使用串行处理
        if total_items < concurrency:
            logger.info(f"数据量较小({total_items}条)，使用串行处理")
            return self._process_file_serial(data_list, debug=debug)

        logger.info(f"启动{concurrency}个进程处理{total_items}条数据")

        results = []
        try:
            # 使用进程池，每个任务处理一条数据
            with Pool(processes=concurrency, initializer=init_worker, initargs=(self.server_ip, self.server_port)) as pool:
                # 使用tqdm显示进度
                with tqdm(total=total_items, desc=f"多进程处理({concurrency}进程)", unit="条") as pbar:
                    # 提交所有任务
                    futures = []
                    for i, data in enumerate(data_list, 1):
                        future = pool.apply_async(process_single_data_worker, (data, i, debug, False))
                        futures.append(future)

                    # 收集结果
                    for future in futures:
                        try:
                            result = future.get(timeout=300)  # 5分钟超时
                            results.append(result)
                            pbar.update(1)

                            # 更新进度条统计
                            pbar.set_postfix({
                                "成功": sum(1 for r in results if r.get("success", False)),
                                "失败": sum(1 for r in results if not r.get("success", False))
                            })
                        except Exception as e:
                            logger.error(f"任务失败: {e}")
                            # 创建失败结果
                            failed_result = {
                                "index": len(results) + 1,
                                "success": False,
                                "error": str(e),
                                "original_data": {}
                            }
                            results.append(failed_result)
                            pbar.update(1)

        except Exception as e:
            logger.error(f"多进程处理时发生错误: {e}")
            # 如果多进程失败，回退到串行处理
            logger.info("回退到串行处理模式")
            return self._process_file_serial(data_list, debug=debug)

        # 按index排序结果
        results.sort(key=lambda x: x.get("index", 0))

        logger.info(f"多进程处理完成，共处理{len(results)}条数据")
        return results

    def save_results(self, results: List[Dict[str, Any]], output_file: str):
        """保存处理结果到文件"""
        try:
            with open(output_file, 'w', encoding='utf-8') as f:
                for result in results:
                    # 简化输出格式，只保留必要信息
                    simplified_result = {
                        "index": result.get("index", 0),
                        "language": result.get("language", ""),
                        "success": result.get("success", False),
                        "full_test_outcome": result.get("full_test_result", {}).get("response", {}).get("exec_outcome", "unknown"),
                        "demo_test_outcome": result.get("demo_test_result", {}).get("response", {}).get("exec_outcome", "unknown"),
                        "full_test_result": result.get("full_test_result", {}),
                        "demo_test_result": result.get("demo_test_result", {}),
                        "original_data": result.get("original_data", {})
                    }
                    f.write(json.dumps(simplified_result, ensure_ascii=False) + '\n')
            logger.info(f"结果已保存到: {output_file}")
        except Exception as e:
            logger.error(f"保存结果时发生错误: {e}")

    def print_detailed_statistics(self, results: List[Dict[str, Any]]):
        """打印详细的统计报告表格"""
        if not results:
            print("\n❌ 没有处理任何数据")
            return

        # 按语言分组统计
        language_stats = {}
        failed_items = []

        for result in results:
            language = result.get("language", "unknown")
            success = result.get("success", False)
            index = result.get("index", 0)

            # 初始化语言统计
            if language not in language_stats:
                language_stats[language] = {
                    "total": 0,
                    "success": 0,
                    "failed": 0,
                    "full_passed": 0,
                    "demo_passed": 0,
                    "both_passed": 0,
                    "failed_indices": []
                }

            # 更新统计
            stats = language_stats[language]
            stats["total"] += 1

            if success:
                stats["success"] += 1
            else:
                stats["failed"] += 1
                # 获取绝对行号和相对行号
                absolute_line = result.get("original_data", {}).get("_absolute_line_number", index)
                relative_line = result.get("original_data", {}).get("_relative_line_number", index)

                stats["failed_indices"].append({
                    "absolute_line": absolute_line,
                    "relative_line": relative_line
                })
                failed_items.append({
                    "index": index,
                    "absolute_line": absolute_line,
                    "relative_line": relative_line,
                    "language": language,
                    "full_outcome": result.get("full_test_result", {}).get("response", {}).get("exec_outcome", "unknown"),
                    "demo_outcome": result.get("demo_test_result", {}).get("response", {}).get("exec_outcome", "unknown"),
                    "full_error": result.get("full_test_result", {}).get("error", ""),
                    "demo_error": result.get("demo_test_result", {}).get("error", "")
                })

            # 详细测试结果统计
            full_outcome = result.get("full_test_result", {}).get("response", {}).get("exec_outcome", "")
            demo_outcome = result.get("demo_test_result", {}).get("response", {}).get("exec_outcome", "")

            if full_outcome == "PASSED":
                stats["full_passed"] += 1
            if demo_outcome == "PASSED":
                stats["demo_passed"] += 1
            if full_outcome == "PASSED" and demo_outcome == "PASSED":
                stats["both_passed"] += 1

        # 打印总体统计
        total_items = len(results)
        total_success = sum(1 for r in results if r.get("success", False))
        total_failed = total_items - total_success

        print("\n" + "="*80)
        print("🎯 执行结果统计报告")
        print("="*80)

        print(f"\n📊 总体统计:")
        print(f"   总处理数据: {total_items} 条")
        print(f"   成功数据:   {total_success} 条 ({total_success/total_items*100:.1f}%)")
        print(f"   失败数据:   {total_failed} 条 ({total_failed/total_items*100:.1f}%)")

        # 使用 PrettyTable 打印各语言详细统计表格
        print(f"\n📋 各语言详细统计:")
        language_table = PrettyTable()
        language_table.field_names = ["语言", "总数", "成功", "失败", "成功率", "Demo通过", "Full通过", "双通过"]
        language_table.align = "l"
        language_table.align["总数"] = "r"
        language_table.align["成功"] = "r"
        language_table.align["失败"] = "r"
        language_table.align["成功率"] = "r"
        language_table.align["Demo通过"] = "r"
        language_table.align["Full通过"] = "r"
        language_table.align["双通过"] = "r"

        # 按语言名称排序添加数据
        for language in sorted(language_stats.keys()):
            stats = language_stats[language]
            success_rate = stats["success"] / stats["total"] * 100 if stats["total"] > 0 else 0

            language_table.add_row([
                language,
                stats["total"],
                stats["success"],
                stats["failed"],
                f"{success_rate:.1f}%",
                stats["demo_passed"],
                stats["full_passed"],
                stats["both_passed"]
            ])

        print(language_table)

        # 使用 PrettyTable 打印失败数据详情
        if failed_items:
            print(f"\n❌ 失败数据详情 (共{len(failed_items)}条):")
            failed_table = PrettyTable()
            failed_table.field_names = ["绝对行号", "相对行号", "语言", "Full测试", "Demo测试", "错误信息"]
            failed_table.align = "l"
            failed_table.align["绝对行号"] = "r"
            failed_table.align["相对行号"] = "r"
            failed_table.max_width["错误信息"] = 40

            for item in sorted(failed_items, key=lambda x: x["absolute_line"]):  # 按绝对行号排序
                # 获取主要错误信息
                error_msg = ""
                if item["full_error"]:
                    error_msg = item["full_error"]
                elif item["demo_error"]:
                    error_msg = item["demo_error"]
                else:
                    error_msg = "执行未通过"

                # 截断过长的错误信息
                if len(error_msg) > 48:
                    error_msg = error_msg[:45] + "..."

                failed_table.add_row([
                    item["absolute_line"],
                    item["relative_line"],
                    item["language"],
                    item["full_outcome"],
                    item["demo_outcome"],
                    error_msg
                ])

            print(failed_table)

            # 按语言分组显示失败行号
            print(f"\n📍 各语言失败行号:")
            for language in sorted(language_stats.keys()):
                if language_stats[language]["failed_indices"]:
                    failed_indices = language_stats[language]["failed_indices"]
                    # 按绝对行号排序
                    failed_indices_sorted = sorted(failed_indices, key=lambda x: x["absolute_line"])

                    print(f"   {language}:")
                    for idx_info in failed_indices_sorted:
                        abs_line = idx_info["absolute_line"]
                        rel_line = idx_info["relative_line"]
                        print(f"     文件行号: {abs_line}, 语言行号: {rel_line} (--line {rel_line})")
        else:
            print(f"\n✅ 所有数据都处理成功！")

        print("\n" + "="*80)


def main():
    parser = argparse.ArgumentParser(description='统一JSONL文件处理器（支持全部语言）')
    parser.add_argument('-i', '--input_file', help='输入的JSONL文件路径')
    parser.add_argument('-o', '--output', help='输出文件路径')
    parser.add_argument('-m', '--max-items', type=int, help='最大处理数量')
    parser.add_argument('-l', '--line', type=int, help='指定处理第几行数据（从1开始）')
    parser.add_argument('--server_ip', help='服务器IP地址', default='21.86.44.177')
    parser.add_argument('--server_port', type=int, help='服务器端口', default=8080)
    parser.add_argument('-d', '--debug', action='store_true', help='启用debug模式')
    parser.add_argument('-c', '--concurrency', type=int, default=5, help='并发进程数（默认5）')
    parser.add_argument('--lang', help='指定处理的编程语言，只处理该语言的数据')

    args = parser.parse_args()
    
    # 检查输入文件是否存在
    if not os.path.exists(args.input_file):
        logger.error(f"输入文件不存在: {args.input_file}")
        return
    
    # 验证并发数
    if args.concurrency < 1:
        logger.error("并发数必须大于0")
        return
    if args.concurrency > 20:
        logger.warning("并发数过高可能对服务器造成压力，建议不超过20")

    # 创建处理器
    processor = UnifiedProcessor(args.server_ip, args.server_port)

    # 处理文件
    results = processor.process_file(args.input_file, args.max_items, args.line, args.debug, args.concurrency, args.lang)

    # 确定输出文件名
    if args.output:
        output_file = args.output
    else:
        # 从输入文件名提取语言信息，生成带语言前缀的输出文件名
        input_basename = os.path.basename(args.input_file)
        base_name = input_basename.replace('.jsonl', '')  # 例如：typescript.jsonl -> typescript

        # 如果指定了语言过滤，在文件名中体现
        if args.lang:
            output_file = f"{base_name}_{args.lang}_results.jsonl"
        else:
            output_file = f"{base_name}_results.jsonl"

    # 保存结果
    if results:
        processor.save_results(results, output_file)
        
        # 生成详细统计报告
        processor.print_detailed_statistics(results)
    else:
        logger.warning("没有处理任何数据")


if __name__ == "__main__":
    # 执行主函数
    main()