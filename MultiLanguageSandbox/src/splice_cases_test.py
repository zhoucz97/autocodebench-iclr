#!/usr/bin/env python3
"""
独立的案例测试工具
读取指定目录下的所有jsonl文件，请求远端submit接口，并统计一致率

依赖库:
pip install requests tqdm prettytable
"""

import os
import json
import requests
import argparse
import glob
from typing import Dict, List, Tuple, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading
from tqdm import tqdm
import time
from prettytable import PrettyTable

class CaseTest:
    def __init__(self, server: str = "21.86.44.175", port: int = 8080, timeout: int = 30, debug: bool = False, splice: bool = False):
        """
        初始化测试工具

        Args:
            server: 服务器地址
            port: 服务器端口
            timeout: submit接口的执行超时时间（秒）
            debug: 调试模式开关
            splice: 拼接模式开关
        """
        self.submit_url = f"http://{server}:{port}/submit"
        self.server = server
        self.port = port
        self.timeout = timeout
        self.debug = debug
        self.splice = splice
        self.request_timeout = 60  # HTTP请求超时时间固定为60秒
        self.lock = threading.Lock()

    def read_jsonl_files(self, directory: str = ".", specific_file: str = None, specific_line: int = None) -> Dict[str, List[Dict]]:
        """
        读取指定目录下的所有jsonl文件或指定的单个文件

        Args:
            directory: 目录路径，默认为当前目录
            specific_file: 指定的单个文件名或路径
            specific_line: 指定的行号（仅读取该行数据）

        Returns:
            Dict[文件名, 数据列表]
        """
        files_data = {}

        if specific_file:
            # 处理指定文件
            if not os.path.isabs(specific_file):
                # 如果不是绝对路径，则相对于指定目录
                file_path = os.path.join(directory, specific_file)
            else:
                file_path = specific_file

            if not os.path.exists(file_path):
                print(f"指定文件不存在: {file_path}")
                return files_data

            if not file_path.endswith('.jsonl'):
                print(f"指定文件不是 .jsonl 格式: {file_path}")
                return files_data

            jsonl_files = [file_path]
            print(f"使用指定文件: {specific_file}")
        else:
            # 查找所有jsonl文件
            pattern = os.path.join(directory, "*.jsonl")
            jsonl_files = glob.glob(pattern)

            if not jsonl_files:
                print(f"在目录 {directory} 中未找到任何 .jsonl 文件")
                return files_data

            print(f"找到 {len(jsonl_files)} 个 .jsonl 文件")

        for file_path in jsonl_files:
            file_name = os.path.basename(file_path)
            language = os.path.splitext(file_name)[0]  # 去掉后缀作为语言名

            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    data_list = []
                    for line_num, line in enumerate(f, 1):
                        line = line.strip()
                        if not line:  # 跳过空行
                            continue

                        # 如果指定了行号，只处理该行
                        if specific_line and line_num != specific_line:
                            continue

                        try:
                            data = json.loads(line)
                            # 添加元数据
                            data['_file_name'] = file_name
                            data['_language'] = language
                            data['_line_num'] = line_num
                            data_list.append(data)

                            # 如果指定了行号，找到后就退出循环
                            if specific_line:
                                break
                        except json.JSONDecodeError as e:
                            print(f"警告: {file_name}:{line_num} JSON解析失败: {e}")
                            continue

                    files_data[file_name] = data_list
                    if specific_line:
                        if data_list:
                            print(f"  {file_name}: 读取第 {specific_line} 行数据")
                        else:
                            print(f"  {file_name}: 第 {specific_line} 行不存在或无效")
                    else:
                        print(f"  {file_name}: 读取 {len(data_list)} 条数据")

            except Exception as e:
                print(f"错误: 读取文件 {file_path} 失败: {e}")
                continue

        return files_data

    def validate_data(self, data: Dict) -> Tuple[bool, str]:
        """
        验证数据是否包含必要字段

        Args:
            data: 数据字典

        Returns:
            (是否有效, 错误信息)
        """
        if self.splice:
            # 拼接模式需要的字段
            required_fields = ['code', 'assertions', 'exec_outcome']

            for field in required_fields:
                if field not in data:
                    return False, f"缺少必要字段: {field}"
                if data[field] is None:
                    return False, f"字段 {field} 为空"

            # 检查code和assertions是否为空字符串
            if not data['code'].strip():
                return False, "code字段为空"
            if not data['assertions'].strip():
                return False, "assertions字段为空"
        else:
            # 普通模式需要的字段
            required_fields = ['all_code', 'exec_outcome']

            for field in required_fields:
                if field not in data:
                    return False, f"缺少必要字段: {field}"
                if data[field] is None:
                    return False, f"字段 {field} 为空"

            # 检查all_code是否为空字符串
            if not data['all_code'].strip():
                return False, "all_code字段为空"

        return True, ""

    def submit_request(self, data: Dict) -> Tuple[bool, Optional[str], str]:
        """
        向远端接口提交请求

        Args:
            data: 包含测试数据的字典

        Returns:
            (是否成功, 响应中的exec_outcome, 错误信息)
        """
        if self.splice:
            # 拼接模式的请求数据格式
            request_data = {
                "src_uid": data['_line_num'],
                "func_code": data['code'],
                "main_code": data['assertions'],
                "lang": data['_language'],
                "request_extensions": {
                    "timeout": float(self.timeout),
                    "debug": self.debug
                }
            }
        else:
            # 普通模式的请求数据格式
            request_data = {
                "src_uid": data['_line_num'],
                "source_code": data['all_code'],
                "lang": data['_language'],
                "request_extensions": {
                    "timeout": float(self.timeout),
                    "debug": self.debug
                }
            }

        try:
            response = requests.post(
                self.submit_url,
                json=request_data,
                timeout=self.request_timeout,
                headers={'Content-Type': 'application/json'}
            )

            if response.status_code == 200:
                try:
                    result = response.json()
                    exec_outcome = result.get('exec_outcome')
                    return True, exec_outcome, response.json()
                except json.JSONDecodeError:
                    return False, None, "响应不是有效的JSON格式"

            elif response.status_code in [400, 500]:
                # HTTP 400和500错误，先尝试获取返回体中的exec_outcome
                try:
                    error_response = response.json()
                    exec_outcome = error_response.get('exec_outcome')

                    if exec_outcome:
                        # 如果有exec_outcome，忽略HTTP错误，继续后续逻辑
                        return True, exec_outcome, error_response
                    else:
                        # 如果没有exec_outcome，则跳过
                        error_detail = ""
                        if 'error' in error_response:
                            error_detail = f" - {error_response['error']}"
                        elif 'message' in error_response:
                            error_detail = f" - {error_response['message']}"
                        else:
                            error_detail = f" - {str(error_response)[:100]}"
                        return False, None, f"HTTP {response.status_code}{error_detail}"

                except json.JSONDecodeError:
                    # 如果响应不是JSON格式，则跳过
                    try:
                        error_detail = f" - {response.text[:100]}"
                    except:
                        error_detail = ""
                    return False, None, f"HTTP {response.status_code}{error_detail}"

            else:
                # 其他HTTP错误，尝试获取详细信息
                error_detail = ""
                try:
                    error_response = response.json()
                    if 'error' in error_response:
                        error_detail = f" - {error_response['error']}"
                    elif 'message' in error_response:
                        error_detail = f" - {error_response['message']}"
                    else:
                        error_detail = f" - {str(error_response)[:100]}"
                except:
                    try:
                        error_detail = f" - {response.text[:100]}"
                    except:
                        error_detail = ""

                return False, None, f"HTTP {response.status_code}{error_detail}"

        except requests.exceptions.Timeout:
            return False, None, f"请求超时 (>{self.request_timeout}秒)"
        except requests.exceptions.ConnectionError as e:
            return False, None, f"连接失败 - {str(e)[:50]}"
        except requests.exceptions.RequestException as e:
            return False, None, f"请求异常 - {str(e)[:50]}"
        except Exception as e:
            return False, None, f"未知异常 - {str(e)[:50]}"

    def process_single_case(self, data: Dict, show_details: bool = False) -> Dict:
        """
        处理单个测试用例

        Args:
            data: 测试数据
            show_details: 是否显示详细的输入输出信息

        Returns:
            处理结果字典
        """
        file_name = data['_file_name']
        line_num = data['_line_num']

        # 如果需要显示详细信息，打印输入参数
        if show_details:
            print("\n" + "=" * 80)
            print(f"测试用例详情 - {file_name}:{line_num}")
            print("=" * 80)
            print("输入参数:")
            print(f"  文件名: {file_name}")
            print(f"  行号: {line_num}")
            print(f"  语言: {data['_language']}")
            print(f"  期望结果: {data.get('exec_outcome', 'N/A')}")
            print(f"  模式: {'拼接模式' if self.splice else '普通模式'}")

            if self.splice:
                print(f"  函数代码长度: {len(data.get('code', ''))}")
                print(f"  主代码长度: {len(data.get('assertions', ''))}")
                print("\n函数代码内容:")
                print("-" * 40)
                print(data.get('code', ''))
                print("-" * 40)
                print("\n主代码内容:")
                print("-" * 40)
                print(data.get('assertions', ''))
                print("-" * 40)
            else:
                print(f"  源代码长度: {len(data.get('all_code', ''))}")
                print("\n源代码内容:")
                print("-" * 40)
                print(data.get('all_code', ''))
                print("-" * 40)

        # 验证数据
        is_valid, error_msg = self.validate_data(data)
        if not is_valid:
            if show_details:
                print(f"\n❌ 数据验证失败: {error_msg}")
            return {
                'file_name': file_name,
                'line_num': line_num,
                'status': 'skipped',
                'reason': f"数据验证失败: {error_msg}",
                'expected': data.get('exec_outcome', 'N/A'),
                'actual': 'N/A',
                'match': False
            }

        # 构建请求数据
        if self.splice:
            # 拼接模式的请求数据格式
            request_data = {
                "src_uid": data['_line_num'],
                "func_code": data['code'],
                "main_code": data['assertions'],
                "lang": data['_language'],
                "request_extensions": {
                    "timeout": float(self.timeout),
                    "debug": self.debug
                }
            }
        else:
            # 普通模式的请求数据格式
            request_data = {
                "src_uid": data['_line_num'],
                "source_code": data['all_code'],
                "lang": data['_language'],
                "request_extensions": {
                    "timeout": float(self.timeout),
                    "debug": self.debug
                }
            }

        if show_details:
            print(f"\n📤 发送请求:")
            print(f"  URL: {self.submit_url}")
            print(f"  请求数据:")
            print(f"    src_uid: {request_data['src_uid']}")
            print(f"    lang: {request_data['lang']}")
            if self.splice:
                print(f"    func_code: {len(request_data['func_code'])} 字符")
                print(f"    main_code: {len(request_data['main_code'])} 字符")
            else:
                print(f"    source_code: {len(request_data['source_code'])} 字符")
            print(f"    request_extensions:")
            print(f"      timeout: {request_data['request_extensions']['timeout']}")
            print(f"      debug: {request_data['request_extensions']['debug']}")

        # 提交请求
        success, actual_outcome, error_msg = self.submit_request(data)

        if not success:
            if show_details:
                print(f"\n❌ 请求失败: {error_msg}")
            return {
                'file_name': file_name,
                'line_num': line_num,
                'status': 'skipped',
                'reason': f"请求失败: {error_msg}",
                'expected': data['exec_outcome'],
                'actual': 'N/A',
                'match': False
            }

        # 对比结果
        expected_outcome = data['exec_outcome']
        if expected_outcome == actual_outcome:
            match = True
        # elif expected_outcome != "PASSED" and actual_outcome != "PASSED":
        #     match = True
        else:
            match = False

        if show_details:
            print(f"\n📥 响应结果:")
            print(f"  实际结果: {actual_outcome}")
            print(f"  期望结果: {expected_outcome}")
            print(f"  匹配状态: {'✅ 匹配' if match else '❌ 不匹配'}")

            if match:
                print(f"\n🎉 测试通过!")
            else:
                print(f"\n⚠️  测试失败!")
                if isinstance(error_msg, dict):
                    err_all_code = error_msg.get("all_code", "")
                    err_exec_compile_message = error_msg.get("exec_compile_message", "")
                    err_exec_runtime_message = error_msg.get("exec_runtime_message", "")
                    err_response_extensions = error_msg.get("response_extensions", "")
                    print(f"  所有代码: \n{err_all_code}")
                    print(f"  编译错误信息: \n{err_exec_compile_message}")
                    print(f"  运行错误信息: \n{err_exec_runtime_message}")
                    print(f"  响应扩展信息: \n{err_response_extensions}")
        return {
            'file_name': file_name,
            'line_num': line_num,
            'status': 'processed',
            'reason': '',
            'expected': expected_outcome,
            'actual': actual_outcome,
            'match': match
        }

    def run_test(self, directory: str = ".", concurrency: int = 5, specific_file: str = None, specific_line: int = None) -> Dict:
        """
        运行测试

        Args:
            directory: 测试文件目录
            concurrency: 并发数
            specific_file: 指定的单个文件
            specific_line: 指定的行号（仅测试该行对应的用例）

        Returns:
            测试结果统计
        """
        if specific_file:
            if specific_line:
                print(f"开始测试指定文件的指定行: {specific_file}:{specific_line}")
            else:
                print(f"开始测试指定文件: {specific_file}")
        else:
            print(f"开始测试，目录: {directory}")
        print(f"服务器地址: {self.server}:{self.port}")
        print(f"提交接口: {self.submit_url}")
        print(f"运行模式: {'拼接模式' if self.splice else '普通模式'}")
        if not specific_line:
            print(f"并发数: {concurrency}")
        print("-" * 60)

        # 读取所有文件或指定文件
        files_data = self.read_jsonl_files(directory, specific_file, specific_line)
        if not files_data:
            return {}

        # 准备所有测试用例
        all_cases = []
        for file_name, data_list in files_data.items():
            all_cases.extend(data_list)

        total_cases = len(all_cases)
        print(f"\n总测试用例数: {total_cases}")

        # 当指定行号时，显示详细信息
        show_details = specific_line is not None
        if show_details:
            print("启用详细输出模式")

        print("-" * 60)

        # 并发处理
        results = []
        if show_details:
            # 详细模式下不使用并发和进度条，直接处理
            for case in all_cases:
                result = self.process_single_case(case, show_details=True)
                results.append(result)
        else:
            # 正常模式使用并发处理
            with ThreadPoolExecutor(max_workers=concurrency) as executor:
                # 提交所有任务
                future_to_case = {
                    executor.submit(self.process_single_case, case): case
                    for case in all_cases
                }

                # 使用进度条显示处理进度
                with tqdm(total=total_cases, desc="处理进度") as pbar:
                    for future in as_completed(future_to_case):
                        result = future.result()
                        results.append(result)
                        pbar.update(1)

        # 统计结果
        return self.calculate_statistics(results, files_data)

    def calculate_statistics(self, results: List[Dict], files_data: Dict) -> Dict:
        """
        计算统计结果

        Args:
            results: 处理结果列表
            files_data: 原始文件数据

        Returns:
            统计结果
        """
        # 按文件分组统计
        file_stats = {}
        overall_stats = {
            'total_cases': len(results),
            'processed_cases': 0,
            'skipped_cases': 0,
            'matched_cases': 0,
            'consistency_rate': 0.0
        }

        for result in results:
            file_name = result['file_name']

            if file_name not in file_stats:
                file_stats[file_name] = {
                    'total': 0,
                    'processed': 0,
                    'skipped': 0,
                    'matched': 0,
                    'consistency_rate': 0.0,
                    'skip_reasons': {}
                }

            file_stats[file_name]['total'] += 1

            if result['status'] == 'processed':
                file_stats[file_name]['processed'] += 1
                overall_stats['processed_cases'] += 1

                if result['match']:
                    file_stats[file_name]['matched'] += 1
                    overall_stats['matched_cases'] += 1
            else:
                file_stats[file_name]['skipped'] += 1
                overall_stats['skipped_cases'] += 1

                # 统计跳过原因
                reason = result['reason']
                if reason not in file_stats[file_name]['skip_reasons']:
                    file_stats[file_name]['skip_reasons'][reason] = 0
                file_stats[file_name]['skip_reasons'][reason] += 1

        # 计算一致率（只针对成功处理的用例）
        if overall_stats['processed_cases'] > 0:
            overall_stats['consistency_rate'] = overall_stats['matched_cases'] / overall_stats['processed_cases']

        for file_name, stats in file_stats.items():
            if stats['processed'] > 0:
                stats['consistency_rate'] = stats['matched'] / stats['processed']

        return {
            'overall': overall_stats,
            'by_file': file_stats,
            'detailed_results': results
        }

    def print_results(self, stats: Dict):
        """
        打印测试结果

        Args:
            stats: 统计结果
        """
        print("\n" + "=" * 80)
        print("测试结果统计")
        print("=" * 80)

        overall = stats['overall']
        print(f"总用例数: {overall['total_cases']}")
        print(f"处理用例: {overall['processed_cases']}")
        print(f"跳过用例: {overall['skipped_cases']}")
        print(f"匹配用例: {overall['matched_cases']}")
        print(f"整体一致率: {overall['consistency_rate']:.2%}")

        # 各文件统计表格 - 使用 PrettyTable
        print("\n各文件统计表格:")
        file_table = PrettyTable()
        file_table.field_names = ["文件名", "总数", "处理", "跳过", "匹配", "一致率"]
        file_table.align = "l"
        file_table.align["总数"] = "r"
        file_table.align["处理"] = "r"
        file_table.align["跳过"] = "r"
        file_table.align["匹配"] = "r"
        file_table.align["一致率"] = "r"

        for file_name, file_stats in stats['by_file'].items():
            file_table.add_row([
                file_name,
                file_stats['total'],
                file_stats['processed'],
                file_stats['skipped'],
                file_stats['matched'],
                f"{file_stats['consistency_rate']:.2%}"
            ])

        print(file_table)

        # 失败和跳过的详细信息表格
        failed_cases = []
        skipped_cases = []

        for result in stats['detailed_results']:
            if result['status'] == 'skipped':
                skipped_cases.append(result)
            elif result['status'] == 'processed' and not result['match']:
                failed_cases.append(result)

        # 显示不匹配的用例 - 使用 PrettyTable
        if failed_cases:
            print("\n不匹配用例详情:")
            failed_table = PrettyTable()
            failed_table.field_names = ["文件名", "行号", "期望结果", "实际结果", "状态"]
            failed_table.align = "l"
            failed_table.align["行号"] = "r"

            for case in failed_cases:
                failed_table.add_row([
                    case['file_name'],
                    case['line_num'],
                    case['expected'],
                    case['actual'],
                    "不匹配"
                ])

            print(failed_table)

        # 显示跳过的用例及错误信息 - 使用 PrettyTable
        if skipped_cases:
            print("\n跳过用例详情（包含错误信息）:")
            skipped_table = PrettyTable()
            skipped_table.field_names = ["文件名", "行号", "期望结果", "错误信息"]
            skipped_table.align = "l"
            skipped_table.align["行号"] = "r"
            skipped_table.max_width["错误信息"] = 60

            for case in skipped_cases:
                # 截断过长的错误信息
                error_msg = case['reason']
                if len(error_msg) > 58:
                    error_msg = error_msg[:55] + "..."

                skipped_table.add_row([
                    case['file_name'],
                    case['line_num'],
                    case['expected'],
                    error_msg
                ])

            print(skipped_table)

        # # 错误统计汇总表格 - 使用 PrettyTable
        # if skipped_cases:
        #     print("\n错误类型统计:")
        #     error_summary = {}
        #     for case in skipped_cases:
        #         reason = case['reason']
        #         # 提取错误类型（取冒号前的部分）
        #         error_type = reason.split(':')[0] if ':' in reason else reason
        #         if error_type not in error_summary:
        #             error_summary[error_type] = 0
        #         error_summary[error_type] += 1
        #
        #     error_table = PrettyTable()
        #     error_table.field_names = ["错误类型", "次数", "占比"]
        #     error_table.align = "l"
        #     error_table.align["次数"] = "r"
        #     error_table.align["占比"] = "r"
        #
        #     total_errors = len(skipped_cases)
        #     for error_type, count in sorted(error_summary.items(), key=lambda x: x[1], reverse=True):
        #         percentage = (count / total_errors) * 100
        #         error_table.add_row([
        #             error_type,
        #             count,
        #             f"{percentage:.1f}%"
        #         ])
        #
        #     print(error_table)

def main():
    """主函数"""
    parser = argparse.ArgumentParser(description='需拼接代码用例测试工具')
    parser.add_argument('--directory', '-d', default='./test/splice_code_cases/', help='测试文件目录 (默认: ./test/splice_code_cases/)')
    parser.add_argument('--file', '-f', help='指定单个测试文件 (可以是文件名或完整路径)')
    parser.add_argument('--lang', '-l', help='指定语言，将测试默认目录下的{语言}.jsonl文件 (例如: --lang csharp)')
    parser.add_argument('--line', type=int, help='指定行号，只测试该行对应的用例（仅在指定文件或语言时生效）')
    parser.add_argument('--server_ip', '-s', default='21.86.44.175', help='服务器地址 (默认: 21.86.44.175)')
    parser.add_argument('--server_port', '-p', type=int, default=8080, help='服务器端口 (默认: 8080)')
    parser.add_argument('--concurrency', '-c', type=int, default=5, help='并发数 (默认: 5)')
    parser.add_argument('--timeout', '-t', type=int, default=30, help='submit接口执行超时时间/秒 (默认: 30)')
    parser.add_argument('--debug', action='store_true', help='调试模式, 默认False代表不保留运行环境')
    parser.add_argument('--splice', action='store_true', help='拼接模式, 使用func_code和main_code字段而非source_code字段')

    args = parser.parse_args()

    # 处理参数冲突检查
    param_count = sum([bool(args.file), bool(args.lang)])
    if param_count > 1:
        print("错误: --file 和 --lang 参数不能同时使用")
        return

    # 检查--line参数的使用条件
    if args.line and not (args.file or args.lang):
        print("错误: --line 参数只能在指定 --file 或 --lang 时使用")
        return

    # 创建测试实例
    tester = CaseTest(server=args.server_ip, port=args.server_port, timeout=args.timeout, debug=args.debug, splice=args.splice)

    # 确定要测试的文件
    specific_file = None
    if args.lang:
        # 使用语言参数构建文件路径
        specific_file = f"{args.lang}.jsonl"
        print(f"使用语言参数: {args.lang}")
        if args.line:
            print(f"将测试文件第 {args.line} 行: {os.path.join(args.directory, specific_file)}")
        else:
            print(f"将测试文件: {os.path.join(args.directory, specific_file)}")
    elif args.file:
        specific_file = args.file
        if args.line:
            print(f"将测试文件第 {args.line} 行: {specific_file}")

    # 运行测试
    start_time = time.time()
    stats = tester.run_test(
        directory=args.directory,
        concurrency=args.concurrency,
        specific_file=specific_file,
        specific_line=args.line
    )
    end_time = time.time()

    if stats:
        # 打印结果
        tester.print_results(stats)

        print(f"\n总耗时: {end_time - start_time:.2f} 秒")
    else:
        print("没有找到有效的测试数据")


if __name__ == "__main__":
    main()
