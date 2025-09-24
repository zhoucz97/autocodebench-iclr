#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Unified JSONL Processor

统一的JSONL处理器，支持全部语言的处理。
对于go、java、cpp、javascript、python这5个特殊语言，使用format_test_code处理后，将完整代码作为source_code传给submit接口。
对于其他语言，使用传统的func_code和main_code方式。
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
    def __init__(self, server_ip: str = "21.86.44.177", server_port: int = 8080):
        self.server_ip = server_ip
        self.server_port = server_port
        self.submit_url = f"http://{server_ip}:{server_port}/submit"
        self.headers = {
            "Content-Type": "application/json"
        }
        # 需要特殊处理的语言（使用format_test_code）
        self.special_languages = ["go", "java", "cpp", "javascript", "python"]
        # self.special_languages = []

    def format_test_code(self, language: str, main_code: str, test_code: str, is_demo_test: bool = True) -> str:
        """使用format_test_code逻辑处理特殊语言的代码.使用沙盒执行得到test output要调用此逻辑。因为此逻辑和bench评测时逻辑不一致。"""
        if language == "javascript":
            if is_demo_test:
                code_req = main_code + "\n" + test_code + "\n\ndemoTesting()"
            else:
                code_req = main_code + "\n" + test_code + "\n\nfullTesting()"
        elif language == "go":
            import_info = self._go_extract_and_merge_imports([main_code, test_code])
            main_code = re.sub(r'import\s*(?:\(.*?\)|"[^"]+")\s*', '', main_code, flags=re.DOTALL)
            main_code = main_code.replace("package main", "")
            main_code = "package main\n" + import_info + "\n" + main_code
            test_code = re.sub(r'import\s*(?:\(.*?\)|"[^"]+")\s*', '', test_code, flags=re.DOTALL)
            test_code = test_code.replace("package main", "")
            if re.search(r'\s*func\s+Test\w+.+', test_code):
                go_test_method = "test"
            else:
                go_test_method = "main"
            if go_test_method == "main":
                main_func = "func main() {demoTesting()}" if is_demo_test else "func main() {fullTesting()}"
            else:
                main_func = ""
            code_req = main_code + "\n" + test_code + "\n\n" + main_func
            # 删除未使用到的import包
            code_req = self._go_remove_unused_imports(code_req)
        elif language == "cpp":
            model_func = main_code.strip().split("int main")[0]
            code_req = model_func + "\n" + test_code
            code_req = "#include <iostream>\nusing namespace std;\n" + code_req
        elif language == "java":
            # 将主函数中的public class，private class 和 protected class都转成class。
            main_code = main_code.replace("public class", "class").replace("private class", "class").replace("protected class", "class")

            # test code
            test_code = test_code.replace("public class", "class").replace("private class", "class").replace("protected class", "class")
            # 提取用到的包
            answer_packages = [line for line in main_code.split('\n') if line.startswith("import ")]
            # 将package开头的行以及导入包的行去除
            answer_code = [line for line in main_code.split('\n') if not (line.startswith("import ") or line.startswith("package "))]
            answer_code = "\n".join(answer_code)
            # 提取测试函数的用到的包
            test_packages = [line for line in test_code.split('\n') if line.startswith("import ")]
            # 对导入的包去重
            all_packages = answer_packages + test_packages
            all_packages = list(set(all_packages))
            all_packages = "\n".join(all_packages)
            # 将package开头的行以及导入包的行去除
            test_code = [line for line in test_code.split('\n') if not (line.startswith("import ") or line.startswith("package "))]
            test_code = "\n".join(test_code)
            # 拼接
            code_req = all_packages + "\n" + answer_code + "\n" + test_code
        elif language == "python":
            code_req = main_code + "\n" + test_code
        else:
            raise Exception(f"不支持的语言类型：{language}")
        return code_req
    
    def _go_extract_and_merge_imports(self, code_blocks: list):
        """提取并合并Go代码中的import语句"""
        imports = set()
        for code in code_blocks:
            # 匹配带括号的多行import和单行import
            matches = re.findall(r'import\s*(?:\((.*?)\)|"([^"]+)")', code, re.DOTALL)
            for match in matches:
                # 处理两种匹配结果
                for imp in match:
                    if imp.strip():
                        # 分割多行import中的各个包
                        for line in imp.split('\n'):
                            line = line.strip()
                            if line and not line.startswith('//'):  # 忽略注释
                                # 去除可能的引号和分号
                                line = line.replace('"', '').replace(';', '').strip()
                                if line:
                                    imports.add(line)

        # 生成合并后的import语句
        if imports:
            merged_import = "import (\n"
            for imp in imports:
                merged_import += f'    "{imp}"\n'
            merged_import += ")"
        else:
            merged_import = ""

        return merged_import

    def _go_remove_unused_imports(self, go_code: str) -> str:
        """移除Go代码中未使用的import包"""
        # 1. 找出所有import语句（包括分组和独立import）
        imports = []

        # 处理分组import (import (...))
        for match in re.finditer(r'import\s*\(([\s\S]*?)\)', go_code):
            imports.append(('grouped', match.start(), match.end(), match.group(1)))

        # 处理独立import (import "pkg")
        for match in re.finditer(r'import\s+"([^"]+)"', go_code):
            imports.append(('single', match.start(), match.end(), match.group(1)))

        # 如果没有import语句，直接返回
        if not imports:
            return go_code

        # 2. 收集所有导入的包
        imported_pkgs = set()
        for imp_type, start, end, content in imports:
            if imp_type == 'grouped':
                # 从分组中提取所有包
                imported_pkgs.update(re.findall(r'"([^"]+)"', content))
            else:  # single
                imported_pkgs.add(content)

        # 3. 找出实际使用的包
        used_pkgs: Set[str] = set()
        for pkg in imported_pkgs:
            pkg_identifier = pkg.split('/')[-1]
            # 排除import部分检查使用情况
            code_without_imports = go_code
            for _, start, end, _ in imports:
                code_without_imports = code_without_imports[:start] + ' ' * (end - start) + code_without_imports[end:]
            pkg_identifier = re.escape(pkg_identifier)
            if re.search(rf'(?<!\w){pkg_identifier}(?!\w)', code_without_imports):
                used_pkgs.add(pkg)

        # 4. 重建代码
        result = []
        last_pos = 0

        for imp_type, start, end, content in sorted(imports, key=lambda x: x[1]):
            # 添加import之前的代码
            result.append(go_code[last_pos:start])

            if imp_type == 'grouped':
                # 处理分组import
                new_lines = []
                for line in content.split('\n'):
                    line = line.strip()
                    if not line:
                        new_lines.append('')
                        continue
                    pkg = re.search(r'"([^"]+)"', line)
                    if pkg and pkg.group(1) in used_pkgs:
                        new_lines.append(line)

                if new_lines:
                    result.append(f'import (\n' + '\n'.join(new_lines) + '\n)')
            else:  # single
                # 处理独立import
                if content in used_pkgs:
                    result.append(f'import "{content}"')

            last_pos = end

        # 添加最后部分代码
        result.append(go_code[last_pos:])

        return ''.join(result)
 
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
                # 没有指定行号，一遍处理即可
                with open(file_path, 'r', encoding='utf-8') as file:
                    for line_num, line in enumerate(file, 1):
                        line = line.strip()
                        if line:
                            try:
                                data = json.loads(line)

                                total_count += 1

                                # 语言过滤
                                if target_language:
                                    data_language = data.get("language", "").lower()
                                    if data_language != target_language.lower():
                                        continue
                                    filtered_count += 1
                                else:
                                    filtered_count += 1

                                # 添加绝对行号和相对行号信息
                                data['_absolute_line_number'] = line_num
                                data['_relative_line_number'] = filtered_count
                                data_list.append(data)
                            except json.JSONDecodeError as e:
                                logger.error(f"第{line_num}行JSON解析错误: {e}")
                                continue

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
        """提取需要的字段"""
        return {
            "language": data.get("language", "").lower(),
            "full_test_func": data.get("full_test_func", ""),
            "demo_test_func": data.get("demo_test_func", ""),
            "main_test_func": data.get("extracted_code", "")
        }
    
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
            
            if is_special_language:
                # 特殊语言：使用format_test_code处理，将完整代码作为source_code
                is_demo_test = (test_type == "demo")
                formatted_code = self.format_test_code(
                    language, 
                    data["main_test_func"], 
                    test_code, 
                    is_demo_test
                )
                
                # 在指定行模式下打印formatted_code
                if print_code:
                    print(f"\n{'='*80}")
                    print(f"🔍 {test_type.upper()} 测试 - {language.upper()} 格式化代码:")
                    print(f"{'='*80}")
                    print(formatted_code)
                    print(f"{'='*80}\n")

                # import pdb
                # pdb.set_trace()

                # 确定Go测试方法
                # if language == "go" and re.search(r'\s*func\s+Test\w+.+', test_code):
                if language == 'go' and "(t *testing.T)" in test_code:
                    go_test_method = "test"
                else:
                    go_test_method = "main"
                
                payload = {
                    "src_uid": str(uuid.uuid4()),
                    "sandbox_type": "General",
                    "source_code": formatted_code,  # 使用格式化后的完整代码
                    "lang": language,
                    "show_log": True,
                    "request_extensions": {
                        "timeout": 30.0,
                        "debug": str(debug).lower(),
                        "go_test_method": go_test_method,
                        "target": "test",
                    }
                }
            else:
                # 普通语言：使用传统的func_code和main_code方式
                payload = {
                    "src_uid": f"0710_bench_test_{test_type}_{int(time.time())}",
                    "func_code": data["main_test_func"],
                    "main_code": test_code,
                    "lang": language,
                    "show_log": "true",
                    "request_extensions": {"timeout": 30, "debug": str(debug).lower()}
                }
            
            # logger.info(f"调用submit接口，测试类型: {test_type}, 语言: {language}, 特殊处理: {is_special_language}, debug: {debug}")
            response = requests.post(self.submit_url, headers=self.headers, json=payload, timeout=60)
            
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
        
        # 检查必要字段是否存在
        if not all(extracted_data.values()):
            logger.warning("数据缺少必要字段，跳过处理")
            return {
                "success": False,
                "error": "缺少必要字段",
                "full_test_result": None,
                "demo_test_result": None,
                "language": extracted_data["language"]
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
            "language": extracted_data["language"],
            "full_test_detail": full_test_result.get("response", {}),
            "demo_test_detail": demo_test_result.get("response", {})
        }
    
    def process_file(self, file_path: str, max_items: int = None, line_number: int = None,
                     debug: bool = False, concurrency: int = 5, target_language: str = None,
                     solution_key: str = 'output') -> List[Dict[str, Any]]:
        """处理整个JSONL文件"""
        logger.info(f"开始处理文件: {file_path}")
        if target_language:
            logger.info(f"语言过滤: 只处理 {target_language} 语言的数据")

        # 读取数据
        data_list = self.read_jsonl_file(file_path, line_number, target_language)
        if not data_list:
            return []
        def _extract_code_blocks(language, model_answer):
            """
            抽取代码块
            """
            extract_code = ""
            if language == "python":
                #pattern = rf"```python(.*?)```"
                pattern = r'```.*?\n(.*?)```'
                matches = re.findall(pattern, model_answer, re.DOTALL)
                blocks = [match.strip() for match in matches]
                
                max_code, max_len = None, -1
                for bl in blocks:
                    lens = len(bl.split("\n"))
                    if lens > max_len:
                        max_code, max_len = bl, lens
                if max_code and max_code != "":
                    extract_code = max_code
            else:
                #pattern_str = rf"```(bash|shell|sh)(.*?)```" if self.language in ["shell", "bash"] else rf'```({self.language})(.*?)```'
                pattern_str = r'```(.*?)\n(.*?)```'
                pattern = re.compile(pattern_str, re.DOTALL)
                code_blocks = pattern.findall(model_answer)
                if len(code_blocks) > 0:
                    extract_code = code_blocks[0][1].strip()
            return extract_code

        for data in data_list:
            extract_code = _extract_code_blocks(data["language"], data[solution_key])
            data["extracted_code"] = extract_code if extract_code else "no code extracted"

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
                        # "full_test_outcome": result.get("full_test_result", {}).get("response", {}).get("exec_outcome", "unknown"),
                        # "demo_test_outcome": result.get("demo_test_result", {}).get("response", {}).get("exec_outcome", "unknown"),
                        "full_test_result": result.get("full_test_result", {}),
                        "demo_test_result": result.get("demo_test_result", {}),
                        # "full_test_output": result.get("full_test_result", {}).get("response", {}).get("exec_cout", ""),
                        # "demo_test_output": result.get("demo_test_result", {}).get("response", {}).get("exec_cout", ""),
                        # "full_test_error": result.get("full_test_result", {}).get("error", ""),
                        # "demo_test_error": result.get("demo_test_result", {}).get("error", ""),
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
            try:
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
            except Exception as e:
                logger.error(f"测试统计结果时发生错误: {e} 数据:\n {result}")
                continue

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

        
def main():
    parser = argparse.ArgumentParser(description='统一JSONL文件处理器（支持全部语言）')
    parser.add_argument('-i', '--input_file', help='输入的JSONL文件路径')
    parser.add_argument('-o', '--output', help='输出文件路径')
    parser.add_argument('-m', '--max-items', type=int, help='最大处理数量')
    parser.add_argument('-l', '--line', type=int, help='指定处理第几行数据（从1开始）')
    parser.add_argument('--server_ip', help='服务器IP地址', default='21.86.44.177')
    parser.add_argument('--server_port', type=int, help='服务器端口', default=8080)
    parser.add_argument('-d', '--debug', action='store_true', help='启用debug模式')
    parser.add_argument('-c', '--concurrency', type=int, default=30, help='并发进程数（默认30）')
    parser.add_argument('--lang', help='指定处理的编程语言，只处理该语言的数据')
    parser.add_argument('--solution_key', default='output', help='指定解决方案所在的键名')

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
    results = processor.process_file(args.input_file, args.max_items, args.line, args.debug, args.concurrency, args.lang, args.solution_key)

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