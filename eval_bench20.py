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
from multiprocessing import Pool
import multiprocessing as mp
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm
from prettytable import PrettyTable

# Logging setup
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('unified_processor.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

# Global processor instance to avoid repeated creation
_global_processor = None


def init_worker(server_ip, server_port):
    """Initialize worker process"""
    global _global_processor
    _global_processor = UnifiedProcessor(server_ip, server_port)


def process_single_data_worker(data, index, debug, print_code=False):
    """Single-task worker function for one data entry"""
    global _global_processor

    result = _global_processor.process_data(data, debug, print_code)
    result["index"] = index
    result["original_data"] = data

    time.sleep(0.2)  # Slight delay to avoid API overload
    return result


class UnifiedProcessor:
    def __init__(self, server_ip: str = "127.0.0.1", server_port: int = 8080):
        self.server_ip = server_ip
        self.server_port = server_port
        self.submit_url = f"http://{server_ip}:{server_port}/submit"
        self.headers = {
            "Content-Type": "application/json"
        }
        # Languages requiring special handling
        self.special_languages = []

    def read_jsonl_file(self, file_path: str, line_number: int = None, target_language: str = None) -> List[Dict[str, Any]]:
        """Read JSONL file and return data list"""
        data_list = []
        total_count = 0
        filtered_count = 0

        try:
            if line_number is not None:
                # Two-pass handling if line number is specified
                with open(file_path, 'r', encoding='utf-8') as file:
                    for line_num, line in enumerate(file, 1):
                        line = line.strip()
                        if line:
                            try:
                                data = json.loads(line)
                                total_count += 1
                                if target_language:
                                    data_language = data.get("language", "").lower()
                                    if data_language == target_language.lower():
                                        filtered_count += 1
                                else:
                                    filtered_count += 1
                            except json.JSONDecodeError as e:
                                logger.error(f"Line {line_num} JSON parse error: {e}")
                                continue

                current_filtered = 0
                with open(file_path, 'r', encoding='utf-8') as file:
                    for line_num, line in enumerate(file, 1):
                        line = line.strip()
                        if line:
                            try:
                                data = json.loads(line)
                                if target_language:
                                    data_language = data.get("language", "").lower()
                                    if data_language != target_language.lower():
                                        continue
                                current_filtered += 1
                                if current_filtered == line_number:
                                    data['_absolute_line_number'] = line_num
                                    data['_relative_line_number'] = current_filtered
                                    data_list.append(data)
                                    break
                            except json.JSONDecodeError:
                                continue
            else:
                # Build language-based mappings
                language_line_mapping = {}
                with open(file_path, 'r', encoding='utf-8') as file:
                    for line_num, line in enumerate(file, 1):
                        line = line.strip()
                        if line:
                            try:
                                data = json.loads(line)
                                total_count += 1
                                data_language = data.get("language", "").lower()
                                if target_language and data_language != target_language.lower():
                                    continue
                                if data_language not in language_line_mapping:
                                    language_line_mapping[data_language] = []
                                language_line_mapping[data_language].append((line_num, data))
                                filtered_count += 1
                            except json.JSONDecodeError as e:
                                logger.error(f"Line {line_num} JSON parse error: {e}")
                                continue

                for language, lang_data_list in language_line_mapping.items():
                    for relative_line_num, (absolute_line_num, data) in enumerate(lang_data_list, 1):
                        data['_absolute_line_number'] = absolute_line_num
                        data['_relative_line_number'] = relative_line_num
                        data_list.append(data)

                data_list.sort(key=lambda x: x['_absolute_line_number'])

            if target_language:
                logger.info(f"Language filter: {target_language} - total {total_count}, matched {filtered_count}, read {len(data_list)}")
            else:
                logger.info(f"Successfully read {len(data_list)} records")
            return data_list
        except FileNotFoundError:
            logger.error(f"File not found: {file_path}")
            return []
        except Exception as e:
            logger.error(f"Error reading file: {e}")
            return []

    def extract_fields(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Extract required fields"""
        def get_field_with_fallback(field_name: str, default_value: str = "") -> str:
            value = data.get(field_name, "")
            return value if value else default_value

        def get_solution_with_fallback() -> str:
            output = data.get("output", "")
            value = self._extract_code_from_output(output)
            return value if value else ""

        return {
            "language": get_field_with_fallback("language", "").lower(),
            "full_test_func": get_field_with_fallback("full_test_func", ""),
            "demo_test_func": get_field_with_fallback("demo_test_func", ""),
            "main_test_func": get_solution_with_fallback()
        }

    def _extract_code_from_output(self, output: str) -> str:
        """Extract code block from output field"""
        if not output:
            return ""
        matches = re.finditer(r'```(\w+)\n(.*?)```', output, flags=re.DOTALL)
        for match in matches:
            code = match.group(2).strip()
            if code:
                return code
        cleaned_output = output.strip()
        if cleaned_output.startswith('```'):
            cleaned_output = cleaned_output[3:]
        if cleaned_output.endswith('```'):
            cleaned_output = cleaned_output[:-3]
        lines = cleaned_output.strip().split('\n')
        if len(lines) > 1:
            return '\n'.join(lines[1:]).strip()
        return cleaned_output.strip()

    def call_submit_api(self, data: Dict[str, Any], test_type: str = "full", debug: bool = False, print_code: bool = False) -> Dict[str, Any]:
        """Call submit API"""
        try:
            language = data["language"]
            is_special_language = language in self.special_languages
            if test_type == "full":
                test_code = data["full_test_func"]
            elif test_type == "demo":
                test_code = data["demo_test_func"]
            else:
                raise ValueError(f"Unsupported test type: {test_type}")

            if not is_special_language:
                payload = {
                    "src_uid": f"bench_test_{test_type}_{int(time.time())}",
                    "func_code": data["main_test_func"],
                    "main_code": test_code,
                    "lang": language,
                    "show_log": "true",
                    "request_extensions": {"timeout": 30, "debug": str(debug).lower()}
                }

            response = requests.post(self.submit_url, headers=self.headers, json=payload, timeout=60)

            if response.status_code == 200:
                result = response.json()
                return {
                    "success": True,
                    "response": result,
                    "status_code": response.status_code
                }
            else:
                logger.error(f"API call failed, status: {response.status_code}, response: {response.text}")
                return {
                    "success": False,
                    "error": f"HTTP {response.status_code}: {response.text}",
                    "status_code": response.status_code
                }
        except requests.exceptions.Timeout:
            logger.error("API call timeout")
            return {"success": False, "error": "Request timeout", "status_code": None}
        except requests.exceptions.RequestException as e:
            logger.error(f"API request exception: {e}")
            return {"success": False, "error": str(e), "status_code": None}
        except Exception as e:
            logger.error(f"Error processing data: {e}")
            return {"success": False, "error": str(e), "status_code": None}

    # (Remaining methods unchanged except logs/text in English...)

def main():
    parser = argparse.ArgumentParser(description='Unified JSONL Processor (all languages supported)')
    parser.add_argument('-i', '--input_file', help='Input JSONL file path')
    parser.add_argument('-o', '--output', help='Output file path')
    parser.add_argument('-m', '--max-items', type=int, help='Max number of records to process')
    parser.add_argument('-l', '--line', type=int, help='Process specific line number (1-based)')
    parser.add_argument('--server_ip', help='Server IP address', default='127.0.0.1')
    parser.add_argument('--server_port', type=int, help='Server port', default=8080)
    parser.add_argument('-d', '--debug', action='store_true', help='Enable debug mode')
    parser.add_argument('-c', '--concurrency', type=int, default=5, help='Number of concurrent processes (default 5)')
    parser.add_argument('--lang', help='Process only data in specified language')

    args = parser.parse_args()

    if not os.path.exists(args.input_file):
        logger.error(f"Input file not found: {args.input_file}")
        return
    if args.concurrency < 1:
        logger.error("Concurrency must be greater than 0")
        return
    if args.concurrency > 20:
        logger.warning("High concurrency may stress the server, recommended <= 20")

    processor = UnifiedProcessor(args.server_ip, args.server_port)
    results = processor.process_file(args.input_file, args.max_items, args.line, args.debug, args.concurrency, args.lang)

    if args.output:
        output_file = args.output
    else:
        input_basename = os.path.basename(args.input_file)
        base_name = input_basename.replace('.jsonl', '')
        if args.lang:
            output_file = f"{base_name}_{args.lang}_results.jsonl"
        else:
            output_file = f"{base_name}_results.jsonl"

    if results:
        processor.save_results(results, output_file)
        processor.print_detailed_statistics(results)
    else:
        logger.warning("No data processed")


if __name__ == "__main__":
    main()
