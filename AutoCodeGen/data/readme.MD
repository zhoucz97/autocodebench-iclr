文件名使用规范XX_language.jsonl，统一字段命名
# 必须字段（required）
1. question（str类型）：用户的问题
2. main_test_func(str类型)：主函数code_block
3. demo_test_func（str类型）：少量case的测试函数
4. full_test_func（str类型）：全量case的测试函数 
5. language（str类型）：语言类型
6. seed_text(str类型)：原始seed文本

# 非必须字段
1. critic_result(str类型)：critic结果


