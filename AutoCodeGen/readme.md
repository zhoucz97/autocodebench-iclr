## 流程

### 1. 随机抽取种子

### 2. 获取自包含可执行代码以及两个测试用例生成器
    
调用V3-0324，通过对种子进行进化或精简，生成一个自包含、可验证的代码块，以及两个调用该代码块的测试生成器（public test generator, private test generator）。

prompt模板：`prompt_templates/python_gen_code_template.txt`

步骤：

- 2.1 获取Prompt   `python3 1build_transform_msg.py --file ....`
- 2.2 调用V3       `python3 2codgen_batched_api_infer.py --file ...`
- 2.3 解析模型输出得到可执行代码及两个测试用例生成器   `python3 3parsing_code_with_input.py --file ...`

### 3. 获取测试函数

prompt模板：`prompt_templates/python_gen_test_template.txt`

- 3.1 把代码块和测试用例生成器拼接后送入沙盒执行，得到测试用例的输出部分  `python3 4exec_sandbox_for_py.py --file ...`
- 3.2 获取Prompt  `python3 5build_gen_test_msg.py --file ..`
- 3.3 调用V3根据测试用例的输出部分，拼接出一个完成的测试函数  `python3 2codgen_batched_api_infer.py --file ...`
- 3.4 解析V3输出，得到测试函数  `python3 6extract_test_function.py --file ...`

### 4. 获取编程问题

prompt模板：`prompt_templates/python_gen_question_template.txt`

有了代码块以及测试函数，生成它们的编程问题。其中public测试函数提示V3将其嵌入到问题中，private测试函数用于测试。

- 4.1 构造Prompt `python3 7build_gen_question_msg.py --file ..`
- 4.2 调用V3生成编程问题  `python3 2codgen_batched_api_infer.py --file ...`
- 4.3 解析V3输出，得到编程问题  `python3 8extract_question.py --file ...`


> 至此已有了<编程问题，测试函数，代码答案>三元组数据，后面步骤只进行filter或refine，不再生成新数据。


### 5. 小模型过滤简单题

使用`Opencoder-8B-Instruct`, `Qwen2.5-Coder-7B-Instruct`, `DeepSeek-Coder-6.7B-Instruct`三个模型做题。
抽取出答案后过沙盒验证，逻辑是：**三个模型都做对的题太简单，扔掉**。

答案抽取： `python3 extract_code.py --file ...`


### 6. V3 + R1 进行Critic

critic prompt模板：`prompt_templates/critic_template.py`

使用`DeepSeek-V3-0324`做题。
然后把 <编程问题，V3回答，参考答案，测试函数，执行器报错/成功信息> 送给R1，评判该题质量。
如果做不对的原因是来自编程问题或测试函数，则把该条数据扔掉。

理论上只把V3做错的题送给R1评判即可。但由于后续人工评估也需要借助模型的Critic信息，因此把做对的题也Critic一下。
逻辑是：**V3既然能做对，说明题目问题不大。做不对的话有可能是题目出的不好，也有可能太难。需要通过Critic看是啥原因**。

    

### 其他

#### 多样性打标

`prompt_templates/tag_template.txt`

#### 效果评测

`python3 extract_code.py --file ...`

`python3 4exec_sandbox_for_py.py --file ...`



沙盒wiki@cmath（https://iwiki.woa.com/p/4014132865?event=confirm_perm_apply&from=wxwork）

`exec_sandbox_for_other.py` 支持cpp, js, go, java

python setup.py sdist bdist_wheel   
twine upload dist/* --repository-url http://mirrors.tencent.com/tencent_pypi/simple/  --username tannerzhang