# AutoCodeBench评测

## 1.准备一份文件
在AutoCodeBench/bench_20.jsonl文件的字段基础上，新增output字段，表示模型的输出答案。


## 2.启动多语言沙盒

### 1. 拉取镜像

```bash
docker pull your-registry/multi-language-sandbox:latest
```

### 2. 启动服务
```bash
docker run -d \
  --name sandbox-service \
  -p 8080:8080 \
  --cap-add=NET_ADMIN \
  your-registry/multi-language-sandbox:latest
```

### 3. 验证服务

服务启动后，可以通过以下方式验证是否正常运行：

```bash
# 检查容器状态
docker ps | grep sandbox

# 测试服务健康状态
curl -X POST http://localhost:8080/submit \
  -H "Content-Type: application/json" \
  -d '{"src_uid": "test-001", "lang": "python", "source_code": "print(\"Hello World\")"}'
```

如果返回包含 `"exec_outcome": "PASSED"` 的JSON响应，说明服务运行正常。


## 3. 评估
