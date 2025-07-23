import os
import requests

access_token = os.getenv("Comment_TOKEN")
pull_id = os.getenv("GITEE_PULL_ID")
repo = "XmacsLabs/goldfish"  
headers = {"Content-Type": "application/json;charset=UTF-8"}
api_base = f"https://gitee.com/api/v5/repos/{repo}/pulls/{pull_id}"

if not access_token or not pull_id:
    print("❌ 缺少必要环境变量：Comment_TOKEN 或 GITEE_PULL_ID")
    exit(1)

# 获取 PR 文件列表
files_url = f"{api_base}/files?access_token={access_token}"
files_response = requests.get(files_url, headers=headers)
if files_response.status_code != 200:
    print("❌ 获取文件失败:", files_response.status_code, files_response.text)
    exit(1)

files = [f["filename"] for f in files_response.json()]
print(f"📂 该 PR 修改了 {len(files)} 个文件")

# 判断是否包含文档修改，并收集文档文件
doc_files = [
    f for f in files if f.endswith((".md")) and f.startswith("devel/")
]

# 获取该 PR 的 HEAD commit SHA
pr_info_url = f"{api_base}?access_token={access_token}"
pr_info_response = requests.get(pr_info_url, headers=headers)
if pr_info_response.status_code != 200:
    print("❌ 获取 PR 信息失败:", pr_info_response.status_code, pr_info_response.text)
    exit(1)

commit_sha = pr_info_response.json().get("head", {}).get("sha", "")
if not commit_sha:
    print("❌ 无法获取 commit SHA")
    exit(1)

# 构建评论内容
if doc_files:
    message_lines = [
        "[CI 自动评论]",
        f"📂 该 PR 修改了 {len(files)} 个文件",
        "该 PR 包含文档修改 ✅，相关文件如下："
    ]
    for f in doc_files:
        link = f"https://gitee.com/{repo}/blob/{commit_sha}/{f}"
        message_lines.append(f"- [{f}]({link})")
    message = "\n".join(message_lines)
else:
    message = "[CI 自动评论]\nPR 提交成功 ✅（未发现文档修改）"

print("📝 准备发送评论内容：\n", message)

# 发送评论
comment_url = f"{api_base}/comments?access_token={access_token}"
payload = {"body": message}
comment_response = requests.post(comment_url, json=payload, headers=headers)

if comment_response.status_code == 201:
    print("✅ 成功评论到 PR 页面")
else:
    print("❌ 评论失败:", comment_response.status_code, comment_response.text)
