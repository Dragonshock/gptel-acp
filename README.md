# gptel-acp

gptel 的 ACP 后端。Grok 通过 `acp.el` 和命令 `grok --no-auto-update agent stdio` 对话。gptel 本体不加载这个文件。

目录就是这份 package：

```
gptel-acp/
  README.md
  gptel-acp.el
  test/gptel-acp-test.el
```

`gptel-acp.el` 放在根目录。把这个目录加进 `load-path` 之后，`(require 'gptel-acp)` 就能找到它。

## 依赖

- Emacs 27.1
- gptel 0.9.9.6，并且带有 generic `gptel-backend-send`。上游 karthink/gptel 还没有这个函数。本机的 `/Users/dragon/src/local/gptel/gptel` 在 `gptel--handle-wait` 里调用它，默认方法仍是 HTTP。
- [acp.el](https://github.com/xenodium/acp.el)。加载 `gptel-acp` 时不加载它，第一次真正发送才加载。
- 本机可执行文件 `grok`。

## 安装

在 gptel 之后加载。这份 straight.el 没有 `:type nil` 的版本库后端，所以用 `:load-path`，不用 straight recipe。

```elisp
(use-package gptel-acp
  :straight nil
  :load-path "/Users/dragon/src/local/gptel-acp"
  :after gptel
  :init
  (setq gptel-acp-reasoning-effort "high")
  :config
  (setq gptel-backend (gptel-get-backend "Grok")
        gptel-model 'grok-4.7-build-fast))
```

加载时如果还没有名为 `"Grok"` 的 backend，package 会注册一个：

- 命令：`grok`
- 参数：`("--no-auto-update" "agent" "stdio")`
- 认证：`auto`
- 默认 profile：`chat`
- 模型：`grok-4.7-build-fast`

要自己再建一个 ACP backend，用 `gptel-make-acp`。`:command`、`:command-params`、`:auth`（`auto`、`api-key` 或 `cached-token`）、`:profile`（`chat` 或 `agent`）是 ACP 自己的参数，其余键和普通 `gptel-backend` 一样。

## 配置

`gptel-model` 设为符号 `grok-4.7-build-fast`。它的 `:acp-model-id` 是字符串 `"grok-4.7-build-fast"`。会话建好以后，package 用 `session/set_config_option` 发送它，`configId` 是 `model`，`value` 是这个字符串。

`gptel-acp-reasoning-effort` 是 effort 的 id，常用 `"low"`、`"medium"`、`"high"`、`"xhigh"`。为 nil 时使用模型属性 `:reasoning-effort`。发送时 `configId` 是 `reasoning_effort`，`value` 同样是纯字符串。

`gptel-acp-profile` 是 buffer 本地变量，取 `chat` 或 `agent`。nil 表示用 backend 上的默认 profile。gptel 菜单里的 `-A` 改的就是它。

- `chat` 的 initialize 只声明读取文本。`writeTextFile` 和 `terminal` 不是 true。
- `agent` 声明读、写和终端。
- rewrite 固定开一个一次性的 chat 会话，不把 session id 写回文件。

`gptel-confirm-tool-calls` 为 nil 时，组请求的那一刻记下 yolo，ACP 权限在工具运行前自动允许。其他值仍走 gptel 原来的确认界面。

认证 `auto`：backend 或 `gptel-api-key` 能给出 key 时用 API key，否则用 `~/.grok` 里已有的 cached token。不要把 key 写进配置文件的明文里。

`gptel-temperature`、`gptel-max-tokens`、`gptel--schema` 留在 buffer 本地。ACP 没有对应字段。它们只出现在新会话第一条 prompt 的说明文字里。system 文本进会话的 `_meta.systemPromptOverride`。

## 使用

加载并设好 backend 之后，gptel 的入口不变：

- 任意 buffer 里 `gptel-send`
- `M-x gptel` 打开专用聊天，并启用 `gptel-mode`
- `gptel-menu` 改模型、system、profile
- `gptel-abort` 取消当前请求，ACP 侧发出 `session/cancel`

对话以 buffer 为源头。成功的一轮会把这些变量写成文件的 local variables：

- `gptel-acp-session-id`
- `gptel-acp-history-hash`
- `gptel-acp-system-hash`
- `gptel-acp-profile`

下次打开文件时，两枚哈希都还对得上，就只发送新的 user 轮。哈希变了，就新开一个会话，并把 buffer 里的旧轮次重放进去。只有 session id、没有哈希的旧文件仍按原来的会话加载。

## 未接

Elisp 工具和 mcp.el 不放进 `session/new` 的 `mcpServers`。ACP 上不由 Emacs 代为执行，也不伪造工具结果。HTTP backend 上，这两类工具仍由 Emacs 执行。

## 测试

不加载 Emacs 的 init。`load-path` 只加上 gptel 检出、本目录，以及 `acp.el` 所在目录：

```bash
/etc/profiles/per-user/dragon/bin/emacs -Q --batch \
  --eval '(setq load-path (append (list "/Users/dragon/src/local/gptel/gptel" "/Users/dragon/src/local/gptel-acp" "/Users/dragon/.config/emacs/straight/build/acp") load-path))' \
  -l /Users/dragon/src/local/gptel-acp/test/gptel-acp-test.el \
  -f ert-run-tests-batch-and-exit
```

通过时退出码为 0，输出里有 `0 unexpected`。
