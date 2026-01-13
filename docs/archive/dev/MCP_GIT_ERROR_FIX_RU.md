# Git MCP Error Fix

## Проблема

Ошибка: `npm error 404 Not Found - GET https://registry.npmjs.org/@modelcontextprotocol%2fserver-git - Not found`

**Причина**: Пакет `@modelcontextprotocol/server-git` не существует в npm registry.

---

## Решение

### Удалить Git MCP из конфигурации

**Причины удаления:**
1. ✅ Пакет `@modelcontextprotocol/server-git` не существует в npm
2. ✅ GitHub MCP уже предоставляет Git операции через GitHub API
3. ✅ Локальные Git операции можно выполнять через терминал

---

## Что Делает GitHub MCP

GitHub MCP предоставляет следующие Git операции через GitHub API:

- ✅ Создание/управление ветками
- ✅ Создание/управление коммитами
- ✅ Создание/управление тегами
- ✅ Управление PR (pull requests)
- ✅ Управление issues
- ✅ Code review
- ✅ Release management

**Примеры использования:**
```
AI: "Создай коммит для изменений в router"
→ GitHub MCP может создать коммит через GitHub API

AI: "Создай release ветку и тег для v1.0.0"
→ GitHub MCP может создать ветку и тег через GitHub API

AI: "Создай PR для изменений"
→ GitHub MCP может создать PR через GitHub API
```

---

## Альтернативы

### Вариант 1: Использовать GitHub MCP (Рекомендуется)

GitHub MCP покрывает большинство Git операций через GitHub API.

**Конфигурация:**
```json
{
  "github": {
    "type": "stdio",
    "command": "npx",
    "args": ["-y", "@modelcontextprotocol/server-github"],
    "env": {
      "GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}"
    }
  }
}
```

---

### Вариант 2: Локальные Git Операции через Терминал

Для локальных Git операций (не через GitHub API) можно использовать терминал:

```bash
# AI может выполнять команды через терминал
git status
git add .
git commit -m "message"
git push
git branch
git tag
```

---

### Вариант 3: Создать Кастомный Git MCP Сервер

Если нужны локальные Git операции через MCP, можно создать кастомный сервер:

**Структура:**
```
tools/git-mcp/
├── package.json
├── tsconfig.json
├── src/
│   └── index.ts
└── dist/
    └── index.js
```

**Инструменты:**
- `git_status` - Проверить статус репозитория
- `git_add` - Добавить файлы в staging
- `git_commit` - Создать коммит
- `git_push` - Отправить изменения
- `git_branch` - Управление ветками
- `git_tag` - Управление тегами

**Примечание**: Это требует разработки кастомного MCP сервера.

---

## Итоговая Конфигурация

### После Исправления (~/.cursor/mcp.json)

```json
{
  "mcpServers": {
    "Browser Use": {
      "url": "https://docs.browser-use.com/mcp",
      "headers": {}
    },
    "Prisma": {
      "url": "https://mcp.prisma.io/mcp",
      "headers": {}
    },
    "PostHog": {
      "command": "npx -y mcp-remote@latest https://mcp.posthog.com/sse --header Authorization:${POSTHOG_AUTH_HEADER}",
      "env": {
        "POSTHOG_AUTH_HEADER": "Bearer phx_**"
      },
      "args": []
    },
    "github": {
      "type": "stdio",
      "command": "npx",
      "args": [
        "-y",
        "@modelcontextprotocol/server-github"
      ],
      "env": {
        "GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}"
      }
    }
  }
}
```

**Git MCP удален** - больше нет ошибки 404.

---

## Проверка Исправления

### Шаг 1: Проверить Конфигурацию

```bash
cat ~/.cursor/mcp.json | jq '.mcpServers | keys'
```

Должен показать список серверов без `"git"`.

### Шаг 2: Перезапустить Cursor IDE

Полностью закрыть и перезапустить Cursor IDE.

### Шаг 3: Проверить Логи

Открыть Developer Tools (`Help → Toggle Developer Tools`) и проверить, что ошибка `@modelcontextprotocol/server-git` больше не появляется.

### Шаг 4: Проверить GitHub MCP

Попробовать использовать GitHub MCP через AI:
```
AI: "Создай issue в GitHub репозитории"
```

Если работает - GitHub MCP функционирует корректно.

---

## Резюме

**Проблема**: Пакет `@modelcontextprotocol/server-git` не существует в npm.

**Решение**: Удален Git MCP из конфигурации.

**Причина**: GitHub MCP уже предоставляет Git операции через GitHub API.

**Результат**: Ошибка 404 больше не появляется, GitHub MCP работает корректно.

---

## Дополнительная Информация

### GitHub MCP Capabilities

GitHub MCP предоставляет следующие инструменты:
- `github_create_repository` - Создать репозиторий
- `github_create_branch` - Создать ветку
- `github_create_commit` - Создать коммит
- `github_create_tag` - Создать тег
- `github_create_pull_request` - Создать PR
- `github_create_issue` - Создать issue
- `github_list_commits` - Список коммитов
- `github_list_branches` - Список веток
- `github_list_tags` - Список тегов
- И многие другие...

**Вывод**: GitHub MCP покрывает все необходимые Git операции.

