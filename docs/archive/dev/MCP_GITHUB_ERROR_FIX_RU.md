# GitHub MCP Server Error Fix

## Проблема

Ошибка: `[error] GitHub MCP Server running on stdio`

## Причины

1. **Отсутствует `"type": "stdio"`** в конфигурации
2. **Переменная `GITHUB_TOKEN` не доступна для Cursor IDE**
3. **Неправильный формат конфигурации**

## Решение 1: Добавить `"type": "stdio"`

### Проблема

В конфигурации отсутствовал явный указатель типа транспорта:

```json
{
  "github": {
    "command": "npx",
    "args": ["-y", "@modelcontextprotocol/server-github"],
    "env": {
      "GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}"
    }
  }
}
```

### Исправление

Добавить `"type": "stdio"`:

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

## Решение 2: Проверка Переменной Окружения

### Проблема

Cursor IDE может не видеть переменную `GITHUB_TOKEN`, если:
- Cursor запущен не из терминала
- Переменная не установлена глобально
- Cursor запущен до установки переменной

### Проверка

```bash
# Проверить переменную
echo $GITHUB_TOKEN

# Должен показать токен
```

### Исправление

1. **Убедиться, что переменная установлена в ~/.bashrc**:
   ```bash
   grep GITHUB_TOKEN ~/.bashrc
   ```

2. **Перезагрузить shell**:
   ```bash
   source ~/.bashrc
   ```

3. **Перезапустить Cursor IDE полностью** (не только окно)

4. **Запустить Cursor из терминала** (чтобы унаследовать переменные):
   ```bash
   cursor .
   ```

---

## Решение 3: Альтернативная Конфигурация

### Если переменная не работает

Можно указать токен напрямую (менее безопасно, но работает):

```json
{
  "github": {
    "type": "stdio",
    "command": "npx",
    "args": ["-y", "@modelcontextprotocol/server-github"],
    "env": {
      "GITHUB_PERSONAL_ACCESS_TOKEN": "github_pat_your_token_here"
    }
  }
}
```

**⚠️ ВНИМАНИЕ**: Не коммитьте файл с реальным токеном в Git!

---

## Решение 4: Использование .env Файла

### Создать ~/.env.secrets

```bash
# ~/.env.secrets (не коммитить!)
GITHUB_TOKEN=github_pat_your_token_here
```

### Загрузить перед запуском Cursor

```bash
export $(cat ~/.env.secrets | xargs)
cursor .
```

---

## Проверка Исправления

### Шаг 1: Проверить Конфигурацию

```bash
cat ~/.cursor/mcp.json | jq '.mcpServers.github'
```

Должно содержать:
- `"type": "stdio"`
- `"command": "npx"`
- `"args": ["-y", "@modelcontextprotocol/server-github"]`
- `"env": { "GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}" }`

### Шаг 2: Проверить Переменную

```bash
echo $GITHUB_TOKEN
```

Должен показать токен.

### Шаг 3: Перезапустить Cursor IDE

Полностью закрыть и перезапустить Cursor IDE.

### Шаг 4: Проверить GitHub MCP

Попробовать использовать GitHub MCP через AI:
```
AI: "Создай issue в GitHub репозитории"
```

Если работает без ошибок - проблема решена.

---

## Troubleshooting

### Ошибка все еще появляется

1. **Проверить логи Cursor IDE**:
   - Открыть Developer Tools (Help → Toggle Developer Tools)
   - Проверить консоль на ошибки

2. **Проверить, что пакет установлен**:
   ```bash
   npx -y @modelcontextprotocol/server-github --version
   ```

3. **Проверить формат токена**:
   - Fine-grained token: `github_pat_...`
   - Classic token: `ghp_...`
   - Оба формата должны работать

4. **Проверить права токена**:
   - `repo` - для работы с репозиториями
   - `workflow` - для работы с workflows
   - `read:org` - для чтения организаций (если нужно)

---

## Итоговая Конфигурация

### Правильная Конфигурация (~/.cursor/mcp.json)

```json
{
  "mcpServers": {
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

### Переменная Окружения (~/.bashrc)

```bash
# GitHub Token for Cursor MCP
export GITHUB_TOKEN="github_pat_your_token_here"
```

---

## Резюме

**Основные исправления:**
1. ✅ Добавить `"type": "stdio"` в конфигурацию
2. ✅ Убедиться, что `GITHUB_TOKEN` установлен в ~/.bashrc
3. ✅ Перезагрузить shell: `source ~/.bashrc`
4. ✅ Перезапустить Cursor IDE полностью
5. ✅ Запустить Cursor из терминала (если возможно)

После этих шагов GitHub MCP должен работать корректно.

