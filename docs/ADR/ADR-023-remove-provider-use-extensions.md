# ADR-023: Remove apps/otp/provider in Favor of Custom Provider Extensions

**Status**: Accepted  
**Date**: 2025-11-22  
**Authors**: Development Team  
**Replaces**: Initial Provider architecture concept

---

## Context

Проект имел два конфликтующих подхода к интеграции внешних LLM/API провайдеров:

1. **`apps/otp/provider`** — отдельный Erlang/OTP проект для адаптеров
2. **Custom Provider Extensions** — паттерн из Extensions API (docs/EXTENSIONS_API.md)

### Проблемы apps/otp/provider

1. **Дублирование концепции**: Extensions API уже определяет тип "Custom Provider"
2. **Нарушение NATS subject convention**: 
   - Provider использовал: `beamline.provider.v1.invoke`
   - Extensions требует: `beamline.provider.{id}.v1`
3. **Монолитность**: Все провайдеры в одном OTP-проекте
4. **Отсутствие Extension Registry**: Нет централизованного управления
5. **Сложность масштабирования**: Добавление провайдера = изменение кода
6. **Язык lock-in**: Только Erlang, тогда как Extensions могут быть на любом языке

### Extensions API преимущества

- ✅ Плагируемость через конфигурацию
- ✅ Независимое развертывание каждого провайдера
- ✅ Множество языков (Go, Rust, Erlang, Python)
- ✅ Extension Registry для централизованного управления
- ✅ Routing Policy для декларативного выбора
- ✅ Полная изоляция (отдельные процессы/сервисы)

---

## Decision

**Удалить `apps/otp/provider` и следовать паттерну Custom Provider Extensions из Extensions API.**

### Действия

1. ✅ Удалена директория `apps/otp/provider/`
2. ✅ Обновлена документация (README.md, docs/CORE_COMPONENTS.md, docs/ARCHITECTURE/repository-structure.md)
3. ✅ Обновлён `.trae/state.json` (убрано из supporting_tools)
4. ✅ Удалены Provider-специфичные документы (CP2_PROVIDER_PREPARATION.md, Provider-Integration-Test-Plan.md)
5. ✅ Создан гайд: `docs/archive/dev/CUSTOM_PROVIDER_EXTENSIONS_GUIDE.md`

---

## Consequences

### Positive

1. **Единый подход**: Один паттерн для всех расширений (pre/validator/post/provider)
2. **Масштабируемость**: Каждый провайдер — независимый сервис
3. **Гибкость**: Любой язык для любого провайдера
4. **Упрощение**: Меньше кода в монорепе
5. **Следование архитектуре**: Extensions API — ядерный компонент

### Negative

1. **Миграция**: Нужно переписать существующие Provider mock'и (если были)
2. **Deployment complexity**: Больше сервисов для развертывания
3. **Learning curve**: Команда должна освоить Extensions API

### Neutral

1. **Extension Registry**: Требуется реализация (PostgreSQL + Mnesia cache)
2. **NATS subjects**: Каждый провайдер получает уникальный subject

---

## Implementation Notes

### Extension Registry Schema

```sql
CREATE TABLE extensions (
    id VARCHAR(255) PRIMARY KEY,
    type VARCHAR(50) NOT NULL CHECK (type IN ('pre', 'validator', 'post', 'provider')),
    subject VARCHAR(255) NOT NULL UNIQUE,
    timeout_ms INTEGER NOT NULL DEFAULT 5000,
    retry INTEGER NOT NULL DEFAULT 1,
    config JSONB,
    created_at TIMESTAMP DEFAULT NOW()
);

-- Example providers
INSERT INTO extensions (id, type, subject, timeout_ms, retry) VALUES
    ('openai_gpt4', 'provider', 'beamline.provider.openai_gpt4.v1', 30000, 2),
    ('anthropic_claude', 'provider', 'beamline.provider.anthropic_claude.v1', 30000, 2);
```

### Router Integration

Router уже имеет:
- ✅ NATS client для pub/sub
- ✅ Policy store (Mnesia/PostgreSQL)
- ✅ Routing decision logic

Требуется добавить:
- [ ] Extension Registry loader (PostgreSQL → Mnesia)
- [ ] Extension lookup by provider_id
- [ ] NATS request-reply к `beamline.provider.{id}.v1`

---

## Migration Path (Future Providers)

Для каждого LLM/API провайдера:

1. **Создать отдельный репозиторий/сервис**
   ```
   providers/openai-extension/
   ├── main.go (или main.rs, main.erl, main.py)
   ├── Dockerfile
   └── k8s/
       └── deployment.yaml
   ```

2. **Зарегистрировать в Extension Registry**
   ```sql
   INSERT INTO extensions (id, type, subject, timeout_ms) 
   VALUES ('openai_gpt4', 'provider', 'beamline.provider.openai_gpt4.v1', 30000);
   ```

3. **Обновить Routing Policy**
   ```json
   {
     "steps": [
       {"action": "select_provider", "provider_id": "openai_gpt4", "weight": 80}
     ]
   }
   ```

4. **Деплой независимо от Router**
   ```bash
   kubectl apply -f providers/openai-extension/k8s/
   ```

---

## References

- [Extensions API](../EXTENSIONS_API.md)
- [Custom Provider Extensions Guide](../archive/dev/CUSTOM_PROVIDER_EXTENSIONS_GUIDE.md)
- [Routing Policy](../ROUTING_POLICY.md)
- [NATS Subjects](../NATS_SUBJECTS.md)
- [ADR Index](../ADR_INDEX.md)

---

## Approval

- [x] Architecture Team
- [x] Development Team
- [ ] Operations Team (requires Extension Registry setup)

**Implementation Priority**: CP2+ (после завершения CP1-LC Router/Gateway)
