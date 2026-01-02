# Руководство по операционным рутинам (Operations Guide)

Этот документ описывает рутинные действия менеджера/техлида по нашему проекту: как запускать основные команды и скрипты, управлять циклом разработки и быстро узнавать текущую ситуацию по репозиторию.

Краткое видение продукта BeamLine и его архитектуры:
- **EN**: [BEAMLINE_VISION_AND_ARCHITECTURE.md](BEAMLINE_VISION_AND_ARCHITECTURE.md) — полный обзор видения и архитектуры.
- **RU**: [BEAMLINE_VISION_OVERVIEW_RU.md](BEAMLINE_VISION_OVERVIEW_RU.md) — краткий обзор на русском.

## Назначение
- Дать единый чеклист повседневных действий.
- Сконцентрировать базовые команды и скрипты.
- Объяснить, какие проверки обязательны перед PR/релизом.

## Статус проекта (CP1-LC)
**Текущий чекпоинт**: CP1-LC (Operational Readiness) ✅  
**Завершено**: Router, Gateway, Rate Limiting, HMAC Chain fixes  
**В прогрессе**: Router Policy Enforcement (RBAC)  
**Следующий**: CP2-PROVIDER после завершения CP1

## Быстрый старт (Dev)
1. Загрузить dev‑окружение:
   ```bash
   source config/env/.env.dev
   ```
2. Базовая проверка проекта локально (похоже на CI):
   ```bash
   bash scripts/dry_run_ci.sh all
   ```
3. Быстрый smoke по схемам/контрактам:
   ```bash
   bash scripts/dry_run_ci.sh schema
   ```
4. Проверить состояние CP1:
   ```bash
   bash scripts/check_cp1_contracts.sh
   ```

## Ежедневные рутины
- Проверить состояние репозитория и прогонить основные проверки:
  ```bash
  bash scripts/run_checks.sh
  ```
- Валидация состояния и HMAC-цепочки (после изменений .trae/*):
  ```bash
  bash scripts/validate_state.sh
  ```
- Линк‑чек документации (перед PR обязательно):
  ```bash
  bash scripts/check_links.sh
  ```
- Сканы безопасности секретов и маскирования:
  ```bash
  bash scripts/check_secret_leaks.sh
  bash scripts/check_hmac_masking.sh
  ```
- Быстрый «CI‑подобный» прогон:
  ```bash
  bash scripts/dry_run_ci.sh all
  ```

## Управление циклом разработки
- План/задачи ведём через обычные тикеты и чек‑листы PR, придерживаемся малых инкрементов.
- Перед архитектурными решениями используем ADR:
  - Создать ADR: "/adr-new" (workflow в IDE) или вручную по шаблону в docs/ADR/ + обновить docs/ADR_INDEX.md.
- Перед PR запускаем локальные проверки:
  - "/ci-like-build" (workflow) или
  - `bash scripts/run_checks.sh` и `bash scripts/dry_run_ci.sh all`.
- Для протокольных/контрактных изменений:
  - "/proto-sync" и "/buf-breaking-against" (при необходимости сравнения) + обновление docs/ARCHITECTURE/api-registry.md.
- **CP1 специфичные проверки**:
  - Границы изоляции: "/cp1-review"
  - Валидация CP1: "/cp1-validate" 
  - Контракты: `bash scripts/check_cp1_contracts.sh`
- **JetStream/NATS/OBS изменения** (ОБЯЗАТЕЛЬНО):
  - Обновить матрицу покрытия: `../apps/otp/router/docs/archive/dev_reports/JETSTREAM_OBS_COVERAGE_MATRIX.md`
  - Обновить сценарии: `../apps/otp/router/docs/archive/dev_reports/JETSTREAM_FAULT_INJECTION_TESTS.md`
  - Добавить scenario IDs в алерты: `apps/otp/router/docs/observability/router-alert-rules.yaml`
  - Добавить scenario IDs в дашборды: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`
  - Валидация: `bash apps/otp/router/scripts/validate_metrics_labels.sh`
  - См. подробности: `CONTRIBUTING.md#jetstreamnatsob-changes`

## Workflow-и в IDE (slash-команды)
- **Что это**
  - Преднастроенные сценарии в IDE (см. .windsurf/workflows/*), которые помогают запускать проверяемые процессы одной командой.
- **Как запускать**
  - В чате IDE ввести слэш-команду, например: `/ci-like-build` и следовать шагам.
  - Либо через палитру команд IDE (если поддерживается) — выбрать нужный workflow.
- **Когда использовать (основные)**
  - **/route-task** — выбрать правильный инструмент (Windsurf/Cursor/TRAE) по типу задачи.
  - **/project-status** — получить краткий статус проекта и прогресс.
  - **/ci-like-build** — локально прогнать все основные проверки, как в CI.
  - **/cp1-review**, **/cp1-validate** — контроль границ и готовности CP1.
  - **/links-check** — проверить ссылки в документации.
  - **/security-scan** — сканы безопасности и утечек секретов.
  - **/dependency-management** — обновление/валидация зависимостей.
  - **/deployment** — деплой и smoke‑проверки (локально/превью).
  - **/adr-new** — создать ADR по архитектурному решению.
  - **/proto-sync**, **/buf-breaking-against** — синхронизировать Proto и проверить breaking‑изменения.
  - **/code-formatting**, **/run-tests-and-fix** — форматирование и запуск тестов с автофиксами.
  - **/state-validate** — валидировать .trae/state и историю.
  - **/compliance-validate** — комплаенс‑проверки и отчёты.
- **Примечания по безопасности**
  - Некоторые workflow запускают локальные команды; IDE может запросить подтверждение. Секреты не передавать, соблюдайте правила из docs/SECURITY_GUIDE.md.
  - В шагах с пометкой `// turbo` часть команд может авто‑выполняться; это касается только безопасных действий.
- **Быстрые примеры**
  - Быстрый статус: `/project-status`
  - Перед PR: `/ci-like-build`, затем `/links-check` и `/security-scan`
  - Архитектурное решение: `/adr-new`
  - Обновление Proto/API: `/proto-sync` → `/buf-breaking-against`
  - Не знаете, куда идти: `/route-task`

## Dashboard управления проектом (Workflows → Результаты)

| Команда | Назначение | Что получите | Когда запускать | Заметки |
|---|---|---|---|---|
| `/project-status` | Сводка состояния проекта | Краткий отчет о прогрессе, ключевые статусы, файлы/ссылки | Ежедневно/стенд‑ап | Быстрый обзор без мутаций |
| `/route-task` | Подбор инструмента (Windsurf/Cursor/TRAE) | Рекомендация инструмента и следующего шага | В начале задачи | Учитывает тип задачи и объем изменений |
| `/ci-like-build` | Локальный прогон, как в CI | Комплексный лог проверок, статус pass/fail | Перед PR и перед мерджем | Может занять время, но даёт уверенность |
| `/links-check` | Проверка ссылок в доках | Отчет о битых ссылках/якорях | Перед PR | Быстрая проверка документации |
| `/security-scan` | Безопасность и секреты | Отчет по утечкам, маскированию, уязвимостям | Перед PR/релизом | Не выводить секреты в логи |
| `/dependency-management` | Управление зависимостями | Обновления/валидация зависимостей и отчеты | По расписанию/перед релизом | Может требовать ручного вмешательства |
| `/deployment` | Деплой + smoke | Превью/локальный деплой и результат smoke | По запросу демо/проверки | Смотри логи шагов деплоя |
| `/adr-new` | Архитектурное решение | Скелет ADR + запись в индекс | Перед существенными изменениями | Стабилизирует обсуждение и историю решений |
| `/proto-sync` | Синхронизация Proto | Обновленные stubs/согласование схем | При изменениях контрактов | В паре с breaking‑чеками |
| `/buf-breaking-against` | Проверка breaking | Отчет о несовместимостях с базой | После `/proto-sync` | Требует указания базовой ветки/тага |
| `/code-formatting` | Форматирование кода | Отформатированные файлы, чистые диффы | Перед PR | Уменьшает шум в обзоре кода |
| `/run-tests-and-fix` | Тесты с автофиксами | Логи тестов, примененные автофиксы | Регулярно в фиче‑ветке | Следить за изменениями автофиксов |
| `/state-validate` | Валидация .trae/state | Отчет валидации состояния и истории | Перед PR и релизом | Связан с HMAC‑цепочкой |
| `/cp1-review` | Ревью границ CP1 | Чеклист соблюдения инвариантов | Перед изменениями CP1 | Архитектурно‑критично |
| `/cp1-validate` | Валидация CP1 | Отчет готовности/нарушений | Перед срезами CP1 | Дополняет `/cp1-review` |
| `/compliance-validate` | Комплаенс | Отчеты соответствия и лицензий | Перед релизом | Смотрите папку reports/ |

### Детализация: `/project-status`
- **Назначение**
  - Получить полную картину текущего состояния проекта: текущий CP, прогресс по компонентам, соответствие спецификациям, рекомендации.
- **Когда использовать**
  - Ежедневно (быстрый обзор), еженедельно (глубокий анализ), перед демо/переприоритизацией.
- **Что анализируется**
  - `.trae/state.json` (current_cp, статусы агентов, no_drift, timestamps)
  - `README.md` (статусы сборки/тестов/Dialyzer при наличии)
  - `docs/CP1_ACCEPTANCE_REPORT.md` (критерии приёмки CP1)
  - Codemaps (архитектурная целостность), Git‑история (активность)
- **Результат**
  - Сводка статусов и рисков, проценты прогресса по CP/компонентам
  - Рекомендации (корректировки ТЗ, приоритеты)
  - Отчёт/дашборд (может сохраняться как `.windsurf/reports/project-status-YYYY-MM-DD.md`)
- **Как запустить**
  - В чате IDE: `/project-status`
  - Либо открыть workflow: `.windsurf/workflows/project-status.md`

## Проверки качества (обязательно перед PR)
- Форматирование и линтинг кода: "/code-formatting" или `bash scripts/run_checks.sh`.
- Линк‑чек документации: `bash scripts/check_links.sh`.
- Секреты и маскирование: `bash scripts/check_secret_leaks.sh`, `bash scripts/check_hmac_masking.sh`.
- Схемы/состояние .trae: `bash scripts/dry_run_ci.sh schema`.

## Работа с контрактами (Proto/REST/NATS)
- Proto: соблюдаем buf‑правила совместимости.
  ```bash
  bash scripts/check_proto.sh
  bash scripts/check_proto_sync.sh
  ```
- REST DTO: источник истины — docs/ARCHITECTURE/api-registry.md. Обновляем через скрипты:
  ```bash
  bash scripts/update_api_registry.sh
  ```
- NATS Subjects: версии обязательны (например, beamline.usage.v1.*). Проверки совместимости см. документацию по маршрутизации.

## Состояние проекта и история (.trae)
- Валидация текущего состояния/истории:
  ```bash
  bash scripts/validate_state.sh
  python3 scripts/verify_hmac_chain.py --verbose
  ```
- В Dev доступен Dev‑only экспорт/импорт через Mnesia (если включено):
  ```bash
  # импорт .trae/* в Mnesia
  bash scripts/beamline_store_export.sh import
  # экспорт Mnesia → .trae/* (атомарно)
  bash scripts/beamline_store_export.sh export
  # проверка HMAC‑цепочки через Erlang
  bash scripts/beamline_store_export.sh verify_chain
  ```
  Примечание: Dev‑хук экспорта автоматом вызывается в `scripts/dry_run_ci.sh` перед валидаторами, если не `CI` и `PRODUCTION=false`.

## Узнать текущую ситуацию быстро
- Полный локальный отчёт, приближенный к CI:
  ```bash
  bash scripts/dry_run_ci.sh all
  ```
- Только схемы/контракты:
  ```bash
  bash scripts/dry_run_ci.sh schema
  ```
- Проверить ссылки/доки:
  ```bash
  bash scripts/check_links.sh
  ```
- Проверить секреты/маскирование:
  ```bash
  bash scripts/check_secret_leaks.sh
  bash scripts/check_hmac_masking.sh
  ```

## Безопасность и секреты
- В CI/Prod обязателен `BEAMLINE_HMAC_SECRET` (не хранить в VCS). В Dev допустим дефолт.
- Не отправляем секреты во внешние системы; логи маскируют чувствительные поля как [REDACTED]/[MASKED].
- Подробнее см.: docs/SECURITY_GUIDE.md, docs/ENVIRONMENT.md.

## Частые команды (шпаргалка)
- Форматирование/линт/проверки:
  ```bash
  bash scripts/run_checks.sh
  ```
- CI‑подобный прогон:
  ```bash
  bash scripts/dry_run_ci.sh all
  ```
- Схемы и .trae:
  ```bash
  bash scripts/dry_run_ci.sh schema
  bash scripts/validate_state.sh
  python3 scripts/verify_hmac_chain.py --verbose
  ```
- Документация: линк‑чек
  ```bash
  bash scripts/check_links.sh
  ```
- Секреты/маскирование:
  ```bash
  bash scripts/check_secret_leaks.sh
  bash scripts/check_hmac_masking.sh
  ```
- API реестр (REST):
  ```bash
  bash scripts/update_api_registry.sh
  ```

## Троблшутинг (коротко)
- "Падает линк‑чекер": убедитесь, что пути и якоря верны, локальные ссылки начинаются с docs/…
- "Buf breaking не проходит": проверьте proto/buf.yaml правила совместимости; при реальном breaking — готовьте версионирование.
- "Валидация .trae не проходит": запустите `bash scripts/validate_state.sh` и `python3 scripts/verify_hmac_chain.py --verbose`, изучите лог; в Dev можно переэкспортировать из Mnesia (см. выше).
- "Нет зависимостей для beamline_store": Dev‑хук пропускается автоматически, CI/Prod не затронуты.

## Полезные ссылки
- ADR индекс: docs/ADR_INDEX.md
- Архитектура (API‑реестр): docs/ARCHITECTURE/api-registry.md
- Маршрутизация/NATS: docs/ROUTING_POLICY.md, docs/ARCHITECTURE/PROTO_NATS_MAPPING.md
- Наблюдаемость: docs/OBSERVABILITY.md, docs/OBSERVABILITY_CONVENTIONS.md
- Безопасность и окружение: docs/SECURITY_GUIDE.md, docs/ENVIRONMENT.md
- Комплаенс/процессы: docs/COMPLIANCE_GOVERNANCE.md
- Использование Dev Mnesia/экспортов: docs/BEAMLINE_STORE_USAGE.md
