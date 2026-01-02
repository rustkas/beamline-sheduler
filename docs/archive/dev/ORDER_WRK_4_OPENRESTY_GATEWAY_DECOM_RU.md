# ORDER-WRK-4-OPENRESTY-002: Де-комиссия NestJS Gateway после миграции на новый HTTP Gateway

**Owner**: wrk-4 (Gateway Lead)  
**CP Phase**: CP4-INTEGRATION (Gateway & UI)  
**Related ADR**: ADR-016 (Gateway Migration from NestJS to alternative HTTP gateway)  
**Depends on**: `ORDER-WRK-4-OPENRESTY-001` (миграция Gateway)

---

## 1. Purpose / Цель

Безопасно удалить NestJS-Gateway (`apps/gateway/`) и все связанные артефакты после успешной миграции на новый HTTP Gateway, не нарушив CP-контракты и инфраструктуру.

В конце этого ORDER:

- Все прод/стейдж/дев окружения используют только новый HTTP Gateway.
- NestJS-Gateway и его CI/infra артефакты удалены из монорепо.
- Документация и ADR-статусы отражают факт де-комиссии.

---

## 2. Preconditions / Предусловия

1. `ORDER-WRK-4-OPENRESTY-001` выполнен:
   - новый HTTP Gateway реализован и стабилен.
   - Контракты и поведение выровнены с NestJS.
   - Dev/Stage используют новый HTTP Gateway как основной Gateway.
2. ADR-016 имеет статус не ниже `accepted` (или согласован командой как актуальное решение).

---

## 3. Tasks / Задачи

### Task W4-OG-DC-1 — Инвентаризация зависимостей NestJS-Gateway

**ID**: CP4-GW-DC-001  
**Цель:** собрать полный список мест, где используется NestJS-Gateway.

**Шаги:**

1. Найти все ссылки на `apps/gateway/` в:
   - docker-compose, Kubernetes-манифестах, infra-скриптах;
   - CI-пайплайнах (`.github/`, `ci/`);
   - документации (docs/).
2. Зафиксировать результаты в коротком отчёте (раздел внутри этого ORDER или отдельный `*_REPORT` в `docs/archive/dev/`).

**Definition of Done:**

- Есть список всех зависимостей NestJS-Gateway (infra, CI, docs).

---

### Task W4-OG-DC-2 — Переключение всех окружений на новый HTTP Gateway

**ID**: CP4-GW-DC-002  
**Цель:** убедиться, что ни одно окружение больше не использует NestJS-Gateway.

**Шаги:**

1. Проверить dev/stage/prod конфигурации:
   - docker-compose, Helm charts, Kubernetes manifests.
2. Убедиться, что:
   - ingress/proxy указывают на новый HTTP Gateway;
   - NestJS-поды/контейнеры не участвуют в траектории запросов.
3. При необходимости обновить конфигурации (отдельными infra-PR).

**Definition of Done:**

- Для всех окружений задокументировано, что HTTP-трафик идёт только через новый HTTP Gateway.

---

### Task W4-OG-DC-3 — Удаление `apps/gateway/` и связанных скриптов

**ID**: CP4-GW-DC-003  
**Цель:** физически удалить NestJS-Gateway из монорепо.

**Шаги:**

1. Удалить каталог `apps/gateway/` (код, конфиги, package.json и т.п.).
2. Обновить:
   - `scripts/local/*`, `scripts/run_checks.sh`, `docker-compose.yml` (если там упоминался NestJS-Gateway);
   - любые helper-скрипты, завязанные на NestJS.
3. Убедиться, что локальный запуск (`scripts/local/up.sh`) и CI проходят без NestJS.

**Definition of Done:**

- `apps/gateway/` отсутствует в репозитории.
- Все скрипты/команды, ранее завязанные на NestJS, либо обновлены, либо удалены.

---

### Task W4-OG-DC-4 — Обновление документации и ADR-статусов

**ID**: CP4-GW-DC-004  
**Цель:** привести документацию в соответствие новой реальности (только новый HTTP Gateway).

**Шаги:**

1. Обновить:
   - `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md` (убедиться, что NestJS упоминается только как historical/legacy, если нужно);
   - `docs/ARCHITECTURE/api-registry.md` (если там были прямые ссылки на NestJS-реализацию);
   - любые dev-доки, ссылающиеся на NestJS-Gateway как на активный компонент.
2. Обновить ADR-статусы:
   - ADR-008 (NestJS for Gateway API) → `superseded` с ссылкой на ADR-016;
   - ADR-016 → `accepted` (после формального решения команды).
3. Обновить ADR_INDEX статистику, если нужно (deprecated/superseded счётчики).

**Definition of Done:**

- В документации активным HTTP-Gateway считается только новый HTTP Gateway.
- ADR-008 помечен как `superseded`, ADR-016 как `accepted`.

---

### Task W4-OG-DC-5 — Финальная валидация CP и CI

**ID**: CP4-GW-DC-005  
**Цель:** убедиться, что удаление NestJS не нарушило CP/CI-инварианты.

**Шаги:**

1. Запустить полный набор проверок:
   - `bash scripts/run_checks.sh`;
   - `bash scripts/dry_run_ci.sh all`;
   - специфические проверочные workflow (`/ci-like-build`, `/links-check`, `/state-validate`, `/cp1-review`, если актуально).
2. Проверить, что нигде не осталось ссылок/упоминаний `apps/gateway/` как активного сервиса.
3. Зафиксировать результат (короткий summary в этом ORDER или отдельном отчёте).

**Definition of Done:**

- Все проверки проходят без ошибок.
- Логи/репорты не содержат ошибок, связанных с отсутствием NestJS-gateway.

---

## 4. Exit Criteria / Критерии завершения ORDER

ORDER-WRK-4-OPENRESTY-002 считается завершённым, когда:

1. Все окружения (dev/stage/prod) подтверждённо используют только новый HTTP Gateway.
2. Каталог `apps/gateway/` и связанные артефакты удалены из репозитория.
3. ADR-008 помечен как `superseded`, ADR-016 как `accepted` (или эквивалентное решение команды).
4. Все стандартные проверки (`scripts/run_checks.sh`, `dry_run_ci.sh all`, docs/links) проходят успешно.
5. Документация не описывает NestJS-gateway как активный компонент.
