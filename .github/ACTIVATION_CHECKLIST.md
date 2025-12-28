# GitHub Actions - Чеклист для Активации

## ✅ Что уже готово

- [x] Созданы 9 workflow файлов в `.github/workflows/`
- [x] Создана полная документация (5 файлов)
- [x] Все джобы из GitLab CI имеют эквиваленты
- [x] Настроены path filters для оптимизации
- [x] Настроен concurrency control
- [x] Настроены artifacts с retention policies

## 📋 Что нужно сделать

### 1. Коммит и Push (ОБЯЗАТЕЛЬНО)

```bash
# Проверить изменения
git status

# Добавить все новые файлы
git add .github/

# Закоммитить
git commit -m "feat: add GitHub Actions CI/CD workflows

- Add main CI workflow (ci.yml)
- Add component-specific workflows (gateway, router, worker)
- Add nightly tests workflow
- Add validation workflows (CP2, CI validate, DevState)
- Add comprehensive documentation
- Full parity with GitLab CI configuration

Closes #XXX"

# Push в ваш репозиторий
git push origin main
# или
git push origin <your-branch-name>
```

### 2. Проверка Workflows на GitHub (СРАЗУ ПОСЛЕ PUSH)

1. Зайти на GitHub репозиторий
2. Перейти на вкладку **Actions**
3. Должны появиться workflows:
   - ✅ CI
   - ✅ Nightly Tests
   - ✅ Gateway Tests (если были изменения в apps/c-gateway)
   - ✅ Router Tests (если были изменения в apps/otp/router)
   - ✅ Worker Tests (если были изменения в apps/caf/processor)

### 3. Настройка Secrets (ЕСЛИ НУЖНО)

Перейти: **Settings → Secrets and variables → Actions**

#### Обязательные (если используются):
```
BEAMLINE_HMAC_SECRET
```

#### Опциональные (для будущего):
```
GHCR_TOKEN (для Docker registry)
DEPLOY_KEY (для deployment)
```

**Как добавить secret:**
1. Settings → Secrets and variables → Actions
2. New repository secret
3. Name: `BEAMLINE_HMAC_SECRET`
4. Secret: `<значение>`
5. Add secret

### 4. Branch Protection Rules (РЕКОМЕНДУЕТСЯ)

Перейти: **Settings → Branches → Branch protection rules**

#### Для ветки `main`:
1. **Require status checks to pass before merging** ✅
2. **Status checks that are required:**
   - ✅ `CI / gateway-tests`
   - ✅ `CI / router-tests`
   - ✅ `CI / worker-tests`
   - ✅ `CI / rust-worker`
   - ✅ `CI / summary`

3. **Require branches to be up to date** ✅

Optional:
- ✅ Require pull request reviews before merging
- ✅ Require approvals (1-2)
- ✅ Dismiss stale pull request approvals

### 5. Первый Тест (РЕКОМЕНДУЕТСЯ)

#### Вариант A: Создать тестовую ветку
```bash
git checkout -b test/github-actions-workflows

# Сделать небольшое изменение
echo "# GitHub Actions Test" >> .github/TEST.md
git add .github/TEST.md
git commit -m "test: verify GitHub Actions workflows"
git push origin test/github-actions-workflows

# Создать PR на GitHub
# Проверить что workflows запущены
```

#### Вариант B: Ручной запуск
1. GitHub → Actions
2. Выбрать workflow (например, "CI")
3. Run workflow → Run workflow
4. Проверить выполнение

### 6. Мониторинг и Проверка (ВАЖНО)

После запуска workflows:

1. **Проверить логи**:
   - Перейти в Actions
   - Кликнуть на запущенный workflow
   - Развернуть jobs и steps
   - Проверить что нет ошибок

2. **Проверить artifacts**:
   - Scroll down в workflow run
   - Секция "Artifacts"
   - Должны быть:
     - Test results
     - Coverage reports
     - Test logs

3. **Проверить summary**:
   - В конце workflow run
   - Должен быть summary с статусами

### 7. Сравнение с GitLab CI (ПРОВЕРКА)

Запустить оба CI/CD параллельно:

```bash
# В GitLab: 
# - Зайти в CI/CD → Pipelines
# - Проверить что pipeline успешен

# В GitHub:
# - Зайти в Actions
# - Проверить что workflows успешны

# ОБА должны быть зелёными! ✅
```

### 8. Документация для команды (ОПЦИОНАЛЬНО)

Добавить в README.md:

```markdown
## CI/CD Status

[![CI](https://github.com/YOUR_ORG/YOUR_REPO/workflows/CI/badge.svg)](https://github.com/YOUR_ORG/YOUR_REPO/actions)
[![Nightly Tests](https://github.com/YOUR_ORG/YOUR_REPO/workflows/Nightly%20Tests/badge.svg)](https://github.com/YOUR_ORG/YOUR_REPO/actions)

We use both GitLab CI and GitHub Actions for continuous integration:
- **GitLab CI**: `.gitlab-ci.yml` - Primary CI/CD
- **GitHub Actions**: `.github/workflows/` - GitHub-native CI/CD

See [.github/QUICKSTART.md](.github/QUICKSTART.md) for details.
```

### 9. Отключение Workflows (ЕСЛИ НУЖНО ОТКАТИТЬСЯ)

Если что-то пошло не так:

```bash
# Временно отключить все workflows
mkdir .github/workflows.disabled
mv .github/workflows/*.yml .github/workflows.disabled/

# Закоммитить и push
git commit -am "temp: disable GitHub Actions workflows"
git push
```

### 10. Настройка Nightly Tests (ОПЦИОНАЛЬНО)

Nightly tests уже настроены на запуск в 2:00 AM UTC.

Чтобы изменить время:
1. Открыть `.github/workflows/nightly-tests.yml`
2. Изменить cron:
   ```yaml
   schedule:
     - cron: '0 2 * * *'  # 2:00 AM UTC
     # Формат: минута час день месяц день_недели
     # Пример: '0 3 * * *' = 3:00 AM UTC
   ```

## 🎯 Приоритеты

### Критические (делать сразу):
1. ✅ Коммит и push workflows
2. ✅ Проверка что workflows запущены
3. ✅ Проверка логов на ошибки

### Важные (делать в течение дня):
4. ⏳ Настройка secrets (если нужны)
5. ⏳ Настройка branch protection
6. ⏳ Первый тестовый запуск

### Желательные (можно сделать позже):
7. ⏰ Мониторинг параллельного запуска GitLab + GitHub
8. ⏰ Добавление badges в README
9. ⏰ Настройка времени nightly tests

## 🚨 Возможные проблемы

### Workflow не запускается
**Причина**: Неправильный путь к файлу  
**Решение**: Проверить что файлы в `.github/workflows/` (не `workflow`)

### Tests падают на GitHub но работают локально
**Причина**: Разница в окружении  
**Решение**: Проверить версии зависимостей, OS, environment variables

### Secrets не работают
**Причина**: Не настроены в Settings  
**Решение**: Settings → Secrets and variables → Actions → Add secret

### Path filters не работают
**Причина**: Синтаксис glob patterns  
**Решение**: Проверить что paths указаны корректно (без leading `/`)

## ✅ Критерий успеха

GitHub Actions считается успешно настроенным когда:

1. ✅ Все workflows появились в Actions tab
2. ✅ Main CI запускается на каждый push/PR
3. ✅ Component workflows запускаются при изменениях
4. ✅ Nightly tests запланированы на 2 AM UTC
5. ✅ Artifacts загружаются корректно
6. ✅ Summary показывает статусы всех jobs
7. ✅ GitLab CI и GitHub Actions оба проходят успешно

## 📞 Нужна помощь?

Читайте документацию:
- 🚀 **Quick Start**: `.github/QUICKSTART.md`
- 📖 **Migration Guide**: `.github/GITHUB_ACTIONS_MIGRATION.md`
- 📊 **Structure**: `.github/WORKFLOW_STRUCTURE.md`
- 📝 **Workflows Docs**: `.github/workflows/README.md`

---

**Последнее обновление**: 2025-12-28  
**Статус**: ✅ Ready for activation
