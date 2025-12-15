DEVSTATE_COMPOSE := devstate-tools/devstate/docker-compose.yml

.PHONY: devstate-up devstate-down devstate-logs devstate-init devstate-health devstate-verify devstate-export
.PHONY: devstate-set-cp
.PHONY: devstate-sync devstate-sync-ci
.PHONY: install-git-hooks install-devstate-hooks

devstate-up:
	@echo "Starting DevState standalone stack (ports: HTTP 3180, PG 55432)"
	docker compose -f $(DEVSTATE_COMPOSE) up -d

devstate-down:
	@echo "Stopping DevState standalone stack"
	docker compose -f $(DEVSTATE_COMPOSE) down

devstate-logs:
	@echo "Tailing DevState logs"
	docker compose -f $(DEVSTATE_COMPOSE) logs -f

devstate-init:
	@echo "Initializing DevState database schema"
	@docker cp devstate-tools/devstate/sql/init_devstate.sql devstate-db:/init_devstate.sql >/dev/null
	@docker compose -f $(DEVSTATE_COMPOSE) exec -T db psql -U devstate -d devstate -f /init_devstate.sql

devstate-health:
	@curl -fsS http://localhost:3180/health || curl -fsS http://localhost:3080/health

devstate-verify:
	@curl -fsS 'http://localhost:3180/v1/devstate/verify?limit=0' || curl -fsS 'http://localhost:3080/v1/devstate/verify?limit=0'

devstate-export:
	@bash devstate-tools/devstate/scripts/devstate_export.sh

devstate-set-cp:
	@if [ -z "$$CP" ]; then echo "Usage: make devstate-set-cp CP=CP1-LC"; exit 1; fi
	@echo "Setting CP=$$CP via DevState"
	@bash devstate-tools/devstate/scripts/devstate_set_cp.sh CP=$$CP

devstate-sync:
	@echo "[STEP 1] Recompute artifact checksums"
	@bash scripts/reports/recompute_checksums.sh || true

	@python3 scripts/recalculate_hmac_chain.py --secret "$$${BEAMLINE_HMAC_SECRET:-beamline-secret-key-v1}" --backup --no-verify || true
	@echo "[STEP 2.5] Recompute checksums after HMAC/history update"
	@bash scripts/reports/recompute_checksums.sh || true
	@echo "[STEP 3] Validate state & history"
	@bash scripts/validate_state.sh

install-devstate-hooks:
	@echo "Installing DevState Git hooks..."
	@bash scripts/install_devstate_hooks.sh
	@echo "✅ DevState hooks installed successfully"
	@echo "Hooks will verify DevState before commits and pushes"

devstate-sync-ci:
	@echo "[STEP 1] Recompute artifact checksums"
	@bash scripts/reports/recompute_checksums.sh
	@echo "[STEP 2] Recalculate HMAC chain (CI)"
	@python3 scripts/recalculate_hmac_chain.py --secret "$$${BEAMLINE_HMAC_SECRET}" --backup
	@echo "[STEP 2.5] Recompute checksums after HMAC/history update (CI)"
	@bash scripts/reports/recompute_checksums.sh
	@echo "[STEP 3] Validate state & history (CI)"
	@CI=true bash scripts/validate_state.sh

install-git-hooks:
	@mkdir -p .git/hooks
	@cp scripts/hooks/pre-push .git/hooks/pre-push
	@chmod +x .git/hooks/pre-push
	@cp scripts/hooks/pre-commit .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "Installed git hooks: pre-push, pre-commit"
