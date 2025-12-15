# DevState HTTP Service (state-manager)

Provides HTTP endpoints for managing DevState backed by PostgreSQL.

## Endpoints

- `GET /health` – service health
- `GET /v1/devstate/verify?limit=0` – verify HMAC chain integrity
- `GET /v1/devstate/state` – read current state
- `POST /v1/devstate/state` – update state (partial JSON patch)
- `POST /v1/devstate/history` – append history entry
- `DELETE /v1/devstate/history/:id` – tombstone history entry (soft delete)
- `POST /v1/devstate/locks` – create lock
- `DELETE /v1/devstate/locks/:id` – release lock
- `GET /v1/devstate/history/search` – search history
- `GET /v1/devstate/export` – export `.trae/state.json` and `.trae/history.json`
- `POST /v1/devstate/import` – import `.trae/state.json` and `.trae/history.json`

## Environment

- `DATABASE_URL` (required)
- `HMAC_SECRET` (required; `BEAMLINE_HMAC_SECRET` supported for legacy only)
- `DEVSTATE_HTTP_PORT` (default `3080`)

## Compose Integration

Service name: `devstate` (container: `trae-devstate`)
Exposed port: `3080`
Gateway proxy route: `GET /v1/devstate/verify`
