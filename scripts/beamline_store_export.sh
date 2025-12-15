#!/usr/bin/env bash
set -euo pipefail

# Beamline Store import/export helper for .trae/state.json and .trae/history.json
# Usage:
#   bash scripts/beamline_store_export.sh import            # import both state and history into Mnesia
#   bash scripts/beamline_store_export.sh export            # export both from Mnesia to .trae files (atomic)
#   bash scripts/beamline_store_export.sh import_state      # import only state
#   bash scripts/beamline_store_export.sh import_history    # import only history
#   bash scripts/beamline_store_export.sh export_state      # export only state (atomic)
#   bash scripts/beamline_store_export.sh export_history    # export only history (atomic)
#   bash scripts/beamline_store_export.sh verify_chain      # verify HMAC chain using BEAMLINE_HMAC_SECRET

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

STATE_PATH="${PROJECT_ROOT}/.trae/state.json"
HISTORY_PATH="${PROJECT_ROOT}/.trae/history.json"

PROFILE=${PROFILE:-default}

if ! command -v rebar3 >/dev/null 2>&1; then
  echo "[FAIL] rebar3 not found. Install rebar3 to use beamline_store."
  exit 1
fi

pushd "$PROJECT_ROOT" >/dev/null

if [[ ! -d "apps/otp/beamline_store" ]]; then
  echo "[FAIL] apps/otp/beamline_store not found. Ensure the OTP app is added."
  exit 1
fi

echo "[INFO] Compiling beamline_store (profile=${PROFILE})"
pushd "apps/otp/beamline_store" >/dev/null
if ! rebar3 as "$PROFILE" compile >/dev/null; then
  echo "[WARN] rebar3 compile failed; beamline_store export will be skipped"
  exit 2
fi
popd >/dev/null

# Prefer local _build under the app directory
APP_BUILD_DIR="${PROJECT_ROOT}/apps/otp/beamline_store/_build/${PROFILE}/lib"
ROOT_BUILD_DIR="${PROJECT_ROOT}/_build/${PROFILE}/lib"

if [[ -d "$APP_BUILD_DIR/beamline_store/ebin" ]]; then
  EBIN_BEAMLINE="${APP_BUILD_DIR}/beamline_store/ebin"
  EBIN_JIFFY="${APP_BUILD_DIR}/jiffy/ebin"
else
  EBIN_BEAMLINE="${ROOT_BUILD_DIR}/beamline_store/ebin"
  EBIN_JIFFY="${ROOT_BUILD_DIR}/jiffy/ebin"
fi

if [[ ! -d "$EBIN_BEAMLINE" ]]; then
  echo "[FAIL] ${EBIN_BEAMLINE} missing after compile"
  exit 1
fi
if [[ ! -d "$EBIN_JIFFY" ]]; then
  echo "[FAIL] ${EBIN_JIFFY} missing after compile (dependency jiffy)"
  exit 1
fi

export ERL_FLAGS="-pa ${EBIN_BEAMLINE} -pa ${EBIN_JIFFY}"

# Use a stable Erlang node name and a persistent Mnesia directory under the repo
ERL_NODE_NAME="beamline_store_dev"
MNESIA_DIR="${PROJECT_ROOT}/.trae/mnesia"
mkdir -p "${MNESIA_DIR}"
ERL_NODE_FLAGS="-sname ${ERL_NODE_NAME} -mnesia dir '\"${MNESIA_DIR}\"'"

cmd=${1:-}
case "$cmd" in
  import)
    erl -noshell $ERL_NODE_FLAGS $ERL_FLAGS -eval "beamline_state:import_file(\"${STATE_PATH}\"), halt()." ;
    erl -noshell $ERL_NODE_FLAGS $ERL_FLAGS -eval "beamline_history:import_file(\"${HISTORY_PATH}\"), halt()." ;;
  export)
    # Atomic write: export to temp files, then rename
    erl -noshell $ERL_NODE_FLAGS $ERL_FLAGS -eval "beamline_state:import_file(\"${STATE_PATH}\"), beamline_history:import_file(\"${HISTORY_PATH}\"), beamline_state:export_file(\"${STATE_PATH}.tmp\"), beamline_history:export_file(\"${HISTORY_PATH}.tmp\"), halt()." ;
    [[ -f "${STATE_PATH}.tmp" ]] && mv -f "${STATE_PATH}.tmp" "${STATE_PATH}" || echo "[WARN] export: state missing; skip write (${STATE_PATH}.tmp not found)";
    [[ -f "${HISTORY_PATH}.tmp" ]] && mv -f "${HISTORY_PATH}.tmp" "${HISTORY_PATH}" || echo "[WARN] export: history missing; skip write (${HISTORY_PATH}.tmp not found)" ;;
  import_state)
    erl -noshell $ERL_NODE_FLAGS $ERL_FLAGS -eval "beamline_state:import_file(\"${STATE_PATH}\"), halt()." ;;
  import_history)
    erl -noshell $ERL_NODE_FLAGS $ERL_FLAGS -eval "beamline_history:import_file(\"${HISTORY_PATH}\"), halt()." ;;
  export_state)
    erl -noshell $ERL_NODE_FLAGS $ERL_FLAGS -eval "beamline_state:export_file(\"${STATE_PATH}.tmp\"), halt()." ;
    [[ -f "${STATE_PATH}.tmp" ]] && mv -f "${STATE_PATH}.tmp" "${STATE_PATH}" || echo "[WARN] export_state: no Mnesia data; skip write (${STATE_PATH}.tmp not found)" ;;
  export_history)
    erl -noshell $ERL_NODE_FLAGS $ERL_FLAGS -eval "beamline_history:export_file(\"${HISTORY_PATH}.tmp\"), halt()." ;
    [[ -f "${HISTORY_PATH}.tmp" ]] && mv -f "${HISTORY_PATH}.tmp" "${HISTORY_PATH}" || echo "[WARN] export_history: no Mnesia data; skip write (${HISTORY_PATH}.tmp not found)" ;;
  verify_chain)
    erl -noshell $ERL_NODE_FLAGS $ERL_FLAGS -eval "io:format(\"~p\\n\", [beamline_history:verify_chain()]), halt()." ;;
  *)
    echo "Usage: $0 {import|export|import_state|import_history|export_state|export_history|verify_chain}"
    exit 2 ;;
esac

popd >/dev/null
