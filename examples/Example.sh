#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# --- Logging Framework ---
declare -A LOG_LEVELS=([DEBUG]=0 [INFO]=1 [WARN]=2 [ERROR]=3 [FATAL]=4)
LOG_LEVEL="${LOG_LEVEL:-INFO}"

log() {
    local level="$1"; shift
    local msg="$*"
    local ts=$(date '+%Y-%m-%dT%H:%M:%S%z')
    [[ ${LOG_LEVELS[$level]:-0} -ge ${LOG_LEVELS[$LOG_LEVEL]:-1} ]] && \
        printf '[%s] %-5s %s\n' "$ts" "$level" "$msg" >&2
}

# --- Parallel Execution with Job Control ---
MAX_JOBS=${MAX_JOBS:-$(nproc)}
declare -A PIDS=()

run_parallel() {
    local cmd="$1"; local id="$2"
    while [[ ${#PIDS[@]} -ge $MAX_JOBS ]]; do
        for pid in "${!PIDS[@]}"; do
            if ! kill -0 "$pid" 2>/dev/null; then
                wait "$pid" || log WARN "Job ${PIDS[$pid]} failed"
                unset PIDS[$pid]
            fi
        done
        sleep 0.1
    done
    eval "$cmd" &
    PIDS[$!]="$id"
    log DEBUG "Started job $id (PID $!)"
}

wait_all() {
    for pid in "${!PIDS[@]}"; do
        wait "$pid" 2>/dev/null || log ERROR "Job ${PIDS[$pid]} failed"
    done
    PIDS=()
}

# --- Config Parser ---
parse_config() {
    local file="$1"
    declare -gA CONFIG=()
    local section=""
    while IFS= read -r line || [[ -n "$line" ]]; do
        line="${line%%#*}"
        [[ -z "${line// /}" ]] && continue
        if [[ "$line" =~ ^\[(.+)\]$ ]]; then
            section="${BASH_REMATCH[1]}"
        elif [[ "$line" =~ ^([^=]+)=(.*)$ ]]; then
            local key="${BASH_REMATCH[1]// /}"
            local val="${BASH_REMATCH[2]## }"
            val="${val%% }"
            CONFIG["${section:+${section}.}${key}"]="$val"
        fi
    done < "$file"
}

# --- Retry with Exponential Backoff ---
retry() {
    local max_attempts="${1:-3}"; shift
    local delay="${1:-1}"; shift
    local attempt=1
    while [[ $attempt -le $max_attempts ]]; do
        if "$@"; then return 0; fi
        log WARN "Attempt $attempt/$max_attempts failed, retrying in ${delay}s..."
        sleep "$delay"
        delay=$((delay * 2))
        ((attempt++))
    done
    log ERROR "All $max_attempts attempts failed"
    return 1
}

# --- Trap & Cleanup ---
CLEANUP_TASKS=()
cleanup() {
    log INFO "Running ${#CLEANUP_TASKS[@]} cleanup tasks..."
    for task in "${CLEANUP_TASKS[@]}"; do eval "$task" || true; done
}
trap cleanup EXIT

# --- Main ---
main() {
    log INFO "Starting deployment pipeline"
    parse_config "deploy.conf"
    
    for service in api worker scheduler; do
        run_parallel "deploy_servi
