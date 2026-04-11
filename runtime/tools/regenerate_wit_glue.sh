#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "${SCRIPT_DIR}/../.." && pwd)"

WIT_BINDGEN_VERSION="0.53.1"
CARGO_TOOLCHAIN="${CARGO_TOOLCHAIN:-stable}"
INSTALL_ROOT="${WIT_BINDGEN_INSTALL_ROOT:-${REPO_ROOT}/build/tools/wit-bindgen-cli-${WIT_BINDGEN_VERSION}}"
WIT_BINDGEN_BIN="${INSTALL_ROOT}/bin/wit-bindgen"

MODE="write"
if [[ "${1:-}" == "--check" ]]; then
  MODE="check"
elif [[ "${1:-}" != "" ]]; then
  echo "usage: $(basename "$0") [--check]" >&2
  exit 2
fi

version_matches() {
  local version_output="$1"
  [[ "${version_output}" == "wit-bindgen ${WIT_BINDGEN_VERSION}" || "${version_output}" == "wit-bindgen-cli ${WIT_BINDGEN_VERSION}" ]]
}

ensure_wit_bindgen() {
  if [[ -x "${WIT_BINDGEN_BIN}" ]]; then
    local version_output
    version_output="$("${WIT_BINDGEN_BIN}" --version 2>/dev/null || true)"
    if version_matches "${version_output}"; then
      return
    fi
  fi

  echo "Installing wit-bindgen-cli ${WIT_BINDGEN_VERSION} with cargo +${CARGO_TOOLCHAIN} into ${INSTALL_ROOT}" >&2
  cargo +"${CARGO_TOOLCHAIN}" install \
    wit-bindgen-cli \
    --version "${WIT_BINDGEN_VERSION}" \
    --locked \
    --root "${INSTALL_ROOT}"

  local version_output
  version_output="$("${WIT_BINDGEN_BIN}" --version 2>/dev/null || true)"
  if ! version_matches "${version_output}"; then
    echo "unexpected wit-bindgen version after install: ${version_output}" >&2
    exit 1
  fi
}

main() {
  ensure_wit_bindgen

  local tmp_dir
  tmp_dir="$(mktemp -d)"
  trap 'rm -rf "${tmp_dir:-}"' EXIT

  local wit_dir="${REPO_ROOT}/runtime/wit/flix-bindings"
  local out_dir="${tmp_dir}/out"
  mkdir -p "${out_dir}"

  "${WIT_BINDGEN_BIN}" c "${wit_dir}" --world flix --out-dir "${out_dir}"

  local generated_c="${out_dir}/flix.c"
  local generated_h="${out_dir}/flix.h"
  local checked_in_c="${REPO_ROOT}/runtime/src/wit/flix.c"
  local checked_in_h="${REPO_ROOT}/runtime/src/wit/flix.h"

  if [[ "${MODE}" == "check" ]]; then
    diff -u "${checked_in_h}" "${generated_h}"
    diff -u "${checked_in_c}" "${generated_c}"
    echo "WIT glue is up to date." >&2
    return
  fi

  cp "${generated_h}" "${checked_in_h}"
  cp "${generated_c}" "${checked_in_c}"
  echo "Updated runtime/src/wit/flix.h and runtime/src/wit/flix.c from runtime/wit/flix-bindings" >&2
}

main
