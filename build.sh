#!/usr/bin/env bash
# Build hive-emacs-hagent: compile .cljel -> .el into elisp/
# Mirrors hive-emacs/build.sh conventions.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="$SCRIPT_DIR/src/cljel"
OUT_DIR="$SCRIPT_DIR/elisp"
CLEL_HOME="${CLEL_HOME:-$HOME/PP/clojure-elisp}"

if [[ ! -d "$CLEL_HOME" ]]; then
  echo "ERROR: clojure-elisp not found at $CLEL_HOME" >&2
  echo "Set CLEL_HOME or clone https://github.com/hive-agi/clojure-elisp" >&2
  exit 1
fi

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

find "$SRC_DIR" -name '*.cljel' | sort | while read -r cljel_file; do
  base=$(basename "$cljel_file")

  if [[ "$base" == *test* ]]; then
    echo "  SKIP (test): $base"
    continue
  fi

  rel=$(realpath --relative-to="$SRC_DIR" "$cljel_file")
  output=$(cd "$CLEL_HOME" && clojure -M:dev -m clojure-elisp.cli compile "$cljel_file" 2>&1)

  if echo "$output" | grep -q "Compiled"; then
    el_file=$(echo "$output" | sed -n 's/.*-> \(.*\.el\).*/\1/p')
    if [[ -f "$el_file" ]]; then
      provide_name=$(grep -oP "^\(provide '\K[^)]+" "$el_file" | head -1)
      if [[ -n "$provide_name" ]]; then
        dest="$OUT_DIR/${provide_name}.el"
        mv "$el_file" "$dest"
        echo "  $provide_name <- $rel"
      else
        echo "  WARN (no provide): $rel"
      fi
    fi
  else
    echo "  FAIL: $rel"
    echo "    $output" | head -3
  fi
done

RUNTIME="$CLEL_HOME/resources/clojure-elisp/clojure-elisp-runtime.el"
if [[ -f "$RUNTIME" ]]; then
  cp "$RUNTIME" "$OUT_DIR/clojure-elisp-runtime.el"
  echo "  clojure-elisp-runtime (copied)"
fi

total=$(find "$OUT_DIR" -name '*.el' | wc -l)
echo ""
echo "Built $total .el files in elisp/"
