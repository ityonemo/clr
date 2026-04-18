#!/bin/bash
# Hook to enforce proper integration test usage
# - Blocks direct bats usage (use run_one.sh or run_integration.sh instead)
# - Blocks piping run_integration.sh to grep/tail/head, requires tee usage

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

# Block direct bats usage - should use run_one.sh or run_integration.sh
if [[ "$COMMAND" =~ ^[[:space:]]*bats[[:space:]] ]]; then
  echo "BLOCKED: Do not run bats directly." >&2
  echo "Use: ./run_one.sh <test_file> for single tests" >&2
  echo "Use: ./run_integration.sh 2>&1 | tee /tmp/integration_results.txt for full suite" >&2
  exit 2
fi

# Only check run_integration.sh usage rules below
if [[ "$COMMAND" != *"run_integration.sh"* ]]; then
  exit 0
fi

# Block if piped to grep, tail, head, etc.
if [[ "$COMMAND" =~ run_integration\.sh.*\|.*(grep|tail|head|awk|sed) ]]; then
  echo "BLOCKED: Do not pipe run_integration.sh to grep/tail/head." >&2
  echo "Use: ./run_integration.sh 2>&1 | tee /tmp/integration_results.txt; echo \"Exit code: \$?\"" >&2
  exit 2
fi

# Allow if using tee properly
if [[ "$COMMAND" =~ run_integration\.sh.*\|.*tee ]]; then
  exit 0
fi

# Block bare run_integration.sh without tee (results won't be captured)
if [[ "$COMMAND" =~ ^[[:space:]]*\.?/?run_integration\.sh[[:space:]]*$ ]] || \
   [[ "$COMMAND" =~ ^[[:space:]]*\.?/?run_integration\.sh[[:space:]]+2\>\&1[[:space:]]*$ ]]; then
  echo "BLOCKED: Capture output with tee." >&2
  echo "Use: ./run_integration.sh 2>&1 | tee /tmp/integration_results.txt; echo \"Exit code: \$?\"" >&2
  exit 2
fi

exit 0
