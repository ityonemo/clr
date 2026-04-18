#!/bin/bash
# PreToolUse hook for Edit tool
# Reviews proposed edits against project-specific rule files using Codex

INPUT=$(cat)
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path')
OLD=$(echo "$INPUT" | jq -r '.tool_input.old_string')
NEW=$(echo "$INPUT" | jq -r '.tool_input.new_string')

# Determine rule file based on file path
RULE_FILE=""
case "$FILE" in
  */analysis/memory_safety.zig) RULE_FILE=".claude/rules/memory_safety.md" ;;
  */analysis/null_safety.zig) RULE_FILE=".claude/rules/null_safety.md" ;;
  */analysis/undefined_safety.zig) RULE_FILE=".claude/rules/undefined_safety.md" ;;
  */analysis/variant_safety.zig) RULE_FILE=".claude/rules/variant_safety.md" ;;
  */analysis/fd_safety.zig) RULE_FILE=".claude/rules/fd_safety.md" ;;
  */analysis/fieldparentptr_safety.zig) RULE_FILE=".claude/rules/fieldparentptr_safety.md" ;;
  */analysis/*_test.zig) RULE_FILE=".claude/rules/analysis_tests.md" ;;
  */Refinements.zig|*/Analyte.zig|*/lib.zig|*/tag.zig)
    RULE_FILE=".claude/rules/refinements_and_gids.md" ;;
  *) exit 0 ;;  # No matching rule, allow edit
esac

# Read the rule file content
MARKDOWN=$(cat "$CLAUDE_PROJECT_DIR/$RULE_FILE")

# Build prompt and call Codex
PROMPT="Please check the proposed code change:
FILE: $FILE
OLD: $OLD
NEW: $NEW

Research the context of the file change and verify that it agrees with the provided guidelines:
$MARKDOWN

If it is acceptable, please emit only the string 'PASSED'.
If it is unacceptable, please emit the string 'FAILED: <explanation-of-failure>'.
If the proposed code is suspiciously complex, please emit the string 'COMPLEX'.

DO NOT comment on anything outside of the guidelines.  For example, if it creates invalid code,
dangling function calls, or non-existent function calls, that is acceptable, as the change might
be preceding another change that will resolve this, allow these concerns to PASS
"

RESULT=$(codex exec resume 019d8960-fa5a-7dc2-91e0-fff2bbb71a65 "$PROMPT" 2>/dev/null)

# Handle response - use grep to handle wrapped output from Codex
if echo "$RESULT" | grep -q "PASSED"; then
    exit 0
elif echo "$RESULT" | grep -q "FAILED"; then
    # Extract the failure message
    FAIL_MSG=$(echo "$RESULT" | grep -o "FAILED:.*" | head -1)
    echo "${FAIL_MSG:-FAILED: unknown reason}" >&2
    exit 2
elif echo "$RESULT" | grep -q "COMPLEX"; then
    jq -n '{
      hookSpecificOutput: {
        hookEventName: "PreToolUse",
        permissionDecision: "ask",
        permissionDecisionReason: "Codex flagged this edit as complex - please review"
      }
    }'
else
    echo "Unexpected Codex response: $RESULT" >&2
    exit 2
fi
