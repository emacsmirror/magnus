#!/bin/sh
# Characterize durable review semantics across two Magnus revisions.

set -eu

BASELINE=${MAGNUS_DIFF_BASELINE:-9f153deb62ba0cf5367dd4838c82d50773c8d0c0}
EMACS=${EMACS:-emacs}
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
CANDIDATE=${MAGNUS_DIFF_CANDIDATE:-$(git -C "$SCRIPT_DIR/../.." rev-parse --show-toplevel)}
RUNNER=$SCRIPT_DIR/runner.el
ROOT=$(mktemp -d "${TMPDIR:-/tmp}/magnus-differential.XXXXXX")
BASELINE_TREE=$ROOT/baseline-tree
KEEP=${MAGNUS_DIFF_KEEP:-0}

cleanup () {
    status=$?
    trap - EXIT HUP INT TERM
    git -C "$CANDIDATE" worktree remove --force "$BASELINE_TREE" >/dev/null 2>&1 || true
    if [ "$status" -eq 0 ] && [ "$KEEP" != 1 ]; then
        rm -rf "$ROOT"
    else
        echo "Magnus differential artifacts: $ROOT" >&2
    fi
    exit "$status"
}
trap cleanup EXIT HUP INT TERM

run_scenario () {
    checkout=$1
    scenario=$2
    state=$3
    output=$4
    mkdir -p "$state"
    MAGNUS_DIFF_CHECKOUT=$checkout \
    MAGNUS_DIFF_SCENARIO=$scenario \
    MAGNUS_DIFF_STATE=$state \
    MAGNUS_DIFF_OUTPUT=$output \
        "$EMACS" --batch -Q -L "$checkout" -l "$RUNNER" \
        -f magnus-differential-main
}

compare () {
    label=$1
    expected=$2
    actual=$3
    if ! cmp -s "$expected" "$actual"; then
        echo "Magnus differential mismatch: $label" >&2
        diff -u "$expected" "$actual" >&2 || true
        return 1
    fi
    echo "ok: $label"
}

git -C "$CANDIDATE" worktree add --quiet --detach "$BASELINE_TREE" "$BASELINE"

run_scenario "$BASELINE_TREE" worktree-semantics \
    "$ROOT/baseline-worktree-state" "$ROOT/baseline-worktree.sexp"
run_scenario "$CANDIDATE" worktree-semantics \
    "$ROOT/candidate-worktree-state" "$ROOT/candidate-worktree.sexp"
compare "managed worktree semantics" \
    "$ROOT/baseline-worktree.sexp" "$ROOT/candidate-worktree.sexp"

run_scenario "$BASELINE_TREE" review-ledger \
    "$ROOT/baseline-state" "$ROOT/baseline.sexp"
run_scenario "$CANDIDATE" review-ledger \
    "$ROOT/candidate-state" "$ROOT/candidate.sexp"
compare "review ledger semantics" "$ROOT/baseline.sexp" "$ROOT/candidate.sexp"

# Upgrade compatibility is directional: copy schema-2 state written by the
# baseline, then make the candidate load, save, and reload that copy.
mkdir -p "$ROOT/handoff-state"
cp -R "$ROOT/baseline-state/reviews" "$ROOT/handoff-state/reviews"
run_scenario "$CANDIDATE" review-handoff \
    "$ROOT/handoff-state" "$ROOT/handoff.sexp"
compare "baseline schema-2 -> candidate" "$ROOT/baseline.sexp" "$ROOT/handoff.sexp"

echo "Magnus differential gate passed ($BASELINE -> candidate)."
