# Troubleshooting

[Back to README](../README.md) · [Getting started](getting-started.md) ·
[Reviews](reviews.md) · [Reference](reference.md)

Start with `M-x magnus-doctor`. It checks the Emacs dependencies, configured
provider commands, Git, managed storage, registered agent directories, and
active coordination watchers without changing them.

## Magnus commands are missing after an update

Restart Emacs. Loaded Emacs Lisp functions and structures do not automatically
become one coherent new version when package files change underneath them.
`M-x magnus-upgrade` deliberately requires a restart after reinstalling.

When testing a local checkout, put its directory before the installed package
in `load-path` and remove stale `.elc` files from the checkout. Use
`M-x locate-library RET magnus RET` to confirm which copy Emacs resolved.

## A provider command is unavailable

Launch the provider once in a normal terminal and finish authentication. Then
check:

```elisp
(executable-find "claude")
(executable-find "codex")
```

GUI Emacs on macOS may inherit a different `PATH` from your shell. Configure an
absolute command if necessary:

```elisp
(setq magnus-claude-executable "/absolute/path/to/claude")
(setq magnus-codex-executable "/absolute/path/to/codex")
```

Configured command prefixes with established arguments are supported.

## The native terminal does not accept input

Make sure `vterm` itself works and that the provider process is live. Visit the
agent from `*magnus*` rather than opening its buffer by name. Magnus reconnects
only tagged terminal buffers and rejects unrelated buffers with colliding
names.

In a Magnus terminal, `C-g` sends Escape to the provider TUI. Ordinary Emacs
uses `ESC` as a Meta prefix.

## Review says the work is uncommitted

Commit the author's work and leave the worktree clean. Magnus intentionally
reviews commits, not a moving collection of staged, unstaged, and untracked
files.

The configured coordination journal and generated instructions are excluded
from this gate. Other tracked or untracked changes block review creation.

## Scope discovery times out

Magnus asks the author which exact committed range belongs to its task. Visit
the author and verify that its terminal is live and able to receive an ordinary
message. A delivery or response that exceeds the configured timeout is
discarded; select the author and request the review again.

Magnus does not guess a commit range when the author cannot answer.

## A reviewer fails or is interrupted

Use `v` or the review action menu during the same Emacs session. You can retry
failed work, interrupt the exact current attempt, or retry a retained candidate
with the same reviewer identity and evidence in a fresh provider session.

Failed candidates are not durable. After an Emacs restart, request the review
again from the current committed evidence. Completed rounds remain available.

## A completed review will not open

Magnus fails closed when durable evidence is missing, corrupt, or inconsistent.
The reader checks manifest revisions, digests, paths, anchors, changed files,
and Git type changes before presenting the report.

Do not edit files below `~/.magnus/reviews/` by hand. If the underlying Git
objects were removed by history rewriting or aggressive garbage collection,
the old immutable round may no longer be readable; archive that lineage and
request a review of current committed work.

## Coordination messages are not delivered

Press `g` in `*magnus*` to poll active and watched projects. Confirm that the
agent is live, that `.magnus-coord.md` is readable, and that its name is spelled
exactly as shown in Magnus. `M-x magnus-doctor` reports idle watchers for active
projects.

Stopped agents do not receive terminal input. Undelivered coordination nudges
are recorded in the project journal rather than signaled as timer errors.

## Magnus state has broader permissions than expected

Doctor warns when the instance-state file or review-storage directories are
accessible more broadly than private mode 600/700. Tighten the reported path's
permissions and rerun Doctor. Magnus rejects symlinks at those managed storage
boundaries.

## Reporting a bug

Include:

- Emacs and Magnus versions;
- the output of `M-x magnus-doctor` with private paths redacted if necessary;
- provider CLI versions;
- the exact command or key sequence;
- the complete error and relevant `*Messages*` entries;
- whether the agent was Claude, Codex, interactive, or headless.

Open an issue at <https://github.com/hrishikeshs/magnus/issues>.
