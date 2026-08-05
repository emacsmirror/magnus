# Magnus

[![MELPA](https://melpa.org/packages/magnus-badge.svg)](https://melpa.org/#/magnus)
[![CI](https://github.com/hrishikeshs/magnus/actions/workflows/ci.yml/badge.svg)](https://github.com/hrishikeshs/magnus/actions/workflows/ci.yml)

Magnus is a Magit-inspired control room for Claude Code and Codex agents in
Emacs. It keeps each interactive agent in its native terminal UI, gives the
team durable identities and coordination state, and can send committed work to
a fresh model for an independent review.

No background server, Magnus-specific plugin, or agent skill is required.
Magnus launches the provider CLIs directly and supplies a shared onboarding and
coordination protocol.

## Why Magnus?

The useful way to run coding agents is often hands-on: keep two agents beside
your editor, watch what they do, redirect one mid-task, and ask another to
review the result. Magnus makes that workflow feel native to Emacs:

- run Claude Code and Codex side by side in `vterm`;
- switch, message, suspend, archive, and resurrect named agents;
- see health, attention, active work, and reviews in one `*magnus*` buffer;
- coordinate concurrent writers without a shared mutable-state bottleneck;
- run durable, headless, cross-provider reviews of exact Git evidence;
- read findings in a folding Magit-style diff rather than a raw model transcript;
- retain agent memory, provider sessions, reviews, and shared project context.

## Requirements

- Emacs 28.1 or newer. CI tests Emacs 28.1, 29.4, and 30.2.
- [vterm](https://github.com/akermu/emacs-libvterm) 0.0.2 or newer.
- [transient](https://github.com/magit/transient) 0.4.0 or newer.
- [magit-section](https://github.com/magit/magit) 3.3.0 or newer.
- Git for immutable review ranges and review navigation.
- [Claude Code](https://docs.anthropic.com/en/docs/claude-code) for Claude
  agents, ordinary headless tasks, or Claude-backed reviews.
- [Codex CLI](https://github.com/openai/codex) for Codex agents or
  Codex-backed reviews.

Only the CLI for the provider you use is required. The default review policy
chooses the provider opposite the author, so install both CLIs for the complete
cross-provider workflow.

## Installation

### MELPA

```elisp
(use-package magnus
  :ensure t
  :bind (("C-c m" . magnus)
         ("C-c M" . magnus-create-instance)))
```

Or run `M-x package-install RET magnus RET`.

### package-vc (Emacs 29+)

Run:

```text
M-x package-vc-install RET https://github.com/hrishikeshs/magnus RET
```

Or add this to your configuration:

```elisp
(unless (package-installed-p 'magnus)
  (package-vc-install "https://github.com/hrishikeshs/magnus"))

(use-package magnus
  :bind (("C-c m" . magnus)))
```

### straight.el

```elisp
(straight-use-package
 '(magnus :type git :host github :repo "hrishikeshs/magnus"))
```

### quelpa

```elisp
(quelpa '(magnus :fetcher github :repo "hrishikeshs/magnus"))
```

After changing the installed Magnus version, restart Emacs before using it.
This avoids mixing already-loaded structures and functions with a different
package version. `M-x magnus-upgrade` performs a guarded package reinstall and
then asks for the same restart.

## Quick start

1. Run `M-x magnus` to open the status buffer.
2. Press `c` for a Claude Code agent, or press `?` and then `X` for Codex.
3. Choose a project directory and work in the native TUI that opens.
4. Create another agent and use `RET` in `*magnus*` to move between them.
5. Put point on an agent and press `v RET` to request an independent review.
6. When the review completes, press `RET` on its row to read the result.

The minibuffer shows context-sensitive hints as point moves among agents,
reviews, and review rounds. Press `?` at any time for the complete dispatcher.

## Native interactive agents

Claude Code and Codex both run directly in `vterm`; Magnus does not emulate
their interfaces or take a second ownership path through an app server. Their
normal terminal composers, approval prompts, slash commands, and streaming
output remain available.

Each Magnus instance has:

- a friendly display name such as `swift-fox`;
- a durable UUID used as its coordination writer identity;
- provider session metadata used to resume archived work;
- a first-person memory at `.claude/agents/NAME/memory.md`;
- the same provider-neutral onboarding and authorization boundaries.

Instance names are unique within Magnus because terminal buffers and legacy
name-based routing use them directly. Rename is intentionally available only
after an instance is archived: Magnus moves `.claude/agents/OLD/` to the new
home transactionally, while the durable UUID, provider session, and review
history stay unchanged.

With a current Claude CLI, Magnus assigns each fresh terminal an exact
`--session-id` and waits for that session's own JSONL record. If the installed
CLI lacks that option, Magnus falls back to detecting the one new session and
allows only one unresolved fresh Claude launch per physical project in that
Emacs session. That serialized compatibility path prevents its concurrent
launches from claiming each other's history.

Magnus passes `MAGNUS_COORD_WRITER_ID` and `MAGNUS_COORD_WRITER_NAME` only to
the agent process. It does not mutate Emacs's global process environment.

Emacs normally consumes `ESC` as a Meta prefix. In Magnus terminal buffers,
`C-g` sends Escape to either provider's TUI.

## Independent reviews

Put point on an author agent in `*magnus*` and press `v RET`. Magnus then:

1. requires a clean author worktree, otherwise telling you to ask the instance
   to commit first;
2. asks the author to publish its current committed checkpoint and records the
   exact base and head Git object IDs;
3. assigns a durable reviewer identity, reusing existing expertise matching
   when possible;
4. chooses the provider opposite the author by default;
5. runs the reviewer headlessly and stores a structured, line-addressed report;
6. notifies the author where the durable notes live.

The request popup offers optional provider (`p`), model (`m`), and reasoning
effort (`e`) overrides. Reviews run one at a time by default, keeping laptop
load predictable; customize `magnus-review-max-concurrent` if desired.

Reviewers run in private detached checkouts derived from the immutable round
number and HEAD. Different rounds never share a mutable worktree, and Magnus
rejects tracked, untracked, or ignored residue in a managed checkout.

Each raw JSONL or stderr artifact is capped by
`magnus-review-max-stream-artifact-bytes` (8 MiB by default); a sibling
`.truncated` diagnostic records overflow. A review attempt also has a watchdog,
controlled by `magnus-review-attempt-timeout` (one hour by default), so a lost
provider process cannot hold the review queue forever.

Reviews are durable work objects, not disposable output buffers. Their
checkpoint requests, immutable rounds, attempts, provider session ID,
findings, delivery state, and read state survive Emacs restarts under
`~/.magnus/reviews/`. A later round keeps the same reviewer identity and
provider session, so the reviewer can verify fixes against its earlier
findings. Archive the review when that body of work is finished.

While a reviewer is executing, the author row shows an animated review badge.
Completed reviews appear in a separate section with an unread marker. The
reader presents the exact `base..head` diff using `magit-section`:

| Key | Review reader action |
|-----|----------------------|
| `TAB` | Fold or unfold a file, hunk, or finding |
| `n` / `p` | Next or previous visible section |
| `N` / `P` | Next or previous finding |
| `RET` | Open the file from the reviewed Git snapshot |
| `e` | Open the corresponding current worktree file |
| `[` / `]` | Previous or next review round |
| `?` | Open review actions |
| `g` | Refresh |
| `q` | Close |

The review actions popup can request another round, resend a checkpoint request,
retry a failed round, interrupt a running reviewer, retry delivery, or archive
the review. Manual interruption remains durable until you explicitly retry.

## Coordination without shared-file collisions

New Magnus agents publish immutable JSON events to their own inbox:

```text
.magnus-coord/
├── writers/
│   ├── WRITER-UUID-A/
│   │   ├── EVENT-ID.json
│   │   └── ...
│   └── WRITER-UUID-B/
│       └── EVENT-ID.json
└── current.md
```

Each writer owns one monotonically sequenced event stream. Writers never
replace one another's files, so two agents can announce work, record a
discovery, or acknowledge a review checkpoint concurrently without a
last-writer-wins rewrite of shared state.

Magnus validates and reduces those events into
`.magnus-coord/current.md`. This file is a generated, human-readable view of
active work, discoveries, decisions, and recent log messages. It is read-only
state: agents and users should not edit it. Retention is bounded and old event
evidence is garbage-collected only after a current projection is safely
written.

For Magnus-managed writers, a lifecycle overlay reconciles the generated
Active Work view with the live registry. A stopped or archived agent, or one
moved to another project, cannot remain visibly active merely because its final
`active.clear` was never written. The underlying durable event evidence is
retained.

The recognized event operations are:

- append a log message;
- set or clear the writer's active work;
- put or remove a discovery or decision;
- publish the exact Git checkpoint for a pending review.

Magnus writes the current event schema and atomic-publication instructions to
`.claude/magnus-instructions.md`. The shared onboarding tells both providers to
read that file and the current projection. Nothing has to be installed into a
Claude or Codex plugin/skill directory.

On Git worktrees, registration adds `/.magnus-coord/` and the generated
`.claude/magnus-instructions.md` to that repository's local
`.git/info/exclude`. This keeps newly generated, untracked coordination
artifacts out of ordinary `git add -A` and review diffs without changing the
project's tracked `.gitignore`.

Conversational log effects notify live agents only after Magnus begins watching
a project; startup does not replay old chatter. The generated current view is
the durable catch-up path. In contrast, unresolved `review.ready` evidence is
replayed and settled exactly so a review checkpoint survives an Emacs restart.

### Legacy compatibility

`.magnus-coord.md` remains a compatibility ingress for agents and tools using
the pre-0.2 shared-file protocol. Magnus reads it alongside event state and
continues to deliver legacy mentions, direct messages, summons, and review-ready
markers. New agents use immutable events; `.magnus-coord/current.md` is the
canonical generated view.

Press `J` to read that generated current view and `C` to inspect or maintain
the legacy ingress. A manual `g` in `*magnus*` polls every active or watched
project, retries transient event-store reads, and re-arms exhausted checkpoint
delivery. Automatic presentation refreshes deliberately avoid that filesystem
work.

## Status-buffer commands

These commands are bound directly in `*magnus*`:

| Key | Action |
|-----|--------|
| `RET` | Visit the agent or review at point |
| `c` | Create a Claude Code agent |
| `v` | Request a review, or show actions for the review at point |
| `V` | Show actions for the review at point |
| `k` | Archive an agent |
| `R` | Resurrect an archived agent |
| `r` | Rename an archived agent and migrate its memory home |
| `s` / `S` | Suspend or resume a Claude Code agent |
| `d` | Change project and start a fresh provider session |
| `m` | Send a message to an agent |
| `t` | Open the provider trace |
| `x` | Open shared project context |
| `J` | Open the generated current coordination view |
| `C` | Open the legacy coordination ingress |
| `a` / `A` | Visit the next attention request / show the queue |
| `P` | Archive all agents |
| `z` | Toggle Do Not Disturb |
| `F` | Generate a session retrospective |
| `n` / `p` | Navigate agents and reviews |
| `g` | Refresh |
| `?` | Open all commands |
| `q` | Quit |

The `?` dispatcher additionally exposes:

| Key | Action |
|-----|--------|
| `c` | Create a Claude Code agent |
| `X` | Create a Codex agent |
| `h` | Create a headless Claude task |
| `o` | Open the completed review at point |
| `D` | Run `magnus-doctor` |
| `J` | Open generated `.magnus-coord/current.md` |
| `C` | Open legacy `.magnus-coord.md` |
| `I` | Open generated agent coordination instructions |
| `H` / `T` | Toggle health / attention monitoring |

## Traces, attention, and health

Press `t` on an agent to read its provider's local session record. Claude
traces include recorded thinking blocks. Codex traces include visible
`[thinking]` engineering journals, user messages, and responses; encrypted
internal reasoning is intentionally not displayed. In a trace buffer, `TAB`
toggles the thinking block at point, `t` toggles all thinking, `n` / `p` move
between responses, `[` loads earlier history, `g` refreshes, `G` jumps to the
end, and `q` closes the window. Disk reads are chunked by
`magnus-trace-read-chunk-bytes`; an unfinished JSONL record is retained only up
to `magnus-trace-max-record-bytes`, then skipped through its newline. Initial
entries and rendered buffer lines have separate bounds.

Magnus detects approval and confirmation prompts and places agents in an
attention queue. Safe, configurable patterns may be auto-approved; set
`magnus-attention-auto-approve-patterns` to `nil` to disable that behavior.

Health indicators summarize terminal activity:

- `+` — active and changing;
- `~` — stale;
- `!` — stuck across consecutive checks;
- `x` — no live buffer or process.

## Shared context and headless tasks

Press `x` for a persistent per-project scratch buffer. It can hold notes and
links, fetch and cache URL content, export to `.magnus-context.md`, or copy its
contents for an agent. Context is stored outside the repository under
`~/.magnus/context/` by default.

`M-x magnus-create-headless` (or `? h` in the status buffer) runs a
fire-and-forget Claude task without a `vterm`. Output appears in a read-only
buffer and the instance ends as `finished` or `errored`; an externally
terminated process is shown as `stopped`. This is distinct from the
provider-neutral headless runner used internally for reviews.

The context buffer binds `C-c C-u` to insert and fetch a URL, `C-c C-f` to
fetch the URL at point, `C-c C-e` to export project context, `C-c C-c` to copy
it, and `C-c C-s` to save.

Automatic low-priority model work—expertise indexing, session retrospectives,
and optional dashboard fortunes—shares one FIFO and runs at most one provider
process at a time. `magnus-background-queue-limit`,
`magnus-background-output-limit`, and `magnus-background-timeout` bound its
waiting work, retained output, and runtime. Expertise indexing reads only the
prefix allowed by `magnus-agents-index-memory-limit`, and synchronous expertise
matching has its own `magnus-expertise-match-timeout`.

`M-x magnus-health-bloomberg` uses bundled static dashboard messages by
default. Set `magnus-health-dashboard-ai-messages` to non-nil only if you want
Claude-generated fortunes; those requests join the same low-priority FIFO.

## Persistence and diagnostics

Magnus keeps durable user data under `~/.magnus/` by default:

| Path | Contents |
|------|----------|
| `~/.magnus/state.el` | Agent registry and provider session metadata |
| `~/.magnus/reviews/` | Review manifests, rounds, evidence, and reports |
| `~/.magnus/context/` | Per-project shared context |
| `~/.magnus/url-cache/` | Fetched context URLs |
| `~/.magnus/agents-index.el` | Dormant-agent expertise index |
| `~/.magnus/attention.el` | Learned attention data |

Run `M-x magnus-doctor`, or press `? D`, for read-only checks of the Emacs
version, required libraries, provider CLIs, Git, durable storage paths, and
registered agent directories. It also reports transient coordination reads and
bounded review-checkpoint retries. If a checkpoint retry is exhausted, fix the
reported cause and press `g` in `*magnus*` to re-arm the exact durable evidence.

## Useful customization

```elisp
;; Provider executables.
(setq magnus-claude-executable "/path/to/claude")
(setq magnus-codex-executable "/path/to/codex")

;; Default project for new agents.
(setq magnus-default-directory "~/workspace")

;; Durable review defaults.
(setq magnus-review-default-provider nil) ; opposite the author
(setq magnus-review-default-effort 'high)
(setq magnus-review-max-concurrent 1)
(setq magnus-review-directory-root "~/.magnus/reviews")
(setq magnus-review-max-stream-artifact-bytes (* 8 1024 1024))
(setq magnus-review-attempt-timeout 3600)

;; Shared low-priority model work.
(setq magnus-background-queue-limit 32)
(setq magnus-background-output-limit (* 256 1024))
(setq magnus-background-timeout 90)
(setq magnus-agents-index-memory-limit (* 64 1024))
(setq magnus-expertise-match-timeout 15)
(setq magnus-health-dashboard-ai-messages nil) ; static fortunes only

;; UI and monitoring.
(setq magnus-status-show-context-hints t)
(setq magnus-status-review-animation-interval 0.4) ; nil for a static badge
(setq magnus-attention-auto-approve-patterns nil)  ; disable auto-approval
(setq magnus-health-check-interval 30)
(setq magnus-health-stale-threshold 120)
(setq magnus-health-stuck-threshold 3)

;; Bounded provider traces.
(setq magnus-trace-read-chunk-bytes (* 64 1024))
(setq magnus-trace-max-record-bytes (* 1024 1024))

;; Coordination retention and reminders.
(setq magnus-coord-state-log-limit 25)
(setq magnus-coord-state-knowledge-limit 100)
(setq magnus-coord-reminder-interval 600) ; nil disables reminders
```

Use `M-x customize-group RET magnus RET` for the full set of options.

## Development

The standard checks are intentionally simple:

```sh
make test
make lint
make lint-compile
make package-lint
make clean
```

CI installs package dependencies and runs all four checks on Emacs 28.1, 29.4,
and 30.2.

The implementation keeps provider transport, terminal setup, headless
execution, durable reviews, coordination storage/reduction/runtime, and UI in
separate modules. See the Commentary section at the top of each `.el` file for
that module's boundary.

## License

MIT
