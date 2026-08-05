# Magnus

[![MELPA](https://melpa.org/packages/magnus-badge.svg)](https://melpa.org/#/magnus)
[![CI](https://github.com/hrishikeshs/magnus/actions/workflows/ci.yml/badge.svg)](https://github.com/hrishikeshs/magnus/actions/workflows/ci.yml)

Magnus is a Magit-inspired control room for Claude Code and Codex agents in
Emacs. It keeps each interactive agent in its native terminal UI, gives the
team durable identities and a shared coordination journal, and can send
committed work to a fresh model for an independent review.

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
- coordinate agents through a shared project journal;
- run headless, cross-provider reviews whose completed rounds are durable;
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
- a durable UUID that survives archive, resurrection, and rename;
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

Emacs normally consumes `ESC` as a Meta prefix. In Magnus terminal buffers,
`C-g` sends Escape to either provider's TUI.

## Independent reviews

Put point on an author agent in `*magnus*` and press `v RET`. Magnus then:

1. requires a clean author worktree, otherwise telling you to ask the instance
   to commit first;
2. asks the author, through its ordinary terminal conversation, which exact
   committed `base..head` range represents its work;
3. assigns a durable reviewer identity, reusing existing expertise matching
   when possible;
4. chooses the provider opposite the author by default;
5. runs the reviewer headlessly and stores a structured, line-addressed report;
6. tells the author where to read and address the completed findings.

The author's reply is read from that exact provider session's local transcript;
Magnus neither guesses a Git range nor asks the agent to write a protocol file.
It accepts only canonical full object IDs, proves the base is an ancestor of the
head, and proves the head is reachable from the current branch. The configured
coordination journal and generated instructions are excluded from the clean
gate; any other tracked or untracked project work blocks review creation.

The request popup offers optional provider (`p`), model (`m`), and reasoning
effort (`e`) overrides. Reviews share Magnus's single background-model FIFO
with indexing and retrospectives, so at most one non-interactive provider
process runs at a time.

Reviewers run in private detached checkouts derived from the immutable round
number and HEAD. Different rounds never share a mutable worktree, and Magnus
rejects tracked, untracked, or ignored residue in a managed checkout.

Magnus deliberately makes pending work disposable. The author query, queue
entry, provider process, failures, and retries live only in the current Emacs
session. Queued terminal delivery, the author's response, and the provider run
are bounded independently by `magnus-review-delivery-timeout`,
`magnus-review-scope-timeout`, and `magnus-review-timeout`. If execution fails,
use the review menu to retry while that Emacs session is still open. If resuming
the provider session itself is the problem, `f` repeats the same candidate with
the same named reviewer, exact evidence, and lineage context in a fresh
provider session. Restarting Emacs discards unfinished execution instead of
replaying it.

Successful rounds are the durable boundary. Their exact Git evidence,
structured findings, Markdown report, read state, reviewer identity, and last
successful provider session survive under `~/.magnus/reviews/`. Asking for the
next round reuses that reviewer name and provider session. Magnus validates and
supplies the complete successful lineage: every historical finding ID remains
reserved, every finding from the immediately preceding round needs a
disposition, and an older resolved finding can retain its identity if it later
resurfaces. Missing or corrupt prior evidence blocks the next round rather than
silently starting over; each result's digest, verdict, and finding count are
bound into the durable round manifest. Archive the lineage when that body of
work is finished.

Review storage has one writer: do not mutate the same review lineage from two
live Emacs processes at once. This matches Magnus's existing single-session
ownership model for its other durable registries.

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

The review actions popup can request another committed round, retry failed
ephemeral work, retry a retained candidate with a fresh reviewer session,
interrupt the current query or reviewer, or archive the completed lineage.

## Shared coordination journal

Agents coordinate through `.magnus-coord.md` in the project root. It contains
the complete coordination protocol and durable project record in four
human-readable sections:

- **Active Work** records each agent's current area, status, and files;
- **Discoveries** preserves project facts and gotchas worth sharing;
- **Decisions** records choices that should outlive a conversation;
- **Log** carries announcements, mentions, and direct messages.

The Log has one ordering invariant: newest first. Agents insert each new entry
immediately below the Log heading's comments and blank preamble; they do not
append at the bottom. Magnus normalizes that storage order to chronological
order for status and retrospective readers.

Magnus writes the protocol instructions to
`.claude/magnus-instructions.md`, and the shared onboarding tells both
providers to read them and the coordination journal. Nothing has to be
installed into a Claude or Codex plugin or skill directory.

Magnus watches the journal while a project is active. New mentions, direct
messages, and summons are delivered to the addressed live agent. Periodic
reminders ask agents to check the journal, update Active Work, and share useful
discoveries; bounded housekeeping keeps the Log readable.

Press `C` to open `.magnus-coord.md`. A manual `g` in `*magnus*` polls every
active or watched project for ordinary coordination messages. Automatic
presentation refreshes deliberately avoid that filesystem work.

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
| `C` | Open the shared coordination journal |
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
| `C` | Open shared `.magnus-coord.md` |
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
registered agent directories.

## Useful customization

```elisp
;; Provider executables.
(setq magnus-claude-executable "/path/to/claude")
(setq magnus-codex-executable "/path/to/codex")

;; Default project for new agents.
(setq magnus-default-directory "~/workspace")

;; Independent review defaults.
(setq magnus-review-default-provider nil) ; opposite the author
(setq magnus-review-default-effort 'high)
(setq magnus-review-directory-root "~/.magnus/reviews")
(setq magnus-review-scope-timeout 180)
(setq magnus-review-timeout 3600)

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

;; Coordination journal housekeeping and reminders.
(setq magnus-coord-log-max-entries 25)
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
execution, completed review lineages, coordination, and UI in separate
modules. See the Commentary section at the top of each `.el` file for that
module's boundary.

## License

MIT
