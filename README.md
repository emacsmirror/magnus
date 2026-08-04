# Magnus

[![MELPA](https://melpa.org/packages/magnus-badge.svg)](https://melpa.org/#/magnus) [![CI](https://github.com/hrishikeshs/magnus/actions/workflows/ci.yml/badge.svg)](https://github.com/hrishikeshs/magnus/actions/workflows/ci.yml)

A magit-inspired interface for managing Claude Code and Codex agents within Emacs.

Run multiple AI agents in parallel, let them communicate to avoid conflicts,
handle their permission requests one at a time, and ask a fresh model to review
committed work without leaving Emacs.

## Screenshots

**Agents coordinating via shared task list:**
![Coordination in action](screenshots/user-action.png)

**Two agents working side by side:**
![Multiple Claude instances](screenshots/multiple-claude-instances.png)

**Coordination diff view:**
![Diff view](screenshots/diff-view.png)

## Why Magnus?

When working with Claude Code, you often want multiple agents working simultaneously:
- One agent refactoring the auth module
- Another writing tests
- A third updating documentation

But this creates problems:
- **File conflicts**: Agents might edit the same files
- **Context sharing**: How do agents know what others are doing?
- **Permission chaos**: Multiple agents asking for input at once

Magnus solves all of this.

## Features

### Instance Management
Spawn, kill, suspend/resume, rename, and switch between Claude Code instances running in vterm buffers. Each instance gets a randomly generated name (like `swift-fox` or `keen-owl`) and runs in its own terminal. Change an instance's working directory with session resume — Claude keeps its full conversation history.

### Optional Codex Instances

Run `M-x magnus-create-codex` to create a Codex instance. Codex is strictly
opt-in: existing commands, saved state, Claude behavior, and Claude storage
paths remain unchanged. Each Codex instance runs the full native Codex TUI in
a vterm buffer and is labeled `[codex]` in the status buffer. Only the `codex`
executable is required; customize its path with `magnus-codex-executable`.

Inside a Codex buffer, use the TUI exactly as you would in a terminal: type at
its composer, answer approvals in its native UI, and use Magnus's `C-g`
binding to send Escape. Direct messages and coordination nudges are typed into
that same vterm through a serialized queue, making the TUI the only session
owner. Magnus holds automated delivery while you are actively in that TUI to
avoid interfering with its composer. Archiving interrupts and closes the TUI;
revisiting the agent resumes its persisted session in a fresh one.

For a new agent, Magnus correlates its unique onboarding prompt with the new
local Codex rollout record and saves that session ID. Concurrent launches are
matched by their prompt rather than by whichever file is newest. Capture is
bounded and fail-safe: if Codex's local record cannot be identified, the live
TUI remains usable, but that session cannot be resurrected automatically.
Magnus's onboarding includes named identity, first-person memory, coordination
etiquette, and candid `[thinking]`/`[response]` engineering journals. Claude's
established prompt remains unchanged.

### Durable Cross-Provider Reviews

Put point on an agent in `*magnus*`, press `? v RET`, and Magnus handles the
rest. It asks the author for a coherent committed checkpoint, infers the exact
Git range, chooses a durable reviewer identity using the existing expertise
index, and runs a headless reviewer from the opposite provider by default.
Provider, model, and reasoning effort remain optional transient overrides.

Reviews are durable work objects rather than terminal instances. Their exact
base and head commits, attempts, provider session, findings, delivery state,
and read state survive Emacs restarts under `~/.magnus/reviews/`. Re-reviewing
keeps the reviewer identity and provider session, gives prior findings stable
IDs, and adds a new immutable round. Headless execution is serialized by
default to keep laptop resource use modest.

Completed reviews appear in their own status section with an unread dot. Press
`RET` to open a Magit-style diff reader: files and hunks fold with `TAB`, `n/p`
navigate sections, `N/P` jump between findings, `RET` opens source from the
exact reviewed commit, and `e` opens the corresponding current worktree file.
Use `[` and `]` to move between review rounds and `?` for re-review, retry,
interrupt, delivery, and archive actions. Completion never steals focus;
Magnus also delivers the durable report location back to the author agent.

### Agent Coordination
Agents communicate through a shared `.magnus-coord.md` file:

```markdown
## Active Work
| Agent | Area | Status | Files |
|-------|------|--------|-------|
| swift-fox | auth module | in-progress | src/auth.ts, src/session.ts |
| keen-owl | api tests | in-progress | tests/api/*.test.ts |

## Discoveries
- The user API returns 404 (not 403) for deleted users — handle both (swift-fox)
- Test fixtures in tests/helpers/ are shared, don't modify without checking (keen-owl)

## Log
[10:30] swift-fox: Starting work on auth. Will touch src/auth/*.ts
[10:31] keen-owl: Got it, I'll avoid those files. Working on API tests.
[10:45] swift-fox: Auth done, committed. @keen-owl you can add auth tests now.
[10:46] keen-owl: Thanks! On it.

## Decisions
- Using JWT for session tokens (swift-fox + keen-owl agreed)
```

Agents are automatically instructed to check and update this file, preventing them from stepping on each other's work.

#### Knowledge Sharing

Agents share discoveries — API quirks, gotchas, patterns — in the **Discoveries** section of the coordination file. Other agents read these on check-in and when reminded, so insights propagate naturally without you having to relay information between agents.

Agents are also nudged to write detailed commit messages capturing what they learned, so knowledge persists in git history even after the coordination file is cleaned up.

#### Automatic Housekeeping

The coordination file stays lean automatically:
- **Log** is trimmed to the last 25 entries on each reminder cycle (configurable via `magnus-coord-log-max-entries`)
- **Discoveries** and **Decisions** are cleared when the last agent leaves a project, keeping the file fresh for the next work session

### Shared Context Buffer
A per-project scratch buffer where you can paste notes, links, Confluence pages, Jira tickets, or any context you want all agents to access.

- Paste URLs and magnus fetches + caches the content
- Export to a file agents can read, or copy to clipboard
- Persists across Emacs sessions (stored in `~/.emacs.d/magnus-context/`)

### Thinking Trace
Press `t` on any instance to open a trace buffer that reads its provider's
session JSONL directly. Claude traces include its recorded thinking blocks;
Codex traces include visible `[thinking]` engineering journals, user messages,
and responses. Codex's encrypted internal reasoning is deliberately not shown.

The trace auto-refreshes so you can watch agents work in real time.

### Direct Messaging
Press `m` to send a message directly to an agent from the status buffer. The message appears in the agent's terminal as if you typed it — Claude acts on it immediately. Agents also receive periodic reminders (every 10 min) that rotate through different messages — nudging them to check the coordination file, share discoveries, and update their status.

### Attention Queue
When agents need user input (permission prompts, confirmations), they join a queue:

```
Instances: 3 [2 need attention]
```

Magnus focuses one instance at a time. Handle the request, press `a`, and the next agent gets focus. No more competing popups.

### Auto-Approve
Common safe operations (file reads, edits, grep, tests) can be auto-approved so agents don't block on routine permissions. When a yes/no prompt matches an allowlisted pattern, Magnus sends `y` automatically.

Built-in allowlist includes: `Read`, `Write`, `Edit`, `Glob`, `Grep`, safe Bash commands (`git`, `ls`, `npm test`, `cargo build`, `pytest`, etc.). Customize via `magnus-attention-auto-approve-patterns`. Set to `nil` to disable.

### Health Monitoring
Magnus tracks agent health by hashing buffer content every 30 seconds. Status indicators appear next to each instance in the status buffer:

- `+` (green) — active, buffer is changing
- `~` (yellow) — stale, no buffer changes for 2+ minutes
- `!` (red) — stuck, stale for 3+ consecutive checks
- `x` — dead, no buffer or process

Toggle health monitoring with `H` in the transient menu. Customize thresholds via `magnus-health-check-interval`, `magnus-health-stale-threshold`, and `magnus-health-stuck-threshold`.

### Headless Mode
Fire-and-forget agents that run as background processes instead of vterm buffers. Ideal for self-contained tasks where you don't need interactive input.

```
M-x magnus-create-headless RET "List all .el files and summarize them" RET
```

Or press `h` in the create dispatch menu (`c` then `h`). Headless agents:
- Run via `claude --print` with stream-json output
- Show output in a read-only buffer
- Cannot receive messages or need permission prompts
- Show as `[finished]` or `[errored]` in the status buffer when done
- Are automatically skipped by the attention queue

### Persistence
Everything persists across Emacs sessions:
- Running instances (reconnects if still alive)
- Context buffers
- Coordination state
- Review checkpoints, reports, sessions, rounds, and unread state

## Requirements

- Emacs 28.1+
- [vterm](https://github.com/akermu/emacs-libvterm)
- [transient](https://github.com/magit/transient) (built into Emacs 28+)
- [magit-section](https://github.com/magit/magit) (for the review reader)
- Claude Code CLI (for Claude instances or Claude-backed reviews)
- Codex CLI (for Codex instances or Codex-backed reviews)

The default opposite-provider review requires the other provider's CLI. For
example, reviewing a Claude author with the default settings launches Codex.

## Installation

### From MELPA

```elisp
(use-package magnus
  :ensure t
  :bind (("C-c m" . magnus)
         ("C-c M" . magnus-create-instance)))
```

Or: `M-x package-install RET magnus RET`

### From GitHub (Emacs 29+)

```elisp
;; One-liner
M-x package-vc-install RET https://github.com/hrishikeshs/magnus RET
```

Or in your config:
```elisp
(unless (package-installed-p 'magnus)
  (package-vc-install "https://github.com/hrishikeshs/magnus"))

(use-package magnus
  :bind (("C-c m" . magnus)
         ("C-c M" . magnus-create-instance)))
```

### With straight.el

```elisp
(straight-use-package
 '(magnus :type git :host github :repo "hrishikeshs/magnus"))
```

### With quelpa

```elisp
(quelpa '(magnus :fetcher github :repo "hrishikeshs/magnus"))
```

After upgrading Magnus in a running Emacs—through `M-x magnus-upgrade`,
package-menu, package-vc, straight, or another package manager—restart Emacs
before opening Magnus again. This prevents already-loaded functions and
instance structures from being mixed with the newly installed package version.
`magnus-upgrade` enforces this restart for upgrades initiated by versions that
include the guard.

## Quick Start

1. `M-x magnus` (or `C-c m`) — Open the status buffer
2. `c` — Create a new Claude Code instance
3. Choose a directory, and a vterm buffer opens with Claude running
4. Create more instances with `c`
5. Switch between them with `RET`
6. Watch them coordinate in the status buffer
7. On an agent, press `? v RET` to request an independent review

## Key Bindings

### Status Buffer (`*magnus*`)

| Key   | Action                       |
|-------|------------------------------|
| `c`   | Create new instance          |
| `RET` | Visit agent or review        |
| `k`   | Archive instance             |
| `R`   | Resurrect archived instance  |
| `r`   | Rename instance              |
| `s/S` | Suspend/resume instance      |
| `d`   | Change directory             |
| `m`   | Send message to agent        |
| `t`   | Open thinking trace          |
| `x`   | Open context buffer          |
| `C`   | Open coordination file       |
| `a/A` | Next/show attention requests |
| `P`   | Archive all instances        |
| `z`   | Toggle Do Not Disturb        |
| `F`   | Session retrospective        |
| `n/p` | Navigate agents and reviews  |
| `g`   | Refresh                      |
| `?`   | Show transient help menu     |
| `q`   | Quit                         |

Review rows use the same `n/p` navigation. `RET` opens the newest unread
completed round (or the latest round when everything is read).

### Transient Menu (`?`)

Press `?` in the status buffer to see all commands organized by category:

**Instance Actions**
| Key | Action              |
|-----|---------------------|
| `c` | Create instance     |
| `k` | Archive instance    |
| `R` | Resurrect archived  |
| `r` | Rename instance     |
| `s` | Suspend instance    |
| `S` | Resume instance     |
| `d` | Change directory    |
| `m` | Send message        |
| `t` | Thinking trace      |
| `P` | Archive all instances |

**Context (shared notes)**
| Key | Action                    |
|-----|---------------------------|
| `x` | Open context buffer       |
| `e` | Export to file in project |
| `w` | Copy to clipboard         |

**Coordination (agent communication)**
| Key | Action                   |
|-----|--------------------------|
| `C` | Open coordination file   |
| `I` | Open agent instructions  |

**Reviews**
| Key | Action                       |
|-----|------------------------------|
| `v` | Configure/request review     |
| `o` | Open review at point         |
| `V` | Review actions               |

The default request is `? v RET`. In the request popup, `p`, `m`, and `e`
override provider, model, and reasoning effort respectively.

The `? V` action popup (or `?` inside a review reader) provides:

| Key   | Action                                      |
|-------|---------------------------------------------|
| `RET` | Open the review                             |
| `r`   | Request a re-review of the next checkpoint |
| `t`   | Retry a failed or interrupted round        |
| `i`   | Interrupt a running headless reviewer      |
| `d`   | Retry delivery of review notes to author   |
| `k`   | Archive the durable review                 |

A manual interrupt remains stopped across Emacs restarts until you explicitly
retry it. From a historical review round, delivery retry targets that round;
from the status buffer it selects the newest completed undelivered round.

**Attention & Health**
| Key | Action                      |
|-----|-----------------------------|
| `a` | Next in attention queue     |
| `A` | Show attention queue        |
| `T` | Toggle attention monitoring |
| `H` | Toggle health monitoring    |

**Navigation**
| Key   | Action                |
|-------|-----------------------|
| `RET` | Visit agent or review |
| `n`   | Next item             |
| `p`   | Previous item         |

### Trace Buffer (`*trace:<name>*`)

| Key | Action          |
|-----|-----------------|
| `g` | Refresh         |
| `G` | Jump to end     |
| `q` | Close           |

### Review Buffer (`*magnus-review: ...*`)

| Key   | Action                                      |
|-------|---------------------------------------------|
| `TAB` | Fold/unfold file, hunk, or finding section |
| `n/p` | Next/previous visible section               |
| `N/P` | Next/previous finding                       |
| `RET` | Open the exact reviewed snapshot            |
| `e`   | Open the file in the current worktree       |
| `[/]` | Previous/next review round                  |
| `?`   | Review actions                              |
| `g`   | Refresh                                     |
| `q`   | Close                                       |

### Context Buffer

| Key       | Action                      |
|-----------|-----------------------------|
| `C-c C-u` | Insert URL and fetch content|
| `C-c C-f` | Fetch URL at point          |
| `C-c C-e` | Export to `.magnus-context.md` in project |
| `C-c C-c` | Copy buffer to clipboard    |
| `C-c C-s` | Save                        |

## How Coordination Works

When you create an instance, magnus:

1. Creates `.magnus-coord.md` in the project root
2. Creates `.claude/magnus-instructions.md` with the coordination protocol
3. Creates `.claude/skills/coordinate/SKILL.md` with check-in steps
4. Logs "Joined the session" in the coordination file
5. Sends a welcome message with an onboarding checklist to the agent

Agents go through a checklist before writing any code:
1. Read the instructions file
2. Read the coordination file and check the Active Work table
3. Announce planned work and files in the Log section
4. Check for conflicts with other agents
5. Add their row to the Active Work table
6. Only then begin coding

### Automatic @mention Notifications

When an agent writes `@swift-fox` in the coordination file, swift-fox automatically receives a notification with the message context. This enables real-time communication between agents without requiring them to poll the file.

The coordination skill file (`.claude/skills/coordinate/SKILL.md`) reinforces the protocol by teaching agents structured check-in steps they can follow independently.

This is advisory coordination — agents are instructed to follow the protocol, and Claude is good at it.

## Tips

### ESC key in Claude buffers

Emacs intercepts `ESC` as a Meta prefix, so it never reaches Claude Code. Magnus automatically maps `C-g` to send ESC to Claude in all instance buffers. Use `C-g` wherever Claude expects you to press Escape (cancel, go back, dismiss).

### Helm / Projectile users

Magnus avoids triggering interactive Helm/Projectile prompts when creating instances. If you still experience hangs, set a default directory:

```elisp
(setq magnus-default-directory "~/workspace")
```

## Customization

```elisp
;; Path to claude executable (default: "claude")
(setq magnus-claude-executable "/path/to/claude")

;; Path to codex executable for optional native TUI instances
(setq magnus-codex-executable "/path/to/codex")

;; Default directory for new instances
(setq magnus-default-directory "~/projects")

;; Where to store instance state (default: ~/.magnus/state.el)
(setq magnus-state-file "~/.magnus/state.el")

;; Durable review defaults
(setq magnus-review-default-provider nil) ; opposite the author
(setq magnus-review-default-effort 'high)
(setq magnus-review-max-concurrent 1)     ; laptop-friendly serialization
(setq magnus-review-directory-root "~/.magnus/reviews")

;; Attention check interval in seconds (default: 10)
(setq magnus-attention-check-interval 10)

;; Patterns that indicate an instance needs attention
(setq magnus-attention-patterns
      '("\\[y/n\\]" "\\[Y/n\\]" "Allow\\?" "Proceed\\?"))

;; Auto-approve patterns (set to nil to disable auto-approval)
(setq magnus-attention-auto-approve-patterns
      '("Read" "Write" "Edit" "Glob" "Grep"
        "Bash(git " "Bash(npm test" "Bash(cargo build"))

;; Disable automatic @mention notifications (default: t)
(setq magnus-coord-mention-notify nil)

;; Coordination reminder interval in seconds (default: 600 / 10 min)
(setq magnus-coord-reminder-interval 300)  ;; 5 minutes
(setq magnus-coord-reminder-interval nil)  ;; disable reminders

;; Max log entries before trimming (default: 25, nil to disable)
(setq magnus-coord-log-max-entries 50)

;; Health monitoring check interval in seconds (default: 30)
(setq magnus-health-check-interval 30)

;; Time before an agent is considered stale (default: 120s)
(setq magnus-health-stale-threshold 120)

;; Consecutive stale checks before "stuck" (default: 3)
(setq magnus-health-stuck-threshold 3)

;; Tools allowed in headless mode (default: "Read Write Edit Glob Grep Bash")
(setq magnus-headless-allowed-tools "Read Write Edit Glob Grep Bash")
```

## Architecture

```
magnus/
├── magnus.el              # Entry point, customization
├── magnus-instances.el    # Instance data structure and registry
├── magnus-persistence.el  # Save/restore state across sessions
├── magnus-process.el      # Process management (vterm + headless)
├── magnus-provider.el     # Additive provider dispatch
├── magnus-provider-codex.el # Optional native Codex TUI provider
├── magnus-provider-claude.el # Read-only Claude review adapter
├── magnus-headless.el     # Provider-neutral JSONL runner
├── magnus-review.el       # Durable review records + Git evidence
├── magnus-review-controller.el # Review execution + delivery
├── magnus-review-ui.el    # Magit-style review reader
├── magnus-trace.el        # Thinking trace JSONL viewer
├── magnus-status.el       # Status buffer UI
├── magnus-transient.el    # Transient popup menus
├── magnus-context.el      # Shared context buffer + URL fetching
├── magnus-coord.el        # Agent coordination protocol
├── magnus-attention.el    # Permission request queue
└── magnus-health.el       # Agent health monitoring
```

## License

MIT
