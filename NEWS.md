# Magnus release notes

## 0.2.0

Magnus 0.2.0 turns the original Claude-oriented instance manager into a
provider-neutral, resource-bounded agent workspace.

### Highlights

- Run Claude Code and Codex side by side in their native `vterm` TUIs, with
  shared durable identity, onboarding, memory, and lifecycle behavior.
- Request durable headless reviews from either provider over exact committed
  Git ranges. Findings open in a folding Magit-style diff, and the same
  reviewer identity and provider session continue across follow-up rounds.
- Coordinate agents through the shared `.magnus-coord.md` project journal.
  Exact `REVIEW-READY` markers make committed review checkpoints durable across
  Emacs restarts.
- Diagnose provider CLIs, libraries, Git, durable storage, coordination state,
  and exhausted checkpoint retries with `M-x magnus-doctor`.

### Behavioral changes

- Renaming is now archived-only and transactional: Magnus moves the agent's
  memory home together with its unique registry name or rolls both back.
  Project moves and resurrection likewise roll registry and process state back
  if provider startup fails.
- Fresh Claude sessions use an exact `--session-id` on current CLIs. Older CLIs
  retain unique-delta discovery, but fresh launches are serialized per physical
  project within an Emacs session until that legacy capture resolves.
- Startup does not replay old conversational chatter, while unresolved
  `REVIEW-READY` markers are retried. A manually terminated headless task is
  `stopped`, while a normal nonzero process exit is `errored`.
- `C` opens the shared coordination journal, and manual `g` polls watched
  projects and re-arms exhausted review-checkpoint delivery.
- AI-generated dashboard fortunes are now opt-in; static messages remain the
  default.

### Resource hardening

- Reviews remain serialized by default, cap each raw stream artifact, and time
  out abandoned attempts so the FIFO can continue.
- Expertise indexing, retrospectives, and optional dashboard generations share
  one low-priority FIFO with a single active process and bounded queue, output,
  and timeout settings; expertise indexing also bounds memory input.
- Headless provider parsing bounds stderr, JSONL records, and retained errors;
  trace viewing uses bounded disk chunks, records, initial entries, and buffer
  lines.
- Coordination-log housekeeping and review-checkpoint retries remain bounded
  and recoverable through status refresh and `magnus-doctor`.
- CI covers Emacs 28.1, 29.4, and 30.2.

The coordination protocol is supplied through ordinary onboarding and
`.claude/magnus-instructions.md`; no Magnus plugin or agent skill is required.
