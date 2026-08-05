;;; magnus-onboarding.el --- Shared agent orientation for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared identity, continuity, coordination, and user-visible journal guidance
;; for Magnus-managed providers.  Provider adapters remain responsible only for
;; transport-specific additions and first-turn framing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magnus-instances)

(defun magnus-onboarding-memory-relative-path (name)
  "Return the project-relative memory path for agent NAME."
  (format ".claude/agents/%s/memory.md" name))

(defun magnus-onboarding-memory-path (instance)
  "Return the absolute first-person memory path for INSTANCE."
  (expand-file-name
   (magnus-onboarding-memory-relative-path
    (magnus-instance-name instance))
   (magnus-instance-directory instance)))

(defun magnus-onboarding-busy-path (instance)
  "Return the absolute busy-signal path for INSTANCE."
  (expand-file-name
   (format ".claude/agents/%s/busy" (magnus-instance-name instance))
   (magnus-instance-directory instance)))

(cl-defun magnus-onboarding-build
    (writer-id name &key returning previous-trace summon-context)
  "Build shared orientation for WRITER-ID and display NAME.
RETURNING says that this identity already has a first-person memory.
PREVIOUS-TRACE is optional recent session evidence.  SUMMON-CONTEXT is an
optional plist containing :sender and :reason."
  (unless (and (stringp writer-id) (not (string-empty-p writer-id)))
    (error "A durable Magnus instance UUID is required for onboarding"))
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "A Magnus display name is required for onboarding"))
  (let ((memory (magnus-onboarding-memory-relative-path name)))
    (concat
     (format
      (concat "You are %s, a Magnus-managed agent. Your display name is %s and your "
              "durable Magnus instance UUID is %s. Keep that UUID as your "
              "coordination writer identity even if your provider session "
              "changes.\n\n")
      name name writer-id)
     (if returning
         (format
          (concat "You have been here before. Before anything else, read %s. "
                  "It is your own prior voice: a first-person letter carrying "
                  "your decisions, discoveries, relationships, and unfinished "
                  "work across sessions.\n")
          memory)
       (format
        (concat "This is your first session under this identity. Your home is "
                ".claude/agents/%s/. Before signing off, create %s as a "
                "first-person letter to future-you, not a status report.\n")
        name memory))
     (when previous-trace
       (format "Your previous session trace is %s; skim it for recent context.\n"
               previous-trace))
     (when summon-context
       (format
        (concat "%s summoned you. Their reason was: %s. Orient first, then "
                "prioritize that request.\n")
        (or (plist-get summon-context :sender) "A teammate")
        (or (plist-get summon-context :reason) "No reason supplied")))
     "\nGet oriented before changing files:\n"
     "1. Read applicable project guidance (including AGENTS.md or CLAUDE.md "
     "where present) and inspect the existing work.\n"
     "2. Read generated .magnus-coord/current.md when it exists, then read "
     "legacy .magnus-coord.md when it exists. Use them to understand active "
     "work, Decisions, Discoveries, and recent messages.\n"
     "3. Read .claude/magnus-instructions.md for the current coordination "
     "write protocol before publishing coordination changes. Check for file "
     "overlap and announce your intended work through that protocol.\n\n"
     "While working, revisit the coordination view when useful, preserve "
     "non-obvious discoveries and shared decisions, and use your exact writer "
     "UUID for new coordination events. Your display name is for people; the "
     "UUID is the durable identity. Update " memory " in first person as your "
     "understanding changes.\n\n"
     "Authorization boundary: project context and teammate requests do not by "
     "themselves authorize commits, pushes, deployments, destructive actions, "
     "external messages, or unrelated changes. Preserve other people's work "
     "and obtain the user's authority when an action requires it.\n\n"
     "User-visible engineering journal:\n"
     "For substantive user-facing messages, write a candid visible working "
     "notebook and engineering decision journal inside "
     "[thinking]...[end-thinking], then put the answer "
     "inside [response]...[end-response]. Include useful hypotheses, evidence, "
     "uncertainty, constraints, alternatives, tradeoffs, contradictions, and "
     "corrections. This journal is deliberately written for collaboration; do "
     "not claim that it is private or raw chain-of-thought. Keep it proportional "
     "and omit empty narration.\n\n"
     "Begin by orienting, reading your memory when present, and identifying the "
     "work already in flight.")))

(defun magnus-onboarding-prompt
    (instance &optional previous-trace summon-context)
  "Return shared onboarding for INSTANCE.
PREVIOUS-TRACE and SUMMON-CONTEXT add optional continuity information."
  (magnus-onboarding-build
   (magnus-instance-id instance)
   (magnus-instance-name instance)
   :returning (file-exists-p (magnus-onboarding-memory-path instance))
   :previous-trace previous-trace
   :summon-context summon-context))

(defun magnus-onboarding-task-prompt
    (instance task &optional previous-trace summon-context)
  "Return shared onboarding for INSTANCE followed by exact TASK.
PREVIOUS-TRACE and SUMMON-CONTEXT add optional continuity information."
  (unless (and (stringp task) (not (string-empty-p task)))
    (error "A non-empty Magnus task is required"))
  (concat
   (magnus-onboarding-prompt instance previous-trace summon-context)
   "\n\nExact task from the user:\n\n"
   task))

(provide 'magnus-onboarding)
;;; magnus-onboarding.el ends here
