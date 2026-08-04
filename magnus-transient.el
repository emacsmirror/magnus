;;; magnus-transient.el --- Transient menus for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides transient popup menus for magnus, inspired by
;; magit's interface.

;;; Code:

(require 'transient)
(require 'magnus-instances)
(require 'magnus-process)
(require 'magnus-review)
(require 'magnus-review-controller)
(require 'magnus-review-ui)
(require 'magnus-status)

(declare-function magnus-context "magnus-context")
(declare-function magnus-context-export-for-agent "magnus-context")
(declare-function magnus-context-copy-for-agent "magnus-context")
(declare-function magnus-coord-open "magnus-coord")
(declare-function magnus-coord-open-instructions "magnus-coord")
(declare-function magnus-attention-next "magnus-attention")
(declare-function magnus-attention-show-queue "magnus-attention")
(declare-function magnus-coord-toggle-dnd "magnus-coord")
(declare-function magnus-retro "magnus-coord")
(declare-function magnus-health-toggle "magnus-health")
(declare-function magnus-process-create-headless "magnus-process")
(declare-function magnus-project-root "magnus")
(declare-function magnus-status--get-review-at-point "magnus-status")

(defvar magnus-review-ui-action-function)

;;; Main dispatch

;;;###autoload (autoload 'magnus-dispatch "magnus-transient" nil t)
(transient-define-prefix magnus-dispatch ()
  "Magnus command dispatcher."
  ["Instance Actions"
   ("c" "Create instance" magnus-status-create)
   ("k" "Archive instance" magnus-status-archive)
   ("R" "Resurrect purged" magnus-status-resurrect-purged)
   ("r" "Rename instance" magnus-status-rename)
   ("s" "Suspend instance" magnus-status-suspend)
   ("S" "Resume instance" magnus-status-resume)
   ("d" "Change directory" magnus-status-chdir)
   ("m" "Send message" magnus-status-send-message)
   ("t" "Thinking trace" magnus-status-trace)
   ("P" "Archive all instances" magnus-status-archive-all)]
  ["Context (shared notes)"
   ("x" "Open context buffer" magnus-context)
   ("e" "Export to file" magnus-context-export-for-agent)
   ("w" "Copy to clipboard" magnus-context-copy-for-agent)]
  ["Coordination (agent communication)"
   ("C" "Open coordination file" magnus-status-coordination)
   ("I" "Open agent instructions" magnus-transient-open-instructions)
   ("F" "Session retrospective" magnus-retro)]
  ["Reviews"
   ("v" "Request review" magnus-review-request-dispatch)
   ("o" "Open review" magnus-transient-review-open)
   ("V" "Review actions" magnus-review-actions)]
  ["Attention (permission requests)"
   ("a" "Next in attention queue" magnus-attention-next)
   ("A" "Show attention queue" magnus-attention-show-queue)
   ("T" "Toggle attention monitoring" magnus-attention-toggle)
   ("H" "Toggle health monitoring" magnus-health-toggle)
   ("z" "Toggle Do Not Disturb" magnus-coord-toggle-dnd)]
  ["Navigation"
   ("RET" "Visit item" magnus-status-visit)
   ("n" "Next item" magnus-status-next)
   ("p" "Previous item" magnus-status-previous)]
  ["Buffer"
   ("g" "Refresh" magnus-status-refresh)
   ("q" "Quit" quit-window)])

;;; Durable reviews

(transient-define-prefix magnus-review-request-dispatch ()
  "Request a durable independent review for the instance at point."
  ["Options (defaults: opposite provider, default model, high effort)"
   ("p" "Provider" "--provider="
    :choices ("opposite" "claude" "codex"))
   ("m" "Model" "--model=")
   ("e" "Effort" "--effort="
    :choices ("low" "medium" "high" "xhigh" "max"))]
  ["Request"
   ("RET" "Request review" magnus-transient-request-review)])

(defun magnus-transient-request-review ()
  "Request a review using the current review transient arguments."
  (interactive)
  (let* ((author (or (magnus-status--get-instance-at-point)
                     (user-error
                      "Put point on the agent whose work should be reviewed")))
         (arguments (transient-args 'magnus-review-request-dispatch))
         (provider-name (transient-arg-value "--provider=" arguments))
         (model (transient-arg-value "--model=" arguments))
         (effort-name (transient-arg-value "--effort=" arguments))
         ;; Preserve an explicit opposite choice so it overrides a customized
         ;; `magnus-review-default-provider' instead of collapsing to nil.
         (provider (and provider-name (intern provider-name)))
         (effort (and effort-name (intern effort-name))))
    (magnus-review-request author
                           :provider provider
                           :model (and model (not (string-empty-p model)) model)
                           :effort effort)
    (magnus-status-refresh)))

(defvar magnus-transient--review nil
  "Review currently targeted by the review action transient.")

(defvar magnus-transient--review-round nil
  "Round currently targeted by the review action transient.")

(defun magnus-transient--review-description ()
  "Return a heading for the current review action transient."
  (if magnus-transient--review
      (format "Review: %s → %s"
              (magnus-review-reviewer-name magnus-transient--review)
              (magnus-review-author-name magnus-transient--review))
    "No review selected"))

(transient-define-prefix magnus-review-actions-menu ()
  "Actions for one durable review."
  ["Review"
   :description magnus-transient--review-description
   ("RET" "Open" magnus-transient-review-open)
   ("r" "Request re-review" magnus-transient-review-rereview)
   ("t" "Retry failed round" magnus-transient-review-retry)
   ("i" "Interrupt running review" magnus-transient-review-interrupt)
   ("d" "Retry author delivery" magnus-transient-review-delivery)
   ("k" "Archive" magnus-transient-review-archive)])

(defun magnus-review-actions (&optional review round)
  "Open actions for REVIEW and optional ROUND.
When called from the status buffer, use the review at point."
  (interactive)
  (setq magnus-transient--review
        (or review (magnus-status--get-review-at-point)
            (user-error "Put point on a review first"))
        magnus-transient--review-round round)
  (transient-setup #'magnus-review-actions-menu))

(defun magnus-transient--selected-review ()
  "Return the review selected by transient context or status point."
  (if (derived-mode-p 'magnus-status-mode)
      (or (magnus-status--get-review-at-point)
          (user-error "Put point on a review first"))
    (or magnus-transient--review
        (user-error "No review selected"))))

(defun magnus-transient-review-open ()
  "Open the selected review in its Magit-style reader."
  (interactive)
  (if (derived-mode-p 'magnus-status-mode)
      (progn
        ;; The main dispatch also appears on instance rows; keep its labeled
        ;; review action honest instead of accidentally visiting that instance.
        (magnus-transient--selected-review)
        (magnus-status-visit))
    (magnus-review-ui-open
     (magnus-transient--selected-review)
     magnus-transient--review-round)))

(defun magnus-transient-review-rereview ()
  "Request a new committed checkpoint for the selected review."
  (interactive)
  (magnus-review-rereview (magnus-transient--selected-review))
  (magnus-status-refresh))

(defun magnus-transient-review-retry ()
  "Retry the selected review's latest failed or interrupted round."
  (interactive)
  (magnus-review-retry (magnus-transient--selected-review))
  (magnus-status-refresh))

(defun magnus-transient-review-interrupt ()
  "Interrupt the selected review's running headless attempt."
  (interactive)
  (let ((review (magnus-transient--selected-review)))
    (when (yes-or-no-p
           (format "Interrupt the review by %s? "
                   (magnus-review-reviewer-name review)))
      (magnus-review-interrupt review)
      (magnus-status-refresh))))

(defun magnus-transient-review-delivery ()
  "Retry delivery of the selected completed review to its author."
  (interactive)
  (magnus-review-retry-delivery (magnus-transient--selected-review)
                                magnus-transient--review-round)
  (magnus-status-refresh))

(defun magnus-transient-review-archive ()
  "Archive the selected durable review without deleting its reports."
  (interactive)
  (let ((review (magnus-transient--selected-review)))
    (if (eq (magnus-review-lifecycle review) 'archived)
        (user-error "Review is already archived")
      (when (yes-or-no-p
             (format "Archive review of %s? "
                     (magnus-review-author-name review)))
        (magnus-review-archive review)
        (magnus-status-refresh)
        (message "Archived review by %s"
                 (magnus-review-reviewer-name review))))))

;;; Create instance menu

(transient-define-prefix magnus-create-dispatch ()
  "Create a new Claude Code instance."
  ["Create Instance"
   ("c" "In current directory" magnus-transient-create-current-dir)
   ("d" "Choose directory" magnus-transient-create-choose-dir)
   ("p" "In project root" magnus-transient-create-project-root)
   ("h" "Headless (fire-and-forget)" magnus-transient-create-headless)])

(defun magnus-transient-create-current-dir ()
  "Create instance in current directory."
  (interactive)
  (magnus-process-create default-directory)
  (magnus-status-refresh))

(defun magnus-transient-create-choose-dir ()
  "Create instance in a chosen directory."
  (interactive)
  (let ((dir (read-directory-name "Directory: " nil nil t)))
    (magnus-process-create dir)
    (magnus-status-refresh)))

(defun magnus-transient-create-project-root ()
  "Create instance in the current project root."
  (interactive)
  (let ((root (magnus-project-root)))
    (if root
        (progn
          (magnus-process-create root)
          (magnus-status-refresh))
      (user-error "Not in a project"))))

(defun magnus-transient-create-headless ()
  "Create a headless (fire-and-forget) Claude Code instance.
Prompts for a task description, uses directory from instance at point
or `default-directory'."
  (interactive)
  (let* ((prompt (read-string "Task prompt: "))
         (dir (if-let ((instance (ignore-errors
                                   (magnus-status--get-instance-at-point))))
                  (magnus-instance-directory instance)
                default-directory)))
    (magnus-process-create-headless prompt dir)
    (magnus-status-refresh)))

;;; Instance actions

(transient-define-prefix magnus-instance-dispatch ()
  "Actions for the instance at point."
  ["Instance"
   :description magnus-transient--instance-description
   ("RET" "Visit" magnus-status-visit)
   ("k" "Archive" magnus-status-archive)
   ("R" "Resurrect" magnus-status-resurrect-purged)
   ("r" "Rename" magnus-status-rename)
   ("s" "Suspend" magnus-status-suspend)
   ("S" "Resume" magnus-status-resume)
   ("d" "Change directory" magnus-status-chdir)
   ("m" "Send message" magnus-status-send-message)
   ("t" "Thinking trace" magnus-status-trace)])

(defun magnus-transient--instance-description ()
  "Return description for current instance."
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (format "Instance: %s" (magnus-instance-name instance))
    "No instance at point"))

(defun magnus-transient-open-instructions ()
  "Open the agent instructions file."
  (interactive)
  (if-let ((instance (car (magnus-instances-list))))
      (magnus-coord-open-instructions (magnus-instance-directory instance))
    (user-error "No instances to get project directory from")))

;; The review reader calls this dispatcher from its `?' binding.  Controller
;; setup installs the same value after restart recovery; assigning it here also
;; makes direct loading of the UI and transient modules behave consistently.
(setq magnus-review-ui-action-function #'magnus-review-actions)

(provide 'magnus-transient)
;;; magnus-transient.el ends here
