; Add this to your .emacs file, or load it from an existing emacs lisp file.

; Prerequisite:
;   You must have downloaded a Scheme system.
;   Specifically, the stuff below assumes that you have downloaded Petite Chez Scheme, and placed in the directory
;   c:/Programs/Chez-Scheme-Version-8.4/. I always, recommend installing stuff in directories without spaces in dir/file names.
; 
;
; Usage: 
;   Split the window in two: M-x split-window-below (eller C-x 2).
;   In top pane: M-x run-petite-interactively.
;   In bottom pane: Bring up a file with some Scheme stuff. If necessary, put your buffer in Scheme mode: M-x scheme-mode.
;   You can send a single form, a number of forms, and the entire buffer with the commands
;   M-x lisp-eval-defun (C-M-x), M-x lisp-eval-region (C-M-y), or M-x lisp-eval-buffer (C-M-z).

(defun run-petite-interactively ()
  (interactive)
  (let ((inferior-lisp-program "/usr/local/bin/petite"))  ; assumes that you have installed Petite Chez Scheme here...
    (run-lisp inferior-lisp-program)))

(defun lisp-eval-buffer () 
  (interactive)
  (lisp-eval-region (point-min) (point-max)))


(defun extend-scheme-mode ()
  (define-key scheme-mode-map "\C-\M-x" 'lisp-eval-defun)      ; always available
  (define-key scheme-mode-map "\C-\M-y" 'lisp-eval-region)     ; available if you are in scheme mode
  (define-key scheme-mode-map "\C-\M-z" 'lisp-eval-buffer)    ; define above

  ; Menu support:
  (define-key scheme-mode-map [menu-bar scheme schemeeval]
    (cons "Evaluate Scheme form(s)" (make-sparse-keymap "SchemeEval")))

  (define-key scheme-mode-map [menu-bar scheme schemeeval eval-buffer]
      '("Current Buffer" . lisp-eval-buffer))

  (define-key scheme-mode-map [menu-bar scheme schemeeval eval-region]
      '("Current Region" . lisp-eval-region))

  (define-key scheme-mode-map [menu-bar scheme schemeeval eval-def]
      '("Current Form" . lisp-eval-defun))
)

; Extends Scheme mode with key bindings for lisp-eval-defun, lisp-eval-region, and lisp-eval-buffer.
(setq scheme-mode-hook
  (if (boundp 'scheme-mode-hook)
      (cons 'extend-scheme-mode scheme-mode-hook)
      (list 'extend-scheme-mode)))
