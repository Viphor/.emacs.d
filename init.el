(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Auto set by settings manager
;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(ansi-color-faces-vector
;   [default default default italic underline success warning error])
; '(ansi-color-names-vector
;   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
; '(custom-enabled-themes (quote (solarized-dark)))
; '(custom-safe-themes
;   (quote
;    ("959a77d21e6f15c5c63d360da73281fdc40db3e9f94e310fc1e8213f665d0278" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
					; )

;; Zenburn
(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t)
  )

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq mac-right-option-modifier 'super)

(define-key global-map [C-home] 'beginning-of-buffer)
(define-key global-map [C-end] 'end-of-buffer)
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

(tool-bar-mode -1)

;; shows line and column numbers in buffers
(global-linum-mode 1)
(setq column-number-mode t)

;; Disable scroll bars
(scroll-bar-mode -1)

;; Auto close parens
(electric-pair-mode 1)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Window sizeing functions
(defun shrink-window-horizontally-by (n)
  "Shrinks the window by n-amount"
  (interactive "Number of ticks to shrink window by (default 1): ")
  (shrink-window-horizontally n))

(defun enlarge-window-horizontally-by (n)
  "Enlarges the window by n-amount"
  (interactive "Number of ticks to enlarge window by (default 1): ")
  (enlarge-window-horizontally n))

;; == DocView mode hook ==
;(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; load cmake mode
(autoload 'cmake-mode "~/CMake/Auxiliary/cmake-mode.el" t)

; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

;; == Indentation mode ==
;(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; == Infer indentation mode ==
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.                    
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if        
  ;; neither, we use the current indent-tabs-mode                               
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))



;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              0.1
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        0
	company-backends                '((company-irony company-gtags company-dabbrev))
	)
  :bind ("C-," . company-complete-common)
  )

;; == CSS company mode ==
(add-hook 'css-mode-hook
	  (lambda ()
	    ;(set (make-local-variable 'company-backends) '(company-css))
	    (local-set-key (kbd "C-,") 'company-css)
	    (infer-indentation-style)))


;; == LaTeX setup ==
(load "~/.emacs.d/init-24-tex.el")
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (use-package company-auctex :ensure t :defer t)
	    (company-auctex-init)
	    (set (make-local-variable 'company-backends)
		 '(company-auctex company-latex-commands company-dabbrev))
	    (local-set-key (kbd "C-:") 'company-complete-common)))
	    ;(local-set-key (kbd "C-,") 'company-latex-commands)
	    ;(infer-indentation-style)))


;; == Scheme settings ==
(load "~/.emacs.d/scheme-setup.el")

;; == SmallTalk mode ==
(setq auto-mode-alist
           (append  '(("\\.st\\'" . smalltalk-mode))
                    auto-mode-alist))
     
;(autoload 'smalltalk-mode "/usr/local/share/emacs/site-lisp/gnu-smalltalk/smalltalk-mode.elc" "" t)
