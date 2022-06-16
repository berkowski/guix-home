;;;; init.el -*- lexical-binding: t; -*-
;;;;
;;;; emacs configuration
;;;;
;;;; Inspired by the rational-emacs project: https://github.com/SystemCrafters/rational-emacs,
;;;; but with less emphasis on modularity.

;;; Some global defaults

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Spaces instead of tabs (setq-default indent-tabs-mode nil)

;; Make scrolling smoother
(setq auto-window-vscroll nil)                             ; Don't automatically ajust to view tall lines 
(customize-set-variable 'fast-but-imprecise-scrolling t)   ; Sacrifice accuracy in scrolling for speed
(customize-set-variable 'scroll-conservatively 101)        ; Never recenter at point (value > 100), just bring point into view
(customize-set-variable 'scroll-margin 0)                  ; Don't auto-scroll when point gets near margin
(customize-set-variable 'scroll-preserve-screen-position t); Keep the point position constant when scrolling

;; Don't polute directories with emacs-generated files
;; backup files go into a single directory
(customize-set-variable 'backup-directory-alist
                        `(("." . ,(concat (or (getenv "XDG_DATA_HOME") "~/.local/share")
                                          "/emacs/backup"))))

;; Better support buffers with long lines
(setq-default bidi-paragraph-direction 'left-to-right)     ; We don't really use right-to-left or bidirectional text
(setq-default bidi-inhibit-bpa t)                          ; Don't try to guess direction
(global-so-long-mode 1)                                    ; Add performance mitigations for files with long lines

(defun zb/kill-buffer-but-keep-window ()
  (let ((buffer-to-kill (current-buffer)))
    (interactive)
    (previous-buffer)
    (kill-buffer buffer-to-kill)))

;;; Help buffer improvements

;; Make `describe-*' screens more helpful
;; guix requirements
;; - emacs-helpful
(require 'helpful)
(define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key [remap describe-symbol] #'helpful-symbol)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key (kbd "C-h F") #'helpful-function)

;; Bind extra `describe-*' commands
(global-set-key (kbd "C-h K") #'describe-keymap)

;; which-key provides prompts for key combinations
;; guix-requirements
;; - emacs-which-key
(require 'which-key)
(customize-set-variable 'which-key-allow-evil-operators t)        ; which-key prompts at evil motions.
(which-key-mode 1)


;;; Evil mode configuration
;; guix dependencies:
;; - emacs-evil
;; - emacs-evil-collection
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)

;; Load evil and enable globally
(require 'evil)
(evil-mode 1)

;; Make C-g revert to normal state
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(global-set-key (kbd "C-M-u") 'universal-argument)

;; C-h is backspace in insert state
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Initialize evil-collection
(evil-collection-init)

;;; Org mode

;; guix dependencies:
;; - emacs-org
;; - emacs-org-appear
(require 'org)
;; org-appear toggle visibility of hidden elements such as emphasis markers or links
(require 'org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)

;;; Version Control

;; magit
;; guix dependencies
;; - emacs-magit
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)  ; the recommended keybinding

;;; Software development

;; Rust
;;
;; guix dependencies:
;; - emacs-rust-mode
(require 'rust-mode)
