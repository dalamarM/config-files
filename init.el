(require 'package)
;(require 'flycheck-clj-kondo)


(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(require 'clj-refactor)
(defun my-clojure-mode-hook ()
    (ido-mode 1)
    (show-paren-mode 1)
    (linum-mode 1)
    (paredit-mode 1)
    (company-mode 1)
    (clj-refactor-mode 1)
    (flycheck-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(visual-regexp-steroids visual-regexp cljr-helm egg russian-holidays crux cl-lib togetherly ace-window dumb-jump avy magit smex ido-completing-read+ flycheck-clj-kondo helm-ag ag flycheck sayid rainbow-delimiters clj-refactor undo-tree helm-cider helm paredit company guide-key helm-projectile zenburn-theme neotree projectile spinner cider))
 '(split-height-threshold 1)
 '(split-width-threshold 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'zenburn t)
(set-frame-font "DejaVu Sans Mono 14" nil t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "M-[") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq cider-repl-display-help-banner nil)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-c C-m"))
(guide-key-mode 1)  ; Enable guide-key-mode
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (setq projectile-switch-project-action 'neotree-projectile-action)
					;(neotree-toggle)
;; (delete-selection-mode 1)
;; (add-hook 'cider-repl-mode-hook #'visual-line-mode)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

(require 'flycheck-clj-kondo)
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(setq ring-bell-function 'ignore)
(helm-cider-mode +1)
(eval-after-load 'clojure-mode
  '(sayid-setup-package))


(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-o") 'ace-window)     
(global-set-key (kbd "C-'") 'avy-goto-char-timer)
(global-set-key (kbd "C-c c") 'cider-jack-in-clj)
(global-set-key (kbd "C-c x") 'cider-connect-clj)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-M-l") 'paredit-copy-as-kill)
(global-set-key (kbd "C-S-f") 'helm-projectile-ag)
(global-set-key (kbd "C-S-t") 'helm-projectile)


;; if the files are not already in the load path
(add-to-list 'load-path "folder-to/visual-regexp/")
(add-to-list 'load-path "folder-to/visual-regexp-steroids/")
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-M-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-M-s") 'vr/isearch-forward) ;; C-M-s


(require 'calendar)

