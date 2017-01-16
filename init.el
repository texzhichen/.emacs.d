;; Packages for General Use
;; ========================================

;; melpa
(package-initialize)
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; emacs-mac
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-z") 'undo)
(require 'redo+)
(global-set-key (kbd "s-Z") 'redo)

;; color theme
(require 'solarized-theme)
(load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)

;; org-mode
(setq org-image-actual-width '(500))
(setq org-startup-truncated nil)

;; line number
(global-linum-mode t)

;; helm
(require 'helm-config)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(helm-mode 1)
(require 'helm-projectile)
(helm-projectile-on)
(require 'helm-ag)
(global-set-key (kbd "C-c a t") 'helm-ag-this-file)
(global-set-key (kbd "C-c a a") 'helm-ag)
(global-set-key (kbd "C-c a i") 'helm-imenu)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; whitespace cleanup on save
(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;; projectile
(require 'projectile)
(projectile-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching nil)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; powerline
(require 'powerline)
(powerline-default-theme)

;; all-the-icons
(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; window numbering
(require 'window-numbering)
(window-numbering-mode)

;; reveal in finder
(require 'reveal-in-osx-finder)
(global-set-key (kbd "C-c z") 'reveal-in-osx-finder)


;; Packages for Specific Filetypes
;; ========================================

;; markdown
(require 'markdown-mode)

;; auctex
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook #'outline-minor-mode)

;; js2
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-strict-missing-semi-warning nil)

;; web-beauify
(require 'web-beautify)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun web-mode-tab-setting()
  (setq indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence 2 200 2))
  (setq tab-width 2)
  (setq indent-line-function 'insert-tab))

(add-hook 'web-mode-hook 'web-mode-tab-setting)

;; outline-minor-mode key map
 (define-prefix-command 'cm-map nil "Outline-")
 ; HIDE
 (define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
 (define-key cm-map "t" 'outline-hide-body)         ; Hide everything
						    ; but headings
						    ; (all body lines)
 (define-key cm-map "o" 'outline-hide-other)        ; Hide other branches
 (define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
 (define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
 (define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
 ; SHOW
 (define-key cm-map "a" 'show-all)          ; Show (expand) everything
 (define-key cm-map "e" 'show-entry)        ; Show this heading's body
 (define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
 (define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
 (define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
 ; MOVE
 (define-key cm-map "u" 'outline-up-heading)                ; Up
 (define-key cm-map "n" 'outline-next-visible-heading)      ; Next
 (define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
 (define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
 (define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
 (global-set-key "\M-o" cm-map)

;; Useful Snippets
;; ========================================

'' backtab
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

;; comment current line if nothing is selected
(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))
(global-set-key "\M-;" 'comment-eclipse)

;; poition of the cursor after isearch
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))


;; Settings
;; ========================================

;; word-wrap
(setq-default word-wrap t)

;; highlight current line
(global-hl-line-mode t)

;; disable backup files
(setq make-backup-files nil)

;; open files in the same frame from command line
(setq ns-pop-up-frames nil)

;; append /usr/local
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

;; disable welcome page
(setq inhibit-startup-screen t)

;; disable toolbar
(tool-bar-mode -1)

;; frame title
(setq frame-title-format '((:eval (if (buffer-file-name)
(abbreviate-file-name (buffer-file-name)) "%b"))))

(set-frame-font "MonacoB2 12")
(set-fontset-font "fontset-default" 'han '("STHeiti"))

;; scroll three line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; three line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; frame position
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 400))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 100))

;; turn on auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)

;; Language settings
(set-language-environment 'English)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (redo+ reveal-in-osx-finder window-numbering smart-tabs-mode magit all-the-icons all-the-icons-dired powerline company yasnippet helm-ag helm-projectile popwin whitespace-cleanup-mode web-mode web-beautify uuidgen solarized-theme smex smartparens projectile markdown-mode js2-mode helm auctex))))
