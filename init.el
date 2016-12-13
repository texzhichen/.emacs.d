;; Packages for General Use
;; ========================================

;; melpa
(package-initialize)
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; color theme
(require 'solarized-theme)
(load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)
;; (require 'zenburn-theme)
;; (load-theme 'zenburn t)

;; line number
(global-linum-mode t)

;; helm
(require 'helm-config)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key "\C-x\ \C-b" 'helm-buffers-list)
(global-set-key "\C-x\ b" 'helm-mini)
(helm-mode 1)
(require 'helm-projectile)
(helm-projectile-on)
(require 'helm-ag)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

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
(global-set-key "\C-x\ \C-r" 'helm-recentf)

;; projectile
(projectile-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Packages for Specific Filetypes
;; ========================================

;; markdown
(require 'markdown-mode)

;; auctex
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; js2
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-strict-missing-semi-warning nil)

;; web-beauify
(require 'web-beautify)


;; Shortcuts
;; ========================================

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

;; disable backup files
(setq make-backup-files nil)

;; open files in the same frame from command line
(setq ns-pop-up-frames nil)

;; append /usr/local
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; disable welcome page
(setq inhibit-startup-screen t)

;; disable toolbar
(tool-bar-mode -1)

;; frame title
(setq frame-title-format '(" Emacs : " (:eval (if (buffer-file-name)
(abbreviate-file-name (buffer-file-name)) "%b"))))

(set-frame-font "MonacoB2 12")
(set-fontset-font "fontset-default" 'han '("STHeiti"))

;; scroll three line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; three line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; frame position
(add-to-list 'default-frame-alist '(left . 400))
(add-to-list 'default-frame-alist '(height . 49))
(add-to-list 'default-frame-alist '(width . 98))

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
    (yasnippet zenburn-theme helm-ag helm-projectile popwin whitespace-cleanup-mode web-mode web-beautify uuidgen solarized-theme smex smartparens projectile markdown-mode js2-mode helm auctex))))
