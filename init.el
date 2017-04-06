;; Packages for General Use
;; ========================================

;; melpa
(package-initialize)
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-follow-mode-persistent t)
 '(package-selected-packages
   (quote
    (tide smex dired-subtree indent-guide company-tern tern ace-window avy org-download redo+ reveal-in-osx-finder smart-tabs-mode magit company yasnippet popwin whitespace-cleanup-mode web-mode web-beautify solarized-theme smartparens projectile markdown-mode js2-mode auctex))))

;; emacs-mac
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-l") 'goto-line)
(require 'redo+)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'redo)
(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)

;; color theme
(require 'solarized-theme)
(load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)

;; show matching parentheses
(show-paren-mode t)

;; line number
(global-linum-mode t)

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-x C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c f") 'counsel-git)
;; (global-set-key (kbd "C-c g") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
(global-set-key (kbd "C-'") 'ivy-avy)
(global-set-key (kbd "C-c s") 'swiper)

(require 'counsel-projectile)
(counsel-projectile-on)

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; whitespace cleanup on save
(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)

;; projectile
(require 'projectile)
(projectile-mode)
(setq projectile-completion-system 'ivy)
(setq projectile-enable-caching nil)

(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c g") 'projectile-ag)
(global-set-key (kbd "C-c b") 'projectile-switch-to-buffer)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(global-set-key (kbd "C-c i") 'yas-expand)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; reveal in finder
(require 'reveal-in-osx-finder)
(global-set-key (kbd "C-c z") 'reveal-in-osx-finder)

;; avy
(require 'avy)
(global-set-key (kbd "s->") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-char)

;; ace-window
(require 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)

;; indent-guide
(require 'indent-guide)
(indent-guide-global-mode)

;; dired-subtree
(define-key dired-mode-map "i" 'dired-subtree-insert)
(define-key dired-mode-map ";" 'dired-subtree-remove)

;; rebinds `^´ to use the same buffer
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

;; Packages for Specific Filetypes
;; ========================================

;; markdown
(require 'markdown-mode)

;; auctex
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook #'outline-minor-mode)

;; ========== <Web Development> ========== ;;

;; scss-mode
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))

;; typescript
(setq typescript-auto-indent-flag nil)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(setq tide-format-options '(:tabSize 2 :indentSize 2))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)


;; web-beauify
(require 'web-beautify)

;; js2
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
;; (setq js2-strict-missing-semi-warning nil)

;; tern
(require 'tern)
(add-hook 'web-mode-hook (lambda () (tern-mode t)))

(setq tern-command (append tern-command '("--no-port-file")))

(require 'company-tern)
(add-to-list 'company-backends 'company-tern)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq web-mode-enable-auto-indentation nil)
(setq web-mode-enable-auto-closing nil)

(defun web-mode-tab-setting()
  (setq indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence 2 200 2))
  (setq tab-width 2)
  (setq indent-line-function 'insert-tab))

(add-hook 'web-mode-hook 'web-mode-tab-setting)

;; This is to force .js files using .jsx settings for ReactJS
(add-hook 'web-mode-hook
      (lambda ()
        ;; short circuit js mode and just do everything in jsx-mode
        (if (equal web-mode-content-type "javascript")
            (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type))))

;; add js2-minor-mode to web-mode only for .js files, e.g., not .html
(add-hook 'find-file-hook 'my-project-hook)
(defun my-project-hook ()
  (when (string= (file-name-extension buffer-file-name) "js")
    (web-mode)
    (js2-minor-mode)))

(global-set-key (kbd "C-j") 
		(lambda () (interactive) (electric-newline-and-maybe-indent)
		  (indent-relative)))
(global-set-key (kbd "C-c j") 'indent-relative)

;; ========== </Web Development> ========== ;;

;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-image-actual-width '(500))
(setq org-startup-truncated nil)
(setq org-src-fontify-natively t)

;; change the way how org-mode opens a link
;; http://stackoverflow.com/questions/3973896/emacs-org-mode-file-viewer-associations
(add-hook 'org-mode-hook
      '(lambda ()
             (setq org-file-apps
                   (append '(
                             ("\\.png\\'" . default)
			     ("\\.pdf\\'" . default)
			     ("\\.tiff\\'" . default)
			     ("\\.jpg\\'" . default)
                             ) org-file-apps ))))

(require 'org-download)
(setq-default org-download-image-dir "./images")

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

;; tab and backtab region
(defun indent-region(numSpaces)
  (progn 
    ; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

    ; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
    )

    (save-excursion ; restore the position afterwards            
      (goto-char regionStart) ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd) ; go to the end of region
      (setq end (line-end-position)) ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil) ; restore the selected region
    )
  )
)

(defun untab-region (N)
  (interactive "p")
  (indent-region -2)
)

(defun tab-region (N)
  (interactive "p")
  (if (use-region-p)
    (indent-region 2) ; region was selected, call indent-region
    (insert "  ") ; else insert four spaces as expected
  )
)

(global-set-key (kbd "<S-tab>") 'untab-region)
(global-set-key (kbd "<tab>") 'tab-region)

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

;; mode line
(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
      (propertize "%02l" 'face 'font-lock-type-face) ","
      (propertize "%02c" 'face 'font-lock-type-face) 
    ") "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    "] "


    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face 'font-lock-preprocessor-face
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))))  
    "] "

    ;; " --"
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    ;; "%-" ;; fill with '-'
    ))

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

