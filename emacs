(setq inhibit-start-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(setq inferior-lisp-program "/usr/local/bin/clisp")
(setq ido-enable-flex-matching t)
(setq ido-everwhere t)
(ido-mode 1)
(defalias 'list-buffers 'ibuffer)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;
;;  Setup melpa.
;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;;
;; Clear M-l and M-h to be re-assigned later.
;;
(global-unset-key "\M-l")
(global-unset-key "\M-h")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (projectile tern-auto-complete tern js2-refactor ac-js2 vue-mode emmet-mode virtualenvwrapper elpy expand-region multiple-cursors beacon highlight-indentation undo_tree yasnippet-snippets which-key web-mode-edit-element use-package try tabbar solarized-theme jedi flycheck evil counsel color-theme buffer-flip auto-yasnippet ace-window ac-php ac-html-bootstrap ac-html)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))

;;
;; Bootstrap `use-package' and setup tabbar, try, etc.
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;
;; Tabbar
;;
(use-package tabbar
  :ensure t
  :config
  (tabbar-mode 1))

 (defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
   "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
               ((eq major-mode 'dired-mode) "emacs")
               (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)


(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 2.0)))))
    ))

(use-package counsel
  :ensure t
  )

;;
;; Swiper is a more advanced search package.
;;
(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    ))

;;
;; avy package to quickly jump to 1/2 character search.
;;
(use-package avy
  :ensure t
  :bind ("C-:" . avy-goto-char))
(global-set-key (kbd "C-'") 'avy-goto-char-2)

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;;
;; Set the theme.
;;
(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-light t))
;; (use-package solarized-theme
;;   :ensure t)
(when (string= (system-name) "prime")
  (load-theme 'solarized-dark t))

;;
;; Setup buffers the way I like.
;;
(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command,
so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
;; Write a message to the *Messages* buffer.
;;  (message "buffer-name: `%s'..." (buffer-name))
  (if (string-equal "*inferior-lisp*" (buffer-name))
      t
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      ))))

(global-set-key (kbd "M-l") 'xah-next-user-buffer)
(global-set-key (kbd "M-h") 'xah-previous-user-buffer)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

;;
;; Advanced Python auto-complete... ran kind of slow.
;;
;;(use-package jedi
;;  :ensure t
;;  :init
;;  (add-hook 'python-mode-hook 'jedi:setup)
;;  (add-hook 'python-mode-hook 'jedi:ac-setup))


;;
;; Save all buffers when on focus-out.
;;
(defun save-all ()
  "Auto-save..."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;;
;; Use pc-selection-mode for copy, paste, select, etc.
;;
(delete-selection-mode)
(cua-mode t)


;; Turn on liners.
(global-linum-mode t)
;;
;; yasnippets
;;
(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))


;;
;; Undo Tree
;;
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;;
;; Misc packages/configs.
;;
(global-hl-line-mode 1)

; flashes the cursor's line when you scroll
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  ; (setq beacon-color "#666600")
  )

(use-package multiple-cursors
  :ensure t)

; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(setq save-interprogram-paste-before-kill t)

;;
;; Highlights vertical tabs, but not sure it's very useful for me.
;;
;; (use-package highlight-indentation
;;   :ensure t
;;   :config
;;   (highlight-indentation-mode t))
;; (set-face-background 'highlight-indentation-face "#e3e3d3")
;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

;;
;; web-mode
;;
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
	'(("django"    . "\\.html\\'")))
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-closing t))
  
;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode.  Adjust indent."
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-toggle-current-element-highlight t))
(web-mode-toggle-current-element-highlight)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))

;;
;; Python3 virtualenv.
;;
(setq py-python-command "python3")
(setq python-shell-interpreter "python3")
(use-package elpy
  :ensure t
  :config
  (elpy-enable))


(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))


;;
;; emmet-mode
;;
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))
(global-set-key (kbd "C-j") 'emmet-expand-line)

;;
;; vue-mode
;;
;; (use-package vue-mode
;;   :ensure t)

;;
;; JavaScript
;;
(use-package js2-mode
  :ensure t
  :ensure ac-js2
  :init
  (progn
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-hook 'js2-mode-hook 'ac-js2-mode)))

(use-package js2-refactor
  :ensure t
  :config
  (progn
    (js2r-add-keybindings-with-prefix "C-c C-m")
    ;; eg. extract function with `C-c C-m ef`.
    (add-hook 'js2-mode-hook #'js2-refactor-mode)))

(use-package tern
  :ensure tern
  :ensure tern-auto-complete
  :config
  (progn
    (add-hook 'js-mode-hook (lambda () (tern-mode t)))
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))

(setq js-indent-level 2)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;;
;; projectile and ivy
;;

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))


(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

;;
;; Indent selection more/less.
;;
(defun my-indent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 2))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-unindent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -2))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(global-set-key (kbd "C->") 'my-indent-region)
(global-set-key (kbd "C-<") 'my-unindent-region)


(provide '.emacs)
;;; .emacs ends here
