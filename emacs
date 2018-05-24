(setq inhibit-start-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ido-enable-flex-matching t)
(setq ido-everwhere t)
(ido-mode 1)
(defalias 'list-buffers 'ibuffer)


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(global-unset-key "\M-l")
(global-unset-key "\M-h")

;;(global-set-key (kbd "M-o")  'mode-line-other-buffer)
;;(defun switch-to-previous-buffer ()
;;      (interactive)
;;      (switch-to-buffer (other-buffer (current-buffer) 1)))
;;(global-set-key (kbd "M-l") 'next-buffer)
;;(global-set-key (kbd "M-h") 'previous-buffer)

;;
;; Bootstrap `use-package'
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package tabbar
  :ensure t
  :config
  (tabbar-mode 1))

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

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))
(global-set-key (kbd "C-'") 'avy-goto-char-2)

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-light t))

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
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

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

(defun save-all ()
  "Auto-save..."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; Turn on line numbers.
(global-linum-mode t)

;;
;; yasnippets
;;
(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (vue-mode emmet-mode virtualenvwrapper elpy expand-region multiple-cursors beacon highlight-indentation undo_tree yasnippet-snippets which-key web-mode-edit-element use-package try tabbar solarized-theme jedi flycheck evil counsel color-theme buffer-flip auto-yasnippet ace-window ac-php ac-html-bootstrap ac-html))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))


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

(use-package highlight-indentation
  :ensure t
  :config
  (highlight-indentation-mode t))
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

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
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

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
(use-package vue-mode
  :ensure t)  
