;;; init.el --- nalexander's config  -*- lexical-binding: t; coding:utf-8; fill-column: 119 -*-

;; Heavily influenced by:
;; - https://git.sr.ht/~bandali/.emacs.d

;;; Code:

;;; Emacs initialization

(defvar nca/before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")
(message "Loading Emacs...done (%.3fs)"
         (float-time (time-subtract nca/before-user-init-time
                                    before-init-time)))

;; temporarily increase `gc-cons-threshhold' and `gc-cons-percentage'
;; during startup to reduce garbage collection frequency.  clearing
;; `file-name-handler-alist' seems to help reduce startup time too.
(defvar nca/gc-cons-threshold gc-cons-threshold)
(defvar nca/gc-cons-percentage gc-cons-percentage)
(defvar nca/file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 400 1024 1024)  ; 400 MiB
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      ;; sidesteps a bug when profiling with esup
      esup-child-profile-require-level 0)

;; set them back to their defaults once we're done initializing
(defun nca/post-init ()
  (setq gc-cons-threshold nca/gc-cons-threshold
        gc-cons-percentage nca/gc-cons-percentage
        file-name-handler-alist nca/file-name-handler-alist))
(add-hook 'after-init-hook 'nca/post-init)

;; increase number of lines kept in *Messages* log
(setq message-log-max 20000)

;; make it easy to edit .emacs
(defun .emacs ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-reuse-buffers t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(desktop-globals-to-save
   '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history kill-ring))
 '(desktop-save-mode t)
 `(directory-abbrev-alist
   '((".*/searchfox.org/mozilla-central/source/" . ,(expand-file-name "~/Mozilla/gecko/"))))
 '(git-commit-summary-max-length 100)
 '(global-flycheck-mode t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules"))
 '(hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible try-expand-dabbrev try-expand-dabbrev-all-buffers try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol))
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(js2-strict-trailing-comma-warning nil)
 '(js2-strict-var-redeclaration-warning nil)
 '(org-startup-folded nil)
 '(pipenv-executable "/usr/local/bin/pipenv")
 '(powerline-default-separator 'wave)
 '(powerline-display-buffer-size nil)
 '(powerline-height 20)
 '(projectile-enable-caching t)
 '(safe-local-variable-values
   '((checkdoc-package-keywords-flag)
     (prompt-to-byte-compile)))
 '(show-paren-mode t nil (paren))
 '(sp-base-key-bindings nil))

;; Hack font is from https://github.com/source-foundry/Hack.
(if (string= system-type "darwin")
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:stipple nil :background "ivory3" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :family "hack"))))
     '(column-marker-1-face ((t (:background "#CF1010")))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:stipple nil :background "ivory3" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :family "hack"))))
   '(column-marker-1-face ((t (:background "#CF1010"))))))

;; optionally, uncomment to supress some byte-compiler warnings
;;   (see C-h v byte-compile-warnings RET for more info)
;; (setq byte-compile-warnings
;;       '(not free-vars unresolved noruntime lexical make-local))


;;; whoami

(setq user-full-name "Nick Alexander"
      user-mail-address "ncalexander@gmail.com")


;;; comment macro

;; useful for commenting out multiple sexps at a time
(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  (declare (indent defun))
  nil)


;;; Package management

;; No package.el  (for emacs 26 and before, uncomment the following)
;; Not necessary when using straight.el
;;   (C-h v straight-package-neutering-mode RET)

(when (and
       (not (featurep 'straight))
       (version< emacs-version "27"))
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  )

;; for emacs 27 and later, we use early-init.el.  see
;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b

;; straight.el

;; Main engine start...

(setq straight-repository-branch "master"
      straight-check-for-modifications '(check-on-save find-when-checking))

;; straight.el for package management.
;; From https://github.com/raxod502/straight.el#getting-started.

(defun nca/bootstrap-straight ()
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Solid rocket booster ignition...

(nca/bootstrap-straight)

;; We have lift off!

(setq straight-use-package-by-default t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; Otherwise, we follow `describe-function` links into `straight/build` rather than `straight/repos`.
(setq find-file-visit-truename t)

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude
               (expand-file-name "~/.emacs.d/straight/build/")))

(defun nca/reload-init ()
  "Reload init.el."
  (interactive)
  (setq nca/file-name-handler-alist file-name-handler-alist)
  (load user-init-file)
  (nca/post-init))

;; use-package
(straight-use-package 'use-package)
(if nil                             ; set to t when need to debug init
    (progn
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
      (require 'use-package))
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(setq use-package-always-defer t)
(require 'bind-key)

;; ;; Bootstrap `use-package'
;; (setq-default use-package-always-defer t ; Always defer load package to speed up startup time
;;               use-package-verbose nil ; Don't report loading details
;;               use-package-expand-minimally t  ; make the expanded code as minimal as possible
;;               use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
;; ;; Integration with use-package
;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)


(when (string= system-type "windows-nt")
  (setq shell-file-name "C:/mozilla-build/msys/bin/bash.exe")
  (let ((paths
         '(
           "C:/mozilla-build/python"
           "C:/mozilla-build/python/Scripts"
           "C:/mozilla-build/python3"
           "C:/mozilla-build/python3/Scripts"
           "C:/mozilla-build/bin"
           "C:/mozilla-build/msys/local/bin"
           "C:/mozilla-build/msys/bin"
           "C:/mozilla-build/nsis-3.01"
           "C:/Windows/System32/Wbem"
           "C:/Windows/System32/WindowsPowerShell/v1.0/"
           "C:/Windows/System32/OpenSSH/"
           "C:/Program Files/dotnet/"
           "C:/Program Files/Microsoft SQL Server/130/Tools/Binn/"
           "C:/Program Files/Microsoft SQL Server/Client SDK/ODBC/170/Tools/Binn/"
           "C:/ProgramData/chocolatey/bin"
           "C:/Git/mingw64/bin"
           "C:/Git/cmd"
           "C:/Users/nalexander/.cargo/bin"
           "C:/Users/nalexander/AppData/Local/Microsoft/WindowsApps"
           "C:/LLVM/bin"
           "C:/Users/nalexander/AppData/Roaming/emax64/libexec/emacs/28.0.50/x86_64-w64-mingw32"
           "C:/Windows/system32"
           "C:/Windows"
           "c:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC/14.25.28610/bin/Hostx64/x64")))
    (setenv "PATH" (mapconcat 'identity paths ";"))
    (setq exec-path (append paths (list "." exec-directory)))))

;; better $PATH (and other environment variable) handling
(use-package exec-path-from-shell
  :defer 0.4
  ;; :init
  ;; (setq exec-path-from-shell-arguments           nil
  ;;       exec-path-from-shell-check-startup-files nil)
  :config
  (unless (string= system-type "windows-nt")
    (exec-path-from-shell-initialize))
  ;; while we're at it, let's fix access to our running ssh-agent
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;; Early load Org from Git version instead of Emacs built-in version
; (straight-use-package 'org-plus-contrib)

(straight-use-package 'bind-key)


;;; Initial setup

;; keep ~/.emacs.d clean
(use-package no-littering
  :demand t
  :config
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (save-place-mode 1)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; separate custom file (don't want it mixing with init.el)
(use-feature custom
  :no-require t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))
  ;; while at it, treat themes as safe
  (setf custom-safe-themes t))

;; only one custom theme at a time
(comment
  (defadvice load-theme (before clear-previous-themes activate)
    "Clear existing theme settings instead of layering them"
    (mapc #'disable-theme custom-enabled-themes)))

;; start up emacs server.  see
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html#Emacs-Server
(use-feature server
  :defer 1
  :config (or (server-running-p) (server-mode)))

;;; VCS.

(straight-use-package
 '(magit :type git :host github :repo "magit/magit"))

;; Avoid a potential performance issue on macOS: https://magit.vc/manual/magit.html#MacOS-Performance.
(when (string= system-type "darwin")
  ;; (setq magit-git-executable "/usr/local/bin/git")
  (setq magit-git-executable "git"))

; (straight-use-package
; '(forge :type git :host github :repo "magit/forge" :after magit))
(straight-use-package '(forge :host github :repo "magit/forge"))

(defun nca/magit-narrow (oldfun &rest args)
  "Restrict `git diff` to files with status changes in `magit-insert-unstaged-changes'.

When using the `fsmonitor` extension, `git status` is very fast,
while `git diff` is very slow: it appears to walk the file tree
without using the `fsmonitor` cache.  This advice works around
this by first collecting files with status changes and then
restricting `git diff` to those files, which avoids walking the
file tree and can be significantly faster for large repositories."
  (let* ((get-dirty-files (lambda ()
                            (mapcar (lambda (s) (car (last (split-string s))))
                                    (apply #'magit-git-items "status" "--porcelain=2" "-z" "--" magit-buffer-diff-files))))
         (dirty-files nil))
    (if (bound-and-true-p magit-refresh-verbose)
        (progn (require 'benchmark)
               (message "  %-50s %s" "nca/magit-narrow"
                        (benchmark-elapse (setq dirty-files (funcall get-dirty-files)))))
      (setq dirty-files (funcall get-dirty-files)))
    (let ((magit-buffer-diff-files (cons ".git" dirty-files)))
      (apply oldfun args))))

(advice-add 'magit-insert-unstaged-changes :around 'nca/magit-narrow)

(straight-use-package
 '(monky :type git :host github :repo "ananthakumaran/monky"
         :fork "ncalexan/monky"))

(straight-use-package
 '(manky :type git :host github :repo "ncalexan/manky"
         :branch "manky"))

(straight-use-package
 '(searchfox :type git :host github :repo "ncalexan/searchfox.el"))

(straight-use-package
 '(mach.el :type git :host github :repo "ncalexan/mach.el"))

(add-to-list 'safe-local-eval-forms
             '(mach-mode 1))

(straight-use-package
 '(erecord-mode :type git :host github :repo "ncalexan/erecord-mode"))

;; By default monky spawns a seperate hg process for every command.
;; This will be slow if the repo contains lot of changes.
;; if `monky-process-type' is set to cmdserver then monky will spawn a single
;; cmdserver and communicate over pipe.
;; Available only on mercurial versions 1.9 or higher
(unless (string= system-type "windows-nt")
  (setq monky-hg-executable (expand-file-name "~/Devel/hg/contrib/chg/chg"))
  (setq monky-process-type nil)
  ;; (setq monky-process-type 'cmdserver)
)

(straight-use-package 'ag)
(straight-use-package 'ripgrep)
(straight-use-package 'deadgrep)
(straight-use-package 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)

(defalias 'pag #'projectile-ag)
(defalias 'prg #'projectile-ripgrep)
(defalias 'pc #'projectile-compile-project)
(defalias 'pr #'projectile-run-project)
(defalias 'ms #'projectile-vc)
(defalias 'occ #'occur)

(defalias 'dms #'magit-describe-section)
(defalias 'dtp #'describe-text-properties)

(straight-use-package 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook #'wgrep-ag-setup)

(straight-use-package 'yaml-mode)

(straight-use-package 'company)
(setq company-tooltip-align-annotations t)

(straight-use-package 'rust-mode)
(with-eval-after-load 'rust-mode
  (require 'company)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

(straight-use-package 'cargo)
(add-hook 'rust-mode-hook #'cargo-minor-mode)

(use-package racer
 :config
 (when (string= system-type "windows-nt")
   (setq racer-rust-src-path
         "/Users/nalexander/.rustup/toolchains/stable-x86_64-pc-windows-msvc/lib/rustlib/src/rust/src")))

(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; (add-hook 'rust-mode-hook #'racer-mode)

(add-hook 'cargo-process-mode-hook #'cargo-minor-mode)

(straight-use-package 'company-racer)

(straight-use-package 'toml-mode)
(defun nca-toml-mode-hook ()
  (if (equal (file-name-nondirectory buffer-file-name) "Cargo.toml")
      (cargo-minor-mode)))
(add-hook 'toml-mode-hook 'nca-toml-mode-hook)

(straight-use-package 'powerline)
(powerline-default-theme)

(straight-use-package 'faff-theme)
(load-theme 'faff t)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; See http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/
(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Don't waste screen real-estate, don't hide the dock
(tool-bar-mode -1)
(setq initial-frame-alist '((width . 140) (height . 53) (top . 22) (left . 407)))
(put 'emacs 'disabled t)
(setq split-width-threshold nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(defun fs ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'maximized))

(when (string= system-type "darwin")
  ;; Make drag and drop not open new frames.
  (setq ns-pop-up-frames nil)

  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  ;; on Mac OS X, don't use `ls --dired`
  (setq dired-use-ls-dired nil)

  ;; See https://blog.vifortech.com/posts/emacs-tls-fix/
  (require 'gnutls)
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun nca/dired-mode-hook ()
    (dired-hide-details-mode))

(use-feature dired
  :config
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target 1)
  (setq dired-hide-details-mode t)
  (setq dired-hide-details-hide-symlink-targets nil)
  :hook (dired-mode . nca/dired-mode-hook))

(straight-use-package 'whitespace)
(straight-use-package 'recentf)
(savehist-mode 1)
(recentf-mode 1)

;; Who came up with whatever was on Meta-g?
(global-set-key "\M-g" 'goto-line)

(use-package goto-line-preview
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

;; Just type y or n for yes no statements
(defalias 'yes-or-no-p 'y-or-n-p)

;; Smarter marking!
(transient-mark-mode t)

(setq vc-handled-backends nil)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Who came up with list-buffers?
(global-set-key "\C-x\C-b" 'ibuffer)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; From https://emacs.stackexchange.com/a/26514.
;; (defun add-server-postfix ()
;;   "Add the name of the connection type and server to the buffer name"
;;   (if (string-match "^/ssh:.*?:" (buffer-file-name (current-buffer)))
;;       (rename-buffer (concat (buffer-name (current-buffer)) "<" (match-string 0 (buffer-file-name (current-buffer))) ">")) nil))
;; (add-hook 'find-file-hook 'add-server-postfix)

;; Change backup behavior to save in a directory
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.backups")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(defalias 'fnd #'find-name-dired)
(defalias 'qr #'query-replace)
(defalias 'qrr #'query-replace-regexp)
(defalias 'eb #'eval-buffer)
(defalias 'er #'eval-region)
(defalias 'ir #'indent-region)

;; http://ergoemacs.org/emacs/emacs_alias.html
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)

(defun rb ()
  (interactive)
  (revert-buffer t t))

;; line endings
(defun line-endings-dos ()
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defun line-endings-unix ()
  (interactive)
  (set-buffer-file-coding-system 'unix))

(straight-use-package 'diffstat)
(defun nca-diff-mode-hook ()
  (local-set-key "\C-c\C-l" 'diffstat))
(add-hook 'diff-mode-hook 'nca-diff-mode-hook)

(straight-use-package 'markdown-mode)

(straight-use-package 'rst)
(with-eval-after-load "rst"
  '(setcdr (assq 'html rst-compile-toolsets)
           '("rst2html_open.sh" ".htm" nil)))

(straight-use-package 'json-mode)
(setq json-encoding-default-indentation "    ")

(add-to-list 'auto-mode-alist '("\\.tzst\\'" . tar-mode))

(add-to-list 'auto-mode-alist '("/\\.?mozconfig.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.build$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.mozbuild$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.configure$" . python-mode))
(add-to-list 'auto-mode-alist '("config\\.status$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

(straight-use-package 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))

(straight-use-package 'nsis-mode)

(defun nca-groovy-mode-hook ()
  (setq c-basic-offset 4)
  (setq fill-column 80))
(add-hook 'groovy-mode-hook 'nca-groovy-mode-hook)

;; From https://www.emacswiki.org/emacs/UnfillParagraph.
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(straight-use-package 'dired-subtree)

(with-eval-after-load 'dired-subtree
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; Tweaks to dired, from https://www.reddit.com/r/emacs/comments/4agkye/how_do_you_customize_dired/d1086np/
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(global-set-key (kbd "C-x C-d") #'dired-jump)
(global-set-key (kbd "C-x 4 C-d") #'dired-jump-other-window)

;; Help ediff clean up after itself
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(defun nca-ediff-cleanup-hook ()
  (ediff-janitor nil nil))
(add-hook 'ediff-cleanup-hook 'nca-ediff-cleanup-hook)

;;; From https://stackoverflow.com/a/19112313
(defun dired-ediff-marked-files ()
  "Run ediff-files on a pair of files marked in dired buffer"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (let ((file1 (nth 0 marked-files))
                 (file2 (nth 1 marked-files)))
             (if (and (file-directory-p file1)
                      (file-directory-p file2))
                 (ediff-directories file1 file2 nil)
               (ediff-files file1 file2))))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (let ((file1 (nth 0 marked-files))
                 (file2 (nth 0 other-marked-files)))
             (if (and (file-directory-p file1)
                      (file-directory-p file2))
                 (ediff-directories file1 file2 nil)
               (ediff-files file1 file2))))
          (t (error "mark exactly 2 files, at least 1 locally")))))

(with-eval-after-load 'dired
  (require 'dired-subtree)
  (require 'dired-x)
  (define-key dired-mode-map "=" 'dired-ediff-marked-files))

(use-package dired-filter)

;; (dired-async-mode 1)

;; Shift the selected region right if distance is postive, left if negative
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:
(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left]  'shift-left)

(require 'pcase)

(defun cb (name)
  (interactive "sFind name in hierarchy: ")
  (let ((regexp (pcase major-mode
                  ('python-mode (format "^\\( *\\(cp\\|c\\)?def.*%s\\|class\\|cdef class\\).*:$" name))
                  ('rust-mode (format "^\\s-*\\(\\_<pub\\((crate)\\)?\\s-*\\)?\\_<\\(fn\\|impl\\|type\\|enum\\|struct\\|use\\|const\\|static\\|trait\\)\\_>.*%s" name)))))
    (occur regexp)))

(defun ub (name)
  (interactive "sFind usages in hierarchy: ")
  (let ((regexp (pcase major-mode
                  ('python-mode (format "^\\(\\( *\\(cp\\|c\\)?def\\|class\\|cdef class\\).*:$\\)\\|.*%s.*" name))
                  ('rust-mode (format "^\\s-*\\(\\_<pub\\((crate)\\)?\\s-*\\)?\\_<\\(fn\\|impl\\|type\\|enum\\|struct\\|use\\|const\\|static\\|trait\\)\\_>.*%s" name)))))
    (occur regexp)))

(straight-use-package 'request)

(use-package realgud)
(use-package realgud-lldb)
;; To use: `(require 'realgud-lldb)' and then `realgud-lldb'.

(with-eval-after-load 'eshell
  (add-to-list
   'eshell-interpreter-alist (cons "^\\(\\./\\)?mach\\'" "/usr/local/bin/python3")))

(defun eshell/mach (&rest args)
  "Use `compile' to run mach in the background."
  (if (and ; eshell-current-subjob-p
	   (eshell-interactive-output-p))
      (let ((compilation-process-setup-function
	     (list 'lambda nil
		   (list 'setq 'process-environment
			 (list 'quote (eshell-copy-environment))))))
	(compile (concat "python3 " (or (locate-dominating-file "mach" "mach") "") "mach " (eshell-flatten-and-stringify args))))
    (throw 'eshell-replace-command
	   (eshell-parse-command "*mach" (eshell-stringify-list
					  (eshell-flatten-list args))))))

(put 'eshell/mach 'eshell-no-numeric-conversions t)

(defun narrow-to-eshell-last-output ()
  (interactive)
  (narrow-to-region eshell-last-output-start eshell-last-output-end))

(defun nca/eshell-mode-hook ()
  (when (string= system-type "windows-nt")
    (eshell/export "MOZILLABUILD=c:\\mozilla-build\\"))
  (eshell/export "INSIDE_EMACS=1")
  (eshell/export "EDITOR=emacsclient")
  (define-key eshell-mode-map (kbd "<tab>")
    (lambda () (interactive) (pcomplete-std-complete)))
  (define-key eshell-mode-map (kbd "C-x n o")
    'narrow-to-eshell-last-output))

(add-hook 'eshell-mode-hook 'nca/eshell-mode-hook)
;; (remove-hook 'eshell-mode-hook 'nca/eshell-mode-hook)

;; (use-package shell-switcher
;;   :custom
;;   (shell-switcher-ask-before-creating-new t))

(use-package shelldon
  :straight (shelldon :type git
                      :host github
                      :repo "Overdr0ne/shelldon"
                      :branch "master"
                      :files ("shelldon.el")))

(use-package package-lint)

(use-package with-editor
  ;; :demand
  :config
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

(defun with-editor-find-file-hook ()
  "Open HG editor invocations in `with-editor-mode`."
  (when (s-matches-p "^hg-editor-" (file-name-nondirectory (file-name-sans-versions (buffer-file-name))))
    (with-editor-mode 1)))

(add-hook 'find-file-hook #'with-editor-find-file-hook)

(use-package diminish)
(eval-after-load "projectile" '(diminish 'projectile-mode))

(use-package selectrum
  :ensure t
  :config
  ;; TODO: C-k to kill buffers inline.  Supposed to use embark?
  (define-key selectrum-minibuffer-map (kbd "C-s") #'selectrum-next-candidate)
  (define-key selectrum-minibuffer-map (kbd "C-r") #'selectrum-previous-candidate))

(use-package prescient
  :ensure t
  :config
  ;; Save command history to disk, so that sorting gets more intelligent over time.
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :ensure t)

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

;; (use-package ivy
;;   :diminish
;;   ivy-mode
;;   :init
;;   (setq ivy-use-virtual-buffers t)
;;   (ivy-mode 1)
;;   :config
;;   (setq ivy-wrap t)
;;   (setq ivy-fixed-height-minibuffer t)
;;   (define-key ivy-minibuffer-map (kbd "C-s") 'ivy-next-line)
;;   (define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line)
;;   (setq ivy-re-builders-alist
;;         '((t . ivy--regex-ignore-order)))
;;   ;; Order buffers by recency: see https://github.com/abo-abo/swiper/issues/1344.
;;   (setq ivy-sort-matches-functions-alist '((t . nil)))
;;   (setq projectile-completion-system 'ivy)
;;   (setq magit-completing-read-function 'ivy-completing-read))

(use-package dumb-jump
  :init
  (progn
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-force-searcher 'rg)
    (dumb-jump-mode 1))
  :bind
  (:map dumb-jump-mode-map
        ("M-." . dumb-jump-go)))

(setq xref-prompt-for-identifier
      '(not
        xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame
        xref-find-references))

;; From https://github.com/joaotavora/eglot/issues/129#issuecomment-444130367:
;; Bridge projectile and project together so packages that depend on project
;; work (like xref).
(defun nca/projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'nca/projectile-project-find-function))

(use-package counsel
  :diminish
  counsel-mode
  :ensure t
  :config
  (counsel-mode 1)
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "M-s s") 'counsel-grep-or-swiper)
  (global-set-key (kbd "M-s r") 'counsel-grep-or-swiper-backward))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))


;; Noodling around searchfox "flags/toggles".

;; (defun foo () (insert "ABCDE
;; TEST"))

;; ;; (add-hook 'minibuffer-setup-hook 'foo)

;; (minibuffer-with-setup-hook
;;     'foo
;;   (ivy-completing-read "prompt" ()))

;; ;;  (call-interactively #'ivy-completing-read))

;; (defun my-read-mb-lines (prompt some-keyseq)
;;   (let ((keymap (copy-keymap minibuffer-local-map)))
;;     (define-key keymap (kbd "C-j") 'newline)
;;     (define-key keymap some-keyseq 'exit-minibuffer)
;;     (read-from-minibuffer prompt nil keymap)))

;; ;; Calling example:

;; (my-read-mb-lines "Insert text (C-s to submit): " (kbd "RET"))

;; Mostly for a good default value for `occur`.
(use-package replace+
  :demand)

;; Helpers for browsing Bugzilla and quickly googling things.
(defun nca/bugzilla-url-at-point ()
  (or (thing-at-point 'url t)
      (let ((f (thing-at-point 'filename t)))
        (and f (concat "https://bugzilla.mozilla.org/show_bug.cgi?id=" f)))))

(defun nca/browse-url-bugzilla ()
  (interactive)
  (cl-letf (((symbol-function 'browse-url-url-at-point) #'nca/bugzilla-url-at-point))
    (call-interactively 'browse-url)))

(defalias 'bz 'nca/browse-url-bugzilla)

(defun nca/google-url-at-point ()
  (or (thing-at-point 'url t)
      (let ((f (thing-at-point 'filename t)))
        (and f (concat "https://www.google.com/search?q=" f)))))

(defun nca/browse-url-google (&rest args)
  (interactive)
  (cl-letf (((symbol-function 'browse-url-url-at-point) #'nca/google-url-at-point))
    (call-interactively 'browse-url)))

(defalias 'g 'nca/browse-url-google)

(defalias 'lp 'list-processes)

;; (straight-use-package
;;  '(tramp :type git :host  :repo "ncalexan/searchfox.el"))

(use-package tramp
  :init
  ;; Use the settings in ~/.ssh/config instead of Tramp's settings.  This can
  ;; allow to reuse a single ssh connection.
  (setq tramp-use-ssh-controlmaster-options nil)

  :config
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (progn
    (setq tramp-connection-properties nil)
    (add-to-list 'tramp-connection-properties
                 (list (regexp-quote "/ssh:nanger:")
                       "direct-async-process" t))
    (add-to-list 'tramp-connection-properties
                 (list (regexp-quote "/ssh:dama:")
                       "login-args" '(("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h")
                                      ("c:/mozilla-build/start-shell.bat")
                                      ;; MOZILLABUILD=C:\\\\\\\\mozilla-build c:/mozilla-build/msys/bin/bash.exe --login -i
                                      ;; ("c:/mozilla-build/msys/bin/bash.exe" "--login" "-i")
                                      )
                       "direct-async-process" t
                       ))
    ;; (clrhash tramp-cache-data)
    ;; (tramp-dump-connection-properties)
    ;; (tramp-cleanup-all-connections)
    (setq tramp-verbose 10))

  (connection-local-set-profile-variables
   'nanger
   '((shell-file-name . "/bin/bash")
     (tramp-remote-path . ("~/.cargo/bin" tramp-default-remote-path))
     (magit-git-executable . "git")
     (monky-hg-executable . "/usr/bin/hg")))
  (connection-local-set-profiles
   '(:machine "nanger")
   'nanger)
  ;; (connection-local-set-profile-variables
  ;;  'dama
  ;;  '((tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h") ("c:/mozilla-build/start-shell.bat")))
  ;;    (monky-hg-executable . "/c/mozilla-build/python/Scripts/hg")))
  ;; (connection-local-set-profiles
  ;;  '(:machine "dama")
  ;;  'dama)
  )

;; (unless (string= system-type "windows-nt")
;;   (use-package tramp
;;     :ensure t
;;     :init
;;     (setq tramp-default-method "scp")
;;     :config
;;     (setq tramp-verbose 6)
;;     (add-to-list 'tramp-remote-path 'tramp-own-remote-path t)

;;     (connection-local-set-profile-variables
;;      'remote-bash
;;      '((shell-file-name . "/bin/bash")
;;        (shell-command-switch . "-c")
;;        (shell-interactive-switch . "-i")
;;        (shell-login-switch . "-l")))

;;     (connection-local-set-profile-variables
;;      'remote-null-device
;;      '((null-device . "/dev/null")))

;;     (connection-local-set-profiles
;;      '(:machine "weirdo")
;;      'remote-bash 'remote-null-device)))

;; From https://www.reddit.com/r/emacs/comments/h138pp/what_is_the_best_method_you_have_found_for/ftqz8l3/.
(defun nca/add-point-to-find-tag-marker-ring (&rest r)
  "Handy advising function to use find-tag-marker-ring (R ignored)."
  (ring-insert find-tag-marker-ring (point-marker)))

(advice-add 'find-function :before 'nca/add-point-to-find-tag-marker-ring)
(advice-add 'describe-function :before 'nca/add-point-to-find-tag-marker-ring)
(advice-add 'counsel-imenu :before 'nca/add-point-to-find-tag-marker-ring)
(advice-add 'projectile-grep :before 'nca/add-point-to-find-tag-marker-ring)
(advice-add 'projectile-ag :before 'nca/add-point-to-find-tag-marker-ring)

;; From https://github.com/DamienCassou/emacs.d/blob/56f7f8e71258a48e6201090299aa95f38b8266fb/init.el#L1018-L1030.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)  ;; Maybe too painful in `python-mode`?
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; From https://stackoverflow.com/a/62282209.
(use-package ansi-color
  :ensure t
  :config
  (defun nca/colorize-compilation-buffer ()
    (when (derived-mode-p 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . nca/colorize-compilation-buffer))

(straight-use-package
 '(cram-test-mode :type git :host github :repo "macmodrov/cram-test-mode"))

(use-package elisp-lint)

(straight-use-package
 '(powershell :type git :host github :repo "jschaf/powershell.el"))

(use-package gh)

(straight-use-package
 '(gist.el :type git :host github :repo "defunkt/gist.el"))

(use-package dtrt-indent
  :config
  (dtrt-indent-global-mode)
  :diminish)

;; Try to handle Mozilla's `.jsonlz4` format: doesn't work because format is non-standard.  Leaving this here to
;; remember how to bump jka-compr next time.
(add-to-list 'jka-compr-compression-info-list
             ["\\.jsonlz4\\'"
              "Lz4 compressing"     "lz4"         ("-c" "-q")
              "Lz4 uncompressing"   "lz4"         ("-c" "-q" "-d")
              t t "LZ4"]
             )
(jka-compr-update)

(use-package json-snatcher)

(defvar xpcshell-file-path "/Users/nalexander/Mozilla/objdirs/objdir-browser-compile/dist/bin/xpcshell"
  "Path to the program used by `run-xpcshell'")

(defvar xpcshell-arguments '()
  "Commandline arguments to pass to `xpcshell'")

(defvar xpcshell-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; ;; example definition
    ;; (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-xpcshell'")

(defvar xpcshell-prompt-regexp "^\\(?:js> *\\)"
  "Prompt for `run-xpcshell'.")

(defun run-xpcshell ()
  "Run an inferior instance of `xpcshell' inside Emacs."
  (interactive)
  (let* ((xpcshell-program xpcshell-file-path)
         (buffer (comint-check-proc "xpcshell")))
    ;; pop to the "*xpcshell*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'xpcshell-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*xpcshell*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "xpcshell" buffer
             xpcshell-program xpcshell-arguments)
      (xpcshell-mode))))

(defun xpcshell--initialize ()
  "Helper function to initialize xpcshell"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode xpcshell-mode comint-mode "xpcshell"
  "Major mode for `run-xpcshell'.

\\<xpcshell-mode-map>"
  nil "xpcshell"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp xpcshell-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  ;; (set (make-local-variable 'font-lock-defaults) '(xpcshell-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) xpcshell-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'xpcshell-mode-hook 'xpcshell--initialize)

(straight-use-package
 '(cucumber.el :type git :host github :repo "michaelklishin/cucumber.el"))

(use-package restclient)

;; (defun nca/tramp-insertion-filter (proc string)
;;   (when (buffer-live-p (process-buffer proc))
;;     (with-current-buffer (process-buffer proc)
;;       (let ((moving (= (point) (process-mark proc))))
;;         (save-excursion
;;           ;; Insert the text, advancing the process marker.
;;           (goto-char (process-mark proc))
;;           (insert string)
;;           (message "nca/tramp 1 %S %S" (process-mark proc) (point))
;;           (let ((end-marker (copy-marker (point)))
;;                 ;; (ansi-color-context-region nil)
;;                 )
;;             (ansi-color-filter-region (point-min) end-marker)
;;             (message "nca/tramp 2 %S %S" (process-mark proc) end-marker)
;;             (goto-char (process-mark proc))
;;             (save-match-data
;;               (while (re-search-forward "$" end-marker t)
;;                 (replace-match "" nil nil)))
;;             (goto-char end-marker))
;;           (set-marker (process-mark proc) (point)))
;;         (if moving (goto-char (process-mark proc)))))))

;; (eval-after-load 'tramp-sh
;;   (progn
;;     (defadvice tramp-open-connection-setup-interactive-shell
;;         (before nca/tramp-open-connection-setup-interactive-shell activate)
;;       "Add process-sentinel to tramp-shells. Kill buffer when process died."
;;       (set-process-filter
;; 	   ;; Arg 0 is proc
;; 	   (ad-get-arg 0)
;;        #'nca/tramp-insertion-filter))))

;; (defun nca/tramp-open-connection-setup-interactive-shell (proc vec)
;;   ;; (message "nca/tramp old process-coding-system is %S" (process-coding-system proc))
;;   ;; (set-process-coding-system proc 'dos 'utf-8-unix)
;;   ;; (message "nca/tramp new process-coding-system is %S" (process-coding-system proc))
;;   (setq ansi-color-context-region nil)
;;   (set-process-filter
;;    proc
;;    #'nca/tramp-insertion-filter))

;; (advice-add 'tramp-open-connection-setup-interactive-shell
;;             :before #'nca/tramp-open-connection-setup-interactive-shell)

;; (advice-remove 'tramp-open-connection-setup-interactive-shell
;;                #'nca/tramp-open-connection-setup-interactive-shell)


;; (setq tramp-methods (assoc-delete-all "sshw" tramp-methods #'equal))

;; (with-eval-after-load 'tramp
;;   (require 'tramp)
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path t)
;;   (add-to-list
;;    'tramp-methods
;;    '("sshw"
;;      (tramp-login-program "ssh")
;;      (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h") ("c:/mozilla-build/start-shell.bat")))
;;      (tramp-async-args (("-q")))
;;      (tramp-direct-async t)
;;      (tramp-remote-shell "/bin/sh")
;;      (tramp-remote-shell-login ("-l"))
;;      (tramp-remote-shell-args ("-c")))))

;; (alist-get "sshw" tramp-methods nil nil #'equal)

;; ((tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-direct-async t) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))

(straight-use-package
 '(kotlin-mode :type git :host github :repo "Emacs-Kotlin-Mode-Maintainers/kotlin-mode"))

(straight-use-package
 '(swift-mode :type git :host github :repo "swift-emacs/swift-mode"))

(straight-use-package 'ztree)

(straight-use-package
 '(flash-region :type git :host github :repo "Fuco1/flash-region"))

(cl-defun magit-describe-section-tree (section &optional (indent 0))
  (message "%d %s" indent (magit-describe-section-briefly section))
  (--each
      (oref section children)
    (magit-describe-section-tree it (1+ indent))))

;; From https://gist.github.com/cpbotha/05e07dee7fd8243ba73339be186c0b88
;; https://emacs.stackexchange.com/a/3843/8743 original code
;; cpbotha.net made small improvements to ergonomics

;; cpbotha changes:
;; - by default extract files WITHOUT their relative directories into DIR,
;;   because that's what I expect in OFMs.
(defun archive-extract-to-file (archive-name item-name command dir keep-relpath)
  "Extract ITEM-NAME from ARCHIVE-NAME using COMMAND. Save to
DIR. If KEEP-RELPATH, extract with relative path otherwise don't."
  (unwind-protect
      (let* ((file-name (if keep-relpath
                            ;; remove the leading / from the file name to force
                            ;; expand-file-name to interpret its path as relative to dir
                            (if (string-match "\\`/" item-name)
                                (substring item-name 1)
                              item-name)
                          ;; by default just strip the path completely
                          (file-name-nondirectory item-name)))
             (output-file (expand-file-name file-name dir))
             (output-dir (file-name-directory output-file)))
        ;; create the output directory (and its parents) if it does
        ;; not exist yet
        (unless (file-directory-p output-dir)
          (make-directory output-dir t))
        ;; execute COMMAND, redirecting output to output-file
        (apply #'call-process
               (car command)            ;program
               nil                      ;infile
               `(:file ,output-file)    ;destination
               nil                      ;display
               (append (cdr command) (list archive-name item-name))))
    ;; FIXME: add unwind forms
    nil))

;; cpbotha changes:
;; - extract to OTHER dired pane, OR to directory containing archive if there
;;   is no other dired pane
(defun archive-extract-marked-to-file (keep-relpath)
  "Extract marked archive items to OUTPUT-DIR. If KEEP-RELPATH is non-nil
   or prefix-arg (C-u) is set, keep relative paths of files in archive,
   otherwise don't."
  (interactive "P")
  (let ((output-dir (or (dired-dwim-target-directory) default-directory))
        (command (symbol-value (archive-name "extract")))
        (archive (buffer-file-name))
        (items (archive-get-marked ?* t))) ; get marked items; t means
                                        ; get item under point if
                                        ; nothing is marked
    (mapc
     (lambda (item)
       (archive-extract-to-file archive
                                ;; get the name from the descriptor
                                (archive--file-desc-ext-file-name item)
                                command output-dir keep-relpath))
     items)))

;; (provide 'archive-extract-to-file)
