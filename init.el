;;; init.el --- nalexander's config  -*- lexical-binding: t; coding:utf-8; fill-column: 119 -*-

;; Speed up bootstrapping.  From https://github.com/yiufung/dot-emacs/blob/master/init.el.
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold 800000
                                    gc-cons-percentage 0.1)
                              (garbage-collect)) t)

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
 '(directory-abbrev-alist
   '((".*/searchfox.org/mozilla-central/source/" . (expand-file-name "~/Mozilla/gecko/"))))
 '(dired-dwim-target t)
 '(git-commit-summary-max-length 100)
 '(global-flycheck-mode t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules"))
 '(hg-binary "/Users/ncalexan/bin/hg")
 '(hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible try-expand-dabbrev try-expand-dabbrev-all-buffers try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol))
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(js2-strict-trailing-comma-warning nil)
 '(js2-strict-var-redeclaration-warning nil)
 '(monky-hg-executable "/usr/local/bin/hg")
 '(org-startup-folded nil)
 '(pipenv-executable "/usr/local/bin/pipenv")
 '(powerline-default-separator 'wave)
 '(powerline-display-buffer-size nil)
 '(powerline-height 20)
 '(projectile-enable-caching t)
 '(python-shell-interpreter "/usr/local/bin/python")
 '(racer-rust-src-path
   "/Users/nalexander/.rustup/toolchains/1.40.0-x86_64-apple-darwin/lib/rustlib/src/rust/src")
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

(if (string= system-type "windows-nt")
 (setq exec-path
   ("c:/Windows/system32" "C:/Windows" "C:/Windows/System32/Wbem" "C:/Windows/System32/WindowsPowerShell/v1.0/" "C:/Windows/System32/OpenSSH/" "C:/Program Files/dotnet/" "C:/Program Files/Microsoft SQL Server/130/Tools/Binn/" "C:/Program Files/Microsoft SQL Server/Client SDK/ODBC/170/Tools/Binn/" "C:/ProgramData/chocolatey/bin" "C:/Git/mingw64/bin" "C:/Git/cmd" "C:/Users/nalexander/.cargo/bin" "C:/Users/nalexander/AppData/Local/Microsoft/WindowsApps" "C:/LLVM/bin" "c:/Users/nalexander/AppData/Roaming/emax64/libexec/emacs/28.0.50/x86_64-w64-mingw32")))

(if (string= system-type "darwin")
 (add-to-list 'exec-path "/usr/local/bin" t))


;; straight.el for package management.
;; From https://github.com/raxod502/straight.el#getting-started.

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
  (load bootstrap-file nil 'nomessage))

;; ;; Bootstrap `use-package'
;; (setq-default use-package-always-defer t ; Always defer load package to speed up startup time
;;               use-package-verbose nil ; Don't report loading details
;;               use-package-expand-minimally t  ; make the expanded code as minimal as possible
;;               use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
;; ;; Integration with use-package
;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

;; Early load Org from Git version instead of Emacs built-in version
(straight-use-package 'org-plus-contrib)

(straight-use-package 'bind-key)

(straight-use-package 'magit)

(straight-use-package
 '(monky :type git :host github :repo "ananthakumaran/monky"
         :fork "ncalexan/monky"))

(straight-use-package
 '(searchfox :type git :host github :repo "ncalexan/searchfox.el"))

;; By default monky spawns a seperate hg process for every command.
;; This will be slow if the repo contains lot of changes.
;; if `monky-process-type' is set to cmdserver then monky will spawn a single
;; cmdserver and communicate over pipe.
;; Available only on mercurial versions 1.9 or higher
(setq monky-process-type 'cmdserver)

(straight-use-package 'ag)
(straight-use-package 'ripgrep)
(straight-use-package 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)

(defalias 'pag #'projectile-ag)
(defalias 'prg #'projectile-ripgrep)
(defalias 'pc #'projectile-compile-project)
(defalias 'pr #'projectile-run-project)
(defalias 'ms #'projectile-vc)

(straight-use-package 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook #'wgrep-ag-setup)

(straight-use-package 'yaml-mode)

(straight-use-package 'company)
(setq company-tooltip-align-annotations t)

(straight-use-package 'rust-mode)
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

(straight-use-package 'cargo)
(add-hook 'rust-mode-hook #'cargo-minor-mode)

(straight-use-package 'racer)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(add-hook 'rust-mode-hook #'racer-mode)

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

(setq dired-recursive-deletes 'top)

(straight-use-package 'whitespace)
(straight-use-package 'recentf)
(savehist-mode 1)
(recentf-mode 1)

;; Who came up with whatever was on Meta-g?
(global-set-key "\M-g" 'goto-line)

;; Just type y or n for yes no statements
(defalias 'yes-or-no-p 'y-or-n-p)

;; Smarter marking!
(transient-mark-mode t)

(setq vc-handled-backends nil)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Interactive switching, please.
(iswitchb-mode 1)
(setq iswitchb-case 1)
(setq iswitchb-delim "
")
(setq iswitchb-buffer-ignore '("^ "))
;; Who came up with list-buffers?
(global-set-key "\C-x\C-b" 'ibuffer)

(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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

(add-to-list 'auto-mode-alist '("\\.build$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.mozbuild$" . python-mode))
(add-to-list 'auto-mode-alist '("config\\.status$" . python-mode))

(straight-use-package 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))

(straight-use-package 'nsis-mode)

(defun nca-groovy-mode-hook ()
  (setq c-basic-offset 4)
  (setq fill-column 80))
(add-hook 'groovy-mode-hook 'nca-groovy-mode-hook)

(eval-after-load 'image-mode '(require 'image-dimensions-minor-mode))

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

(defun cb (name)
  (interactive "sFind name in hierarchy: ")
  (let ((regexp (case major-mode
                  (python-mode (format "^\\( *\\(cp\\|c\\)?def.*%s\\|class\\|cdef class\\).*:$" name))
                  (rust-mode (format "^\\s-*\\(\\_<pub\\((crate)\\)?\\s-*\\)?\\_<\\(fn\\|impl\\|type\\|enum\\|struct\\|use\\|const\\|static\\|trait\\)\\_>.*%s" name)))))
    (occur regexp)))

(defun ub (name)
  (interactive "sFind usages in hierarchy: ")
  (let ((regexp (case major-mode
                  (python-mode (format "^\\(\\( *\\(cp\\|c\\)?def\\|class\\|cdef class\\).*:$\\)\\|.*%s.*" name))
                  (rust-mode (format "^\\s-*\\(\\_<pub\\((crate)\\)?\\s-*\\)?\\_<\\(fn\\|impl\\|type\\|enum\\|struct\\|use\\|const\\|static\\|trait\\)\\_>.*%s" name)))))
    (occur regexp)))

(straight-use-package 'request)
