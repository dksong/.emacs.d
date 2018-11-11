(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Go
(require 'go-mode)

;; CMake
(autoload 'cmake-mode "cmake-mode" nil t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(setq markdown-command "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments")

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(defun linux-c-mode ()
  (interactive)
  (c-mode)
  (setq indent-tabs-mode t)
  (c-set-style "linux-tabs-only"))

;(add-to-list 'auto-mode-alist '("\.c$" . linux-c-mode))

(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default fill-column 80)
(setq column-number-mode t)
(setenv "MANWIDTH" "72")
(global-linum-mode 1)
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(require 'auto-complete)
(global-auto-complete-mode t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)
(set-terminal-parameter nil 'background-mode 'dark)
(set-frame-parameter nil 'background-mode 'light)
(setq solarized-high-contrast-mode-line t)
(enable-theme 'solarized)
(set-frame-font "Menlo 14" nil t)
;(add-to-list 'default-frame-alist '(height . 78))
;(add-to-list 'default-frame-alist '(left . 10))
;(add-to-list 'default-frame-alist '(width . 150))

(require 'xcscope)
(setq cscope-option-do-not-update-database t)
(setq cscope-index-recursively t)
(cscope-setup)

(require 'multiple-cursors)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-x ,") 'previous-multiframe-window)

(require 'llvm-mode)
(require 'tablegen-mode)

(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp")
(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       "~/.emacs.d/site-lisp/magit/Documentation/"))

(require 'virtualenvwrapper)

(custom-set-faces
 '(magit-diff-added ((t (:background "black" :foreground "green3"))))
 '(magit-diff-added-highlight ((t (:background "black" :foreground "green3"))))
 '(magit-diff-removed ((t (:background "black" :foreground "red3"))))
 '(magit-diff-removed-highlight ((t (:background "black" :foreground "red3")))))

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

;; I don't use the default binding of 'C-x 5', so use toggle-frame-split instead
(global-set-key (kbd "C-x 5") 'toggle-frame-split)

(setq TeX-PDF-mode t)
(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/Library/Tex/texbin"))
  (setq TeX-view-program-selection
  '((output-dvi "DVI Viewer")
    (output-pdf "PDF Viewer")
    (output-html "HTML Viewer")))
  (setq TeX-view-program-list
  '(("DVI Viewer" "open %o")
    ("PDF Viewer" "open -a TeXShop %o")
    ("HTML Viewer" "open %o"))))

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t) t))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "Make")))

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
