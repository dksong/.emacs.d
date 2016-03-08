(setq-default indent-tabs-mode nil)
(setq tab-width 2)
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
(add-to-list 'default-frame-alist '(height . 78))
(add-to-list 'default-frame-alist '(left . 10))
(add-to-list 'default-frame-alist '(width . 150))
(require 'xcscope)
(setq cscope-index-recursively t)
(cscope-setup)
(require 'multiple-cursors)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
