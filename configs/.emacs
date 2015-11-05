(add-to-list 'load-path "~/.emacs.d/")

(set-language-environment "utf-8")

(setq load-path (cons "/usr/lib64/erlang/lib/tools-2.8.1/emacs" load-path))
(setq erlang-root-dir "/usr/lib64/erlang")
(setq exec-path (cons "/usr/lib64/erlang/bin" exec-path))
(require 'erlang-start)

(require 'cua-base)
(cua-mode t)

(tool-bar-mode nil)
(show-paren-mode t)
(column-number-mode t)
(line-number-mode t)

(setq-default
 indent-tabs-mode nil
 frame-title-format
 (list
  '((buffer-file-name
     " %f"
     (dired-directory
      dired-directory
      (revert-buffer-function
       " %b"
       ("%b - Dir: " default-directory)))))))

(setq
 c-basic-offset 4
 tab-width 4
 default-tab-width 4
 inhibit-startup-message t
 delete-auto-save-files t
 delete-old-versions t
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 auto-save-mode nil
 compilation-read-command nil)

(blink-cursor-mode -1)

(global-set-key [(control home)] 'beginning-of-buffer)
(global-set-key [(control end)] 'end-of-buffer)
(global-set-key "\M-g" 'goto-line)
(global-set-key [f2] 'save-buffer)
(global-set-key [f6] 'delete-other-windows)
(global-set-key [f8] 'indent-buffer)
(global-set-key [f9] 'compile)

(defun indent-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "Droid Sans Mono")))))
