; require package system
(require 'package)

; add repos
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)

; load installed packages
(package-initialize)

; require use-package to declare dependencies
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

; automatically install missing packages
(setq use-package-always-ensure t)

; use evil
(use-package evil)
(evil-mode 1)

; disable startup message
(setq inhibit-startup-message t)

; set fontsize to 10
(set-face-attribute 'default nil :height 100)

; show linenumbers
(global-linum-mode t)

; disable backup files
(setq backup-inhibited t)

; disable auto-save
(setq auto-save-default nil)
