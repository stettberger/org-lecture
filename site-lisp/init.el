(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((whitespace-mode) (flyspell-mode . 1) (ospl-mode . 1))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(toggle-debug-on-error)

(message "\n\nStarting org-lecture emacs daemon\n")
(defun log (fmt &rest args)
  (apply 'message (cons (concat "[org-lecture] " fmt) args)))

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq load-dir (file-name-directory user-init-file))
(add-to-list 'load-path load-dir)
(log "lisp library: %s" load-dir)

(setq org-lecture-src-dir default-directory)
(log "source files: %s" org-lecture-src-dir)

(require 'package)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(setq package-user-dir load-dir)
(package-initialize)

(require 'org)
(require 'ob-tangle-sra)
(require 'ox-html)
(require 'htmlize)

(setq org-tangle-prologue "\\OrgLectureSectionStart{{{{property(ITEM)}}}}{{{{n(block)}}}}")
(setq org-tangle-epilogue "\\OrgLectureSectionStop{{{{property(ITEM)}}}}{{{{n(block,-)}}}}")

;; Configure org-mode
(setq org-html-klipsify-src nil)
(setq org-html-htmlize-font-prefix  "org-")
(setq org-html-htmlize-output-type 'css)
(setq org-html-head-include-scripts nil)
(setq org-html-postamble "")

(let ((custom (format "%s/custom.el" org-lecture-src-dir)))
  (when (file-exists-p custom)
    (log "Loading %s" custom)
    (load-file custom)))

(log "Emacs has started.")
