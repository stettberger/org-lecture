

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

(setq org-sra-src-dir default-directory)
(log "source files: %s" org-sra-src-dir)

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

;; Configure org-mode
(setq org-html-klipsify-src nil)
(setq org-html-htmlize-font-prefix  "org-")
(setq org-html-htmlize-output-type 'css)
(setq org-html-postamble
      (with-temp-buffer
        (insert-file-contents
         (format "%s/COPYING.footer.html" org-sra-src-dir))
        (buffer-string)))

(message "  ... emacs is started.")
