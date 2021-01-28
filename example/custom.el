(log "Custom Initialization")

(setq org-html-postamble
      (with-temp-buffer
        (insert-file-contents
         (format "%s/footer.html" org-lecture-src-dir))
        (buffer-string)))

