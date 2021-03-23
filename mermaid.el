;; mermaid mode
(require 'compile)

(defvar mermaid-mode-hook nil
  "initial hook for mermaid mode"
  )

(defun mermaid-compilation-mode-hook ()
  "Hook function to set local value for `compilation-error-screen-columns'."
  ;; In Emacs > 20.7 compilation-error-screen-columns is buffer local.
  (or (assq 'compilation-error-screen-columns (buffer-local-variables))
      (make-local-variable 'compilation-error-screen-columns))
  (setq compilation-error-screen-columns nil))

(defvar mermaid-output-format 'png
  "The format of generated file")

(defvar mermaid-verbose nil
  "Show verbose information when run compiler")

(defvar mermaid-compiler nil
  "The compiler used to generate output")

(defvar mermaid-mode-map
  (let ((map (make-sparse-keymap)))
    ;; add key bindings for mermaid mode here
    ;;
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\C-c\C-c" 'mermaid-compile)
    (define-key map "\C-c\C-v" 'mermaid-view)
    map)
  "Keymap for mermaid mode")

(defconst mermaid-font-lock-keywords-1
  '(("^[ \t]*\\(graph\\|subgraph\\|end\\|loop\\|alt\\|gantt\\|title\\|section\\|dateFormat\\|sequenceDiagram\\|opt\\|participant\\|note\\|else\\|gitGraph\\|options\\)" . font-lock-keyword-face)
    ("^[ \t]*graph[ \t]+\\(TD|\\TB\\|BT\\RL\\|LR\\)" . font-lock-keyword-face)
    ("%%\\(.*$\\)" . font-lock-comment-face)
    ("{\\(.*\\)}" . font-lock-string-face)
    (":\\([^%\012]*\\)[^%\012]*$" . font-lock-warning-face)
    )
  "keyword in mermaid mode"
  )

(defconst mermaid-new-scope-regexp
  "^[ \t]*\\(loop\\|opt\\|subgraph\\|graph\\|sequenceDiagram\\|gantt\\|gitGraph\\|{\\)\\([ \t]*\\|$\\)"
  "keyword to start a new scope(indent level)")

(defconst mermaid-end-scope-regexp
  "^[ \t]*\\(end\\|}\\)\\([ \t]*\\|$\\)"
  "keyword for end a scope(maybe also start a new scope)")

(defconst mermaid-section-regexp
  "^[ \t]*\\(section\\)[ \t]+"
  "section keyword")

(defconst mermaid-else-regexp
  "^[ \t]*\\(else\\)"
  "else keyword")

(defconst mermaid-alt-regexp
  "^[ \t]*\\(alt\\)"
  "alt keyword")

(defun mermaid-output-ext ()
  "get the extendsion of generated file"
  (if (eq mermaid-output-format 'svg)
      ".svg"
    ".png"))

(defun mk-mermaid-command (file-name)
  (let ((cmd (format "mermaid %s %s %s"
		     (if (eq mermaid-output-format 'svg)
			 "--svg"
		       "")
		     (if mermaid-verbose
			 "--verbose"
		       "")
		     file-name)))
    cmd))

(defun mk-mmdc-command (file-name)
  (let ((cmd (format "mmdc -i %s -o %s"
		     file-name
		     (format "%s.%s"
			     file-name
			     mermaid-output-format))))
    cmd))

(defun get-mermaid-compiler ()
  (unless mermaid-compiler
    (setq mermaid-compiler
	  (let ((mermaid (executable-find "mermaid")))
	    (if mermaid
		"mermaid"
	      (if (executable-find "mmdc")
		  "mmdc"
		"/bin/touch"))))))

(defun run-compiler (compiler)
  (let ((cmd (pcase compiler
	       ('mermaid (mk-mermaid-command (buffer-file-name)))
	       ('mmdc (mk-mmdc-command (buffer-file-name))))))
    (let ((buffer-name "*mermaid compilation")
	  (compilation-mode-hook (cons 'mermaid-compilation-mode-hook
				       compilation-mode-hook)))
      (compilation-start cmd nil
			 #'(lambda (mode-name)
			     buffer-name)))))

;;;###autoload
(defun mermaid-compile ()
  (interactive)
  (let ((compiler (get-mermaid-compiler)))
    (cond
     ((string= compiler "mermaid")
      (run-compiler 'mermaid))
     ((string= compiler "mmdc")
      (run-compiler 'mmdc))
     (t (message "unknown mermaid compiler %s"
		 compiler)))))

;;;###autoload
(defun mermaid-view ()
  (interactive)
  (let ((dst-file-name (concat (buffer-file-name)
                               (mermaid-output-ext))))
    (if (file-exists-p dst-file-name)
        (find-file-other-window dst-file-name)
      (error "Please compile the it first!\n"))))

;; disable debug in default
;;
(defvar mermaid-debug-enabled nil
  "enable/disable debug")

(defmacro mermaid-debug (fmt &rest args)
  `(when mermaid-debug-enabled
     (message ,fmt ,@args)))

(defun mermaid-indent-line ()
  "indent current line in mermaid mode"
  (interactive)
  (mermaid-debug "line no @ %d\n" (line-number-at-pos))
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let (cur-indent)
      (cond
       ((looking-at mermaid-end-scope-regexp)
        (progn
          (mermaid-debug "found end scope\n")
          (save-excursion
            (forward-line -1)
            (if (or (looking-at mermaid-new-scope-regexp)
                    (looking-at mermaid-alt-regexp))
                (setq cur-indent (current-indentation))
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))))
       ((looking-at mermaid-section-regexp)
        (let ((found-section nil)
              (need-search t))
          (mermaid-debug "found section\n")
          (save-excursion
            (while need-search
              (forward-line -1)
              (cond
               ((looking-at mermaid-section-regexp)
                (progn
                  (mermaid-debug "found section\n")
                  (setq found-section t)
                  (setq cur-indent (current-indentation))
                  (mermaid-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
               ((or (looking-at mermaid-new-scope-regexp)
                    (looking-at mermaid-alt-regexp))
                (progn
                  (mermaid-debug "found new scope\n")
                  (setq cur-indent (+ (current-indentation) tab-width))
                  (mermaid-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
               ((looking-at mermaid-end-scope-regexp)
                (progn
                  (mermaid-debug "found end scope\n")
                  (setq cur-indent (current-indentation))
                  (mermaid-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
               ((bobp)
                (progn
                  (setq cur-indent 0)
                  (setq need-search nil)))
               (t t))))
          (if (< cur-indent 0)
              (setq cur-indent 0))))
       ((looking-at mermaid-else-regexp)
        (let ((need-search t))
          (mermaid-debug "else\n")
          (save-excursion
            (while need-search
              (forward-line -1)
              (cond
               ((or (looking-at mermaid-else-regexp)
                    (looking-at mermaid-alt-regexp))
                (progn
                  (mermaid-debug "found matched alt/else\n")
                  (setq cur-indent (current-indentation))
                  (mermaid-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
               ((looking-at mermaid-end-scope-regexp)
                (progn
                  (mermaid-debug "found end\n")
                  (setq cur-indent (- (current-indentation) tab-width))
                  (setq need-search nil)))
               ((bobp)
                (progn
                  (setq cur-indent 0)))
               (t t))))))
       (t
        (let ((need-search t)
              (start-scope (looking-at mermaid-new-scope-regexp)))
          (mermaid-debug "normal indent\n")
          (save-excursion
            (while need-search
              (forward-line -1)
              (cond
               ((looking-at mermaid-end-scope-regexp)
                (progn
                  (mermaid-debug "found end scope\n")
                  (setq cur-indent (current-indentation))
                  (mermaid-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
                ((or (looking-at mermaid-new-scope-regexp)
                     (looking-at mermaid-alt-regexp))
                 (progn
                   (mermaid-debug "found begin scope\n")
                   (setq cur-indent (+ (current-indentation) tab-width))
                   (mermaid-debug "cur-indent %d\n" cur-indent)
                   (setq need-search nil)))
                ((looking-at mermaid-section-regexp)
                 (progn
                   (mermaid-debug "found section \n")
                   (if start-scope
                       (setq cur-indent (current-indentation))
                     (setq cur-indent (+ (current-indentation) tab-width)))
                   (mermaid-debug "cur-indent %d\n" cur-indent)
                   (setq need-search nil)))
                ((bobp)
                 (progn
                   (setq cur-indent 0)
                   (setq need-search nil)))))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;;;###autoload
(defun mermaid-mode ()
  "Major mode for editing mermaid scripts"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mermaid-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(mermaid-font-lock-keywords-1))
  (set (make-local-variable 'indent-line-function)
       'mermaid-indent-line)
  (set (make-local-variable 'comment-start) "%")
  (set (make-local-variable 'comment-end) "")
  (setq major-mode 'mermaid-mode)
  (setq mode-name "mermaid")
  (run-hooks 'mermaid-mode-hook))

(provide 'mermaid-mode)
