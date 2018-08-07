;; mermaid mode

(defvar mermaid-mode-hook nil
  "initial hook for mermaid mode"
  )

(defun mermaid-compilation-mode-hook ()
  "Hook function to set local value for `compilation-error-screen-columns'."
  ;; In Emacs > 20.7 compilation-error-screen-columns is buffer local.
  (or (assq 'compilation-error-screen-columns (buffer-local-variables))
      (make-local-variable 'compilation-error-screen-columns))
  (setq compilation-error-screen-columns nil))

(defvar puppeteer-config-file "c:/Users/Administrator/AppData/Roaming/.emacs.d/mermaid/puppeteer-config.json"
  "The puppeteer config file to make it work with Chromimum")

(defvar mermaid-output-format 'svg
  "The format of generated file")

(defvar mermaid-verbose nil
  "Show verbose information when run compiler")

(defvar mermaid-compiler "mmdc"
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
  (if (eq mermaid-output-format 'png)
      ".png"
    ".svg"))

;;;###autoload
(defun mermaid-compile ()
  (interactive)
  (let ((cmd (concat mermaid-compiler
                     (if mermaid-verbose
                         " --verbose "
                       " ")
                     " -p "
                     puppeteer-config-file
                     " -i "
                     (buffer-file-name)
                     " -o "
                     (buffer-file-name)
                     (mermaid-output-ext)))
        (buf-name "*mermaid compilation")
        (compilation-mode-hook (cons 'mermaid-compilation-mode-hook compilation-mode-hook)))
    (if (fboundp 'compilation-start)
        (compilation-start cmd nil
                           #'(lambda (mode-name)
                               buf-name))
      (compile-internal cmd "No more errors" buf-name))))

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
              (setq cur-indent (- (current-indentation) default-tab-width)))
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
                  (setq cur-indent (+ (current-indentation) default-tab-width))
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
                  (setq cur-indent (- (current-indentation) default-tab-width))
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
                   (setq cur-indent (+ (current-indentation) default-tab-width))
                   (mermaid-debug "cur-indent %d\n" cur-indent)
                   (setq need-search nil)))
                ((looking-at mermaid-section-regexp)
                 (progn
                   (mermaid-debug "found section \n")
                   (if start-scope
                       (setq cur-indent (current-indentation))
                     (setq cur-indent (+ (current-indentation) default-tab-width)))
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
  (run-hooks 'mermaid-mode-hook)
  )

(provide 'mermaid-mode)
