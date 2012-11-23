;;; chicken-scheme.el --- Scheme-mode extensions for Chicken Scheme

;; Copyright 2012 Daniel Leslie
;; Author: Daniel Leslie dan@ironoxide.ca
;; URL: http://github.com/dleslie/chicken-scheme
;; Version: 1.0.0

;; Licensed under the GPL3
;; A copy of the license can be found at the above URL

;;; Commentary:
;; A suite of extensions for scheme-mode that grew out of necessity.
;;
;; Chicken Scheme does play well with SLIME (See also: chicken-slime.el), but
;; I often find myself working on software that is slow-level and unstable
;; enough to make such dependence on REPL reliability rather frustrating.
;;
;; Thus chicken-scheme.el was born. It does not rely on a running Scheme to
;; provide auto-complete support for your application. A suite of customization
;; variables are available to configure from which modules symbols should be
;; loaded and what sort of package prefixes can be expected.
;;
;; Auto-complete is configured to support prefixed symbols, to allow for
;; full recognition of symbols in modules that may have been imported with a
;; prefix modifier. The `chicken-prefix` variable may be customized to declare
;; what characters can be used as prefix delimiters.
;;
;; C-? is bound in Scheme Modes to fetch documentation for the symbol at the
;; current point. This obeys the prefix rules used for auto-complete.
;;
;; Further customization is available in the chicken-scheme customization group.
;;
;; Loading of the first scheme file may take some time as the Chicken Modules
;; are parsed for symbols on first-load. All subsequent scheme files do not
;; incur this load hitch. Consider running an Emacs daemon.
;;
;; Tags are also supported. 
;;
;; Installation:
;; Place in your load path. Add the following to your .emacs:
;;
;; (require 'chicken-scheme)
;;
;; I recommend you also add the following:
;;
;; (add-hook 'scheme-mode-hook 'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode-enable)

(require 'auto-complete)
(require 'scheme)

;;; Code:

(defun chicken-installed-modules ()
  "Use chicken-status to discover all installed Chicken modules."
  (interactive "r")
  (let ((default-directory "~/")
        modules)
    (with-temp-buffer
      (insert (shell-command-to-string "chicken-status -files"))
      (beginning-of-buffer)
      (while (re-search-forward "/\\([^/\.]+\\)\\.so" nil t)
        (when (match-string 0)
          (if (and (not (equalp "chicken-doc" (match-string 1))) ; Doesn't play well with csi in emacs?
                   (not (equalp "chicken-doc-text" (match-string 1)))
                   (not (equalp "bind-translator" (match-string 1))))
              (push (match-string 1) modules)))))
    modules))

(defgroup chicken-scheme
  nil "Chicken Scheme Extensions")

(defcustom chicken-scheme-tags-file ()
  "Extra tags file to load for pattern matching and syntax hilighting."
  :type 'file
  :group 'chicken-scheme)

(defcustom chicken-ac-modules (chicken-installed-modules)
  "Modules to load symbols from for `auto-complete'."
  :type '(repeat string)
  :group 'chicken-scheme)

(defcustom chicken-prefix
  ;"[^:#]*[:#]\\(.*\\)"
  ":#"
  "Defines the characters to use to identify the prefix separator that may be present for autocomplete matches.  Defaults to : and #."
  :type 'string
  :group 'chicken-scheme)

(defface ac-chicken-scheme-candidate-face
  '((t (:inherit 'ac-candidate-face)))
  "Face for chicken scheme candidate menu."
  :group 'chicken-scheme)

(defface ac-chicken-scheme-selection-face
  '((t (:inherit 'ac-selection-face)))
  "Face for the chicken scheme selected candidate."
  :group 'chicken-scheme)

;; Hardcoded r5rs-symbols
(defvar r5rs-symbols '(abs acos and angle append apply asin assoc assq assv atan begin boolean? caar cadr call-with-current-continuation call-with-input-file call-with-output-file call-with-values car case cdddar cddddr cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric? char-ready? char-upcase char-upper-case? char-whitespace? char<=? char<? char=? char>=? char>? char? close-input-port close-output-port complex? cond cons cos current-input-port current-output-port define define-syntax delay denominator display do dynamic-wind else eof-object? eq? equal? eqv? eval even? exact->inexact exact? exp expt floor for-each force gcd if imag-part inexact->exact inexact? input-port? integer->char integer? interaction-environment lambda lcm length let let* let-syntax letrec letrec-syntax list list->string list->vector list-ref list-tail list? load log magnitude make-polar make-rectangular make-string make-vector map max member memq memv min modulo negative? newline not null-environment null? number->string number? numerator odd? open-input-file open-output-file or output-port? pair? peek-char port? positive? procedure? quasiquote quote quotient rational? rationalize read read-char real-part real? remainder reverse round scheme-report-environment set! set-car! set-cdr! setcar sin sqrt string string->list string->number string->symbol string-append string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? string-copy string-fill! string-length string-ref string-set! string<=? string<? string=? string>=? string>? string? substring symbol->string symbol? syntax-rules tan transcript-off transcript-on truncate values vector vector->list vector-fill! vector-length vector-ref vector-set! vector? with-input-from-file with-output-to-file write write-char zero?))

(defun chicken-load-tags (scheme-tags-location)
  "Load tag file entries into the tag table and inject them into font-lock.
Argument SCHEME-TAGS-LOCATION The tags file from which to extract the tags."
  (interactive)
  (let ((existing-tags tags-table-list))
    (setq tags-table-list nil)
    (visit-tags-table scheme-tags-location)
    (tags-completion-table)
    (setq tags-table-list existing-tags))
  t)

(defun chicken-load-symbols (module-list)
  "Load symbols from Chicken.
Argument MODULE-LIST The modules to extract symbols from."
  (let ((symbols))
    (dolist (module module-list)
      (let* ((output (shell-command-to-string (format "csi -q -w -e \"(use %s)\" -e \"(display (map car (##sys#macro-environment)))\" -e \"(display (##sys#environment-symbols (interaction-environment)))\"" module)))
             (cleaned (replace-regexp-in-string "[^ ]*[\]\[#.\(\)]+[^ ]*" "" output)))
        (setq symbols  (concat cleaned " " symbols))
        (message (format "Retrieved symbols from Chicken Module %s" module))))
    (delete-dups (eval (read (concat "'(" symbols ")"))))))

(defvar ac-chicken-symbols-candidates-cache nil)
(defun ac-chicken-symbols-candidates ()
  "Use `chicken-ac-modules' to generate `auto-complete' candidates."
  (if (or (equalp nil ac-chicken-symbols-candidates-cache)
          (not (equalp chicken-ac-modules (car ac-chicken-symbols-candidates-cache))))
      (setq ac-chicken-symbols-candidates-cache
            `(,chicken-ac-modules 
              . ,(delq nil
                       (mapcar '(lambda (s)
                                  (condition-case err
                                      (let ((n (symbol-name s)))
                                        (cons n n))
                                    (wrong-type-argument '())))
                               (chicken-load-symbols chicken-ac-modules))))))
  (cdr ac-chicken-symbols-candidates-cache))

(defun ac-chicken-doc (symbol-name)
  "Use chicken-doc to recover documentation for a given symbol.
Argument SYMBOL-NAME The symbol to recover documentation for."
  (shell-command-to-string (format "chicken-doc %s" (substring-no-properties symbol-name))))

(defconst chicken-scheme-font-lock-keywords '() 
   "Extended highlighting for Scheme modes using Chicken keywords.")

(defun chicken-load-font-lock-keywords ()
  "Load chicken keywords into font-lock."
  (interactive)
  (setq font-lock-defaults 
         `((chicken-scheme-font-lock-keywords) 
           nil ; don't do strings and comments
           nil ; don't do case sensitive
           ((,(replace-regexp-in-string (concat "[" chicken-prefix "]") "a" "+-*/.<>=!?$%_&~^:") . "w")) 
           beginning-of-defun 
           (font-lock-mark-block-function . mark-defun)
           ))
  (if (equalp nil chicken-scheme-font-lock-keywords)
      (chicken-cache-font-lock-keywords)))

(defun chicken-cache-font-lock-keywords ()
  "Cache font-lock keywords for Chicken."
  (message "Caching Chicken font-lock-keywords")
  (setq chicken-scheme-font-lock-keywords
        (append '()
         scheme-font-lock-keywords-1
         scheme-font-lock-keywords-2
         (eval-when-compile
           (let* ((kw (sort (mapcar (lambda (p) (car p)) (ac-chicken-symbols-candidates)) 'string<))
                  (nkw (length kw))
                  (step 100))
             (loop
              with result = '()
              for ptr from 0 by step
              while (< ptr nkw)
              do
              (let ((window (last (butlast kw (- nkw (+ ptr step))) step)))
                (setq result (append result 
                                     (list `(,(regexp-opt window 'words)  (1 font-lock-builtin-face))))))
              finally
              return result))))))

(defvar ac-source-chicken-symbols
  '((candidates . ac-chicken-symbols-candidates)
    (candidate-face . ac-chicken-scheme-candidate-face)
    (selection-face . ac-chicken-scheme-selection-face)
    (symbol . "f")
    (requires . 2)
    (document . ac-chicken-doc)
    (cache)))

(defvar ac-source-chicken-symbols-prefixed
  `((candidates . ac-chicken-symbols-candidates)
    (candidate-face . ac-chicken-scheme-candidate-face)
    (selection-face . ac-chicken-scheme-selection-face)
    (symbol . "f")
    (requires . 2)
    (document . ac-chicken-doc)
    (prefix . ,(concat "[^" chicken-prefix "]*[" chicken-prefix "]\\(.*\\)"))
    (cache)))

(defun chicken-scheme-hook ()
  "Hook for Chicken into scheme-mode."
  (interactive)
  (font-lock-mode)
  (chicken-load-font-lock-keywords)
  (font-lock-refresh-defaults)
  (if chicken-scheme-tags-file
      (chicken-load-tags scheme-tags-file))
  (make-local-variable 'ac-sources)
  (setq ac-sources
        (append ac-sources '(ac-source-chicken-symbols
                             ac-source-chicken-symbols-prefixed
                             ac-source-words-in-buffer)))
  (message "Chicken Scheme ready."))

(defun chicken-show-help ()
  "Show documentation for the symbol at the present point."
  (interactive)
  (message (ac-chicken-doc 
            (replace-regexp-in-string 
             (concat "[^" chicken-prefix "]*[" chicken-prefix "]+") 
             "" 
             (symbol-name (symbol-at-point))))))

(define-key scheme-mode-map (kbd "C-?") 'chicken-show-help)

(add-hook 'scheme-mode-hook 'chicken-scheme-hook)

(provide 'chicken-scheme)

;;; chicken-scheme.el ends here
