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
;; Tags are also supported. 

(require 'paredit)
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

(defcustom chicken-ac-modules '("chicken")
  "Modules to load symbols from for `auto-complete'."
  :type '(repeat string)
  :group 'chicken-scheme)

(defcustom chicken-prefix
  "[^:#]*[:#]\\(.*\\)"
  "Defines the regex to use to identify the prefix separator that may be present for autocomplete matches.  Defaults to : and #."
  :type 'regexp
  :group 'chicken-scheme)

(defface ac-chicken-scheme-candidate-face
  '((t (:inherit 'ac-candidate-face)))
  "Face for chicken scheme candidate menu."
  :group 'chicken-scheme)

(defface ac-chicken-scheme-selection-face
  '((t (:inherit 'ac-selection-face)))
  "Face for the chicken scheme selected candidate."
  :group 'chicken-scheme)

(defun add-font-lock-keywords (modes new-keywords)
  "Add provided keywords to the current font-lock set.
Argument MODES A list of modes in which to inject the keywords.
Argument NEW-KEYWORDS A list of keywords to use for font-lock."
  (mapc (lambda (mode)
          (font-lock-add-keywords mode `((, (concat "(\\(" (regexp-opt (mapcar 'symbol-name (remove-if 'numberp new-keywords)) t) "\\)\\>")
                                            (1 font-lock-keyword-face)))))
        modes)
  t)

(defun remove-font-lock-keywords (modes new-keywords)
  "Remove provided keywords from the current font-lock set.
Argument MODES A list of modes from which to remove the keywords.
Argument NEW-KEYWORDS A list of keywords to remove from font-lock."
  (mapc (lambda (mode)
          (font-lock-remove-keywords mode `((, (concat "(\\(" (regexp-opt (mapcar 'symbol-name (remove-if 'numberp new-keywords)) t) "\\)\\>")
                                               (1 font-lock-keyword-face)))))
        modes)
  t)

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
    (add-font-lock-keywords '(scheme-mode inferior-scheme-mode) tags-completion-table)
    (setq tags-table-list existing-tags))
  t)

(defun chicken-load-symbols (module-list)
  "Load symbols from Chicken.
Argument MODULE-LIST The modules to extract symbols from."
  (let ((symbols))
    (dolist (module module-list)
      (let* ((output (shell-command-to-string (format "csi -q -w -e \"(use %s)\" -e \"(display (map car (##sys#macro-environment)))\" -e \"(display (##sys#environment-symbols (interaction-environment)))\"" module)))
             (cleaned (replace-regexp-in-string "[^ ]*[\]\[#.\(\)]+[^ ]*" "" output)))
        (setq symbols  (concat cleaned " " symbols))))
    (delete-dups (eval (read (concat "'(" symbols ")"))))))

(defun ac-chicken-symbols-candidates ()
  "Use `chicken-ac-modules' to generate `auto-complete' candidates."
  (delq nil
        (mapcar '(lambda (s)
                   (condition-case err
                       (let ((n (symbol-name s)))
                         (cons n n))
                     (wrong-type-argument '())))
                (chicken-load-symbols chicken-ac-modules))))

(defun ac-chicken-doc (symbol-name)
  "Use chicken-doc to recover documentation for a given symbol.
Argument SYMBOL-NAME The symbol to recover documentation for."
  (shell-command-to-string (format "chicken-doc %s" (substring-no-properties symbol-name))))

(defun chicken-load-font-lock-keywords ()
  "Load chicken keywords into font-lock."
  (interactive "r")
  (add-font-lock-keywords '(scheme-mode) (chicken-load-symbols chicken-ac-modules)))

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
    (prefix . ,chicken-prefix)
    (cache)))

(add-hook 'scheme-mode-hook
          '(lambda ()
             (enable-paredit-mode)
             (if chicken-scheme-tags-file
                 (chicken-load-tags scheme-tags-file))
             (make-local-variable 'ac-sources)
             (setq ac-sources
                   (append ac-sources '(ac-source-chicken-symbols
                                        ac-source-chicken-symbols-prefixed
                                        ac-source-words-in-buffer
                                        ac-source-words-in-same-mode-buffers)))
             (chicken-load-font-lock-keywords)))

(provide 'chicken-scheme)

;;; chicken-scheme.el ends here
