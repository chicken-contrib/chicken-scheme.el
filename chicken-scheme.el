;;; chicken-scheme.el --- Scheme-mode extensions for Chicken Scheme

;; Copyright 2013 Daniel Leslie
;; Author: Daniel Leslie <dan@ironoxide.ca>
;; URL: http://github.com/dleslie/chicken-scheme
;; Version: 1.1.0

;; Licensed under the GPL3
;; A copy of the license can be found at the above URL

;;; Contributors:
;; Dan Leslie
;; Mao Junhua - Disk Caching

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
;; Two Auto-Complete sources are available:
;;   ac-source-chicken-symbols
;;   ac-source-chicken-symbols-prefixed
;;
;; Prefixed symbols are those which have been mutated after importing a library.
;; See the chicken-prefix custom variable for customization options.
;;
;; I recommend you also add the following:
;;
;; (add-hook 'scheme-mode-hook 'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode-enable)


(require 'scheme)

;;; Code:

(defun chicken-dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (chicken-dump-vars varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun chicken-dump-vars (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))
               
(defun chicken-remove-error-module (module-list)    
;   (setq del-list '("library" "foreign" "iup" "iup-glcanvas" "iup-pplot" "iup-dialogs" "iup-controls" "iup-base" "canvas-draw-iup" "canvas-draw-gl"))
    (setq del-list '("library" "foreign"))
;   (setq del-list '())
    (dolist (del-elem del-list)
      (delete del-elem module-list)))

(defun chicken-installed-modules ()
  "Use chicken-status to discover all installed Chicken modules."
  (interactive "r")
  (let ((default-directory "~/")
        (modules '("srfi-1" "srfi-4" "srfi-13" "srfi-14" "srfi-18" "srfi-69" "lolevel" "tcp" "ports" "extras" "data-structures" "files" "foreign" "irregex" "library" "posix" "utils")))
    (with-temp-buffer
      (insert (shell-command-to-string "chicken-status -files"))
      (beginning-of-buffer)
      (while (re-search-forward "/\\([^/\.]+\\)\\.so" nil t)
        (when (match-string 0)
          (if (and (not (equal "chicken-doc" (match-string 1))) ; Doesn't play well with csi in emacs?
                   (not (equal "chicken-doc-text" (match-string 1)))
                   (not (equal "bind-translator" (match-string 1))))
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
    ;(tags-completion-table)
    (setq tags-table-list existing-tags))
  t)

(defun chicken-load-symbols (module-list)
  "Load symbols from Chicken.
Argument MODULE-LIST The modules to extract symbols from."
  (let ((symbols))
    (chicken-remove-error-module module-list)
    (if (file-exists-p "~/.emacs.d/.Dans-chicken-scheme-symbols-dump.el")
      (progn
          (message "~/.emacs.d/.Dans-chicken-scheme-symbols-dump.el exist, load it.")
          (load "~/.emacs.d/.Dans-chicken-scheme-symbols-dump.el"))
      (dolist (module module-list)
        (let* ((output (shell-command-to-string (format "csi -q -w -e \"(use %s)(display (map car (##sys#macro-environment)))(display (map car (##sys#current-environment)))\"" module)))
               (cleaned (replace-regexp-in-string "[^ ]*[\]\[#.\(\),'`<>:]+[^ ]*" "" output)))
          (setq symbols (concat cleaned " " symbols))
          (message (format "Retrieved symbols from Chicken Module %s" module))))
      (chicken-dump-vars-to-file '(symbols) "~/.emacs.d/.Dans-chicken-scheme-symbols-dump.el"))
    (delete-dups (eval (read (concat "'(" symbols ")"))))))

(defvar ac-chicken-symbols-candidates-cache '())
(defun ac-chicken-symbols-candidates ()
  "Use `chicken-ac-modules' to generate `auto-complete' candidates."
  (if (or (equal nil ac-chicken-symbols-candidates-cache)
          (not (equal chicken-ac-modules (car ac-chicken-symbols-candidates-cache))))
      (setq ac-chicken-symbols-candidates-cache
            `(,chicken-ac-modules 
              . ,(delq nil
                       (mapcar #'(lambda (s)
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
  (if (equal nil chicken-scheme-font-lock-keywords)
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
    (prefix . ,(concat "[^ \t\r\n" chicken-prefix "]*[" chicken-prefix "]\\(.*\\)"))
    (cache)))

(defun chicken-scheme-hook ()
  "Hook for Chicken into scheme-mode."
  (interactive)
  (font-lock-mode)
  (chicken-load-font-lock-keywords)
  (font-lock-refresh-defaults)
  (if chicken-scheme-tags-file
      (chicken-load-tags chicken-scheme-tags-file))
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
