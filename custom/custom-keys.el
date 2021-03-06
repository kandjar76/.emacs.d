;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Keyboard Customization                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note:
;;  So far, f5, f6, f8, f9 are available to be map (f6->other window/never used)


(define-key global-map [(control right)]         	'forward-word)
(define-key global-map [(control left)]          	'backward-word)
(define-key global-map [(control prior)]               	'beginning-of-buffer)
(define-key global-map [(control next)]                	'end-of-buffer)
(define-key global-map [(control meta g)]        	'goto-line)
(define-key global-map [(control backspace)]            'backward-delete-word)
(define-key global-map [(control tab)]                  'dabbrev-expand)

(define-key global-map [(control f)]             	'list-matching-lines)
(define-key global-map [(control meta f)]             	'ag-occur)
(define-key global-map [(control ?`)]			'highlight-current-word)

(define-key global-map [(meta left)]                    'backward-sexp)
(define-key global-map [(meta right)]                   'forward-sexp)

(define-key global-map [(control ?c) (control ?r)]	'revert-buffer-now)
(define-key global-map [(control ?x) (?k)]		'kill-buffer-now)
(define-key global-map [(control ?x) (control ?d)]	'dired)

(define-key global-map [(control ?x) (?+)]	        'increment-numbers-region)
(define-key global-map [(control ?z)]                   'repeat)

(define-key global-map [(control +)]                    'redo)
(define-key global-map [(control f7)]                   'compile)

(define-key global-map [(insert)]                       nil)
(define-key global-map [deletechar]                     'delete-char)


;; Disable mouse-2 default behavior which is "kill" or "yank" something like that...
(define-key global-map [(mouse-2)]               	nil)
;;(define-key global-map [(drag-mouse-1)]                 nil)

;; Electric split window / buffer window:
(define-key global-map [(control ?x)(?2)]               'electric-split-window-vertically)
(define-key global-map [(control ?x)(?3)]               'electric-split-window-horizontally)

;; Copy/Cut/Paste keys:
(define-key global-map [(control insert)]               'copy-region-as-kill) ; kill-ring-save -- will add a delay to show the "copied" area
(define-key global-map [(control delete)]        	'kill-selected-region)
(define-key global-map [(shift insert)]                 'yank)
(define-key global-map [(shift delete)]          	'yank-pop)

;; Quick-Research keys:
(define-key global-map [(control f3)]			'quick-search-forward)
(define-key global-map [(f3)]				'repeat-quick-search-forward)
(define-key global-map [(control shift f3)]		'quick-search-backward)
(define-key global-map [(shift f3)]			'repeat-quick-search-backward)

;; Cscope keys:
(define-key global-map [(f10)]                          'cscope-find-this-symbol-no-prompting)
(define-key global-map [(f12)]                          'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f9)]                   'cscope-find-this-text-string)
(define-key global-map [(control f10)]                  'cscope-find-this-symbol)
(define-key global-map [(control f11)]                  'cscope-find-functions-calling-this-function)
(define-key global-map [(control f12)]                  'cscope-find-global-definition)
(define-key global-map [(control shift f9)]             'cscope-find-this-text-string-no-updates)
(define-key global-map [(control shift f10)]            'cscope-find-this-symbol-no-updates)
(define-key global-map [(control shift f11)]            'cscope-find-functions-calling-this-function-no-updates)
(define-key global-map [(control shift f12)]            'cscope-find-global-definition-no-updates)
(define-key global-map [(control ?*)]                   'cscope-pop-mark)

;; lisp-interaction-mode:
(define-key lisp-interaction-mode-map [(control c) ?(] 'elisp-surround-with-paren)
(define-key emacs-lisp-mode-map       [(control c) ?(] 'elisp-surround-with-paren)
(define-key lisp-interaction-mode-map [(control c) (control a)] 'align-let)
(define-key emacs-lisp-mode-map       [(control c) (control a)] 'align-let)


;; Dired+ keys:
(add-hook 'dired-mode-hook
	  '(lambda ()
	     (define-key dired-mode-map [?S] 'dired-mark-files-containing-regexp)
	     (define-key dired-mode-map [?s] 'dired-mark-files-regexp)
	     (define-key dired-mode-map [?K] 'diredp-omit-unmarked)
	     (define-key dired-mode-map [?I] 'dired-kill-subdir)
	     (define-key dired-mode-map [(control up)] 'dired-tree-up)
	     (define-key dired-mode-map [(control down)] 'dired-tree-down)
	     (define-key dired-mode-map [(control left)] 'dired-hide-subdir)
	     ;(define-key dired-mode-map [return] 'dired-find-alternate-file)
	     ;(define-key dired-mode-map [(control return)] 'dired-find-file)
	     (define-key dired-mode-map [backspace] 'dired-up-directory)
	     (define-key dired-mode-map [(control ?=)] 'diredp-ediff)
	     (define-key dired-mode-map [?=] 'dired-ediff-marked-files)
	     (define-key dired-mode-map [?n] 'dired-next-marked-file)
	     (define-key dired-mode-map [?p] 'dired-prev-marked-file)
	     ;;(define-key dired-mode-map [?/] 'dired-isearch-forward)
	     ;;(define-key dired-mode-map [?n] 'dired-isearch-forward-regexp)
	     ;;(define-key dired-mode-map [?p] 'dired-isearch-backward-regexp)
	     ))

;; Buffer-menu keys:
(define-key Buffer-menu-mode-map [?R] 'Buffer-menu-mark-every-files-to-revert)
(define-key Buffer-menu-mode-map [?S] 'Buffer-menu-mark-every-files-to-save)
(define-key Buffer-menu-mode-map [?D] 'Buffer-menu-mark-every-files-to-delete)
(define-key Buffer-menu-mode-map [?r] 'Buffer-menu-mark-file-to-revert)
(define-key Buffer-menu-mode-map [?=] 'Buffer-menu-diff-buffer-with-file)

;; project-buffer-mode / iproject Keys:
(define-key global-map [(control x) (?p) (?n)] 'iproject-new)
(define-key global-map [(control x) (?p) (?f)] 'project-buffer-find-file)


;; Spu keys:
(eval-after-load "spu-mode"
    '(progn
       (define-key spu-mode-map [(control c) ?h] 'spu-highlight-registers-mode)
       (define-key spu-mode-map [(control c) ?j] 'spu-highlight-latency-mode)
       (define-key spu-mode-map [(control c) ?s] 'spu-stall-check)
       (define-key spu-mode-map [(control c) ?n] 'spu-nopify)
       (define-key spu-mode-map [(control c) ?l] 'spu-highlight-loop-mode)
       (define-key spu-mode-map [(f1)] 'spu-helper)
       ))

;; Rectangle keys:
(define-key global-map [(control ?x) (?r) (?i)]	'increase-numbers-on-rectangle)
(define-key global-map [(control ?x) (?r) (?s)] 'replace-regexp-rectangle)
(define-key global-map [(control ?x) (?r) (?r)] 'replace-string-rectangle)

;; Scroll other window:
(define-key global-map [(meta down)] '(lambda(arg) (interactive "p") (scroll-other-window arg)))
(define-key global-map [(meta up)]   '(lambda(arg) (interactive "p") (scroll-other-window-down arg)))

;; Show trailing spaces: (keep or not...???)
(define-key global-map [(control ?x) (?t)] '(lambda(arg) (interactive "p") (setq show-trailing-whitespace (not show-trailing-whitespace))))


;; In emacs 23, c^spc will activate the transient mode... :(
(define-key global-map [(control ? )]   '(lambda(arg) (interactive "P") (set-mark-command arg) (deactivate-mark)))

;; Diff mode extra key *for read-only diff buffer*:
(eval-after-load "diff-mode"
  '(progn
     (define-key diff-mode-shared-map [?a] 'diff-apply-hunk)
     (define-key diff-mode-shared-map [?s] 'diff-split-hunk)
     (define-key diff-mode-shared-map [?t] 'diff-test-hunk)))

;; Git mapping:
(define-key global-map [(control ?x) (?g) (?=)] 'git-diff-current-buffer)
(define-key global-map [(control ?x) (?g) (?l)] 'git-log-current-buffer)
(define-key global-map [(control ?x) (?g) (?s)] 'git-status)
(eval-after-load "git"
  '(progn
     (define-key git-status-mode-map [(control ?=)] 'git-diff-current-file)))

;; Org mode:
(define-key global-map [(control ?c) (?a)] 'org-agenda)
(define-key global-map [(control ?c) (?r)] 'org-remember)
(eval-after-load "org"
  '(progn
     (define-key org-mode-map [(control ?f)] 'org-occur)
     ))

;; Full screen mode:
(define-key global-map [(meta return)] 'toggle-fullscreen)


;; Key Translation Table:
;; so that alt-key works partially under Prompt on iPad.

;(define-key key-translation-map [?\C-h] [?\C-?]) ; Unmask 'delete' as backspace
(let ((translations '(    229 [?\M-a]  nil [?\M-b]   231 [?\M-c]  8706 [?\M-d]   nil [?\M-e]
			  402 [?\M-f]  169 [?\M-g]   729 [?\M-h]   nil [?\M-i]  8710 [?\M-j]
			  730 [?\M-k]  172 [?\M-l]   181 [?\M-m]   nil [?\M-n]   248 [?\M-o]
			  960 [?\M-p]  339 [?\M-q]   174 [?\M-r]   223 [?\M-s]  8224 [?\M-t]
			  nil [?\M-u] 8730 [?\M-v]  8721 [?\M-w]  8776 [?\M-x]   165 [?\M-y]
			  937 [?\M-z]
;;                         96 [?\M-~]  -- This changes ` into M-`
			  161 [?\M-1]   162 [?\M-4]   163 [?\M-3]   167 [?\M-6]
			  170 [?\M-9]  171 [?\M-\\]  175 [?\M-<]   176 [?\M-*]   177 [?\M-+]
			  182 [?\M-7]  183 [?\M-\(]  186 [?\M-0]   187 [?\M-|]   191 [?\M-\?]
			  198 [?\M-\"] 230 [?\M-']   247 [?\M-/]   728 [?\M->]  8211 [?\M-\-]
			  8212 [?\M-_] 8216 [?\M-\]] 8217 [?\M-}]  8218 [?\M-\)] 8220 [?\M-\[]
			  8221 [?\M-{] 8225 [?\M-&]  8226 [\?M-8]  8249 [?\M-#]  8250 [?\M-$]
			  8260 [?\M-!] 8364 [\?M-@]  8482 [?\M-2]  8734 [\?M-5]  8800 [?\M-=]
			  8804 [?\M-,] 8805 [?\M-.] 64257 [?\M-%] 64258 [?\M-^]
			  )))
  (while translations
    (let ((key (car translations)) (def (cadr translations)))
      (if key
	  (define-key key-translation-map (make-string 1 key) def)))
    (setq translations (cddr translations))))
