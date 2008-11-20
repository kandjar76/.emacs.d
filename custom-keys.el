;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Keyboard Customization                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note:
;;  So far, f5 -> f9 are available to be map (f6->other window/never used)


(define-key global-map [(control right)]         	'forward-word)
(define-key global-map [(control left)]          	'backward-word)
(define-key global-map [(control prior)]               	'beginning-of-buffer)
(define-key global-map [(control next)]                	'end-of-buffer)
(define-key global-map [(control meta g)]        	'goto-line)
(define-key global-map [(insert)]                	'overwrite-mode)
(define-key global-map [(control backspace)]            'backward-delete-word)
(define-key global-map [(control tab)]                  'dabbrev-expand)

(define-key global-map [(control f)]             	'list-matching-lines)
(define-key global-map [(control ?`)]			'highlight-current-word)

(define-key global-map [(meta left)]                    'backward-sexp)
(define-key global-map [(meta right)]                   'forward-sexp)

(define-key global-map [(control ?c) (?r)]		'revert-buffer-now)
(define-key global-map [(control ?x) (?k)]		'kill-buffer-now)
(define-key global-map [(control ?x) (control ?d)]	'dired)

(define-key global-map [(control ?x) (?+)]	        'increment-numbers-region)
(define-key global-map [(control ?z)]                   nil)

(define-key global-map [(control +)]                    'redo)
(define-key global-map [(control f7)]                   'compile)

(define-key global-map [(insert)]                       nil)

;; Disable mouse-2 default behavior which is "kill" or "yank" something like that... 
(define-key global-map [(mouse-2)]               	nil)
(define-key global-map [(drag-mouse-1)]                 nil)

;; Electric split window / buffer window:
(define-key global-map [(control ?x)(?2)]               'electric-split-window-vertically)
(define-key global-map [(control ?x)(?3)]               'electric-split-window-horizontally)

;; Copy/Cut/Paste keys:
(define-key global-map [(control insert)]               'copy-region-as-kill) ; kill-ring-save -- will add a delay to show the "copied" area
(define-key global-map [(control delete)]        	'kill-selected-region)
(define-key global-map [(shift insert)]                 'yank)
(define-key global-map [(shift delete)]          	'yank-pop)

;; Bookmark keys:
(define-key global-map [(control f2)]                   'bm-toggle)
(define-key global-map [(f2)]                           'bm-next)
(define-key global-map [(shift f2)]                     'bm-previous)
(define-key global-map [(control shift f2)]             'bm-remove-all)

;; Quick-Research keys:
(define-key global-map [(control f3)]			'quick-search-forward)
(define-key global-map [(f3)]				'repeat-quick-search-forward)
(define-key global-map [(control shift f3)]		'quick-search-backward)
(define-key global-map [(shift f3)]			'repeat-quick-search-backward)

;; Cscope keys:
(define-key global-map [(f10)]                          'cscope-find-this-symbol-no-prompting-no-updates)
(define-key global-map [(f12)]                          'cscope-find-global-definition-no-prompting-no-updates)
(define-key global-map [(control f9)]                   'cscope-find-this-text-string-no-updates)
(define-key global-map [(control f10)]                  'cscope-find-this-symbol-no-updates)
(define-key global-map [(control f11)]                  'cscope-find-functions-calling-this-function-no-updates)
(define-key global-map [(control f12)]                  'cscope-find-global-definition-no-updates)
(define-key global-map [(control shift f9)]             'cscope-find-this-text-string)
(define-key global-map [(control shift f10)]            'cscope-find-this-symbol)
(define-key global-map [(control shift f11)]            'cscope-find-functions-calling-this-function)
(define-key global-map [(control shift f12)]            'cscope-find-global-definition)
(define-key global-map [(control ?*)]                   'cscope-pop-mark)

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
	     (define-key dired-mode-map [return] 'dired-find-alternate-file)
	     (define-key dired-mode-map [(control return)] 'dired-find-file)
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
(define-key Buffer-menu-mode-map [?r] 'Buffer-menu-mark-file-to-revert)
(define-key Buffer-menu-mode-map [?=] 'Buffer-menu-diff-buffer-with-file)

;; Spu keys:
(eval-after-load "spu-mode"
    '(progn
       (define-key spu-mode-map [(control c) ?h] 'spu-highlight-registers-mode)
       (define-key spu-mode-map [(control c) ?j] 'spu-highlight-latency-mode)
       (define-key spu-mode-map [(control c) ?s] 'spu-stall-check)
       (define-key spu-mode-map [(control c) ?n] 'spu-nopify)
       (define-key spu-mode-map [(control c) ?l] 'spu-highlight-loop-mode)))

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

