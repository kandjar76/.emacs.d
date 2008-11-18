;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Keyboard Customization                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [(control right)]         	'forward-word)
(define-key global-map [(control left)]          	'backward-word)
(define-key global-map [(control prior)]               	'beginning-of-buffer)
(define-key global-map [(control next)]                	'end-of-buffer)
(define-key global-map [(control meta g)]        	'goto-line)
(define-key global-map [(insert)]                	'overwrite-mode)

(define-key global-map [(control insert)]               'copy-region-as-kill) ; kill-ring-save -- will add a delay to show the "copied" area
(define-key global-map [(control delete)]        	'kill-region)
(define-key global-map [(shift insert)]                 'yank)
(define-key global-map [(shift delete)]          	'yank-pop)

(define-key global-map [(control f)]             	'list-matching-lines)

(define-key global-map [(control f3)]			'search-current-word-forward)
(define-key global-map [(control shift f3)]		'search-current-word-backward)
(define-key global-map [(f3)]				'repeat-search-current-word-forward)
(define-key global-map [(shift f3)]			'repeat-search-current-word-backward)

(define-key global-map [(control ?`)]			'highlight-current-word)

(define-key global-map [(meta left)]                    'backward-sexp)
(define-key global-map [(meta right)]                   'forward-sexp)

(define-key global-map [(mouse-1)]               	'mouse-track)
(define-key global-map [(mouse-2)]               	nil)
(define-key global-map [(mouse-3)]               	nil) ; 'popup-mode-menu)

(define-key global-map [(control ?c) (?r)]		'revert-buffer-now)
(define-key global-map [(control ?x) (?k)]		'kill-buffer-now)
