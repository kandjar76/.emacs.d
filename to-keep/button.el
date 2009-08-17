;; How to insert a link:

(defun myfun (button)
  (message (format "Button [%s]" (button-label button))))

(define-button-type 'my-button2
  'action 'myfun
  'follow-link t
  'help-echo "Click button")

(insert-text-button "xyz" :type 'my-button2)


;; Creation of a button without underline?

(insert (propertize "test" 'mouse-face 'highlight
		    'help-echo "mouse-2: visit this file in other window"))

;; Check project-buffer-mode! :)