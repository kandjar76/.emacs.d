(set-window-margins (get-buffer-window (current-buffer)) 7)
(add-text-properties 1 2 (list 'display
			       (list
				(list (list 'margin 'left-margin)
				      "ok") )))
(add-text-properties 2 3 '(display
		((margin left-margin) "HI!")))

;; Adding the margin text:

(put-text-property 1 2 'display
		'((margin left-margin) "HI!"))
(overlay-put (make-overlay 2 3 (current-buffer)) 'before-string "s")

;; Removed it:
(remove-text-properties 1 2 '(display))
(delete-overlay ...)

====

;; SOLUTION:
(setq left-margin-width 5)

(setq o (make-overlay 2 3 (current-buffer)))
(overlay-put o
	     'before-string (propertize "x" 'display
					'((margin left-margin) "HI!")))
(delete-overlay o)



(setq left-margin-width 3)

;;********************************************************************************
;;********************************************************************************


(setq header-line-format header) ;; Set the header of the window...
(setq header-line-format
      (concat (propertize " " 
			  'display 
			  '(space :align-to 0))
	      "Very interesting"))

(setq header-line-format
      (concat (propertize " " 
			  'display 
			  '(space :align-to 0))
	      "Value of blah"
	      (propertize " "
			  'display
			  '(space :align-to 30))
	      "ok at column 30"
	      ))

;; Turn whitespace chars in the header into stretch specs so
;; they work regardless of the header-line face.
(while (string-match "[ \t\n]+" header pos)
  (setq pos (match-end 0))
  (put-text-property (match-beginning 0) pos 'display
		     ;; Assume fixed-size chars in the buffer.
		     (list 'space :align-to pos)
		     header))


;;********************************************************************************
;;********************************************************************************


(remove-text-properties  1 10 (list 'display))

(add-text-properties 1 2 (list 'display
			       (list
				(list (list 'margin 'left-margin)
				 codestack-entry-image) "test"
				)))

(defimage codestack-entry-image ((:type xpm :ascent center :data "/* XPM */
static char * sb_tag+_xpm[] = {
\"20 13 4 1\",
\"      c None\",
\".     c #828282\",
\"+     c #000000\",
\"@     c #FFF993\",
\"    ............... \",
\"   .+++++++++++++++.\",
\"  .+@@@@@@@@@@@@@@+.\",
\" .+@@@@@@++@@@@@@@+.\",
\".+@@@@@@@++@@@@@@@+.\",
\".+@++@@++++++@@@@@+.\",
\".+@++@@++++++@@@@@+.\",
\".+@@@@@@@++@@@@@@@+.\",
\" .+@@@@@@++@@@@@@@+.\",
\"  .+@@@@@@@@@@@@@@+.\",
\".  .+++++++++++++++.\",
\"    ............... \",
\"                    \"};"))
  "Image used for the display of a codestack entry.")




(defimage codestack-entry-image ((:type xpm :ascent center :data "/* XPM */
static char * sb_tag+_xpm[] = {
\"20 12 4 1\",
\"      c None\",
\".     c #828282\",
\"+     c #000000\",
\"@     c #FFF993\",
\"                                                          \",
\"                                                          \",
\"                                                          \",
\"   @@@@@@@    @@@@@    @@@@@      @@@@@                   \",
\"      @      @     @   @    @    @     @                  \",
\"      @      @     @   @     @   @     @                  \",
\"      @      @     @   @     @   @     @                  \",
\"      @      @     @   @    @    @     @                  \",
\"      @       @@@@@    @@@@@      @@@@@                   \",
\"                                                          \",
\"                                                          \",
\"                                                          \",
\"                                                          \"};"))
  "Image used for the display of a codestack entry.")


(insert-image codestack-entry-image)


;; 12 12 4        1
;; wd hg nb_color char-per-pixel

(defimage ione((:type xpm :ascent center :data "/* XPM */
static char * sb_tag+_xpm[] = {
\"12 12 4 1\",
\"      c None\",
\".     c #828282\",
\"+     c #000000\",
\"@     c #FFF993\",
\"     ..     \",
\"   ..@@.    \",
\" ...@@@.    \",
\" .@@@@@.    \",
\".@@..@@.    \",
\" ....@@.    \",
\"    .@@.    \",
\"    .@@.    \",
\" ....@@.... \",
\".@@@@@@@@@@.\",
\" .......... \",
\"            \"};"))
  "Test.")

(insert-image ione)
(clear-image-cache)

(defimage itst((:type xpm :ascent center :data "/* XPM */
static char * sb_tag+_xpm[] = {
\"12 12 4 1\",
\"      c None\",
\".     c #828282\",
\"*     c #000000\",
\"@     c #FFF993\",
\"************\",
\"*....@@....*\",
\"*.. @@@....*\",
\"*..@@@@....*\",
\"*.@@.@@....*\",
\"*....@@....*\",
\"*....@@....*\",
\"*....@@....*\",
\"*....@@....*\",
\"*.@@@@@@@@.*\",
\"************\",
\"            \"};")))
(insert-image itst)



(defimage t10 ((:type xpm :ascent center :data "/* XPM */
static char * sb_tag+_xpm[] = {
\"26 12 4 1\",
\"      c None\",
\".     c #FF0000\",
\"+     c #000000\",
\"@     c #424242\",
\"                          \",
\"                          \",
\"..........................\",
\".                        .\",
\". @@@@@  @@  @@@    @@   .\",
\".   @   @  @ @  @  @  @  .\",
\".   @   @  @ @   @ @  @  .\",
\".   @   @  @ @   @ @  @  .\",
\".   @   @  @ @  @  @  @  .\",
\".   @    @@  @@@    @@   .\",
\".                        .\",
\"..........................\",
\"                          \"};"))
  "Image used for the display of a codestack entry.")

(insert-image t9)
                 TODO