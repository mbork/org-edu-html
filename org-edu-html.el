;; Org-edu-HTML - HTML-based exporter for educational materials

(require 'ox-html)

(org-export-define-derived-backend 'edu-html 'html
  :translate-alist '((template . org-edu-html-template)
		     (plain-list . org-edu-html-plain-list)
		     (item . org-edu-html-item)
		     (underline . org-edu-html-underline))
  :options-alist '((:edu-ok-name "EDU_OK_NAME" nil "OK!" t)
		   (:edu-wrong-name "EDU_WRONG_NAME" nil "Wrong..." t)
		   (:edu-check-name "EDU_CHECK_NAME" nil "Check" t)
		   (:edu-show-name "EDU_SHOW" nil "Show" t)
		   (:edu-hide-name "EDU_HIDE" nil "Hide" t))
  :menu-entry '(?e "Export to Edu-HTML"
		   ((?E "As HTML buffer" org-edu-html-export-as-edu-html)
		    (?e "As HTML file" org-edu-html-export-to-edu-html)
		    (?o "As HTML file and open"
			(lambda (a s v b)
			  (if a (org-edu-html-export-to-edu-html t s v b)
			    (org-open-file (org-edu-html-export-to-edu-html nil s v b))))))))

(defun org-edu-html-export-as-edu-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as Edu-HTML.  The function is modeled
after org-latex-export-as-latex."
  (interactive)
  (org-export-to-buffer 'edu-html "*Org Edu-HTML Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))

(defun org-edu-html-export-to-edu-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Edu-HTML file.  The function is
modeled after org-latex-export-as-latex."
  (interactive)
  (let ((outfile (org-export-output-file-name ".html" subtreep)))
    (org-export-to-file 'edu-html outfile
      async subtreep visible-only body-only ext-plist)))

(defvar org-edu-html-jquery-address "./jquery-2.1.1.min.js"
  "Where to get jQuery from.")

(defvar org-edu-html-stylesheet "./org-edu-html-default.css"
  "The CSS stylesheet.")

(defun org-edu-html-build-jquery-config ()
  (concat
   (format "<script src=\"%s\"></script>\n" org-edu-html-jquery-address)
   "<script src=\"./org-edu-html.js\"></script>\n"))

(defun org-edu-html-build-postamble (info)
  (format "<div id=\"postamble\">
  <p>Created with Org-Edu-HTML (c) 2014-2015 Marcin `mbork' Borkowski</p>
  <p class=\"author\">Author: %s (%s)</p>
  <p class=\"date\">Date: %s</p>
</div>"
	  (org-element-interpret-data (plist-get info :author))
	  (org-element-interpret-data (plist-get info :email))
	  (org-element-interpret-data (plist-get info :date))))

(defun org-edu-html-encode-plain-text-to-js-string (text)
  "Convert TEXT to something suitable for JS string by using
first org-html-encode-plain-text and then escaping quotes and
ticks."
  (mapc
   (lambda (pair)
     (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
   '(("\\\\" . "\\\\") ("'" . "\\'") ("\"" . "\\\""))) ; the first one is funny, isn't it?
  (org-html-encode-plain-text text))

(defun org-edu-html-template (contents info)
  (concat
   "<!DOCTYPE html>\n"
   "<html>\n"
   "<head>\n"
   (org-html--build-meta-info info)	;!!
   (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" org-edu-html-stylesheet "\">")
;   (org-html--build-head info)		;!!
   (org-html--build-mathjax-config info) ;!!
   (format
    (mapconcat #'identity
	       '("<script>"
		 "var okName='%s';"
		 "var wrongName='%s';"
		 "var checkName='%s';"
		 "var showName='%s';"
		 "var hideName='%s';"
		 "</script>")
	       "\n")
    (org-edu-html-encode-plain-text-to-js-string (plist-get info :edu-ok-name))
    (org-edu-html-encode-plain-text-to-js-string (plist-get info :edu-wrong-name))
    (org-edu-html-encode-plain-text-to-js-string (plist-get info :edu-check-name))
    (org-edu-html-encode-plain-text-to-js-string (plist-get info :edu-show-name))
    (org-edu-html-encode-plain-text-to-js-string (plist-get info :edu-hide-name)))
   "\n"
   (org-edu-html-build-jquery-config)
   "</head>\n"
   "<body>\n"
   ;; Global div.
   (format "<%s id=\"%s\">\n"
	   (nth 1 (assq 'content org-html-divs))
	   (nth 2 (assq 'content org-html-divs)))
   ;; Title (may be superfluous)
   (let ((title (plist-get info :title)))
     (format "<h1 class=\"title\">%s</h1>\n"
	     (org-export-data (or title "") info)))
   contents
   ;; Closing the global div.
   (format "</%s>\n"
	   (nth 1 (assq 'content org-html-divs)))
   ;; Postamble (TODO: should probably be changed)
   (org-edu-html-build-postamble info)
   ;; Closing document.
   "</body>\n</html>\n"))

(defun org-edu-html-plain-list (plain-list contents info)
  (cond
   ((string= (org-export-read-attribute :attr_edu plain-list :test) "mct")
    (org-edu-html-mct-test plain-list contents info))
   ((string= (org-export-read-attribute :attr_edu plain-list :test) "sct")
    (org-edu-html-mct-test plain-list contents info t))
   ((string= (org-export-read-attribute :attr_edu plain-list :test) "select")
    (org-edu-html-select-test plain-list contents info))
   (t (org-html-plain-list plain-list contents info))))

(defun org-edu-html-mct-test (plain-list contents info &optional sct)
  (format "<form>\n<fieldset class=\"%s\">\n%s\n</fieldset>\n</form>"
   (if sct "sct" "mct")
   contents))

(defun org-edu-html-select-test (plain-list contents info)
  (format "<form>\n<select class=\"sct-sel\">\n%s</select>\n</form>" contents))

(defun org-edu-html-item (item contents info)
  ;; (org-html-item (item contents info)))
  (cond
   ((string=
     (org-export-read-attribute :attr_edu (org-element-property :parent item) :test) "mct")
    (org-edu-html-mct-item item contents info))
   ((string=
     (org-export-read-attribute :attr_edu (org-element-property :parent item) :test) "sct")
    (org-edu-html-mct-item item contents info t))
   ((string=
     (org-export-read-attribute :attr_edu (org-element-property :parent item) :test) "select")
    (org-edu-html-select-item item contents info))
   (t (org-html-item item contents info))))

(defun right-answer-code ()
  "Value of the `value' attribute of a checkbox of the right answer."
  "1")

(defun wrong-answer-code ()
  "Value of the `value' attribute of a checkbox of the wrong answer."
  "0")

(defun org-edu-html-mct-item (item contents info &optional sct)
  (let* ((state (org-element-property :checkbox item))
	 (name (number-to-string (org-export-get-ordinal
				  (org-element-property :parent item)
				  info
				  '(plain-list))))
	 (id (concat name (format "%s" (org-export-get-ordinal item info)))))
    (format
     "<div><input type=\"%s\" name=\"%s\" id=\"%s\" value=\"%s\">\n<span class=\"label\">%s</span></div>"
     (if sct "radio" "checkbox")
     name
     id
     (if (eql state 'on) (right-answer-code) (wrong-answer-code))
     contents)))

(defun org-edu-html-select-item (item contents info)
  (let* ((state (org-element-property :checkbox item))
	 (name (number-to-string (org-export-get-ordinal
				  (org-element-property :parent item)
				  info
				  '(plain-list))))
	 (id (concat name (format "%s" (org-export-get-ordinal item info)))))
    (format "<option value=\"%s\">%s</option>"
	    (if (string= state "on") 1 0)
	    (if (string-match "\n$" contents)
		(substring contents 0 -1)
	      contents))))

(defun org-edu-html-check-for-block-type (element block-type)
  "Check whether ELEMENT is inside a BLOCK-TYPE (which is a
string) special block."
  (let ((parent (org-export-get-parent element)))
    (cond ((and (eq (car parent) 'special-block)
		(string= (org-element-property :type parent) block-type))
	   t)
	  (parent
	   (org-edu-html-check-for-block-type parent block-type)))))

(defun org-edu-html-underline (underline contents info)
  "This (ab)uses underline to make cloze tests.  If the underline
is anywhere within a CLOZE special block, it will be turned into
a cloze."
  (if (org-edu-html-check-for-block-type underline "CLOZE")
      (org-edu-html-cloze underline contents info)
    (org-html-underline underline contents info)))

(defun org-edu-html-cloze (cloze contents info)
  (format "<input type=\"text\" name=\"%s\" correct=\"%s\">"
	  (org-export-get-ordinal (org-export-get-parent (org-export-get-parent cloze))
				  info
				  '(special-block)
				  (lambda (el info)
				    (string= (org-element-property :type el) "CLOZE")))
	  contents))
