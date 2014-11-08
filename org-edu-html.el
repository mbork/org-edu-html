;; Org-edu-HTML - HTML-based exporter for educational materials

(require 'ox-html)

(org-export-define-derived-backend 'edu-html 'html
  :translate-alist '((template . org-edu-html-template)
		     (plain-list . org-edu-html-plain-list)
		     (item . org-edu-html-item)))

(defun org-edu-html-template (contents info)
  (concat
   "<!DOCTYPE html>\n"
   "<html>\n"
   "<head>\n"
   (org-html--build-meta-info info)	;!!
;   (org-html--build-head info)		;!!
   (org-html--build-mathjax-config info) ;!!
   "</head>\n"
   "<body>\n"
   (org-html--build-pre/postamble 'preamble info)
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
   ;; Postamble (should probably be changed)
   (org-html--build-pre/postamble 'postamble info)
   ;; Closing document.
   "</body>\n</html>\n"))

(defun org-edu-html-plain-list (plain-list contents info)
  (cond
   ((string= (org-export-read-attribute :attr_edu plain-list :test) "mct")
    (org-edu-html-mct-test plain-list contents info))
   ((string= (org-export-read-attribute :attr_edu plain-list :test) "sct")
    (org-edu-html-mct-test plain-list contents info t))
   (t (org-html-plain-list plain-list contents info))))

(defun org-edu-html-mct-test (plain-list contents info &optional sct)
  (format "<form>\n<fieldset class=\"%s\">\n%s\n</fieldset>\n</form>"
   (if sct "sct" "mct")
   contents))

(defun org-edu-html-item (item contents info)
  ;; (org-html-item (item contents info)))
  (cond
   ((string=
     (org-export-read-attribute :attr_edu (org-element-property :parent item) :test) "mct")
    (org-edu-html-mct-item item contents info))
   ((string=
     (org-export-read-attribute :attr_edu (org-element-property :parent item) :test) "sct")
    (org-edu-html-mct-item item contents info t))
   (t (org-html-item item contents info))))

(defun right-answer-code ()
  "Value of the `value' attribute of a checkbox of the right answer."
  "1")

(defun wrong-answer-code ()
  "Value of the `value' attribute of a checkbox of the wrong answer."
  "0")

(defun org-edu-html-mct-item (item contents info &optional sct)
  (let ((checkbox (org-element-property :checkbox item)))
  (format "<div><input type=\"%s\" value=\"%s\">\n%s</div>"
	  (if sct "radiobutton" "checkbox")
	  (if (eql checkbox 'on) (right-answer-code) (wrong-answer-code))
	  contents)))
