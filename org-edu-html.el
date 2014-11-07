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
  (let* ((type (org-element-property :type plain-list))
	 (test (org-export-read-attribute :attr_edu plain-list :test)))
    (format "%s\n%s%s"
	    (org-edu-html-begin-plain-list type test)
	    contents
	    (org-html-end-plain-list type test))))

(defun org-edu-html-begin-plain-list (type test)
  (let ((type-class (case type
		      (ordered "ol")
		      (unordered "ul")
		      (descriptive "dl"))))
    (format "<%s class=\"org-%s%s\">" type-class type-class
	    (if test (concat " " test) ""))))
