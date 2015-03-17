;; Org-edu-HTML - HTML-based exporter for educational materials

(require 'ox-html)
(require 'org-one-to-many)

(defvar org-edu-html-split-tag "split"
  "Tag to mark where splitting should occur.")

(org-export-define-derived-backend 'edu-html 'html
  :translate-alist '((template . org-edu-html-template)
		     (plain-list . org-edu-html-plain-list)
		     (item . org-edu-html-item)
		     (special-block . org-edu-html-special-block)
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

(defun org-edu-html-special-block (special-block contents info)
  "Transcode a special block into a suitable <div> as in ox-html,
but with one twist: if the block type is \"hidden\", check for
:show and :hide keywords and encode them as suitable attributes.

Part of this function is copied (with simplifications) from
org-html-special-block."
  (if (not (string= (downcase (org-element-property :type special-block)) "hidden"))
      (org-html-special-block special-block contents info)
    (let* ((keywords-string (save-excursion
			      (buffer-substring-no-properties
			       (progn (goto-char (org-element-property :post-affiliated special-block))
				      (let ((case-fold-search t)) (re-search-forward "#\\+begin_hidden *"))
				      (point))
			       (progn (end-of-line)
				      (point)))))
	   (keywords-alist (keywords-string-to-alist keywords-string))
	   (contents (or contents ""))
	   (attributes (org-export-read-attribute :attr_html special-block))
	   (attributes (plist-put attributes :class "hidden"))
	   (show-name (assoc "show" keywords-alist))
	   (hide-name (assoc "hide" keywords-alist)))
      (if show-name
	  (setq attributes (plist-put attributes :data-show-text (cdr show-name))))
      (if hide-name
	  (setq attributes (plist-put attributes :data-hide-text (cdr hide-name))))
      (setq attributes (org-html--make-attribute-string attributes))
      (format "<div %s>\n%s\n</div>" attributes contents))))

(defun keywords-string-to-alist (keywords-string)
  "Convert a string with keywords (looking like in a plist) into an alist
\(with keys being strings, without leading colons).  For instance,
this: \":foo bar :baz qux \" gets converted into `((\"baz\"
. \"qux\") (\"foo\" . \"bar\"))'.  Notice that the trailing space
gets trimmed."
  (let ((start 0) keywords-alist)
    (while (string-match ":\\([a-z]+\\) \\([^:]*\\)\\( \\|$\\)" keywords-string start)
      (push (cons (match-string 1 keywords-string)
		  (save-match-data
		    (string-trim-right (match-string 2 keywords-string))))
	    keywords-alist)
      (setq start (match-end 2)))
    keywords-alist))

(defun org-edu-html-split-and-generate-html ()
  "Generate an empty directory with a unique name, use
org-one-to-many to split the current org buffer into individual
files in that directory, and convert them all to html.  Returns the
list of generated html files."
  (let* ((dirname (make-temp-file "org-edu-html" t))
	 (files (org-one-to-many org-edu-html-split-tag dirname)))
    (mapcar (lambda (infile)
	      (org-export-file 'edu-html
			       (concat dirname "/" infile)
			       (concat dirname
				       "/"
				       (file-name-base infile)
				       ".html")))
	    files)))

(defvar org-edu-html-dependencies ()
  "List of filenames (currently only inline images and bitmaps
with equations if dvipng or imagemagick is used) that the
currently translated file depends on.")

(defun org-edu-html-store-latex-fragment (link)
  "Given a LINK, store the filename somewhere and return the link
back."
  (push (if (string-match "^<img src=\"\\([^\"]+\\)\"" link)
	    (match-string-no-properties 1 link)
	  "")
	org-edu-html-dependencies)
  link)

(advice-add 'org-html-latex-fragment :filter-return #'org-edu-html-store-latex-fragment)

(defun org-edu-html-store-inline-image (oldfun link rules)
  "Given the function `org-html-inline-image-p' and its
arguments, store the filename somewhere and return the link."
  (let ((result (funcall oldfun link rules)))
    (if result
	(push (org-element-property :path link) org-edu-html-dependencies))
    result))

(advice-add 'org-export-inline-image-p :around #'org-edu-html-store-inline-image)

(defun org-export-file (backend infile outfile)
  "Loads INFILE to a temp buffer and exports it to OUTFILE, using
BACKEND exporter.  Return a list whose car is OUTFILE and whose
cdr is the list of dependencies."
  (setq org-edu-html-dependencies ())
  (with-temp-buffer
    (insert-file-contents infile)
    (org-mode)
    (cons (org-export-to-file backend outfile) org-edu-html-dependencies)))

;; The "backend" org-scorm-manifest transforms an "empty" outline
;; (i.e., one containing only headlines with links, as produced by
;; org-one-to-many) into an xml "organization" structure needed by
;; SCORM.
(org-export-define-backend 'org-scorm-manifest
  '((headline . org-scorm-manifest-headline)
    (link . org-scorm-manifest-link)))

(defun org-scorm-manifest-link (link desc info)
  "Convert the link to an XML element suitable for inclusion in
a SCORM package manifest."
  (let ((resource-empty-p
	 (with-temp-buffer
	   (insert-file-contents (org-element-property :path link))
	   (forward-line)
	   (eobp))))
    (format "<item identifier=\"%s\"%s>\n<title>%s</title></item>"
	    (concat (file-name-sans-extension (org-element-property :path link)) "-item")
	    (if resource-empty-p ""
	      (concat " identifierref=\""
		      (file-name-sans-extension (org-element-property :path link))
		      "-resource\""))
	    desc)))

(defun org-scorm-manifest-headline (headline contents info)
  "Transcode a headline with a link from an intermediate org file
used to generate a SCORM package.  There is one extremely dirty
hack: if the contents are not empty, we need to put them before
the closing \"</item\" of the link from the headline, so we use regex
replacement in a string."
  (let ((link (org-export-data (org-element-property :title headline) info)))
    (if contents
	(replace-regexp-in-string "</item>$"
				  (concat contents "</item>")
				  link t t)
      link)))

(provide 'org-edu-html)
