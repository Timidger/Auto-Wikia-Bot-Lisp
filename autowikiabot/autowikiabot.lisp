(in-package :autowikiabot)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :drakma)
  (require :cl-json)
  (require :cl-reddit))

(defconstant *REDIRECT-STRING* "REDIRECT ")
(defconstant *TEXT-START* "abstract\":")
(defconstant *DISAMBIGUATION* "disambiguation")
(defconstant *PUNCTUATION* (list #\! #\? #\.))
(defconstant *WIKIA-SEARCH-STRING* ".wikia.com/wiki/")
(defconstant *URL-ENDING-CHARS* (list #\Space
				      #\# #\$ #\%
				      #\* #\+ #\, #\- #\.
				      #\/ #\; #\< #\=
				      #\> #\@ #\[ #\\
				      #\] #\^ #\` #\{ #\|
				      #\} #\~))

(setq drakma:*text-content-types* (cons '("application" . "json") drakma:*text-content-types*))

(defmacro hofeach (hof var list &body body)
  `(funcall ,hof (lambda (,var) ,@body) ,list))

(defun summarize (sub-wikia title)
  "Summarizes the wikia article. Redirects are automatically handled and disambiguation pages supressed"
  (let ((summary (get-summary (get-json sub-wikia :title title :action 'SimpleJSON) sub-wikia)))
    (if (not (search *DISAMBIGUATION* summary))
	summary)))

(defun get-summary (json sub-wikia)
  "Gets the complete summary from the wikia, handling redirects by default"
  (let* ((summary (cdr (assoc :text (cadr (assoc :content json))))))
    (handle-if-redirect summary sub-wikia)))

(defun handle-if-redirect (summary sub-wikia)
  "If the abstract contains a redirect, that is returned to be the new title value"
  (let ((redirect (search-add *REDIRECT-STRING* summary)))
    (if redirect
	(summarize sub-wikia (subseq summary redirect))
	summary)))

(defun get-json (sub-wikia &key title page-id (action 'Details))
  "Gets the JSON from the wikia API call"
  (multiple-value-bind (page-id title)
      (page-title-and-id sub-wikia :title title :page-id page-id)
    (let ((url (format-api-url sub-wikia action)))
      ;; Ignore the other information passed back to us
      (multiple-value-bind (return-json status-code)
	  (api-call url title page-id)
	(if (= status-code 200)
	    (cdadar (json:decode-json-from-string return-json))
	    (error (format nil "Could not find page \"~a\" on wikia \"~a\". ~a"
			   title sub-wikia status-code)))))))

(defun api-call (url &optional title page-id)
  (drakma:http-request url :method :post
		       :parameters `(("titles" . ,title)
				     ("id" . ,page-id)
				     ("abstract" . "500")
				     ("lang" .  "en"))))

(defun format-api-url (sub-wikia type)
  (let ((base-url (format nil "http://~a.wikia.com/api/v1/" (string-downcase sub-wikia))))
    (case type
      (Details
       (concatenate 'string base-url "Articles/Details?"))
      (SimpleJSON
       (concatenate 'string base-url "Articles/AsSimpleJson?")))))

(defun page-title-and-id (sub-wikia &key title page-id)
  (when (not (and title page-id))
    (let ((json (cdadar
		 (json:decode-json-from-string
		  (api-call (format-api-url sub-wikia 'Details) title page-id)))))
      (setf page-id (cdr (assoc :id json)))
      (setf title (cdr (assoc :title json)))))
  (values (int->string page-id) title))

(defun find-multiple (seq keys &optional (start 0))
  "Returns the first index of the first character in keys that appears in seq"
  (let ((pos (hofeach #'position-if character (subseq seq start (length seq))
	       (hofeach #'some key-character keys
		 (char= character key-character)))))
    (if pos (+ start pos))))

(defun int->string (number)
  (if (stringp number)
      number
      (write-to-string number)))

(defun search-add (sequence1 sequence2)
  "Wrapper around search that also adds the length of sequence1 to the result"
  (let ((search-result (search sequence1 sequence2)))
    (if search-result
	(values (+ (length sequence1) search-result) search-result))))

;; Macro to allow me to access JSON objects easily using nested assoc accesses
(defmacro assoc-nest (alist &rest assoc-list)
  "Wrapper for a-lists so that items can be grabbed either by key or index.
  assoc-list is a list of keys/indexes that are accessed sequentually from the
  provided alist. When bounds are overstepped, nil is returned"
  (let ((nested-assoc-accesses (gensym)))
    (setf nested-assoc-accesses alist)
    (loop
       for item in assoc-list
       do
	 (if (numberp item)
	     (setf nested-assoc-accesses `(nth ,item ,nested-assoc-accesses))
	     (setf nested-assoc-accesses `(cdr (assoc ,(intern (string-upcase (symbol-name (cadr item))) "KEYWORD") ,nested-assoc-accesses))))
	 
       finally (return nested-assoc-accesses))))


(defun get-new-comment-data (json)
  "Get the data from the comments in the JSON from http://reddit.com/comments.json"
  (let ((comments (assoc-nest json 'data 'children)))
    (hofeach #'mapcar comment comments
	(list (assoc-nest comment 'data 'id)
	      (assoc-nest comment 'data 'body)))))

(defun get-all-comments ()
  "Grab the next set of (25) comments"
  (json:decode-json-from-string
   (drakma:http-request "http://reddit.com/comments.json"
			:method :get
			:user-agent cl-reddit:*user-agent*
			:preserve-uri t)))

;; TODO: Make the t1_ come about automagically (it's the "kind" value in JSON)
(defun reply-to-comment (user id text)
  "Posts a reply to the id of the given comment (omit t1_)"
  (cl-reddit:api-comment user
			 :thing-id (concatenate 'string "t1_" id)
			 :text text))

(defun grab-comments-continuously ()
  "Grabs all new comments that are posted to Reddit continuously"
  (handler-bind
      ((json:unencodable-value-error
	(lambda (err)
	  (declare (ignore err))
	  (invoke-restart 'json:substitute-char "?"))))
    (loop while t do
	 (let ((comments (get-new-comment-data (get-all-comments))))
	   (loop
	      for comment in comments
	      do (multiple-value-bind (title sub-wikia)
		     (find-wikia-reference (cdr comment))
		   (when title
		     (reply-to-comment user (car comment)
				       (summarize sub-wikia title))))
	      finally
		(sleep 1))))))

(defun find-wikia-reference (comment)
  "Searches the comment for any reference to a wikia link.
  Returns the title and the sub-wikia the reference was found on"
  
  (multiple-value-bind (url-end url-start)
      (search-add *WIKIA-SEARCH-STRING* comment)
    (let ((wikia-title-start (1+ (position #\/ comment
					   :from-end t
					   :end url-start)))
	  (title-end (or (find-multiple comment *URL-ENDING-CHARS* url-end)
			 (length comment))))
      (values (subseq comment url-end title-end)
	      (subseq comment wikia-title-start url-start)))))