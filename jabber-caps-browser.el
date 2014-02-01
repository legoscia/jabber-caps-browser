;;; jabber-caps-browser.el ---                    -*- lexical-binding: t; -*-

(require 'tree-widget)
(require 'cl-lib)

(defun jabber-caps-browser ()
  (interactive)
  (with-current-buffer (get-buffer-create "*jabber-caps-browser*")
    (erase-buffer)
    (widget-minor-mode)
    (widget-create
     'tree-widget
     :tag "Entity capabilities information"
     :open t
     :args
     (list
      (widget-convert
       'tree-widget
       :tag "By software name"
       :open t
       :expander 'jabber-caps-browser-expand-software-names)
      (widget-convert
       'tree-widget
       :tag "By feature"
       :expander 'jabber-caps-browser-expand-features)))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun jabber-caps-browser-expand-software-names (_)
  (let (types)
    (maphash
     (lambda (key value)
       (let* ((identities (first value))
	      (name (jabber-caps-browser-identities-to-name identities))
	      (entry (assoc name types)))
	 (if entry
	     (push key (cdr entry))
	   (push (list name key) types))))
     jabber-caps-cache)
    (setq types (cl-sort types #'string< :key #'car))
    (mapcar
     (lambda (type)
       (let ((name (car type))
	     (keys (cdr type)))
	 (widget-convert
	  'tree-widget
	  :tag name
	  :caps-keys keys
	  :expander 'jabber-caps-browser-expand-keys)))
     types)))

(defun jabber-caps-browser-expand-keys (widget)
  (cl-flet
      ((key-features
	(key)
	(copy-sequence (second (gethash key jabber-caps-cache))))
       (sort-features
	(features)
	(cl-sort features #'string< :key #'jabber-caps-browser-maybe-name-feature))
       (feature-item
	(feature)
	(widget-convert 'item :value (jabber-caps-browser-maybe-name-feature feature))))
    (let* ((keys (widget-get widget :caps-keys))
	   (common-features
	    ;; Don't bother trying to determine common features if
	    ;; there is only one version.
	    (when (cdr keys)
	      (sort-features
	       (cl-reduce
		(lambda (a b) (cl-intersection a b :test #'string=))
		keys :key #'key-features))))
	   (version-widgets
	    (mapcar
	     (lambda (key)
	       (widget-convert
		'tree-widget
		:value key
		:args
		(cons
		 (widget-convert
		  'tree-widget
		  :tag "Online contacts"
		  :caps-key key
		  :expander 'jabber-caps-browser-expand-online-contacts)
		 (mapcar #'feature-item
			 (sort-features
			  (cl-set-difference
			   ;; common-features is nil if we only have one version.
			   (key-features key) common-features
			   :test #'string=))))))
	     keys)))
      (if (null (cdr keys))
	  ;; Just one version?  Skip "Common features".
	  version-widgets
	(cons
	 (widget-convert
	  'tree-widget
	  :tag "Common features"
	  :args (mapcar #'feature-item common-features))
	 version-widgets)))))

(defun jabber-caps-browser-expand-online-contacts (widget)
  (let ((key (widget-get widget :caps-key))
	contacts)
    (mapatoms
     (lambda (jid-symbol)
       (mapc
	(lambda (resource-info)
	  (when (equal key (plist-get (cdr resource-info) 'caps))
	    (push (jabber-jid-displayname jid-symbol) contacts)))
	(get jid-symbol 'resources)))
     jabber-jid-obarray)
    (mapcar
     (lambda (contact)
       (widget-convert 'item :value contact))
     (sort contacts #'string<))))

(defun jabber-caps-browser-expand-features (_)
  (let (features-keys)
    (maphash
     (lambda (key value)
       (let* ((features (second value)))
	 (mapc
	  (lambda (feature)
	    (let ((entry (assoc feature features-keys)))
	      (if entry
		  (push key (cdr entry))
		(push (list feature key) features-keys))))
	  features)))
     jabber-caps-cache)
    (setq features-keys
	  (cl-sort features-keys #'string<
		   :key (lambda (feature-keys)
			  (jabber-caps-browser-maybe-name-feature
			   (car feature-keys)))))
    (mapcar
     (lambda (feature-keys)
       (let ((feature (car feature-keys))
	     (keys (cdr feature-keys))
	     names-keys)
	 (mapc
	  (lambda (key)
	    (let* ((name (jabber-caps-browser-identities-to-name
			  (first (gethash key jabber-caps-cache))))
		   (entry (assoc name names-keys)))
	      (if entry
		  (push key (cdr entry))
		(push (list name key) names-keys))))
	  keys)
	 (setq names-keys (cl-sort names-keys #'string< :key #'car))
	 (widget-convert
	  'tree-widget
	  :tag (jabber-caps-browser-maybe-name-feature feature)
	  :args
	  (mapcar
	   (lambda (name-keys)
	     (widget-convert
	      'tree-widget
	      :tag (car name-keys)
	      :caps-keys (cdr name-keys)
	      :expander 'jabber-caps-browser-expand-keys))
	   names-keys))))
     features-keys)))

(defun jabber-caps-browser-identities-to-name (identities)
  (mapconcat
   (lambda (identity)
     (concat (aref identity 0)
	     " (" (aref identity 1)
	     "/" (aref identity 2) ")"))
   identities
   " + "))

(defvar jabber-caps-browser-feature-names :not-loaded)

(defvar jabber-caps-browser-xmpp-data-dir "~/src/xmpp/"
  "Get this from git://gitorious.org/xmpp/xmpp.git")

(defun jabber-caps-browser-maybe-load-feature-names ()
  (when (eq jabber-caps-browser-feature-names :not-loaded)
    (jabber-caps-browser-load-feature-names)))

(defun jabber-caps-browser-load-feature-names ()
  (let ((features-file (expand-file-name
			  "registrar/disco-features.xml"
			  jabber-caps-browser-xmpp-data-dir)))
    (if (not (file-exists-p features-file))
	(progn
	  (warn "Cannot open %s to read XMPP namespace info"
		features-file)
	  (setq jabber-caps-browser-feature-names nil))
      ;; Need to use xmllint, because xml.el doesn't expand external
      ;; entities.
      (with-temp-buffer
	(let ((exit-code (call-process "xmllint" nil (current-buffer) nil
				       "--noent" features-file)))
	  (if (not (zerop exit-code))
	      (warn "Cannot process %s: exit code %s, output %s"
		    features-file exit-code (buffer-string))
	    (let* ((xml-data (car (xml-parse-region (point-min) (point-max))))
		   (vars (jabber-xml-get-children xml-data 'var)))
	      (setq jabber-caps-browser-feature-names
		    (mapcar
		     (lambda (var-entry)
		       (let* ((var-name (jabber-xml-path var-entry '(name "")))
			      (var-desc (jabber-xml-path var-entry '(desc "")))
			      (var-doc
			       (or (jabber-xml-path var-entry '(doc link ""))
				   (jabber-xml-path var-entry '(doc ""))))
			      (description
			       (mapconcat
				'identity
				;; Remove boring texts
				(cl-remove-if
				 (lambda (text)
				   (cl-some
				    (lambda (regexp) (string-match-p regexp text))
				    '("^See XEP-....$"
				      "^See RFC ....$"
				      "^DEPRECATED$"
				      "^N/A$")))
				 (list var-doc var-desc))
				" - ")))
			 (cons var-name description)))
		     vars)))))))))

(defun jabber-caps-browser-lookup-feature (feature)
  (jabber-caps-browser-maybe-load-feature-names)
  (cdr (assoc feature jabber-caps-browser-feature-names)))

(defun jabber-caps-browser-maybe-name-feature (feature)
  (let ((description (jabber-caps-browser-lookup-feature feature)))
    (if description
	(format "%s (%s)" description feature)
      feature)))
