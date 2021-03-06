;;; jabber-caps-browser.el --- explore features advertised by Jabber clients  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Magnus Henoch

;; Author: Magnus Henoch
;; Version: 0.1
;; Package-Requires: ((jabber "0.8.92") (cl-lib "1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module is an addon to jabber.el, the Emacs Jabber client.
;; jabber.el supports XEP-0115: Entity Capabilities, which means that
;; it is able to gather information about features supported by other
;; Jabber clients.  This information can be viewed in the variable
;; `jabber-caps-cache', but jabber-caps-browser offers a visualisation
;; of the data.
;;
;; To use it, type M-x jabber-caps-browser.  As jabber.el accumulates
;; data while it is connected but doesn't save it to disk, you're more
;; likely to have interesting information after having been connected
;; for a while.
;;
;; The first time you run the browser, it will look for data files
;; published by the XMPP Standards Foundation, and display a warning
;; if it cannot find what it's looking for.  See the documentation for
;; the variable `jabber-caps-browser-xmpp-data-dir' for more
;; information.

;;; Code:

(require 'tree-widget)
(require 'cl-lib)
(require 'jabber-disco)

;;;###autoload
(defun jabber-caps-browser ()
  (interactive)
  (unless (boundp 'jabber-caps-cache)
    (error "`jabber-caps-cache' is not bound.  Is your jabber.el too old?"))
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
       (unless (floatp (first value))
	 (let* ((identities (first value))
		(name (jabber-caps-browser-identities-to-name identities))
		(entry (assoc name types)))
	   (if entry
	       (push key (cdr entry))
	     (push (list name key) types)))))
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
	    (push
	     (if (assoc (symbol-name jid-symbol) *jabber-active-groupchats*)
		 ;; We got this from a participant in an MUC
		 (format "%s in %s"
			 (car resource-info) (jabber-jid-displayname jid-symbol))
	       (jabber-jid-displayname jid-symbol))
	     contacts)))
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
     (let ((name (aref identity 0))
	   (category (aref identity 1))
	   (type (aref identity 2)))
       ;; name is optional
       (if name
	   (format "%s (%s/%s)" name category type)
	 (format "%s/%s" category type))))
   ;; If an entity has multiple identities, sort them by name, putting
   ;; identities without names last.
   (sort
    identities
    (lambda (a b)
      (let ((a-name (aref a 0))
	    (b-name (aref b 0)))
	(if a-name
	    (if b-name
		(string< a-name b-name)
	      ;; If B has no name, A goes first
	      t)
	  ;; If A has no name, B goes first
	  nil))))
   " + "))

(defvar jabber-caps-browser-feature-names :not-loaded)

(defvar jabber-caps-browser-xmpp-data-dir "~/src/xmpp/"
  "Path to data files published by the XMPP Standards Foundation.
If you download these files, the caps browser can provide slightly
more readable descriptions of the features, instead of plain
namespace URIs.

You can get these files from git://gitorious.org/xmpp/xmpp.git .
See http://xmpp.org/about-xmpp/xsf/xsf-source-control/ for more
information.")

(defun jabber-caps-browser-maybe-load-feature-names ()
  (when (eq jabber-caps-browser-feature-names :not-loaded)
    (jabber-caps-browser-load-feature-names)))

(defun jabber-caps-browser-load-feature-names ()
  (let ((features-file (expand-file-name
			"registrar/disco-features.xml"
			jabber-caps-browser-xmpp-data-dir)))
    (if (not (file-exists-p features-file))
	(progn
	  (warn "Cannot open %s to read XMPP namespace info.  See the documentation of `jabber-caps-browser-xmpp-data-dir' for more information"
		features-file)
	  (setq jabber-caps-browser-feature-names nil))
      (let* ((xml-data (jabber-caps-browser-load-xml-file features-file))
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
	       vars)))))

  (let ((namespaces-file (expand-file-name
			  "registrar/namespaces.xml"
			  jabber-caps-browser-xmpp-data-dir)))
    (if (not (file-exists-p namespaces-file))
	(warn "Cannot open %s to read XMPP namespace info.  See the documentation of `jabber-caps-browser-xmpp-data-dir' for more information"
	      namespaces-file)
      (let* ((xml-data (jabber-caps-browser-load-xml-file namespaces-file))
	     (ns (jabber-xml-get-children xml-data 'ns)))
	(mapc
	 (lambda (ns-entry)
	   (let* ((var-name (jabber-xml-path ns-entry '(name "")))
		  (var-doc
		   (or (jabber-xml-path ns-entry '(doc link ""))
		       (jabber-xml-path ns-entry '(doc "")))))
	     ;; Only fill in what we didn't get from the features
	     ;; file.  In principle, everything that we might receive
	     ;; as a disco feature should be documented in
	     ;; disco-features.xml, but it shouldn't hurt to look up
	     ;; features as namespaces in case the first lookup fails.
	     (unless (assoc var-name jabber-caps-browser-feature-names)
	       (push (cons var-name var-doc) jabber-caps-browser-feature-names))))
	 ns)
	t))))

(defun jabber-caps-browser-load-xml-file (filename)
  ;; Need to use xmllint, because xml.el doesn't expand external
  ;; entities.
  (with-temp-buffer
    (let ((exit-code (call-process "xmllint" nil (current-buffer) nil
				   "--noent" filename)))
      (if (zerop exit-code)
	  (car (xml-parse-region (point-min) (point-max)))
	(warn "Cannot process %s: exit code %s, output %s"
	      filename exit-code (buffer-string))
	nil))))

(defun jabber-caps-browser-lookup-feature (feature)
  (jabber-caps-browser-maybe-load-feature-names)
  (cdr (assoc feature jabber-caps-browser-feature-names)))

(defun jabber-caps-browser-maybe-name-feature (feature)
  (let ((description (jabber-caps-browser-lookup-feature feature)))
    (if description
	(format "%s (%s)" description feature)
      feature)))

(provide 'jabber-caps-browser)
;;; jabber-caps-browser.el ends here
