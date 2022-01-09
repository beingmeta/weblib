;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-
;;; Copyright (C) 2010-2020 beingmeta, inc. All rights reserved
;;; Copyright (C) 2020-2022 Kenneth Haase (ken.haase@alum.mit.edu).

(in-module 'net/ldap)

(use-module '{webtools curl logger reflection varconfig opts})
(use-module '{texttools logger})
(define %used_modules '{varconfig})

(define-init %loglevel %warn%)

(define-init ldap-host "localhost")
(define-init ldap-port 389)
(define-init ldap-root #f)

(define-init default-ldap-scope "base")

(varconfig! 'ldap:host ldap-host)
(varconfig! 'ldap:root ldap-root)

(module-export! '{->dn domain->dn ldap-url ldap/search ldap/get ldap-escape})

;;;; LDAP escaping

(define no-escape-pat 
  '{(GREEDY (and (isalnum+) (isascii+))) (chars* "=?-._~:/[]@!$&'()*+,;")})
(module-export! 'no-escape-pat)

(define (ldap-escape string)
  "Generates an LDAP escaped URL string, using uriencode. "
  ;; LDAP has slightly weird escaping rules, particularly not escaping
  ;;  characters like = and & which uriencode would normally escape. So
  ;;  we slice the string into spans to be left alone and spans to be escaped
  (if (not string) ""
      (let* ((segs (textslice string (qc no-escape-pat) 'sep)))
	(string-subst
	 (apply glom
		(forseq (seg segs i)
		  (if (even? i) (uriencode seg "") seg)))
	 "?" "\\3f"))))

;;;; Generating distinquished names

(define (domain->dn domain)
  (stringout
    (doseq (name (textslice domain "." #f) i)
      (printout (if (> i 0) ",") (printout "dc=" name)))))

(define (->dn string)
  (let* ((at (and string (position #\@ string)))
	 (head (and at (slice string 0 at))))
    (cond (at (glom (if (position #\= head) head ) ","
		(domain->dn (slice string (1+ at)))))
	  ((not (string? string)) "")
	  ((and (position #\. string)
		(not (or (position #\= string) (position #\, string))))
	   (domain->dn string))
	  (else string))))
							  
(define (attribs-list attribs)
  (cond ((ambiguous? attribs)
	 (stringout (do-choices (elt attribs i) (printout (if (> i 0) ",") elt))))
	((string? attribs) attribs)
	((symbol? attribs) (symbol->string attribs))
	((sequence? attribs)
	 (stringout (doseq (elt attribs i) (printout (if (> i 0) ",") elt))))
	(else "")))

(define (->filter arg)
  (stringout (do-choices (each arg i)
	       (if (> i 0) (printout ","))
	       (cond ((string? arg) (printout each))
		     ((table? arg)
		      (do-choices (key (getkeys arg) i)
			(do-choices (value (get arg key) i)
			  (printout (if (> i 0) ",")
			    key "=" value))))
		     (else)))))
(module-export! '->filter)

(defambda (ldap-url host port base attribs scope filter)
  (glom "ldap://" host (and port ":") port "/" (ldap-escape (->dn base))
    "?" (ldap-escape attribs) "?" scope
    (and filter "?") (and filter (ldap-escape (->filter filter)))))

(define (spec->hostname s)
  (if (position #\: s)
      (slice s 0 (position #\: s))
      s))
(define (spec->port s)
  (if (position #\: s)
      (slice s (1+ (position #\: s)) )
      389))

;;; Parsing results

(define dn-rule
  #((bol) #("DN:" (spaces) (label DN (not> (eol)))) (label body (not> #((bol) "DN:")))))
(define attrib-rule
  #((bol) (spaces) #((label attrib (not> ":")) ":" (spaces*) (label value (not> (eol))))))

(define (parse-result string (opts #f))
  (define typemap (getopt opts 'typemap))
  (define rawstrings (testopt opts 'symbolize #f))
  (define rawvals (testopt opts 'parsevals #f))
  (for-choices (root (text->frames dn-rule string))
    (let ((result (if rawstrings
		      `#["dn" ,(get root 'dn)]
		      `#[dn ,(get root 'dn)])))
      (do-choices (attrib.value (text->frames attrib-rule (get root 'body)))
	(if rawstrings
	    (add! result (get attrib.value 'attrib) (get attrib.value 'value))
	    (let* ((attrib (get attrib.value 'attrib))
		   (value (get attrib.value 'value))
		   (typeinfo (try (getopt typemap attrib) (getopt opts 'parseopts #f))))
	      (cond ((symbol? typeinfo)
		     (set! attrib typeinfo)
		     (set! typeinfo (getopt typemap attrib)))
		    ((or (lowercase? attrib)
			 (testopt opts 'symbolize attrib)
			 (testopt opts 'symbolize #t))
		     (set! attrib (symbolize attrib))
		     (when (testopt typemap attrib) (set! typeinfo (getopt typemap attrib)))))
	      (cond (rawvals)
		    ((applicable? (getopt typeinfo 'parser))
		     (onerror (set! value ((getopt typeinfo 'parser) value)) #f))
		    ((or (testopt opts 'parsevals attrib)
			 (testopt opts 'parsevals #t)
			 (testopt typeinfo 'parseval))
		     (onerror (set! value (parse-arg value)) #f))
		    ((or (testopt typeinfo 'unparsed)
			 (testopt opts 'unparsed attrib)))
		    ((textmatch '(isdigit+) value) 
		     (onerror (set! value (string->number value)) #f)))
	      (add! result attrib value))))
      result)))
	      
;;;; Top levels

(define (ldap/search filter (opts #f))
  (define host-arg (getopt opts 'host ldap-host))
  (let* ((search-base (->dn (getopt opts 'dn (getopt opts 'root ldap-root))))
	 (url (ldap-url (spec->hostname host-arg) 
			(getopt opts 'port (spec->port host-arg))
			search-base
			(attribs-list (getopt opts 'attribs ""))
			(getopt opts 'scope "sub")
			filter))
	 (response (urlget url #[notes text]))
	 (text (get response '%content))
	 (return-type (getopt opts 'return)))
    (debug%watch "ldap/search" URL)
    (cond ((overlaps? return-type '{mime response}) response)
	  ((overlaps? return-type '{raw string}) text)
	  (else (parse-result text opts)))))

(defambda (ldap/get obj attribs (opts #f))
  (define host-arg (getopt opts 'host ldap-host))
  (let* ((search-base (->dn (getopt opts 'dn (getopt opts 'root ldap-root))))
	 (url (ldap-url (spec->hostname host-arg) 
			(getopt opts 'port (spec->port host-arg))
			(try (get obj 'dn) (get obj "dn"))
			(attribs-list attribs)
			(getopt opts 'scope "base")
			#f))
	 (response (urlget url #[notes text]))
	 (text (get response '%content))
	 (return-type (getopt opts 'return)))
    (debug%watch "ldap/get" URL "\n" obj)
    (cond ((overlaps? return-type '{mime response}) response)
	  ((overlaps? return-type '{raw string}) text)
	  ((singleton? attribs) (get (parse-result text (cons [skipdn #t] opts)) attribs))
	  (else (parse-result text opts)))))


