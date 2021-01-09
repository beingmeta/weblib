;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-
;;; Copyright (C) 2014-2020 beingmeta, inc. All rights reserved

(in-module 'curlcache)

(use-module '{webtools curl varconfig logger})
(define %used_modules '{varconfig xhtml/auth})

(define-init curlcache-default #t)
(varconfig! curlcache:default curlcache-default)
(define-init curlcache #f)
(varconfig! curlcache curlcache config:boolean+parse)

(module-export! '{curlcache/get curlcache/reset!})

(define (curlcache/get (cachesym))
  (default! cachesym
    (and curlcache (if (symbol? curlcache) curlcache 'curlcache)))
  (if cachesym
      (try (thread/get cachesym)
	   (if curlcache-default
	       (let ((handle (curlopen)))
		 (thread/set! cachesym handle)
		 handle)
	       (frame-create #f)))
      (frame-create #f)))

(define (curlcache/reset! (cachesym) (force))
  (default! force (or (bound? cachesym) curlcache-default))
  (default! cachesym
    (and curlcache (if (symbol? curlcache) curlcache 'curlcache)))
  (if (or force (exists? (thread/get cachesym)))
      (let ((handle (curlopen)))
	(thread/set! cachesym handle)))
  cachesym)



