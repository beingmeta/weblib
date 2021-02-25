;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-

;;; Copyright (C) 2005-2020 beingmeta, inc. All rights reserved
;;; Copyright (C) 2020-2021 beingmeta, LLC

(in-module 'xhtml/signature)

(module-export! '{sig/make sig/check})

(define sigmod (get-module 'crypto/signature))

(define sig/make (get sigmod 'sig/make))
(define sig/check (get sigmod 'sig/check))

