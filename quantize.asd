;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:ql-quantize
  (:use :cl :asdf))

(in-package :ql-quantize)

(defsystem quantize
    :name "quantize"
    :version "0.0.0"
    :maintainer "fouric"
    :author "fouric"
    :license "All rights reserved"
    :description "quantization of the self"

    :serial t
    :pathname "src"
    :components ((:file "package")
		 (:file "quantize"))
    :depends-on (:fouriclib))
