;;; -*- Mode: Lisp -*-

    ;; This is a pure Common Lisp port of Elite for Emacs <http://members.fortunecity.com/salkosuo/elite-for-emacs/>.
    ;; It uses none of the original codebase (except for individual syllables in the planet descriptions)

    ;; Copyright (C) 2012 Leo Zovic aka Inaimathi

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :cl-leet-system (:use :cl :asdf))
(in-package :cl-leet-system)

(asdf:defsystem cl-leet
  :version "0.1"
  :author "leo.zovic@gmail.com"
  :maintainer "leo.zovic@gmail.com"
  :licence "GPL3"
  :description "Common Lisp port"
  :depends-on (:cl-who :parenscript :cl-css :clsql :clsql-mysql :hunchentoot :ironclad :cl-base64)
  :components ((:file "package")
	       (:file "util" :depends-on ("package"))
	       (:file "model" :depends-on ("package" "util"))
	       (:file "data" :depends-on ("package" "util" "model"))
	       (:file "controller" :depends-on ("package" "util" "model"))
	       (:file "view" :depends-on ("package" "util" "model"))))