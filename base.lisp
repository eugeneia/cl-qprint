;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; base.lisp: The Program
;;;; Copyright (C) 2004 Robert Marlow <rob@bobturf.org>
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage cl-qprint
  (:use :cl
	:babel)
  (:nicknames :qprint)
  (:export :encode
	   :decode))

(in-package :cl-qprint)

(defun char-code-ascii (char)
  "Return ASCII code for CHAR."
  (let ((string (make-string 1 :initial-element char)))
    (aref (string-to-octets string :encoding :ascii) 0)))

(defun decode (input)
  "INPUT must be a STRING or a character STREAM. Reads quoted-printable
encoding from INPUT and produces the equivalent
{(VECTOR (UNSIGNED-BYTE 8))}."
  (let* ((in-stream
	  (etypecase input
	    (string (make-string-input-stream input))
	    (stream input)))
	 (input-length (file-length in-stream))
	 (buffer (make-array input-length
			     :element-type '(unsigned-byte 8))))
    (loop for char = (read-char in-stream nil 'eof)
       for i from 0 to (1- input-length)
       when (eq char 'eof) return (subseq buffer 0 i)
       do (if (char= char #\=)
	      (let ((char2 (read-char in-stream)))
		;; Check for and convert all newlines (LF or CRLF)
		;; to nothing. The = indicates a soft line break.
		(if (member char2 '(#\return #\linefeed)
			    :test #'char=)
		    (let ((char3 (read-char in-stream nil 'eof)))
		      (unless (or (eql char3 'eof)
				  (and (char= char3 #\linefeed)
				       (char= char2 #\return)))
			(setf (aref buffer i) (char-code-ascii char3))))
		    ;; If not a newline the = indicates encoding
		    (setf (aref buffer i)
			  (parse-integer
			   (format nil "~C~C"
				   char2
				   (read-char in-stream))
			   :radix 16))))
	      (setf (aref buffer i) (char-code-ascii char))))
    buffer))


(defun cr-lf (stream)
  "Prints a CRLF sequence to STREAM. RFC 2045 mandates CRLF for newlines"
  (write-char #\return stream)
  (write-char #\linefeed stream))

(defun stream-vector (stream)
  "Return {(VECTOR (UNSIGNED-BYTE 8))} of STREAM."
  (let* ((stream-length (file-length stream))
	 (buffer (make-array stream-length
			     :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    buffer))

(defun code-char-ascii (code)
  "Return ASCII character for CODE."
  (let ((buffer (make-array 1
			    :element-type '(unsigned-byte 8)
			    :initial-element code)))
    (aref (babel:octets-to-string buffer :encoding :ascii) 0)))
			    

(defparameter *ascii-!* (char-code-ascii #\!)
  "ASCII code for exclamation mark character.")

(defparameter *ascii-~* (char-code-ascii #\~)
  "ASCII code for tilde character.")

(defparameter *ascii-=* (char-code-ascii #\=)
  "ASCII code for equal sign character.")

(defparameter *ascii-newline* (char-code-ascii #\Newline)
  "ASCII code for newline character.")

(defparameter *ascii-space* (char-code-ascii #\Space)
  "ASCII code for space character.")

(defparameter *ascii-tab* (char-code-ascii #\Tab)
  "ASCII code for tab character.")

(defun encode (input &key encode-newlines)
  "INPUT must be either a VECTOR or a STREAM with ELEMENT-TYPE of
{(UNSIGNED-BYTE 8)}. Reads from INPUT and produces a quoted-printable
encoded string."
  (let ((out-stream (make-string-output-stream))
	(input-vector (etypecase input
			(vector input)
			(stream (stream-vector input))))
	(ws nil))
    (loop for i from 0 to (1- (length input-vector))
       for byte = (aref input-vector i) do
	 
       ;; ws on the end of a line must be encoded
	 (when ws
	   (if (= byte *ascii-newline*)
	       (format out-stream "=~2,'0X" (char-code-ascii ws))
	       (write-char (code-char-ascii ws) out-stream)))
      
	 (cond

	   ;; Ensure newlines are CR-LF
	   ((= byte *ascii-newline*)
	    (if encode-newlines
		(format out-stream "=0D=0A")
		(cr-lf out-stream)))

	   ;; Keep track of whitespace in case of following newlines
	   ((or (= byte *ascii-space*)
		(= byte *ascii-tab*))
	    (setf ws byte))
	
	   ;; Encode non-printable characters and =
	   ((or (< byte *ascii-!*)
		(> byte *ascii-~*)
		(= byte *ascii-=*))
	    (format out-stream "=~2,'0X" byte))

	   ;; Else just print the character.
	   (t (write-char (code-char-ascii byte) out-stream)))

       ;; Keep track of whitespace in case we hit a newline
	 (unless (or (= byte *ascii-space*)
		     (= byte *ascii-tab*))
	   (setf ws nil)))
    (get-output-stream-string out-stream)))