;;;; Encode/decode quoted-printable.
;;;;
;;;; Copyright (C) 2004 Robert Marlow <rob@bobturf.org>
;;;; Copyright (C) 2013 Max Rottenkolber <max@mr.gy>
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

(defpackage cl-qprint
  (:documentation
   "Encode/decode quoted-printable.")
  (:use :cl
	:flexi-streams)
  (:nicknames :qprint)
  (:export :encode
	   :decode))

(in-package :cl-qprint)

(defun char-code-ascii (char)
  "Return ASCII code for CHAR."
  (let ((string (make-string 1 :initial-element char)))
    (aref (string-to-octets string :external-format :ascii) 0)))

(defun decode-byte (character-1 character-2)
  "Decode byte encoded by CHARACTER-1 and CHARACTER-2."
  (parse-integer (format nil "~C~C" character-1 character-2)
		 :radix 16))

(defun decode (input)
  "INPUT must be a STRING or a character STREAM. Reads quoted-printable
encoding from INPUT and produces the equivalent
{(VECTOR (UNSIGNED-BYTE 8))}."
  (let ((in (etypecase input
	      (string (make-string-input-stream input))
	      (stream input))))
    (with-output-to-sequence (out :element-type '(unsigned-byte 8))
      (loop for char = (read-char in nil 'eof)
	 while (not (eq char 'eof))
	 do (if (char= char #\=)
		(let ((char2 (read-char in)))
		  ;; Check for and convert all newlines (LF or CRLF)
		  ;; to nothing. The = indicates a soft line break.
		  (if (member char2 '(#\return #\linefeed)
			      :test #'char=)
		      (let ((char3 (read-char in nil 'eof)))
			(unless (or (eql char3 'eof)
				    (and (char= char3 #\linefeed)
					 (char= char2 #\return)))
			  (write-byte (char-code-ascii char3) out)))
		      ;; If not a newline the = indicates encoding
		      (write-byte
		       (decode-byte char2 (read-char in))
		       out)))
		(write-byte (char-code-ascii char) out))))))

(defun cr-lf (stream)
  "Prints a CRLF sequence to STREAM. RFC 2045 mandates CRLF for newlines"
  (write-char #\return stream)
  (write-char #\linefeed stream))

(defun code-char-ascii (code)
  "Return ASCII character for CODE."
  (let ((buffer (make-array 1
			    :element-type '(unsigned-byte 8)
			    :initial-element code)))
    (aref (octets-to-string buffer :external-format :ascii) 0)))
			    

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

(defun encode (input &key columns encode-newlines)
  "INPUT must be either a VECTOR or a STREAM with ELEMENT-TYPE of
{(UNSIGNED-BYTE 8)}. Reads from INPUT and produces a quoted-printable
encoded string."
  (let ((in (etypecase input
	      (vector (make-in-memory-input-stream input))
	      (stream input)))
	(ws nil)
	(last-line-break 0))
    (with-output-to-string (out)
      (loop for byte = (read-byte in nil 'eof)
	    for position = (file-position out)
	 while (not (eq byte 'eof)) do

	 ;; Put in a soft line break if the line's gotten too long
	   (when (and columns
		      (>= (- position last-line-break) columns))
	     (write-char #\= out)
	     (cr-lf out)
	     (setf last-line-break position))

	 ;; ws on the end of a line must be encoded
	   (when ws
	     (if (= byte *ascii-newline*)
		 (format out "=~2,'0X" (char-code-ascii ws))
		 (write-char (code-char-ascii ws) out)))

	   (cond

	     ;; Ensure newlines are CR-LF
	     ((= byte *ascii-newline*)
	      (if encode-newlines
		  (format out "=0D=0A")
		  (cr-lf out)))

	     ;; Keep track of whitespace in case of following newlines
	     ((or (= byte *ascii-space*)
		(= byte *ascii-tab*))
	      (setf ws byte))
	
	     ;; Encode non-printable characters and =
	     ((or (< byte *ascii-!*)
		  (> byte *ascii-~*)
		  (= byte *ascii-=*))
	      (format out "=~2,'0X" byte))

	     ;; Else just print the character.
	     (t (write-char (code-char-ascii byte) out)))
	   
	 ;; Keep track of whitespace in case we hit a newline
	   (unless (or (= byte *ascii-space*)
		       (= byte *ascii-tab*))
	     (setf ws nil))))))