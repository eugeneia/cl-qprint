;;;; Encode/decode quoted-printable.
;;;;
;;;; Copyright (C) 2004 Robert Marlow <rob@bobturf.org>
;;;; Copyright (C) 2013, 2015 Max Rottenkolber <max@mr.gy>
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
   "Encode and decode quoted-printable encoded strings as defined by
    [RFC 2045](http://tools.ietf.org/html/rfc2045).")
  (:use :cl
	:flexi-streams)
  (:nicknames :qprint
              :quoted-printable)
  (:export :encode
	   :decode
           :decode-error))

(in-package :cl-qprint)

(defun char-code-ascii (char)
  "Return ASCII code for CHAR."
  (let ((string (make-string 1 :initial-element char)))
    (aref (string-to-octets string :external-format :ascii) 0)))

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

(defparameter *ascii-whitespace* (list (char-code-ascii #\Space)
                                       (char-code-ascii #\Tab)
                                       (char-code-ascii #\Newline)
                                       (char-code-ascii #\Return))
  "List of ASCII codes of whitespace characters.")

(defun byte-encoding (byte)
  "Produce quoted-printable encoding string for BYTE."
  (format nil "=~2,'0X" byte))

(defparameter *encoded-crlf*
  (format nil "~a~a"
          (byte-encoding (char-code-ascii #\Return))
          (byte-encoding (char-code-ascii #\Newline)))
  "CRLF string.")

(defparameter *soft-line-break*
  (format nil "=~a~a" #\Return #\Newline)
  "Soft line break.")

(defun upper-case-hex-p (char)
  "Predicate to check if CHAR is a upper case hexadecimal digit."
  (find char "0123456789ABCDEF"))

(defun encode-byte-p (byte)
  "Predicate to check wether BYTE has to be encoded in quoted-printable
encoding."
  (or (< byte *ascii-!*)
      (> byte *ascii-~*)
      (= byte *ascii-=*)))

(defun encode-byte (byte)
  "Encode BYTE for quoted-printable encoding and return string."
  (if (encode-byte-p byte)
      (byte-encoding byte)
      (make-string 1 :initial-element (code-char-ascii byte))))

(defun encode (input &key (columns 76))
  "*Arguments and Values:*

   _input_—a _vector_ or a _stream_ with element type
   {(unsigned-byte 8)}.

   _columns_—a positive _integer_.

   *Description*:

   {encode} reads from _input_ and returns a _quoted-printable_ encoded
   _string_. _Columns_ denotes the maximum line length of _string_."

  ;; Does *not* convert LF to CRLF but encodes all whitespace instead to
  ;; ensure full input integrity.
  (let ((in (etypecase input
              ;; Coerce input type.
	      (vector (make-in-memory-input-stream input))
	      (stream input))))
    (with-output-to-string (out)
      (loop for byte = (read-byte in nil 'eof)
            for position = (file-position out)
            for last-line-break = 0 then last-line-break
         while (not (eq byte 'eof)) do
           (let ((encoded (encode-byte byte)))
             ;; Line too long?
             (when (> (+ (- position last-line-break) (length encoded))
                      columns)
               ;; Soft line break.
               (write-string *soft-line-break* out)
               (setf last-line-break
                     (+ position 3))) ; +3 because of soft line break.
             ;; Encoded representation of byte.
             (write-string encoded out))))))

(define-condition decode-error (error) ()
  (:documentation
   "The _type_ {decode-error} consists of error conditions related to
    malformed encodings."))

(defun decode-byte (char2 char3)
  "Decode byte encoded by CHAR2 and CHAR2."
  (parse-integer (format nil "~C~C" char2 char3) :radix 16))

(defun decode-quoted (char2 char3 &key error-p)
  "Decode quoted CHAR2 and CHAR3, signal DECODE-ERROR when ERROR-P is non
NIL and sequence is invalid."
  (cond
    ;; A soft line break, do nothing.
    ((and (eq char2 #\Return)
          (eq char3 #\Newline))
     nil)
    ;; An encoded byte, decode it.
    ((and (upper-case-hex-p char2)
          (upper-case-hex-p char3))
     (decode-byte char2 char3))
    ;; Otherwise malformed encoding. When ERROR-P is non NIL signal error
    ;; otherwise do nothing.
    (error-p (error 'decode-error))))

(defun plain-char-code (char &key error-p)
  "Return ASCII code for unencoded CHAR if possible otherwise return
NIL or signal DECODE-ERROR depending on ERROR-P."
  (let ((code (char-code-ascii char)))
    (if (and (encode-byte-p code)
             (not (member code *ascii-whitespace*)))
        (when error-p
          (error 'decode-error))
        code)))

(defun decode (input &key error-p)
  "*Arguments and Values:*

   _input_—a _string_ or a _character stream_.

   _error-p_—a _generalized boolean_. The default is false.

   *Description*:

   {decode} reads from _quoted-printable_ encoded _input_ and returns a
   decoded _vector_ with {(unsigned-byte 8)} as its _element type_.

   *Exceptional Situations:*

   If _error-p_ is _true_, an error of _type_ {decode-error} signaled
   when _input_ is malformed."
  (let ((in (etypecase input
	      (string (make-string-input-stream input))
	      (stream input))))
    (with-output-to-sequence (out :element-type '(unsigned-byte 8))
      (loop for char = (read-char in nil 'eof)
	 while (not (eq char 'eof))
	 do (let ((decoded
                   (if (char= char #\=)
                       ;; Encoded char or soft line break.
                       (let ((char1 (read-char in nil 'eof))
                             (char2 (read-char in nil 'eof)))
                         (or (decode-quoted char1 char2 :error-p error-p)
                             ;; Ignore bad soft line breaks (very common)
                             ;; (= followed by LF).
                             (when (and (eq #\Newline char1)
                                        (not (eq 'eof char2)))
                               (unread-char char2 in)
                               nil)))
                       ;; Non encoded char.
                       (plain-char-code char :error-p error-p))))
              (when decoded ; Ignore values that couldn't be decoded.
                (write-byte decoded out)))))))
