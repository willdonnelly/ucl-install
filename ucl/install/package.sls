#!r6rs
;; UCL INSTALL PACKAGE
;;   Routines for manipulating package files and manifests
(library (ucl install package)
  (export extract-manifest verify-manifest
          query-field package-name package-depends
          package-contents package-matching
          package-file)
  (import (rnrs) (ucl prelude) (ucl install ffi archive))

;; EXTRACT-MANIFEST p
;;   Given a package file P, extract the manifest and return it as a
;;   Scheme datum
(define (extract-manifest p)
  (verify-manifest
    (replace-error (error 'extract-manifest "unable to read manifest" p)
      (let ((text (archive-file-data p "MANIFEST")))
        (call-with-port (open-string-input-port text) get-data)))))

(define (verify-manifest data)
  (replace-error
    (error 'verify-manifest "manifest is missing required field" 'package)
    (assert (assoc 'package data)))
  (replace-error
    (error 'verify-manifest "manifest is missing required field" 'version)
    (assert (assoc 'version data)))
  (replace-error
    (error 'verify-manifest "manifest field 'package' must be a single symbol")
    (begin
      (assert (equal? (length (query-field 'package data)) 1))
      (assert (symbol? (car (query-field 'package data))))))
  data)

;; QUERY-FIELD field data
;;   Extract the given field from the manifest data
(define (query-field f d) (cdr (or (assoc f d) (list f))))

;; PACKAGE-NAME d
;;   Given a package manifest D, return a sanitized string
;;   containing the name
(define (package-name d)
  (car (query-field 'package d)))

;; PACKAGE-DEPENDS d
;;   Return all package dependencies in package manifest D
(define (package-depends d)
  (define (packages p)
    (or (symbol? p) (and (list? p) (equal? 'package (car p)))))
  (define (package-name p) (if (symbol? p) p (cadr p)))
  (map package-name (filter packages (query-field 'depends d))))

;; PACKAGE-CONTENTS p
;;   List all files in package P
(define (package-contents p)
  (archive-contents p))

;; PACKAGE-MATCHING p gs
;;   List all files matching the code GS in the package at path P
(define (package-matching p gs)
  (apply append (map (curry glob-expand (package-contents p)) gs)))

;; PACKAGE-FILE p f d
;;   Extract file F from package P to directory D
(define (package-file p f d)
  (archive-file p f d))

;; MATCHES-GLOB glob file
;;  Returns true only if the given file satisfies the glob
(define (matches-glob glob file)
  (cond
    ;; base cases
    ((and (null? glob) (null? file)) #t)
    ((null? glob) #f)
    ((null? file) (and (equal? (car glob) #\*) (null? (cdr glob))))
    ;; star matching
    ((equal? (car glob) #\*)
     (or (matches-glob (cdr glob) file)
         (and (not (equal? (car file) #\/)) (matches-glob glob (cdr file)))))
    ;; ordinary characters
    (else (and (equal? (car glob) (car file))
               (matches-glob (cdr glob) (cdr file))))))

;; GLOB-EXPAND dir glob
;;  Expand glob GLOB, against files FILES
(define (glob-expand files glob)
  (map list->string
    (filter (curry matches-glob (string->list glob))
      (map string->list files))))

)
