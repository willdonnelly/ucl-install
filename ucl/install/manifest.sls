#!r6rs
;; UCL INSTALL MANIFEST
;;   Useful procedures for extracting the needed information
;;   from a package manifest.

(library (ucl install manifest)
  (export manifest? pkg-name pkg-files query-field)
  (import (rnrs) (ucl prelude))

;; MANIFEST? x
;;   Determine if the object is a valid package manifest
(define (manifest? x)
  (and
    (list? x)
    (>= (length x) 3)
    (equal? (car x) 'package)
    (symbol? (cadr x))
    (list? (caddr x))
    (>= (length (caddr x)) 1)
    (for-all number? (caddr x))))

;; PKG-NAME x
;;   Get a package's name from its manifest
(define (pkg-name x)
  (assert (manifest? x))
  (cadr x))

;; QUERY-FIELD f x
;;   Return the contents of field F of manifest X
(define (query-field f x)
  (assert (manifest? x))
  (let ((field (assoc f (cdddr x))))
    (or (and field (cdr field)) '())))

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
;;  Expand shell glob GLOB, against files FILES
(define (glob-expand files glob)
  (map list->string
    (filter (curry matches-glob (string->list glob))
      (map string->list files))))

;; PKG-FILES available manifest
;;  Given a list of available files, expand the globs in the
;;  'code' clause of MANIFEST and return a list of the files
;;   which match.
(define (pkg-files available manifest)
  (assert (manifest? manifest))
  (apply append
    (map (curry glob-expand available)
      (query-field 'code manifest))))

)
