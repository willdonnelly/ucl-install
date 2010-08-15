#!r6rs
;; UCL INSTALL DOWNLOAD
;;   All relevant functionality for downloading and caching
;;   packages. Exports the function PKG-DOWNLOAD, which
;;   accepts a package name or a pair of (name . version),
;;   and returns the path to the locally cached file.
(library (ucl install download)
  (export pkg-download)
  (import (rnrs) (ucl prelude) (ucl process) (ucl environment))

;; REPO-PACKAGES repo
;;  Return a list of package filenames available from REPO
(define (repo-packages repo)
  (with-warning (print "warning: can't get package list from %\n" repo) '()
    (break-string #\newline (shell "curl %/.packages.gz | gunzip" repo))))

;; HAS-PACKAGE? package repo
;;   Is PACKAGE available from REPO?
(define (has-package? package repo)
  (define filenames (repo-packages repo))
  (define (pkg-name f)
    (list->string (reverse (cdr
      (drop-while (lambda (c) (not (equal? #\- c)))
        (reverse (string->list f)))))))
  (if (exists (curry equal? (symbol->string package)) (map pkg-name filenames))
      repo #f))

;; PKG-VERSIONS name pkgs
;;   Given a package filename list PKGS and a package NAME,
;;   return a list of all available versions in the list.
(define (pkg-versions name pkgs)
  (define (pkg-name f)
    (list->string (reverse (cdr
      (drop-while (lambda (c) (not (equal? #\- c)))
        (reverse (string->list f)))))))
  (define (pkg-version f)
    (map string->number
      (break-string #\.
        (substring f
          (+ (string-length (symbol->string name)) 1)
          (- (string-length f) 7)))))
  (map pkg-version
    (filter (lambda (f) (equal? (symbol->string name) (pkg-name f)))
      pkgs)))

;; PKG-NEWEST name pkgs
;;   Return the newest version of NAME in list PKGS
(define (pkg-newest name pkgs)
  (car (reverse
    (list-sort (lambda (a b) (not (exists > a b)))
      (pkg-versions name pkgs)))))

;; PKG-EXACT name vers pkgs
;;   Check that version VERS of package NAME is in PKGS.
;;   Returns VERS if true, errors out otherwise.
(define (pkg-exact name vers pkgs)
  (with-error (print "error: no such version % of package %\n" vers name)
    (assert (exists (lambda (v) (equal? vers v)) (pkg-versions name pkgs))))
  vers)

;; CACHE-PACKAGE name vers repo
;;   Ensure that package NAME version VERS is stored in UCL-CACHE,
;;   downloading it from REPO if necessary.
(define (cache-package name vers repo)
  (define ucl-cache (getenv "UCL_CACHE"))
  (define (dottify vs)
    (apply string-append (intersperse "." (map number->string vs))))
  (define filename (print "%-%.tar.gz" name (dottify vers)))
  (unless (file-exists? (print "%/%" ucl-cache filename))
    (with-error (print "error: unable to download % from %" filename repo)
      (begin
        (shell "mkdir -p %" ucl-cache)
        (shell "curl -o %/% %/%" ucl-cache filename repo filename))))
  (print "%/%" ucl-cache filename))

;; PKG-DOWNLOAD package
;;   Find PACKAGE in the repositories and download it to the cache if
;;   necessary, returning the eventual path to the file.
(define (pkg-download package)
  (define ucl-remotes (break-string #\space (getenv "UCL_SERVERS")))
  (define pkg-name (if (symbol? package) package (car package)))
  (define repo (exists (curry has-package? package) ucl-remotes))
  (with-error (print "error: no repository contains package %\n" package)
    (assert repo))
  (let ((version (if (symbol? package)
                     (pkg-newest pkg-name (repo-packages repo))
                     (pkg-exact pkg-name (cdr package) (repo-packages repo)))))
    (cache-package pkg-name version repo)))

)
