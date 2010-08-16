#!r6rs
;; UCL INSTALL CACHING
;;   Takes a package specifier, either a local file path, a
;;   symbolic package name, or a (name . version) pair, and
;;   returns the path to a local copy of the package.
(library (ucl install caching)
  (export pkg-cache ucl-update)
  (import (rnrs)
    (ucl prelude)
    (ucl process)
    (ucl filesystem)
    (ucl environment)
    (ucl install paths))

(define (repo-packages repo)
  (display (print "downloading %/.packages.gz\n" repo))
  (with-error (print "unable to pull package list from %" repo)
    (let ((text (shell "curl %/.packages.gz | gunzip" repo)))
      (call-with-port (open-string-input-port text) get-data))))

(define (ucl-update)
  (display "updating package listing\n")
  (mkpath ucl-cache)
  (write-file-data (print "%/.packages" ucl-cache)
    (map cons ucl-servers (map repo-packages ucl-servers))))

(define (packages-data)
  (unless (file-exists? (print "%/.packages" ucl-cache))
    (ucl-update))
  (read-file-data (print "%/.packages" ucl-cache)))

; given a repository, package name, and version, make sure the package is cached
(define (cache-from repo name version)
  (define (dottify vs)
    (apply string-append (intersperse "." (map number->string vs))))
  (define file (print "%-%.tar.gz" name (dottify version)))
  (unless (file-exists? (print "%/%" ucl-cache file))
    (display (print "caching '%/%'\n" repo file))
    (with-error (print "error: unable to download '%' from '%'" file repo)
      (shell "curl --create-dirs -o %/% %/%" ucl-cache file repo file)))
  (print "%/%" ucl-cache file))

(define (latest-version spec repo-data)
  (let ((repo (car repo-data)) (pkgs (cdr repo-data)))
    (let ((latest (assoc spec pkgs)))
      (if latest (cons repo latest) #f))))

(define (cache-latest spec)
  (define latest (exists (curry latest-version spec) (packages-data)))
  (unless latest
    (error 'cache-latest (print "no version of '%' in repositories" spec)))
  (cache-from (car latest) (cadr latest) (cddr latest)))

(define (exact-version spec repo-data)
  (let ((repo (car repo-data)) (pkgs (cdr repo-data)))
    (if (exists (curry equal? spec) pkgs) repo #f)))

(define (cache-specific spec)
  (define repo (exists (curry exact-version spec) (packages-data)))
  (unless repo
    (error 'cache-specific (print "could not find % in the repositories" spec)))
  (cache-from repo (car spec) (cdr spec)))

(define (pkg-cache spec)
  (cond
    ((string? spec) spec)
    ((symbol? spec) (cache-latest spec))
    ((list? spec) (cache-specific spec))
    (else (error 'pkg-cache "invalid package specifier"))))
)
