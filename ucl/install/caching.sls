#!r6rs
;; UCL INSTALL CACHING
;;   Takes a package specifier, either a local file path, a
;;   symbolic package name, or a (name . version) pair, and
;;   returns the path to a local copy of the package.
(library (ucl install caching)
  (export pkg-cache)
  (import (rnrs) (ucl prelude) (ucl process) (ucl environment))

(define ucl-servers (break-string #\space (getenv "UCL_SERVERS")))
(define ucl-cache (getenv "UCL_CACHE"))

(define (repo-packages repo)
  (define text (shell "curl %/.packages.gz | gunzip" repo))
  (define port (open-string-input-port text))
  (call-with-port port get-data))

; given a repository, package name, and version, make sure the package is cached
(define (cache-from repo name version)
  (define (dottify vs)
    (apply string-append (intersperse "." (map number->string vs))))
  (define file (print "%-%.tar.gz" name (dottify version)))
  (unless (file-exists? (print "%/%" ucl-cache file))
    (with-error (print "error: unable to download '%' from '%'" file repo)
      (shell "curl --create-dirs -o %/% %/%" ucl-cache file repo file)))
  (print "%/%" ucl-cache file))

(define (latest-version spec repo)
  (let ((latest (assoc spec (repo-packages repo))))
    (if latest (cons repo latest) #f)))

(define (cache-latest spec)
  (define latest (exists (curry latest-version spec) ucl-servers))
  (unless latest
    (error 'cache-latest (print "no version of '%' in repositories" spec)))
  (cache-from (car latest) (cadr latest) (cddr latest)))

(define (exact-version spec repo)
  (if (exists (curry equal? spec) (repo-packages repo))
      repo
      #f))

(define (cache-specific spec)
  (define repo (exists (curry exact-version spec) ucl-servers))
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
