#!r6rs
(library (ucl install paths)
  (export ucl-servers ucl-cache ucl-files
          meta-file pkgs-file code-file
          cache-file cache-idxs)
  (import (rnrs) (ucl prelude) (ucl environment))

(define ucl-servers (break-string #\space (getenv "UCL_SERVERS")))
(define ucl-cache (getenv "UCL_CACHE"))
(define ucl-files (getenv "UCL_FILES"))

(define (meta-file pkg) (template "%/.meta/%.meta" ucl-files pkg))
(define (pkgs-file) (template "%/.meta/packages-installed" ucl-files))
(define (code-file file) (string-append ucl-files "/" file))

(define (cache-file f) (string-append ucl-cache "/" f))
(define (cache-idxs) (string-append ucl-cache "/.packages"))

)
