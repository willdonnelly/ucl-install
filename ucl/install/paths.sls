#!r6rs
(library (ucl install paths)
  (export ucl-servers ucl-cache ucl-files meta-file pkgs-file code-file)
  (import (rnrs) (ucl prelude) (ucl environment))

(define ucl-servers (break-string #\space (getenv "UCL_SERVERS")))
(define ucl-cache (getenv "UCL_CACHE"))
(define ucl-files (getenv "UCL_FILES"))

(define (meta-file pkg) (print "%/.meta/%.meta" ucl-files pkg))
(define (pkgs-file) (print "%/.meta/packages-installed" ucl-files))
(define (code-file file) (string-append ucl-files "/" file))

)
