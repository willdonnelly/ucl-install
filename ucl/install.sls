#!r6rs
(library (ucl install)
  (export ucl-install ucl-uninstall)
  (import (rnrs) (ucl prelude) (ucl process) (ucl environment)
    (ucl install download) (ucl install manifest))

;; FETCH-DEPS pkg depslist
;;   Fetch all unmet dependencies listed in DEPSLIST and write PKG to the
;;   reverse dependencies files.
(define (fetch-deps pkg depslist)
  (define ucl-files (getenv "UCL_FILES"))
  (define packages
    (append
      (filter (lambda (p) (symbol? p)) depslist)
      (filter (lambda (p) (and (list? p) (equal? 'package (car p)))) depslist)))
  (define (install-deps package)
    (unless (file-exists? (print "%/.meta/%/installed" ucl-files package))
      (ucl-install package)))
  (define (write-rdeps package)
    (shell "mkdir -p %/.meta/%/rdep/" ucl-files package)
    (shell "touch %/.meta/%/rdep/%" ucl-files package pkg))
  (map install-deps packages)
  (map write-rdeps packages))

;; FILE-OWNER f
;;   Given a file F, figure out which package installed it
(define (file-owner f)
  (define ucl-files (getenv "UCL_FILES"))
  (define owner
    (with-error (print "error: no package owns file %\n" f)
      (shell "grep -lx '%' %/.meta/*/owns" f ucl-files)))
  (define drop-front (string-length (print "%/.meta/" ucl-files)))
  (define drop-back (string-length "/owns"))
  (string->symbol
    (substring owner drop-front (- (string-length owner) drop-back))))

;; PKG-INSTALL path
;;   Given PATH to a package to be installed, ensure that all
;;   dependencies are installed, resolve file conflicts, and
;;   install the files.
(define (pkg-install path)
  (define ucl-files (getenv "UCL_FILES"))
  ;; grab the manifest from the package file
  (define manifest
    (with-error "error: unable to read package manifest, is it malformed?\n"
      (get-datum (open-string-input-port (shell "tar -xOf % MANIFEST" path)))))
  ;; ensure that all dependencies are installed
  (fetch-deps (pkg-name manifest) (query-field 'depends manifest))
  ;; expand the code globs against the package contents
  (let* ((contents (break-string #\newline (shell "tar -tf %" path)))
         (install (pkg-files contents manifest)))
    ;; check for file conflicts and ask the user to remove their owners
    (let* ((existing? (lambda (f) (file-exists? (print "%/%" ucl-files f))))
           (conflicts (filter existing? install))
           (owners (nub (map file-owner conflicts))))
      (for ((owner owners))
        (display
          (print "'%' would conflict with files installed by '%'. Please uninstall '%'.\n"
            (pkg-name manifest) owner owner))))
    ;; create package metadata
    (shell "mkdir -p %/.meta/%/" ucl-files (pkg-name manifest))
    (shell "mkdir -p %/.meta/%/rdep/" ucl-files (pkg-name manifest))
    (shell "touch %/.meta/%/installed" ucl-files (pkg-name manifest))
    (shell "touch %/.meta/%/owns" ucl-files (pkg-name manifest))
    (shell "touch %/.meta/%/syms" ucl-files (pkg-name manifest))
    ;; install the package files
    (shell "mkdir -p %" ucl-files)
    (for ((file install))
      (shell "tar -x -C % -f % %" ucl-files path file)
      (shell "echo '%' >> %/.meta/%/owns" file ucl-files (pkg-name manifest)))
    ;; create any necessary symlinks
    (for ((link (query-field 'symlink manifest)))
      (shell "ln -s %/% %/%" ucl-files (car link) ucl-files (cdr link))
      (shell "echo '%' >> %/.meta/%/syms" (cdr link) ucl-files (pkg-name manifest)))))

;; UCL-INSTALL package
;;   Install PACKAGE, which can be either a file path to install
;;   or a name to be looked up in the repositories.
(define (ucl-install package)
  (display (print "installing package %\n" package))
  (if (string? package)
      (pkg-install package)
      (pkg-install (pkg-download package))))

;; UCL-UNINSTALL pkg-name - Remove PKG-NAME from the library source tree
(define (ucl-uninstall package)
  (define ucl-files (getenv "UCL_FILES"))
  (unless (file-exists? (print "%/.meta/%/installed" ucl-files package))
    (error 'ucl-uninstall (print "error: package '%' is not installed" package)))
  (let ((rdep-ls
          (with-warning "" ""
            (shell "ls -1 %/.meta/%/rdep/" ucl-files package))))
    (let ((rdeps (if (string? rdep-ls) (break-string #\newline rdep-ls) '()))
          (files (break-string #\newline (shell "cat %/.meta/%/owns" ucl-files package)))
          (links (break-string #\newline (shell "cat %/.meta/%/syms" ucl-files package))))
      (display (print "removing rdeps: %\n" rdeps))
      (map ucl-uninstall (map string->symbol rdeps))

      (display (print "removing files: %\n" files))
      (map (lambda (f) (shell "rm %/%" ucl-files f)) files)

      (display (print "removing symlinks: %\n" links))
      (map (lambda (f) (shell "rm %/%" ucl-files f)) links)

      (display "removing metadata\n")
      (shell "rm -r %/.meta/%" ucl-files package)
      (shell "rm -f %/.meta/*/rdep/%" ucl-files package)
      (shell "cd % && find . -depth -type d -empty -delete" ucl-files))))

)

;; UCL-INSTALL Procedure
;; =====================
;;
;; If it's not a string
;;   Find the first repository containing pkg-name
;;   If it's not versioned
;;     Check the if latest version is cached
;;   If it is versioned
;;     Check if the requested version is cached
;;   If it's not cached, download it
;; For all unmet dependencies, recurse and install
;; For all dependencies, create the appropriate rdep files
;; If there are file conflicts
;;   Look up which packages installed them
;;   Ask the user if they want the packages removed
;;     If so, remove them and continue
;; Install files from the package and write filenames to the owns file
;;
;; UCL-UNINSTALL Procedure
;; =======================
;;
;; For all reverse dependencies
;;   Go uninstall them first
;; For all installed files
;;   Go remove them next
;; Delete the metadata directory
;; Delete all files '.meta/*/rdep/<package>'
