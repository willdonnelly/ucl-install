#!r6rs
(library (ucl install)
  (export ucl-update ucl-install ucl-uninstall)
  (import (rnrs)
    (ucl prelude)
    (ucl filesystem)
    (ucl environment)
    (ucl install paths)
    (ucl install caching)
    (ucl install package))

(define (ucl-install pkgspec)
  (ucl-install* pkgspec #t))

(define (ucl-install* pkgspec explicit)
  (define path (pkg-cache pkgspec))
  (define data (extract-manifest path))
  (for ((dep (package-depends data)))
    (unless (package-installed? dep)
      (unless (equal? dep (package-name data)) ;; prevent infinite loops
        (display (template "'%' depends on '%'\n" (package-name data) dep))
        (ucl-install* dep #f))))
  (let ((meta (install-package path data)))
    (write-file-data (meta-file (package-name data))
      (append-metadata data meta explicit)))
  (with-file-data (pkgs-file)
    (curry cons (package-name data))))

(define (package-installed? pkg)
  (file-exists? (meta-file pkg)))

(define (install-package path data)
  (display (template "installing package '%'\n" path))
  (mkpath (string-append ucl-files "/.meta"))
  (let ((files (package-matching path (query-field 'code data)))
        (symlinks (query-field 'symlink data)))
    (for ((file (append files (map cdr symlinks))))
      (when (file-exists? (code-file file))
        (error 'install-package
          (template "file '%' conflicts with package '%'" file
            (file-owner file)))))
    (for ((file files))
      (display (template "installing file '%'\n" file))
      (package-file path file ucl-files))
    (for ((sym symlinks))
      (display (template "symlinking '%' to '%'\n" (car sym) (cdr sym)))
      (symlink (code-file (car sym)) (code-file (cdr sym))))
    `((files . ,(append files (map cdr symlinks))))))

(define (file-owner file)
  (define (owns pkg)
    (and
      (member file
        (cdr
          (assoc 'files
            (read-file-data (meta-file pkg)))))
      pkg))
  (exists owns (read-file-data (pkgs-file))))

(define (append-metadata data meta explicit)
  `((package ,(package-name data))
    (version . ,(query-field 'version data))
    (depends . ,(package-depends data))
    (explicit . ,explicit) . ,meta))

(define (ucl-uninstall pkgname)
  (unless (package-installed? pkgname)
    (error 'ucl-uninstall (template "package '%' is not installed\n" pkgname)))
  (for ((rdep (find-rdeps pkgname)))
    (display (template "'%' depends on '%'\n" rdep pkgname))
    ;; avoid recursive issues, however unlikely they are
    (unless (equal? rdep pkgname)
      ;; it could have been removed when we
      ;; uninstalled a prior rdep
      (when (package-installed? rdep)
        (ucl-uninstall rdep))))
  (display (template "removing '%'\n" pkgname))
  (let ((data (read-file-data (meta-file pkgname))))
    (for ((file (cdr (assoc 'files data))))
      (display (template "removing file '%'\n" file))
      (delete-file (code-file file))))
  (with-file-data (pkgs-file)
    (curry remove pkgname))
  (delete-file (meta-file pkgname)))

(define (find-rdeps pkg)
  (filter (curry depends-on pkg)
    (read-file-data (pkgs-file))))

(define (depends-on dep pkgname)
  (define data (read-file-data (meta-file pkgname)))
  (member dep (cdr (assoc 'depends data))))
)
