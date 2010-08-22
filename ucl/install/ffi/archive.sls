#!r6rs
;; UCL INSTALL FFI ARCHIVE
;;   Routines for manipulating archive files via the FFI
(library (ucl install ffi archive)
  (export archive-contents
          archive-file
          archive-file-data)
  (import (rnrs) (ucl prelude) (ucl ffi))

(define libarchive (load-library "libarchive.so"))

(define arch-new
  (get-function libarchive
    "archive_read_new" '() 'pointer))
(define arch-cmp
  (get-function libarchive
    "archive_read_support_compression_all" '(pointer) 'sint))
(define arch-fmt
  (get-function libarchive
    "archive_read_support_format_all" '(pointer) 'sint))
(define arch-open
  (get-function libarchive
    "archive_read_open_filename" '(pointer string uint) 'sint))
(define arch-next
  (get-function libarchive
    "archive_read_next_header" '(pointer pointer) 'sint))
(define arch-path
  (get-function libarchive
    "archive_entry_pathname" '(pointer) 'pointer))
(define arch-extract
  (get-function libarchive
    "archive_read_extract" '(pointer pointer sint) 'sint))
(define arch-size
  (get-function libarchive
    "archive_entry_size" '(pointer) 'slong))
(define arch-data
  (get-function libarchive
    "archive_read_data" '(pointer pointer uint) 'sint))
(define arch-finish
  (get-function libarchive
    "archive_read_finish" '(pointer) 'sint))

(define libc (load-library "libc.so.6" "libc.so"))

(define chdir
  (get-function libc "chdir" '(string) 'sint))

(define (with-archive path proc)
  (define arc (arch-new))
  (unless (file-exists? path)
    (error 'archive-contents "file does not exist" path))
  (arch-cmp arc)
  (arch-fmt arc)
  (unless (zero? (arch-open arc path 10240))
    (error 'archive-contents "error opening file" path))
  (let ((result (proc arc)))
    (arch-finish arc)
    result))

(define (with-entry arc proc)
  (define ent (malloc (sizeof 'pointer)))
  (let ((result
    (call/cc
      (lambda (exit)
        (let loop ()
          (if (zero? (arch-next arc ent))
              (begin
                (proc (pointer-get 'pointer 0 ent) exit)
                (loop))
              (exit #f)))))))
    (free ent)
    result))

(define (archive-contents path)
  (define names '())
  (with-archive path
    (lambda (arc)
      (with-entry arc
        (lambda (ent exit)
          (set! names (cons (string-read (arch-path ent)) names))))))
  names)

(define (archive-file archive-path file-path dest-path)
  (unless (zero? (chdir dest-path))
    (error 'archive-file "unable to change directory" dest-path))
  (unless (with-archive archive-path (lambda (arc)
            (with-entry arc (lambda (ent exit)
              (when (equal? (string-read (arch-path ent)) file-path)
                (exit (zero? (arch-extract arc ent #x200))))))))
    (error 'archive-file "no such file in archive" archive-path file-path)))

(define (archive-file-data archive-path file-path)
  (with-archive archive-path (lambda (arc)
    (with-entry arc (lambda (ent exit)
      (when (equal? (string-read (arch-path ent)) file-path)
        (let* ((size (arch-size ent))
               (mem (malloc size)))
          (unless (equal? size (arch-data arc mem size))
            (error 'archive-data "error extracting file contents"
              archive-path file-path))
          (exit
            (bytevector->string
              (bytevector-read mem size)
              (native-transcoder))))))))))

)
