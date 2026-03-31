(use-modules (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-71)
             (guix channels)
             (guix derivations)
             (guix discovery)
             (guix gexp)
             (guix memoization)
             (guix monads)
             (guix packages)
             (guix store)
             (guix ui)
             (guix utils)
             (guix scripts pull)
             (guix build utils)
             (guix build-system linux-module)
             (gnu packages)
             (gnu packages package-management)
             (nongnu packages))

(define (kernel-or-kernel-module? package)
  (or (string-prefix? "linux" (package-name package))
      (eq? linux-module-build-system
           (package-build-system package))))

(define (derivation->output-path-closure drv)
  (define (derivation->output-paths drv)
    (map (match-lambda
           ((_ . output)
            (derivation-output-path output)))
         (derivation-outputs drv)))

  (append-map derivation->output-paths
              (cons drv
                    (map derivation-input-derivation
                         (derivation-prerequisites drv)))))

;; See also (@@ (guix scripts weather) package-outputs).
(define* (package-outputs packages #:optional (system (%current-system)))
  (define (lower-object/no-grafts obj system)
    (mlet* %store-monad ((previous (set-grafting #f))
                         (drv      (lower-object obj system))
                         (_        (set-grafting previous)))
      (return drv)))

  (let ((packages (filter (lambda (package)
                            (or (not (package? package))
                                (supported-package? package system)))
                          packages)))
    (format (current-error-port)
            "computing ~h package derivations for ~a...~%"
            (length packages) system)

    (foldm %store-monad
           (lambda (package result)
             (mlet %store-monad ((drv (lower-object/no-grafts package system)))
               (if (substitutable-derivation? drv)
                   (match (derivation->output-paths drv)
                     (((names . items) ...)
                      (return (append items result))))
                   (return result))))
           '()
           packages)))

;; See also (@ (gnu packages) all-packages).
(define all-packages
  (mlambda (modules)
    (define visited (make-hash-table))

    (fold-packages
     (lambda (package result)
       (if (hashq-ref visited package)
           result
           (begin
             (hashq-set! visited package #t)
             (match (package-replacement package)
               ((? package? replacement)
                (hashq-set! visited replacement #t)
                (cons* replacement package result))
               (#f
                `(,@(if (and (kernel-or-kernel-module? package)
                             (package-source package))
                        (list (package-source package))
                        '())
                  ,package
                  ,@result))))))
     '()
     modules
     #:select? (negate package-superseded))))

(define (store-file-name->hash+name file)
  ;; "/gnu/store/12h5bwhbl29ha47bqrrhdp499cbiaxlg-m4-1.4.19" ->
  ;; "12h5bwhbl29ha47bqrrhdp499cbiaxlg" + "m4-1.4.19"
  (values
   (substring file
              (+ 1 ;The slash after %store-directory.
                 (string-length (%store-directory)))
              (+ 1
                 (string-length (%store-directory))
                 %store-hash-string-length))
   (substring file
              (+ 2 ;The slash after %store-directory, and the dash after the hash.
                 (string-length (%store-directory))
                 %store-hash-string-length))))

(define (sort-and-dedup lst)
  (define (less a b)
    (let ((a-hash a-name (store-file-name->hash+name a))
          (b-hash b-name (store-file-name->hash+name b)))
      (cond
       ((string-ci=? a-name b-name)
        (string< a-hash b-hash))
       (else
        (string-ci< a-name b-name)))))
  (delete-duplicates
   (stable-sort lst less)))

(define (output-to file lst)
  (with-atomic-file-output file
    (lambda (port)
      (for-each
       (cut format port "~a~%" <>)
       (sort-and-dedup lst)))))


;;;
;;; Entry point.
;;;

(define %systems
  '("x86_64-linux"
    "aarch64-linux"))

(define %guix
  (with-store store
    (derivation->output-path-closure
     (run-with-store store
       (package->derivation
        (guix-for-channels
         (list (find (lambda (channel)
                       (eq? (channel-name channel) 'guix))
                     (channel-list '((channel-file . "channels.lock"))))))
        #:graft? #f)))))

(define %guix-packages
  (let ((modules (all-modules %default-package-module-path)))
    (with-store store
      (concatenate
       (run-with-store store
         (mapm %store-monad
               (cut package-outputs (all-packages modules) <>)
               %systems))))))

(define %nonguix-packages
  (let ((modules (all-modules %nongnu-package-module-path)))
    (with-store store
      (concatenate
       (run-with-store store
         (mapm %store-monad
               (cut package-outputs (all-packages modules) <>)
               %systems))))))

(begin
  (output-to "guix.txt"
             %guix)
  (output-to "guix-packages.txt"
             (lset-difference string=?
                              %guix-packages
                              %guix))
  (output-to "nonguix-packages.txt"
             (lset-difference string=?
                              %nonguix-packages
                              (append %guix %guix-packages))))
