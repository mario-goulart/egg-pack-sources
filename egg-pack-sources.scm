(module egg-pack-sources ()

(import scheme chicken)
(use srfi-1 srfi-13 files posix utils extras irregex data-structures setup-api)

(define chicken-install
  (make-parameter "chicken-install"))

(define force-versions?
  (make-parameter #f))

;; for installing eggs from the installer script
(define chicken-install-args
  (make-parameter ""))

(define installer-script "install.sh")

(define installer
  (let ((eggs '()))
    (lambda (#!optional egg)
      (if egg
          (set! eggs (cons egg eggs))
          (reverse (delete-duplicates eggs equal?))))))

(define eggs/versions
  ;; Eggs with versions as specified by user
  (let ((e/v '()))
    (lambda args
      (cond ((null? args)
             e/v)
            ((null? (cdr args))
             (alist-ref (car args) e/v equal?))
            (else
             (set! e/v (alist-update! (car args) (cadr args) e/v)))))))

(define (fetch-egg egg)
  (system* (sprintf "~a -r ~a" (chicken-install) egg)))

(define (egg-dependencies meta-file)
  (let ((meta-data (with-input-from-file meta-file read)))
    (define (deps key)
      (or (and-let* ((d (assq key meta-data)))
            (cdr d))
          '()))
    (append (deps 'depends)
            (deps 'needs))))

(define (egg-pack-sources egg:version)
  (let* ((egg-tokens (string-split egg:version ":"))
         (egg (car egg-tokens))
         (version (and (not (null? (cdr egg-tokens)))
                       (cadr egg-tokens))))
    (fetch-egg egg:version)
    (installer egg)
    (let ((deps (egg-dependencies (make-pathname egg egg "meta"))))
      (for-each (lambda (dep)
                  (let* ((version (and (pair? dep)
                                       (cadr dep)))
                         (dep (->string (if (pair? dep)
                                            (car dep)
                                            dep)))
                         (requested-version (eggs/versions dep)))
                    (when (and version
                               requested-version
                               (not (force-versions?))
                               (not (version>=? requested-version version)))
                      (fprintf (current-error-port)
                               (string-append
                                "You requested version ~a for ~a, but ~a depends on version ~a. "
                                "Aborting.\n")
                               requested-version
                               dep
                               egg
                               version)
                      (exit 1))
                    (unless (directory? dep)
                      (egg-pack-sources dep)
                      (installer egg))))
                deps))
    (write-installer!)))

(define (write-installer!)
  (with-output-to-file installer-script
    (lambda ()
      (print "#!/bin/sh")
      (for-each (lambda (egg)
                  (printf "cd ~a; ~a ~a; cd ..\n"
                          egg
                          (chicken-install)
                          (chicken-install-args)))
                (installer))))
  (change-file-mode installer-script
                    (bitwise-ior perm/irwxu
                                 perm/ixgrp perm/irgrp
                                 perm/iroth perm/ixoth)))

(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port #<#EOF
Usage: #this [ <options> ] <egg1>[:<version>] [ <egg2>[:<version>] ... ]
       #this --help | -help | -h

<options>:

--output-dir=<outdir>
  directory where to write egg sources (default: $PWD)

--chicken-install=<path>
  path to chicken-install (default: get from $PATH)

--chicken-install-args=<args>
  arguments for chicken-install (default: empty)

--force-versions
  fetch versions specified on the command the line, even if they don't
  satisfy the requirements by other eggs.

EOF
)
    (when exit-code (exit exit-code))))


(let* ((args (command-line-arguments))
       (non-option-args (remove (lambda (arg)
                                  (string-prefix? "-" arg))
                                args)))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (when (null? non-option-args)
    (usage 1))

  (chicken-install (or (cmd-line-arg '--chicken-install args)
                       (chicken-install)))

  (chicken-install-args (or (cmd-line-arg '--chicken-install-args args)
                            (chicken-install-args)))

  (force-versions? (and (member "--force-versions" args) #t))

  (let ((outdir (or (cmd-line-arg '--output-dir args)
                    (current-directory))))

    (create-directory outdir 'with-parents)
    (change-directory outdir)

    (let* ((eggs non-option-args)
           (eggs-with-version
            (filter (lambda (egg)
                      (string-index egg #\:))
                    eggs))
           ;; prioritize packing versioned eggs -- the requested
           ;; versions won't be clobbered in case those eggs are
           ;; dependencies of other eggs
           (eggs/with-version-first
            (append eggs-with-version
                    (remove (lambda (egg)
                              (string-index egg #\:))
                            eggs))))
      (for-each (lambda (egg)
                  (apply eggs/versions (string-split egg ":")))
                eggs-with-version)
      (for-each (lambda (egg)
                  (egg-pack-sources egg))
                eggs/with-version-first))))

) ;; end module
