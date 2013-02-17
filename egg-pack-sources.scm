(module egg-pack-sources ()

(import scheme chicken)
(use srfi-1 srfi-13 files posix utils extras irregex data-structures)

(define chicken-install
  (make-parameter "chicken-install"))

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

(define (fetch-egg egg)
  (system* (sprintf "~a -r ~a" (chicken-install) egg)))

(define (egg-dependencies meta-file)
  (let ((meta-data (with-input-from-file meta-file read)))
    (define (deps key)
      (or (and-let* ((d (assq key meta-data)))
            (cdr d))
          '()))
    (map (lambda (dep)
           (if (pair? dep)
               (car dep)
               dep))
         (append (deps 'depends)
                 (deps 'needs)))))

(define (egg-pack-sources egg)
  (fetch-egg egg)
  (installer egg)
  (let ((deps (egg-dependencies (make-pathname egg egg "meta"))))
    (for-each (lambda (dep)
                (let ((dep (symbol->string dep)))
                  (unless (directory? dep)
                    (egg-pack-sources dep)
                    (installer egg))))
              deps))
  (write-installer!))

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
                  (current-output-port))))
    (fprintf port "Usage: ~a [ <options> ] <egg1> [ <egg2> ... ]\n"
             (pathname-strip-directory (program-name)))
    (fprintf port "\n<options>:\n")
    (fprintf port "  --output-dir=<outdir>          directory where to write egg sources (default: $PWD)\n")
    (fprintf port "  --chicken-install=<path>       path to chicken-install (default: get from $PATH)\n")
    (fprintf port "  --chicken-install-args=<args>  arguments for chicken-install (default: empty)\n"))
  (when exit-code (exit exit-code)))


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

  (let ((outdir (or (cmd-line-arg '--output-dir args)
                    (current-directory))))

    (create-directory outdir 'with-parents)
    (change-directory outdir)

    (for-each (lambda (egg)
                (egg-pack-sources egg))
              non-option-args)))

) ;; end module
