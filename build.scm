(import (chicken base) (chicken format) (chicken process) (chicken process-context) srfi-1 srfi-13 compile-file)
(define args (command-line-arguments))

(define (plot-try-compile header ldflags cppflags)
  (and (try-compile 
	(string-append "#include <stdio.h>\n"
		       header "\n" 
		       "int main(int argc, char **argv) { const char *s = pl_libplot_ver; return 0; }\n")
	ldflags: ldflags
	cflags: cppflags
        verbose: #t)
       (cons ldflags cppflags)))

(define-syntax plot-test 
  (syntax-rules ()
    ((_ (flags ...))
     (condition-case (plot-try-compile flags ...)
		     (t ()    #f)))))

(define ld+cpp-options
  (or (plot-test ("#include <plot.h>" "-lplot" ""))
      (plot-test ("#include <plot.h>" "-lplot" "-I/usr/include/plot"))
      (plot-test ("#include <plot.h>" "-lplot" "-I/opt/local/include"))
      (error "unable to figure out location of libplot")))

(define cmd (intersperse (append args (filter identity
                                       (list (sprintf "-L \"~A\"" (car ld+cpp-options)) 
                                             (and (not (string-null? (cdr ld+cpp-options)))
                                                  (sprintf "\"~A\"" (cdr ld+cpp-options)))))) " "))
(print cmd)
(system (string-concatenate cmd))

