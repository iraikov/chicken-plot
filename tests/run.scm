
(use plot)


(define (ccurve plotter  maxorder dx dy order)
  (if (>= order maxorder)
      (fcontrel plotter dx dy)
      (begin
	(ccurve plotter maxorder
		(* 0.5 (- dx dy))
		(* 0.5 (+ dx dy))
		(+ order 1))
	(ccurve plotter maxorder
		(* 0.5 (+ dy dx))
		(* 0.5 (- dy dx))
		(+ order 1)))
      ))


(define (simple-test plotter max-order)
  (openpl plotter)
  (fspace plotter 0. 0. 1000. 1000.)
  (flinewidth plotter 0.25 )
  (pencolorname plotter "blue" )
  (erase plotter )
  (fmove plotter 600. 300. )
  (ccurve plotter max-order 0. 400. 0 )
  (closepl plotter)
  )



(define (main)
  (print "libplot version: " (libplot-version))
  (let ((plotter (make-plotter (PNG) (open-output-file "testplot.png")
			       (list (PAGESIZE "A4") (INTERLACE #t)
				     (X_AUTO_FLUSH #f) (META_PORTABLE #t)))))
    (simple-test plotter 5)
    (delete-plotter plotter)))

(main)
