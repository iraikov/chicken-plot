
;;
;; Chicken interface to the libplot API.
;;
;; Copyright 2011 Ivan Raikov.
;;
;; Based on the Ocamlplot library by Olivier Andrieu.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(define stub_pl_linedash_r
  (foreign-lambda* void ((ptr_plPlotter plotter)
			 (value dashes)
			 (unsigned-int offset))
#<<END
     int *c_dashes, i, n ;
     value l;

     if (!(Val_is_truep(Val_is_pairp(dashes))))
	  chicken_error("invalid dashes value", "linedash", dashes);

     n = Val_list_length(dashes);

     CMalloc(c_dashes, n);
     {
	  for (i = 0, l = dashes; !(Val_is_truep(Val_is_null_listp(l))); i++, l = Val_cdr(l)) 
	  {
	       c_dashes[i] = Val_to_int(Val_car(l));
	  }
     }
     
     pl_linedash_r(plotter, n, c_dashes, Val_to_int(offset)) ;
     
     free(c_dashes) ;		/* Guess it's safe ... */
END
))


(define stub_pl_flinedash_r
  (foreign-lambda* void ((ptr_plPlotter plotter)
			 (value dashes)
			 (unsigned-int offset))
#<<EOF
     double *c_dashes ;
     int i, n; value l; 
	
     if (!(Val_is_truep(Val_is_pairp(dashes))))
	  chicken_error("invalid dashes value", "flinedash", dashes);

     n = Val_list_length(dashes);

     CMalloc(c_dashes, n);
     { 
	  for (i = 0, l = dashes; !(Val_is_truep(Val_is_null_listp(l))); i++, l = Val_cdr(l)) 
	  {
	       c_dashes[i] = Val_to_double(Val_car(l));
	  }

     }
     pl_flinedash_r(plotter, n, c_dashes, Val_to_double(offset)) ;
     free(c_dashes) ;		/* Guess it's safe ... */
EOF
))


(define stub_pl_alabel_r
  (foreign-lambda* void ((ptr_plPlotter plotter)
			 (h-justify x_justify)
			 (v-justify y_justify)
			 (c-string s))
#<<EOF
     char h_just[] = { 'l', 'c', 'r' };
     char v_just[] = { 'b', 'x', 'c', 'C', 't' };

     pl_alabel_r(plotter, h_just[ x_justify ],  v_just[ y_justify ], s);
EOF
))


(define stub_pl_marker_r
  (foreign-lambda void "pl_marker_r" 
		  (nonnull-c-pointer plPlotter)
		  unsigned-int 
		  unsigned-int 
		  unsigned-int
		  unsigned-int))

;; TODO: invoke this using marker enum type projection/injection

(define stub_pl_markerrel_r
  (foreign-lambda void "pl_markerrel_r" 
		  (nonnull-c-pointer plPlotter)
		  unsigned-int
		  unsigned-int
		  unsigned-int
		  unsigned-int))

;; TODO: invoke this using marker enum type projection/injection

(define stub_pl_fmarker_r
  (foreign-lambda void "pl_fmarker_r" 
		  (nonnull-c-pointer plPlotter)
		  double
		  double
		  unsigned-int
		  double))

;; TODO: invoke this using marker enum type projection/injection

(define stub_pl_fmarkerrel_r
  (foreign-lambda void "pl_fmarkerrel_r" 
		  (nonnull-c-pointer plPlotter)
		  double
		  double
		  unsigned-int 
		  double))

;; TODO: invoke this using marker enum type projection/injection


(define stub_pl_capmod_r
  (foreign-lambda* void ((ptr_plPlotter plotter)
			 (capmod capmod))
#<<EOF
  char *mode[] = { "butt", "round", "projecting", "triangular" } ;
  pl_capmod_r(plotter, mode[ capmod ]);
EOF
))

(define stub_pl_fillmod_r
  (foreign-lambda* void ((ptr_plPlotter plotter)
			 (fillmod fillmod))
#<<EOF
  char *mode[] = { "even-odd", "nonzero-winding" } ;

  pl_fillmod_r(plotter, mode[ fillmod ]);
EOF
))


(define stub_pl_joinmod_r
  (foreign-lambda* void ((ptr_plPlotter plotter)
			 (joinmod joinmod))
#<<EOF
  char *mode[] = { "miter", "round", "bevel" } ;

  pl_joinmod_r(plotter, mode[ joinmod ]);
EOF
))


(define stub_pl_linemod_r
  (foreign-lambda* void ((ptr_plPlotter plotter)
			 (linemod linemod))
#<<EOF
  char *mode[] = { "solid", "dotted", "dotdashed", "shortdashed", "longdashed",
		   "dotdotdashed", "dotdotdotdashed", "disconnected" } ;

  pl_linemod_r(plotter, mode[ linemod ]);
EOF
))

(define stub_pl_orientation_r
  (foreign-lambda* void ((ptr_plPlotter plotter)
			 (direction direction))
#<<EOF
  int mode[] = { -1, 1 } ;

  pl_orientation_r(plotter, mode[ direction ]);
EOF
))


(define stub_pl_labelwidth_r
  (foreign-lambda long "pl_labelwidth_r"
		  ptr_plPlotter
		  c-string))


(define stub_pl_flabelwidth_r
  (foreign-lambda double "pl_flabelwidth_r"
		  ptr_plPlotter
		  c-string))


(define stub_pl_fontname_r
  (foreign-lambda long "pl_fontname_r"
		  ptr_plPlotter
		  c-string))


(define stub_pl_fontsize_r
  (foreign-lambda long "pl_fontsize_r"
		  ptr_plPlotter
		  unsigned-int))


(define stub_pl_ffontsize_r
  (foreign-lambda double "pl_ffontsize_r"
		  ptr_plPlotter
		  double))


(define stub_pl_textangle_r
  (foreign-lambda long "pl_textangle_r"
		  ptr_plPlotter
		  unsigned-int))


(define stub_pl_ftextangle_r
  (foreign-lambda double "pl_textangle_r"
		  ptr_plPlotter
		  double))


(define stub_pl_bgcolor_r
  (foreign-lambda* void 
		  ((ptr_plPlotter plotter)
		   (value color))
#<<EOF
  int red,green,blue;

  separate_colors(color, &red, &green, &blue);

  pl_bgcolor_r(plotter, red, green, blue);
EOF
))		  


(define stub_pl_color_r
  (foreign-lambda* void 
		  ((ptr_plPlotter plotter)
		   (value color))
#<<EOF
  int red,green,blue;

  separate_colors(color, &red, &green, &blue);

  pl_color_r(plotter, red, green, blue);
EOF
))


(define stub_pl_fillcolor_r
  (foreign-lambda* void 
		  ((ptr_plPlotter plotter)
		   (value color))
#<<EOF
  int red,green,blue;

  separate_colors(color, &red, &green, &blue);

  pl_fillcolor_r(plotter, red, green, blue);
EOF
))


(define stub_pl_pencolor_r
  (foreign-lambda* void 
		  ((ptr_plPlotter plotter)
		   (value color))
#<<EOF
  int red,green,blue;

  separate_colors(color, &red, &green, &blue);

  pl_pencolor_r(plotter, red, green, blue);
EOF
))


;;  Control Functions 


(define stub_pl_openpl_r
  (foreign-lambda void "pl_openpl_r" ptr_plPlotter))



(define stub_pl_bgcolorname_r
  (foreign-lambda void "pl_bgcolorname_r"
		  ptr_plPlotter
		  c-string))


(define stub_pl_erase_r
  (foreign-lambda void "pl_erase_r"
		  ptr_plPlotter))



(define stub_pl_space_r
  (foreign-lambda void "pl_space_r"
		  ptr_plPlotter
		  unsigned-int unsigned-int
		  unsigned-int unsigned-int))


(define stub_pl_fspace_r
  (foreign-lambda void "pl_fspace_r"
		  ptr_plPlotter
		  double double
		  double double))


(define stub_pl_space2_r
  (foreign-lambda void "pl_space2_r"
		  ptr_plPlotter
		  unsigned-int unsigned-int
		  unsigned-int unsigned-int
		  unsigned-int unsigned-int))


(define stub_pl_fspace2_r
  (foreign-lambda void "pl_fspace2_r"
		  ptr_plPlotter
		  double double
		  double double
		  double double))


(define stub_pl_havecap_r
  (foreign-lambda void "pl_havecap_r"
		  ptr_plPlotter
		  c-string))


(define stub_pl_flushpl_r
  (foreign-lambda void "pl_flushpl_r"
		  ptr_plPlotter))

(define stub_pl_closepl_r
  (foreign-lambda void "pl_closepl_r"
		  ptr_plPlotter))


;;  Object Drawing Functions 
  


(define-syntax define-shape-plot
  (er-macro-transformer
  (lambda (x r c)
    (match-let (((_ name arity) x))
	       (let ((plname     (string->symbol (sprintf "pl_~A_r" name)))
		     (stubname   (string->symbol (sprintf "stub_pl_~A_r" name)))
		     (fplname    (string->symbol (sprintf "pl_f~A_r" name)))
		     (fstubname  (string->symbol (sprintf "stub_pl_f~A_r" name)))
		     (relplname     (string->symbol (sprintf "pl_~Arel_r" name)))
		     (relstubname   (string->symbol (sprintf "stub_pl_~Arel_r" name)))
		     (frelplname    (string->symbol (sprintf "pl_f~Arel_r" name)))
		     (frelstubname  (string->symbol (sprintf "stub_pl_f~Arel_r" name)))
		     (%begin (r 'begin)))
		 `(,%begin
		   (define-plotter-stub ,stubname ,plname
		     . ,(list-tabulate arity (lambda (x) 'int)))
		   (define-plotter-stub ,fstubname ,fplname
		     . ,(list-tabulate arity (lambda (x) 'double)))
		   (define-plotter-stub ,relstubname ,relplname
		     . ,(list-tabulate arity (lambda (x) 'int)))
		   (define-plotter-stub ,frelstubname ,frelplname
		     . ,(list-tabulate arity (lambda (x) 'double)))
		   ))
               ))
  ))
	

;; TODO: check signedness
(define-shape-plot arc 6)

(define-shape-plot bezier2 6)

(define-shape-plot bezier3 8)

(define-shape-plot box 4)

(define-shape-plot circle 3)

(define-shape-plot cont 2)

(define-shape-plot ellarc 6)

(define-shape-plot ellipse 5)

(define-shape-plot line 4)

(define-shape-plot point 2)

(define-shape-plot move 2)


(define stub_pl_endpath_r
  (foreign-lambda void "pl_endpath_r"
		  ptr_plPlotter))


(define stub_pl_endsubpath_r
  (foreign-lambda void "pl_endsubpath_r"
		  ptr_plPlotter))


(define stub_pl_closepath_r
  (foreign-lambda void "pl_closepath_r"
		  ptr_plPlotter))


(define stub_pl_label_r
  (foreign-lambda void "pl_label_r"
		  ptr_plPlotter
		  c-string))


;;  Attribute-setting Functions 

  
(define-syntax define-attrset
  (er-macro-transformer
  (lambda (x r c)
    (match-let (((_ name argtype) x))
	       (let ((plname             (string->symbol (sprintf "pl_~A_r" name)))
		     (stubname           (string->symbol (sprintf "stub_pl_~A_r" name))))
		 `(define-plotter-stub ,stubname ,plname ,argtype)))
    ))
  )


(define-attrset colorname c-string)

(define-attrset fillcolorname c-string)

;; TODO: check sign
(define-attrset filltype unsigned-int)

(define-attrset fmiterlimit double)

;; TODO: check sign
(define-attrset linewidth unsigned-int)

(define-attrset flinewidth double)

(define-attrset pencolorname c-string)

;; TODO: check sign
(define-attrset pentype unsigned-int)


(define stub_pl_restorestate_r
  (foreign-lambda void "pl_restorestate_r"
		  ptr_plPlotter))

(define stub_pl_savestate_r
  (foreign-lambda void "pl_savestate_r"
		  ptr_plPlotter))


;;  Transformation Functions 


(define stub_pl_fsetmatrix_r
  (foreign-lambda long "pl_fsetmatrix_r"
		  ptr_plPlotter
		  double double double double double double))


(define stub_pl_fconcat_r
  (foreign-lambda long "pl_fconcat_r"
		  ptr_plPlotter
		  double double double double double double))


(define stub_pl_frotate_r
  (foreign-lambda long "pl_frotate_r"
		  ptr_plPlotter
		  double))


(define stub_pl_fscale_r
  (foreign-lambda long "pl_fscale_r"
		  ptr_plPlotter
		  double double))


(define stub_pl_ftranslate_r
  (foreign-lambda long "pl_ftranslate_r"
		  ptr_plPlotter
		  double double))


