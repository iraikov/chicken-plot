
;;
;; Chicken interface to the libplot API.
;;
;; Copyright 2011-2019 Ivan Raikov.
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

(module plot

 (
  libplot-version 
  params-new
  params-delete
  params-copy
  param-set
  param-unset
  
  make-plotter
  delete-plotter 
  
  linedash
  flinedash
  
  alabel
  marker
  fmarker
  markerrel
  fmarkerrel
  capmod 
  fillmod
  joinmod
  linemod
  orientation
  labelwidth 
  flabelwidth 
  fontname 
  ;;TODO: ffontname 
  fontsize 
  ffontsize 
  textangle 
  ftextangle 
  bgcolor 
  color 
  fillcolor 
  pencolor 
  
  openpl
  bgcolorname 
  erase 
  space 
  fspace 
  space2 
  fspace2 
  havecap 
  flushpl 
  closepl 

  arc 
  farc 
  arcrel 
  farcrel 
  bezier2 
  fbezier2 
  
  bezier2rel 
  fbezier2rel 
  bezier3 
  fbezier3 
  bezier3rel 
  fbezier3rel 
  box 
  fbox 
  boxrel 
  fboxrel 
  circle 
  fcircle 
  circlerel 
  fcirclerel 
  cont 
  fcont 
  contrel 
  fcontrel 
  ellarc 
  fellarc 
  ellarcrel 
  fellarcrel 
  ellipse 
  fellipse 
  ellipserel 
  fellipserel 
  endpath
  endsubpath 
  closepath 
  label 
  line 
  fline 
  linerel 
  flinerel 
  point 
  fpoint 
  pointrel 
  fpointrel 
  
  colorname
  fillcolorname
  filltype
  fmiterlimit
  linewidth
  flinewidth
  move
  moverel
  fmove
  fmoverel
  pencolorname
  pentype
  restorestate
  savestate
  
  fsetmatrix
  fconcat
  frotate
  fscale
  ftranslate

  plotter-type?
  X PNG PNM GIF SVG AI PS CGM FIG PCL
  HPGL REGIS TEK META


  plotter-parameter?
  DISPLAY  BITMAPSIZE PAGESIZE ROTATION BG_COLOR EMULATE_COLOR MAX_LINE_LENGTH INTERLACE TRANSPARENT_COLOR
;  plotter-specific parameters 
  CGM_ENCODING CGM_MAX_VERSION GIF_ANIMATION GIF_DELAY GIF_ITERATIONS HPGL_ASSIGN_COLORS HPGL_OPAQUE_MODE
  HPGL_PENS HPGL_ROTATE HPGL_VERSION META_PORTABLE PCL_ASSIGN_COLORS PCL_BEZIERS  PNM_PORTABLE TERM
  USE_DOUBLE_BUFFERING VANISH_ON_DELETE  X_AUTO_FLUSH

  )
 
 (import scheme (chicken base) (chicken foreign)
         datatype matchable (only srfi-1 every))
 (import-for-syntax matchable (chicken format) (only srfi-1 list-tabulate))

#>

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

#include <plot.h>		/* libplot header */

static void chicken_panic (C_char *) C_noret;
static void chicken_panic (C_char *msg)
{
  C_word *a = C_alloc (C_SIZEOF_STRING (strlen (msg)));
  C_word scmmsg = C_string2 (&a, msg);
  C_halt (scmmsg);
  exit (5); /* should never get here */
}

static void chicken_throw_exception(C_word value, C_word loc) C_noret;
static void chicken_throw_exception(C_word value, C_word loc)
{
  char *aborthook = C_text("\003syserror-hook");

  C_word *a = C_alloc(C_SIZEOF_STRING(strlen(aborthook)));
  C_word abort = C_intern2(&a, aborthook);

  abort = C_block_item(abort, 0);
  if (C_immediatep(abort))
    chicken_panic(C_text("`##sys#error-hook' is not defined"));

#if defined(C_BINARY_VERSION) && (C_BINARY_VERSION >= 8)
  C_word rval[4] = { abort, C_SCHEME_UNDEFINED, value, loc };
  C_do_apply(4, rval);
#else
  C_save(value);
  C_do_apply(1, abort, C_SCHEME_UNDEFINED);
#endif
}

void chicken_error (char *msg, const char *loc, C_word obj) 
{
  size_t msglen;
  C_word *a;
  C_word scmmsg, scmloc;
  C_word list;

  msglen = strlen (msg);
  a = C_alloc (C_SIZEOF_STRING (msglen) + C_SIZEOF_LIST(2));
  scmmsg = C_string2 (&a, (char *) msg);
  list = C_list(&a, 2, scmmsg, obj);
  a = C_alloc (C_SIZEOF_STRING (strlen(loc)));
  scmloc = C_string2 (&a, (char *) loc);
  chicken_throw_exception(list, scmloc);
}


/* Chicken C interface declarations */
#define value                 C_word

#define Val_return(v)         C_return(v)

#define Val_false             C_SCHEME_FALSE
#define Val_true              C_SCHEME_TRUE
#define Val_unit              C_SCHEME_UNDEFINED
#define Val_int(i)            (C_fix(i))
#define Val_double(n,ptr)     (ptr=C_alloc (C_SIZEOF_FLONUM), C_flonum (&ptr, n))
#define Val_ptr(p,ptr)        (ptr=C_alloc(C_SIZEOF_POINTER), C_mpointer (&ptr, p))
#define Val_string(s,n,ptr)   (ptr=C_alloc(C_SIZEOF_STRING(n)), C_string (&ptr,n,s))


#define Val_list_length(v)    C_i_length(v)
#define Val_car(v)            C_u_i_car(v)
#define Val_cdr(v)            C_u_i_cdr(v)
#define Field(v,i)            C_block_item(v,i)


#define Val_to_cpointer_nn(v)  C_c_pointer_nn(v)
#define Val_to_int(v)          C_unfix(v)
#define Val_to_double(v)       C_c_double(v)
#define Val_to_cstring(v)      C_c_string(v)
#define Val_to_fd(v)           fileno(C_port_file(v))


#define Val_is_truep(v)       C_truep(v)
#define Val_is_pairp(v)       C_i_pairp(v)
#define Val_is_stringp(v)     C_i_stringp(v)
#define Val_is_booleanp(v)    C_booleanp(v)
#define Val_is_structurep(v)  C_structurep(v)
#define Val_is_integerp(v)    C_fixnump(v)
#define Val_is_null_listp(v)  C_i_null_list_p(v)
#define Val_is_portp(v)       C_portp(v)


/* ************************************************** */
/*  macros for mallocs ... */

#define Malloc(a,n)   ((a)=malloc((n)*sizeof(*a)))
#define CMalloc(a,n)  if(Malloc(a,n)==NULL) \
                        { \
			     fprintf (stderr, "libplot: out of memory"); \
                             abort(); \
			} 


/* ************************************************** */
/* EXCEPTIONS HANDLING */
  
/* raise a Chicken exception for error reporting */
int raise_libplot_exn(const char *msg) 
{
     chicken_error(msg, "libplot", C_SCHEME_UNDEFINED);
     return -1 ;
}

/*  use the library error handler to raise exception */
/*  warnings use the default handler (i.e write a message to the error stream) */
int (*libplot_error_handler)(const char *) = &raise_libplot_exn ;


/* ************************************************** */
/* CONVERSION C STRUCT <-> SCHEME VALUE */

/*  structure keeping track of the plotter object and the i/o streams */
/*  associated with it */

struct wrap_plPlotterStruct 
{
     plPlotter * plotter ;
     FILE *      in_file ;
     FILE *      out_file ;
     FILE *      err_file ;
};
typedef struct wrap_plPlotterStruct wrap_plPlotter ;

#define Val_to_wrapplot(p)    ((wrap_plPlotter *)(Val_to_cpointer_nn (p)))
#define Val_to_plotter(p)     (Val_to_wrapplot(p)->plotter)


value is_alloc_plotter(value plotter)
{
 if(Val_to_wrapplot(plotter) == NULL)
   Val_return(Val_false);
 else 
   Val_return(Val_true);
}

/* close the C streams and deallocate the structure */
void clean_up(wrap_plPlotter * plotter)
{
  if(plotter == NULL)
  {
       fprintf(stderr, "libplot: deallocating deleted value");
       abort();
  }
  if (plotter->in_file)
       fclose(plotter->in_file);
  if (plotter->out_file)
       fclose(plotter->out_file);
  if (plotter->err_file)
       fclose(plotter->err_file);

  free(plotter); 
}

void terminate_plot(wrap_plPlotter * plotter)
{
  clean_up(plotter);
// TODO:  Val_to_plotter(plotter) = NULL;
}

/* the plPlotterParams structure */
#define Val_to_plotparams(p)    ((plPlotterParams *)(Val_to_cpointer_nn(p)))


/* ************************************************** */
/* CONVERSION C STRINGS <-> VARIANT TYPES */


/* mapping of the Chicken variant type for plotter type to  */
/* the string used by the library */
static char *plotter_names[] = { 
  "X", "png", "pnm", "gif", "svg",
  "ai", "ps", "cgm", "fig", "pcl", "hpgl", "regis",
  "tek", "meta" /*,"Xdrawable"*/ } ;



/* convert non-constant constructor to a string  */
/* representing its argument */
char *string_param(value param)
{
     char *s;
     s = Val_to_cstring(Field(param, 2));

     return s;
}

char *bool_param(value param)
{
     if(Val_is_truep(Field(param, 2)))
	  return "yes";
     else 
	  return "no";
}

char *int_param(char *buff, size_t len, value param)
{
     snprintf(buff, len, "%ld", Val_to_int(Field(param, 2)));
     return buff;
}


/* ************************************************** */
/* CONVERSION C STREAMS <-> PORTS */

/* map a port to a C stream */
FILE * stream_of_port(value chan, const char * mode)
{
     int des;
     FILE * res;
     if (!(Val_is_truep(Val_is_portp (chan)))) 
        return NULL;
     des = dup(Val_to_fd(chan));
     res = fdopen(des, mode);
     return res;
}



/* *************************************************** */
/* LIBPLOT INTERFACE : FUNCTIONS FOR MANAGING PLOTTERS */

char *libversion (void)
{
     char *s = (char *)pl_libplot_ver;
     return s;
}

wrap_plPlotter *wrap_pl_newpl_r(int v_type, value v_in, value v_out, value v_err, plPlotterParams *params)
{
     wrap_plPlotter * plot ;
     char * c_pltype = plotter_names[ v_type ] ;

     CMalloc(plot, 1) ;  
     {
	  if (Val_is_truep (v_in))
	  {
	       plot->in_file = stream_of_port(v_in, "r") ;
	       if(! plot->in_file)
	       {
		    clean_up(plot);
		    chicken_error("could not access port", "newpl", v_in) ;	
	       }
	  }
	  else
	  {
	       plot->in_file = NULL ;
	  }

	  if (Val_is_truep (v_out))
	  {
	       plot->out_file = stream_of_port(v_out, "w") ;
	       if(plot->out_file==NULL)
	       {
		    clean_up(plot);
		    chicken_error("could not access port", "newpl", v_out) ;	
	       }
	  } 
	  else
	  {
	       plot->out_file = NULL ;
	  }

	  if (Val_is_truep (v_err))
	  {
	       plot->err_file = stream_of_port(v_err, "w") ;
	       if(plot->err_file==NULL)
	       {
		    clean_up(plot);
		    chicken_error("could not access port", "newpl", v_err) ;	

	       }
	  } 
	  else
	  {
	       plot->err_file = NULL ;
	  }
	  
     }

     plot->plotter = pl_newpl_r(c_pltype, 
				plot->in_file, plot->out_file, plot->err_file, 
				params) ;

     if (!(plot->plotter > 0))
     {
	  clean_up(plot) ;
	  chicken_error("could not create plotter", "newpl", v_type) ;	
     }

     Val_return (plot);
}


value wrap_pl_deletepl_r (wrap_plPlotter *plot)
{
     value *ptr;

     if (pl_deletepl_r(plot->plotter) < 0)
     {
	  chicken_error("could not delete plotter", "deletepl", Val_ptr(plot,ptr));
     }

     terminate_plot(plot);

     Val_return (Val_unit);
}

plPlotterParams *wrap_pl_newplparams()
{
     plPlotterParams * params = pl_newplparams();

     if(! params)
	  chicken_error("could not create parameter struct", "newplparams", Val_unit);

     Val_return (params);
}

value wrap_pl_deleteplparams(plPlotterParams *params)
{
     pl_deleteplparams(params);

// TODO:     Val_to_plotparams(params) = NULL ;
// Use C_set_block_item

     Val_return (Val_unit);
}

plPlotterParams *wrap_pl_copyplparams(plPlotterParams *params)
{
     value *ptr;
     plPlotterParams * new_params;

     new_params = pl_copyplparams(params);
     if (!(new_params))
	  chicken_error("could not copy parameter struct", "copyplparams", Val_ptr(params,ptr));

     Val_return (new_params);
}

value wrap_pl_setplparam(plPlotterParams *params, value paramval)
{
     char buffer[32] ;		/* should be enough room to store any int */
     char c_param_name[32];
     char *c_param = NULL; 
     value tag, s; value v;

     if (!(Val_is_truep(Val_is_structurep(paramval))))
     {
      chicken_error ("invalid parameter value (not a structure)",
                     "setplparam", paramval);
     }

     if (!(Val_is_truep(Val_is_stringp(tag = Field(Field(paramval,0),1)))))
     {
      chicken_error ("invalid parameter value (cannot determine type tag)",
                     "setplparam", paramval);
     } else
     {
        if ((strncmp(Val_to_cstring(tag), "plot#plotter-parameter", 17)) != 0)
	{
         chicken_error ("invalid parameter value (not of type plotter-parameter)",
                        "setplparam", paramval);
        }
     }

     memset(c_param_name,0,32);
     s = Field(Field(paramval,1),1);
     strncpy(c_param_name,Val_to_cstring(s),C_i_string_length(s));
     
     v = Field(paramval,2);

     if (C_immediatep(v))
     {
	 if (Val_is_truep(Val_is_integerp(v)))
	    {
  	      c_param = int_param(buffer, 32, paramval); 
	    } else
	    {
	      if (Val_is_truep(Val_is_booleanp(v)))
	      {
	        c_param = bool_param(paramval); 
              }  
            }
     }
     else
     {   
       if (Val_is_truep(Val_is_stringp(v)))
       {
          c_param = string_param(paramval);
       }
       else
       {
        chicken_error ("invalid parameter value (unknown type)",
                       "setplparam", paramval);
       }
     }

     if (pl_setplparam(params, c_param_name, c_param) < 0) 
     {  
       chicken_error("could not set parameter", "setplparam", paramval);
     }

     Val_return (Val_unit);
}

value wrap_pl_unsetplparam(plPlotterParams *params, value paramval)
{
     value s; char *c_param_name;

     if (!(Val_is_truep(Val_is_structurep(paramval))))
     {
      chicken_error ("invalid parameter value",
                     "unsetplparam", paramval);
     }

     if (!(Val_is_truep(Val_is_stringp(s = Field(Field(paramval,0),1)))))
     {
      chicken_error ("invalid parameter value",
                     "unsetplparam", paramval);
     } else
     {
        if ((strncmp(Val_to_cstring(s),"plotter-parameter",17)) != 0)
	{
         chicken_error ("invalid parameter value",
                        "unsetplparam", paramval);
        }
     }

     c_param_name = Val_to_cstring(Field(paramval,1));
     if(pl_setplparam(params, c_param_name, NULL) < 0)
     {
      chicken_error("could not unset parameter",
                    "unsetplparam", paramval);
     }

     Val_return (Val_unit);
}


void separate_colors(value color, int* red, int *green, int *blue)
{
  *red   = Val_int(Field(color, 0));
  *green = Val_int(Field(color, 1));
  *blue  = Val_int(Field(color, 2));
}

plPlotter * unwrap_plPlotter(wrap_plPlotter *plot)
{ 
   return plot->plotter;
}

<#

(define-syntax define-enumerated-type
  (er-macro-transformer
  (lambda (x r c)
    (match-let (((_ typename pred vector inject project . rest) x))
    (let ((%define  (r 'define))
	  (%begin   (r 'begin))
	  (%if      (r 'if)))
      `(,%begin
	(,%define (,pred x)    (##sys#structure? x ',typename))
	(,%define (,project x) (##sys#slot x 2))
	(,%define (,inject i)  
		  (and (integer? i) (positive? i) (< i (vector-length ,vector)) 
		       (vector-ref ,vector i)))
	,(let loop ((variants rest) (i 0) (defs (list)))
	   (if (null? variants) 
	       `(,%begin ,@defs)
	       (let* ((variant  (car variants))
		      (def  `(,%define ,variant   
				       (##sys#make-structure ',typename ',(car variant) ,i))))
		 (loop (cdr variants) (+ i 1) (cons def defs)))))
	,(let loop ((variants rest) (defs (list)))
	   (if (null? variants) 
	       `(,%define ,vector (vector ,@(reverse defs)))
	       (let* ((variant  (car variants))
		      (def  `(,(car variant))))
		 (loop (cdr variants) (cons def defs)))))
	))
    ))
  ))


(define-syntax define-plotter-stub
  (syntax-rules ()
    ((_ stubname plname argtype ...) 
     (define stubname
       (foreign-lambda void plname
		       ptr_plPlotter
		       argtype ...)))))


(define-foreign-type value scheme-object)

(define-foreign-type plPlotterParams "plPlotterParams")
(define-foreign-type wrap_plPlotter "wrap_plPlotter")
(define-foreign-type plPlotter "plPlotter")

(define unwrap-plotter
  (foreign-lambda nonnull-c-pointer "unwrap_plPlotter" (nonnull-c-pointer wrap_plPlotter)))

(define-foreign-type ptr_plPlotter (nonnull-c-pointer plPlotter) unwrap-plotter)


(define-enumerated-type  plotter-type plotter-type? plotter-type-vector
  plotter-type-inject plotter-type-project  
  (X) (PNG) (PNM) (GIF) (SVG) (AI) (PS) (CGM) (FIG) (PCL)
  (HPGL) (REGIS) (TEK) (META))

(define-foreign-type plotter-type unsigned-int plotter-type-project plotter-type-inject)


(define-datatype plotter-parameter plotter-parameter?
;;  common parameters 
  (DISPLAY (s string?))			;; X 
  (BITMAPSIZE (s string?))		;; bitmap plotters 
  (PAGESIZE (s string?))		;; vector plotters 
  (ROTATION (s string?))		;; all plotters except meta 
  (BG_COLOR (s string?))		;; some plotters 
  (EMULATE_COLOR (b boolean?))		;; all plotters 
  (MAX_LINE_LENGTH (i integer?))	;; nearly all plotters 
  (INTERLACE (b boolean?))		;; GIF PNG 
  (TRANSPARENT_COLOR (s string?))	;; GIF PNG 
;  plotter-specific parameters 
  (CGM_ENCODING (s string?)) 
  (CGM_MAX_VERSION (s string?)) 
  (GIF_ANIMATION (b boolean?)) 
  (GIF_DELAY (i integer?))
  (GIF_ITERATIONS (i integer?))
  (HPGL_ASSIGN_COLORS (b boolean?))
  (HPGL_OPAQUE_MODE (b boolean?))
  (HPGL_PENS (s string?)) 
  (HPGL_ROTATE (s string?)) 
  (HPGL_VERSION (s string?))
  (META_PORTABLE (b boolean?))
  (PCL_ASSIGN_COLORS (b boolean?))
  (PCL_BEZIERS (b boolean?))
  (PNM_PORTABLE (b boolean?))
  (TERM (s string?)) 
  (USE_DOUBLE_BUFFERING (b boolean?))
  (VANISH_ON_DELETE (b boolean?)) 
  (X_AUTO_FLUSH (b boolean?)))

;;  The Xdrawable plotter is unsupported  
;;  | X_DRAWABLE_COLORMAP | X_DRAWABLE_DISPLAY | X_DRAWABLE_DRAWABLE1 
;;  | X_DRAWABLE_DRAWABLE2 | X_DRAWABLE_VISUAL 


(define-enumerated-type 
  h-justify h-justify? h-justify-vector 
  h-justify-inject h-justify-project
  (Left) (HCenter) (Right))

(define-foreign-type h-justify unsigned-int h-justify-project h-justify-inject)

(define-enumerated-type 
  v-justify v-justify? v-justify-vector 
  v-justify-inject v-justify-project
  (Bottom) (Baseline) (VCenter) (Cap-line) (Top))

(define-foreign-type v-justify unsigned-int v-justify-project v-justify-inject)


(define-enumerated-type 
  marker marker? marker-vector 
  marker-inject marker-project
  (No-marker) (Dot) (Plus) (Asterisk) (Circle) (Cross) (Square)
  (Triangle) (Diamond) (Star) (Inv-triangle) (Starburst ))


(define-enumerated-type 
  capmod capmod? capmod-vector 
  capmod-inject capmod-project
  (Butt) (CRound) (Projecting) (Triangular))

(define-foreign-type capmod unsigned-int capmod-project capmod-inject)

(define-enumerated-type 
  fillmod fillmod? fillmod-vector 
  fillmod-inject fillmod-project
  (Alternate) (Winding) )

(define-foreign-type fillmod unsigned-int fillmod-project fillmod-inject)

(define-enumerated-type 
  joinmod joinmod? joinmod-vector 
  joinmod-inject joinmod-project
  (Miter) (JRound) (Bevel) )

(define-foreign-type joinmod unsigned-int joinmod-project joinmod-inject)

(define-enumerated-type 
  linemod linemod? linemod-vector 
  linemod-inject linemod-project
  (Solid) (Dotted) (Dotdashed) (Shortdashed) (Longdashed)
  (Dotdotdashed) (Dotdotdotdashed) (Disconnected) )

(define-foreign-type linemod unsigned-int linemod-project linemod-inject)


(define-enumerated-type 
  direction direction? direction-vector 
  direction-inject direction-project
  (Clockwise) (Counterclockwise) )

(define-foreign-type direction unsigned-int direction-project direction-inject)

(include "plotstubs.scm")


;; check if C plotter is still allocated 
(define plotter-alloc?
  (foreign-lambda value "is_alloc_plotter" value))

(define libplot-version (foreign-lambda c-string "libversion"))


;; Manage parameters 

(define params-new
  (foreign-lambda (nonnull-c-pointer plPlotterParams) "wrap_pl_newplparams" ))

(define params-delete
  (foreign-lambda void "wrap_pl_deleteplparams" (nonnull-c-pointer plPlotterParams) ))

(define params-copy
  (foreign-lambda (nonnull-c-pointer plPlotterParams) "wrap_pl_copyplparams" (nonnull-c-pointer plPlotterParams) ))


(define param-set
  (foreign-lambda void "wrap_pl_setplparam" (nonnull-c-pointer plPlotterParams)  value ))

(define param-unset
  (foreign-lambda void "wrap_pl_unsetplparam" (nonnull-c-pointer plPlotterParams) value ))

;; plotter constructor
(define plotter-new
  (foreign-lambda (nonnull-c-pointer wrap_plPlotter) "wrap_pl_newpl_r" plotter-type value value value (nonnull-c-pointer plPlotterParams)))


;;  wrapper for creating and initializing plotters 

(define (make-plotter type out params #!key (err #f))
  (let ((plotter-params (params-new)))
    (for-each (lambda (x) (param-set plotter-params x)) params)
    (let ((plotter (plotter-new type #f out err plotter-params)))
      (params-delete plotter-params)
      plotter)))

(define delete-plotter 
  (foreign-lambda void "wrap_pl_deletepl_r" (nonnull-c-pointer wrap_plPlotter)))

(define (linedash plotter dashes offset)
  (and (pair? dashes) (every integer? dashes)
       (stub_pl_linedash_r plotter dashes offset)))

(define (flinedash plotter dashes offset)
  (and (pair? dashes) (every number? dashes)
       (stub_pl_flinedash_r plotter dashes offset)))

(define alabel stub_pl_alabel_r)

(define marker stub_pl_marker_r)

(define fmarker stub_pl_fmarker_r)

(define markerrel  stub_pl_markerrel_r)

(define fmarkerrel stub_pl_markerrel_r)

(define capmod stub_pl_capmod_r)

(define fillmod stub_pl_fillmod_r)

(define joinmod stub_pl_joinmod_r)

(define linemod stub_pl_linemod_r)

(define orientation stub_pl_orientation_r)

(define labelwidth stub_pl_linewidth_r)

(define flabelwidth stub_pl_flinewidth_r)

(define fontname stub_pl_fontname_r)

;;TODO: (define ffontname stub_pl_ffontname_r)

(define fontsize stub_pl_fontsize_r)

(define ffontsize stub_pl_ffontsize_r)

(define textangle stub_pl_textangle_r)

(define ftextangle stub_pl_ftextangle_r)

(define bgcolor stub_pl_bgcolor_r)

(define color stub_pl_color_r)

(define fillcolor stub_pl_fillcolor_r)

(define pencolor stub_pl_pencolor_r)

;; Control Functions 

(define openpl stub_pl_openpl_r)

(define bgcolorname stub_pl_bgcolorname_r)

(define erase stub_pl_erase_r)

(define space stub_pl_space_r)

(define fspace stub_pl_fspace_r)

(define space2 stub_pl_space2_r)

(define fspace2 stub_pl_fspace2_r)

(define havecap stub_pl_havecap_r)

(define flushpl stub_pl_flushpl_r)

(define closepl stub_pl_closepl_r)

;;  Object Drawing Functions 

(define arc stub_pl_arc_r)

(define farc stub_pl_farc_r)

(define arcrel stub_pl_arcrel_r)

(define farcrel stub_pl_farcrel_r)

(define bezier2 stub_pl_bezier2_r)

(define fbezier2 stub_pl_fbezier2_r)


(define bezier2rel stub_pl_bezier2rel_r)

(define fbezier2rel stub_pl_fbezier2rel_r)

(define bezier3 stub_pl_bezier3_r)

(define fbezier3 stub_pl_fbezier3_r)

(define bezier3rel stub_pl_bezier3rel_r)

(define fbezier3rel stub_pl_fbezier3rel_r)

(define box stub_pl_box_r)

(define fbox stub_pl_fbox_r)

(define boxrel stub_pl_boxrel_r)

(define fboxrel stub_pl_fboxrel_r)

(define circle stub_pl_circle_r)

(define fcircle stub_pl_fcircle_r)

(define circlerel stub_pl_circlerel_r)

(define fcirclerel stub_pl_fcirclerel_r)

(define cont stub_pl_cont_r)

(define fcont stub_pl_fcont_r)

(define contrel stub_pl_contrel_r)

(define fcontrel stub_pl_fcontrel_r)

(define ellarc stub_pl_ellarc_r)

(define fellarc stub_pl_fellarc_r)

(define ellarcrel stub_pl_ellarcrel_r)

(define fellarcrel stub_pl_fellarcrel_r)

(define ellipse stub_pl_ellipse_r)

(define fellipse stub_pl_fellipse_r)

(define ellipserel stub_pl_ellipserel_r)

(define fellipserel stub_pl_fellipserel_r)

(define endpath	stub_pl_endpath_r)

(define endsubpath stub_pl_endsubpath_r)

(define closepath stub_pl_closepath_r)

(define label stub_pl_label_r)

(define line stub_pl_line_r)

(define fline stub_pl_fline_r)

(define linerel stub_pl_linerel_r)

(define flinerel stub_pl_flinerel_r)

(define point stub_pl_point_r)

(define fpoint stub_pl_fpoint_r)

(define pointrel stub_pl_pointrel_r)

(define fpointrel stub_pl_fpointrel_r)

;;  Attribute-setting Functions 
(define colorname stub_pl_colorname_r)

(define fillcolorname stub_pl_fillcolorname_r)

(define filltype stub_pl_filltype_r)

(define fmiterlimit stub_pl_fmiterlimit_r)

(define linewidth stub_pl_linewidth_r)

(define flinewidth stub_pl_flinewidth_r)

(define move stub_pl_move_r)

(define moverel stub_pl_moverel_r)

(define fmove stub_pl_fmove_r)

(define fmoverel stub_pl_fmoverel_r)

(define pencolorname stub_pl_pencolorname_r)

(define pentype stub_pl_pentype_r)

(define restorestate stub_pl_restorestate_r)

(define savestate stub_pl_savestate_r)

;;  Mapping Functions 

(define fsetmatrix stub_pl_fsetmatrix_r)

(define fconcat stub_pl_fconcat_r)

(define frotate stub_pl_frotate_r)

(define fscale stub_pl_fscale_r)

(define ftranslate stub_pl_ftranslate_r)

)
