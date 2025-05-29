/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id: point_col.c,v 1.5 1997/09/02 12:54:24 xleroy Exp $ */

#include "libgraph.h"

value gr_point_color(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  XImage * im;
  int rgb;

  gr_check_open();
  im = XGetImage(grdisplay, grbstore.win, x, Bcvt(y), 1, 1, (-1), ZPixmap);
  rgb = gr_rgb_pixel(XGetPixel(im, 0, 0));
  XDestroyImage(im);
  return Val_int(rgb);
}

                     
