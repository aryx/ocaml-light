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

/* $Id: sound.c,v 1.5 1997/09/02 12:54:24 xleroy Exp $ */

#include "libgraph.h"

value gr_sound(value vfreq, value vdur)
{
  XKeyboardControl kbdcontrol;

  gr_check_open();
  kbdcontrol.bell_pitch = Int_val(vfreq);
  kbdcontrol.bell_duration = Int_val(vdur);
  XChangeKeyboardControl(grdisplay, KBBellPitch | KBBellDuration,
                         &kbdcontrol);
  XBell(grdisplay, 0);
  kbdcontrol.bell_pitch = -1;   /* restore default value */
  kbdcontrol.bell_duration = -1; /* restore default value */
  XChangeKeyboardControl(grdisplay, KBBellPitch | KBBellDuration,
                         &kbdcontrol);
  XFlush(grdisplay);
  return Val_unit;
}


