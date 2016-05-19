/* gpsct03.i - -- INCLUDE FILE TO FIND COST SET (SCT_DET)               */
/* Copyright 1986-2004 QAD Inc., Carpinteria, CA, USA.                  */
/* All rights reserved worldwide.  This is an unpublished work.         */
/* $Revision: 1.14 $                                                     */
/*V8:ConvertMode=Maintenance                                            */
/* REVISION: 7.0     LAST MODIFIED: 08/28/91    BY: pma *F003**/
/* REVISION: 8.5     LAST MODIFIED: 03/12/98    BY: *J2G9*  Sachin Shah */
/* REVISION: 8.6     LAST MODIFIED: 04/02/98    BY: *J2JR*  Sachin Shah */
/* REVISION: 9.1     LAST MODIFIED: 08/13/00    BY: *N0KS* myb          */
/* Old ECO marker removed, but no ECO header exists *F0PN*              */
/* Revision: 1.11  BY: Katie Hilbert  DATE: 04/04/01 ECO: *P008*   */
/* Revision: 1.12  BY: Robin McCarthy DATE: 01/25/02 ECO: *P000* */
/* $Revision: 1.14 $ BY: Paul Donnelly (SB) DATE: 06/28/03 ECO: *Q00F* */
/*-Revision end---------------------------------------------------------------*/

/******************************************************************************/
/* All patch markers and commented out code have been removed from the source */
/* code below. For all future modifications to this file, any code which is   */
/* no longer required should be deleted and no in-line patch markers should   */
/* be added.  The ECO marker should only be included in the Revision History. */
/******************************************************************************/
/*************************************************************/
/*
    This include file returns the specified cost element as the
    {mfdeclre.i} variable glxcst and/or curcst for a specific part/site.
*/
/*
    This file searches based on the in_gl_set and in_cur_set, therefore
    the in_mstr for this part-site must be available before this include
    file is invoked or else values of 0.00 will be returned.

    This file may be used within "for each in_mstr" blocks.
*/
/*
   {&cost}   = sct cost element to return (sct_mtl_tl, sct_cst_tot, etc.)
           Returns the value glxcst for the gl cosr set and
                 curcst for the current cost set.
 in_gl_cost_site points to GL costs; in_site points to Current costs
*/
/*************************************************************/
   assign
      glxcst = 0
      curcst = 0.
   if not available icc_ctrl then
      for first icc_ctrl  where icc_ctrl.icc_domain = global_domain no-lock:
      end.

   if available in_mstr then do:
      /* GET GL COSTS BY in_gl_cost_site*/
      if in_gl_set = "" then do:

         for first sct_det fields( sct_domain sct_part sct_sim sct_site {&cost})
            where sct_det.sct_domain = global_domain and  sct_sim = icc_gl_set
            and sct_part = in_part
             and sct_site = in_gl_cost_site no-lock: end.
         end.
      else do:

         for first sct_det fields( sct_domain sct_part sct_sim sct_site {&cost})
            where sct_det.sct_domain = global_domain and  sct_sim = in_gl_set
            and sct_part = in_part
             and sct_site = in_gl_cost_site no-lock: end.
         end.
      if available sct_det then glxcst = {&cost}.

      /* GET CURRENT COSTS BY in_site */
      if in_cur_set = "" then do:

         for first sct_det fields( sct_domain sct_part sct_sim sct_site {&cost})
            where sct_det.sct_domain = global_domain and  sct_sim = icc_cur_set
            and sct_part = in_part
            and sct_site = in_site no-lock: end.
         end.
      else do:

         for first sct_det fields( sct_domain sct_part sct_sim sct_site {&cost})
            where sct_det.sct_domain = global_domain and  sct_sim = in_cur_set
            and sct_part = in_part
            and sct_site = in_site no-lock: end.
         end.
      if available sct_det then curcst = {&cost}.
   end.
