/*V8:ConvertMode= Report                                                     */
/*V8:WebEnabled=No                                                           */
/*V8:RunMode=Character,Windows                                               */
/*****************************************************************************/
/* PROCEDURE NAME    : xxprnsw.p                                             */
/* PROCEDURE TYPE    : Maintainance                                          */
/* DESCRIPTION       : Printer Server Switcher                               */
/* INCLUDE FILES     :                                                       */
/* CALLED BY         :                                                       */
/* CALLED PROCEDURES :                                                       */
/* INCLUDE FILES     :                                                       */
/* INCLUDED IN       :                                                       */
/* PARAMETERS PASSED :                                                       */
/* DATABASE          : qaddb                                                 */
/* TABLES READ       :                                                       */
/* TABLES UPDATED    :                                                       */
/* NOTES             :                                                       */
/*****************************************************************************/
/* CREATED BY  : Jerry Gu            DATE: Aug-15-2013   ECO#:               */
/*****************************************************************************/

{mfdtitle.i}

define new shared variable part1         like pt_part.
define new shared variable part          like pt_part.
define new shared variable line          like pt_prod_line .
define new shared variable line1         like pt_prod_line .
define new shared variable part_group    like pt_group.
define new shared variable part_group1   like pt_group.
define new shared variable part_type     like pt_part_type.
define new shared variable part_type1    like pt_part_type.
define new shared variable site          like in_site.
define new shared variable site1         like in_site.
define new shared variable as_of_date    as date initial today.
define new shared variable rpt_type      as character format "!(1)"
                                         label "Detail/Summary/Excessive" init "D" no-undo.

form
    part         colon 25
    part1        colon 49 label {t001.i}         skip
    line         colon 25
    line1        colon 49 label {t001.i}         skip
    part_group   colon 25
    part_group1  colon 49 label {t001.i}         skip
    part_type    colon 25
    part_type1   colon 49 label {t001.i}         skip
    site         colon 25
    site1        colon 49 label {t001.i}         skip
    as_of_date   colon 25 label "Effective Date" skip
    rpt_type     colon 25
    skip
with frame a side-labels width 80.

/* VIEW Frame a. */

repeat:

    if site1 = hi_char  then site1 = "".
    if line1 = hi_char  then line1 = "".
    if part1 = hi_char  then part1 = "".
    if part_group1 = hi_char  then part_group1 = "".
    if part_type1 = hi_char   then part_type1 = "".
    if as_of_date = ?         then as_of_date = today.

    update
        part
        part1
        line
        line1
        part_group
        part_group1
        part_type
        part_type1
        site
        site1
        as_of_date
        rpt_type validate(index("dse", rpt_type) <> 0,
                           "ERROR: Interval must be (D)etail (S)ummary (E)xcessive.  Please re-enter.")
    with frame a .

    bcdparm = "".
    {mfquoter.i part}
    {mfquoter.i part1}
    {mfquoter.i line}
    {mfquoter.i line1}
    {mfquoter.i part_group}
    {mfquoter.i part_group1}
    {mfquoter.i part_type}
    {mfquoter.i part_type1}
    {mfquoter.i site}
    {mfquoter.i site1}
    {mfquoter.i as_of_date}
    {mfquoter.i rpt_type}

    if site1 = "" then site1 = hi_char.
    if line1 = "" then line1 = hi_char.
    if part1 = "" then part1 = hi_char.
    if part_group1 = "" then part_group1 = hi_char.
    if part_type1  = "" then part_type1  = hi_char.
    if as_of_date  = ?  then as_of_date  = today.

    {mfselbpr.i "printer" 320 "nopage"}
    {mfphead.i}

    {gprun.i ""xxprnsw1a.p""}

    /* REPORT TRAILER */
    {mfrtrail.i}

end.  /* repeat */
