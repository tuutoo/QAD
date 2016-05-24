/* V8:ConvertMode=Maintenance                                               */
/* V8:RunMode=Character,Windows                                             */
/****************************************************************************/
/* xxcnsodlbt.p  : sales order for line  batch delete                       */
/* MFG/PRO Ver   : eB2.1 Sp5  - Character                                   */
/* Description   :                                                          */
/* Called From   :                                                          */
/* Called Program:                                                          */
/* Include Files :                                                          */
/* Included in   :                                                          */
/* Database      : qaddb                                                    */
/* Tables read   :                                                          */
/* Tables updated:                                                          */
/****************************************************************************/
/* CREATED BY    : david wei        DATE: 04/02/2011  ECO#: 20110328163824  */
/* Modify        : david wei        DATE: 10/27/2011  ECO#: 20111026110104  */
/* Last Modified : Jerry Gu         DATE: Jan-29-2013 ECO#: 20130125202922  */
/* Last Modified : Lina Su          DATE: Mar-11-2013 ECO#: 20130312120017  */
/*               : add quantity to invoice(sod_qty_inv) validation          */
/* Last Modified : Jerry Gu         DATE: Mar-26-2013 ECO#: 20130312140113  */
/*               :   1)Define so cancel percentage by prod line             */
/****************************************************************************/
{sobtbvar.i "new"}
{mfdtitle.i}
DEFINE VARIABLE m_file          AS CHAR    FORMAT "X(40)" NO-UNDO.
/* jerryg add start ECO:202922 */
DEFINE VARIABLE m_so_duedate       AS INT    NO-UNDO.
DEFINE VARIABLE m_so_percentage    AS DEC    NO-UNDO.
DEFINE VARIABLE m_prefix           AS CHAR   NO-UNDO.
/* jerryg add end   ECO:202922 */
define new shared variable sonbr           like sod_nbr.
define new shared variable soline          like sod_line.
define temp-table tt1_det
        field tt1_po       like so_po
        field tt1_nbr      like so_nbr
        field tt1_billto   like so_bill
        field tt1_line     like sod_line
        field tt1_part     like sod_part
        field tt1_ord_date like so_ord_date
        field tt1_due_date like sod_due_date
        field tt1_qty_ord  like sod_qty_ord
        field tt1_qty_ship like sod_qty_ship
        field tt1_prod_line like sod_prodline   /* jerryg debug: */
        field tt1_qty_open like sod_qty_ord.

/* jerryg add start ECO:140113 */
define temp-table tt2_det
        field tt2_prodline       like sod_prodline
        field tt2_percentage      as   dec
        INDEX tt2_prodline IS PRIMARY tt2_prodline ASCENDING.
/* jerryg add end ECO:140113 */

define var m_reccnt as integer                   no-undo.


{cxcustom.i "SOSOMTP.P"}
/* jerryg add start ECO:202922 */
FIND FIRST CODE_mstr WHERE CODE_domain  = GLOBAL_domain
                          AND CODE_fldname = "xx_exact_wo"
                          AND CODE_value   = "PREFIX"
                          NO-LOCK NO-ERROR.
   IF AVAILABLE code_mstr then M_PREFIX = code_cmmt.
   ELSE m_prefix = "EX".

FIND FIRST code_mstr WHERE code_domain  = global_domain
                          AND code_fldname = "xx_so_backorder_cancel"
                          AND code_value   = "duedate_tolerance_auto"
                          AND code_cmmt    <> ""
                        NO-LOCK NO-ERROR.

IF AVAIL code_mstr THEN DO:
    ASSIGN
           m_so_duedate  = INT(code_cmmt)
           NO-ERROR.
END.
ELSE DO:
    MESSAGE "ERROR: GCM xx_so_backorder_cancel/duedate_tolerance_auto not defined." .
    PAUSE.
    LEAVE.
END.

FIND FIRST code_mstr WHERE code_domain  = global_domain
                          AND code_fldname = "xx_so_backorder_cancel"
                          AND code_value   = "percentage"
                          AND code_cmmt    <> ""
                        NO-LOCK NO-ERROR.

IF AVAIL code_mstr THEN DO:
    ASSIGN
           m_so_percentage  = DEC(code_cmmt)
           NO-ERROR.
END.
ELSE DO:
    MESSAGE "ERROR: GCM xx_so_backorder_cancel/percentage not defined." .
    PAUSE.
    LEAVE.
END.
/* jerryg add end   ECO:202922 */

/* jerryg add start ECO:140113 */
FOR EACH code_mstr NO-LOCK
    WHERE code_domain  = global_domain
      AND code_fldname = "xx_so_backorder_cancel"
      AND code_value   matches "percentage,*"
      AND code_cmmt    <> "":
    FIND FIRST tt2_det NO-LOCK
        WHERE tt2_prodline = REPLACE( ENTRY(2,code_value), "*", "" ) NO-ERROR.
    IF NOT AVAILABLE tt2_det
      THEN DO:
        create tt2_det.
        assign
         tt2_prodline  = REPLACE( ENTRY(2,code_value), "*", "" )
         tt2_percentage = DEC(code_cmmt).
    END.  /* IF NOT AVAILABLE tt2_det */
END.  /* FOR EACH code_mstr */
/* jerryg add end ECO:140113 */


{mfselbpr.i "printer" 132 nopage}
{mfphead.i}

empty temp-table tt1_det.
      m_file = "xxsodel"
               + SUBSTRING(STRING(TODAY),1,2)
               + SUBSTRING(STRING(TODAY),4,2)
               + SUBSTRING(STRING(TODAY),7,2).
m_reccnt  = 0 .


for each sod_det no-lock where sod_domain = global_domain
/*david:0104*/ and sod_nbr = sod_nbr and sod_due_date < today - m_so_duedate /* 7 */     /* jerryg ECO:202922 */
          and sod_qty_all = 0 and sod_qty_ship > 0
          and sod_qty_inv = 0 , /*linas20130312120017*/
/* jerryg del start ECO:140113 */
          /* and sod_qty_ship / sod_qty_ord  >= m_so_percentage /* 0.95 */, */                 /* jerryg ECO:202922 */
/* jerryg del end ECO:140113 */
    each so_mstr no-lock where so_domain = sod_domain
         and so_nbr = sod_nbr and so_stat = "":
      if sod__chr09 /* so__chr02 */ = "Yes,Yes,No" then do:                              /* jerryg ECO:202922 */

         FIND FIRST wo_mstr WHERE wo_domain = GLOBAL_DOMAIN
               AND wo_nbr begins sod_nbr + "." + string(sod_line)
               AND wo_status <> "C" no-lock no-error.
         if avail wo_mstr then next.
      end.  /* full size*/
      if sod__chr09 /* so__chr02 */ = "Yes,Yes,Yes" then do:                        /* jerryg ECO:202922 */
         find first wo_mstr WHERE wo_domain = GLOBAL_DOMAIN
              and wo_status <> "C" and wo_nbr begins m_prefix /* "EX" */            /* jerryg ECO:202922 */
              and wo_part = sod_part
              and wo__chr01 matches "*" + so_nbr + "*" no-lock no-error.
         if avail  wo_mstr then next.
      end. /* exact */

/* jerryg add start ECO:140113 */
find first tt2_det where sod_prodline begins tt2_prodline no-lock no-error.
    if (avail tt2_det and sod_qty_ship / sod_qty_ord < tt2_percentage) or
       (not avail tt2_det and sod_qty_ship / sod_qty_ord < m_so_percentage)
        then next.
    else do:
/* jerryg add end ECO:140113 */
        create tt1_det.
        assign tt1_po       = so_po
             tt1_nbr      = sod_nbr
             tt1_bill     = so_bill
             tt1_line     = sod_line
             tt1_part     = sod_part
             tt1_ord_date = so_ord_date
             tt1_due_date = sod_due_date
             tt1_qty_ord  = sod_qty_ord
             tt1_qty_ship = sod_qty_ship
             tt1_prod_line = sod_prodline    /* jerryg debug: */
             tt1_qty_open = sod_qty_ord - sod_qty_ship.
        m_reccnt = m_reccnt + 1.
        if m_reccnt > 200 then leave.
    end. /* if not avail tt2_det */     /* jerryg ECO:140113 */
end.  /* for sod_det */
output to value(m_file).
FOR EACH tt1_det:
    for first sod_det where sod_domain = global_domain
        and sod_nbr = tt1_nbr and sod_line = tt1_line exclusive-lock.
        assign
            sonbr  = sod_nbr
            soline = sod_line .
            {gprun.i ""solndel.p"" "(input no)"}
            disp tt1_po tt1_nbr tt1_bill tt1_line tt1_part
                 tt1_ord_date tt1_due_date tt1_qty_ord tt1_qty_ship
                 tt1_qty_open tt1_prod_line tt1_qty_ship / tt1_qty_ord column-label "pct" with frame xx width 250 down.
    end.  /* for sod_det  */
END.      /*for each tt1_det*/
output close.
{mftrl080.i}
