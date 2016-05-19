/*V8:ConvertMode= Report                                                     */
/*V8:WebEnabled=No                                                           */
/*V8:RunMode=Character,Windows                                               */
/*****************************************************************************/
/* PROCEDURE NAME    : xxsmexrp1a.p                                          */
/* PROCEDURE TYPE    : Report                                                */
/* DESCRIPTION       : Slow Moving Report                                    */
/* INCLUDE FILES     :                                                       */
/* CALLED BY         : xxsmexrp01.p                                          */
/* CALLED PROCEDURES :                                                       */
/* INCLUDE FILES     :                                                       */
/* INCLUDED IN       :                                                       */
/* PARAMETERS PASSED :                                                       */
/* DATABASE          : qaddb                                                 */
/* TABLES READ       :                                                       */
/* TABLES UPDATED    :                                                       */
/* NOTES             :                                                       */
/*****************************************************************************/
/* CREATED BY  : Jerry Gu            DATE: Jun-05-2013   ECO#:20130603164804 */
/*****************************************************************************/

{mfdeclre.i}

define shared variable part1          like pt_part.
define shared variable part           like pt_part.
define shared variable line           like pt_prod_line .
define shared variable line1          like pt_prod_line .
define shared variable part_group     like pt_group.
define shared variable part_group1    like pt_group.
define shared variable part_type      like pt_part_type.
define shared variable part_type1     like pt_part_type.
define shared variable site           like in_site.
define shared variable site1          like in_site.
define shared variable as_of_date     as date initial today.
define shared variable rpt_type       as character format "!(1)"
                                      label "Detail/Summary/Excessive" no-undo.

define variable i              as integer no-undo.
define variable asof_recid     as   recid no-undo.
define variable tt1_recid1     as   recid no-undo.

define variable last_year      as date no-undo.
define variable m_year         like cph_year .
define variable m_month        as integer format ">9" .
define variable m_annual_qty   like tr_qty_loc no-undo.
define variable m_tot_excess_qty like tr_qty_loc no-undo.

define variable m_item_lot    like ld_lot      no-undo.
define variable m_mov_type    like tr_type     no-undo.
define variable m_mov_date    like tr_effdate  no-undo.
define variable m_days_diff   as   int         no-undo.
define variable m_rsv_pct     as   dec         no-undo.
define variable m_rsv_amt     like ar_amt      no-undo.



/* Delare Temp-Tabel */
/* Detail temp-table-1 */
define temp-table tt1_det no-undo
    field tt1_item_type        as   char format "x(8)"
    field tt1_entity           like ar_entity
    field tt1_site             like ld_site
    field tt1_loc              like ld_loc
    field tt1_part             like pt_part
    field tt1_desc             like pt_desc1
    field tt1_added            like pt_added
    field tt1_part_type        like pt_part_type
    field tt1_prod_line        like pt_prod_line
    field tt1_um               like pt_um
    field tt1_status           like pt_status
    field tt1_lot              like ld_lot
    field tt1_qty_oh           like ld_qty_oh
    field tt1_qty_loc          like tr_qty_loc
    field tt1_gl_cost          like sct_cst_tot
    field tt1_value            like ar_amt
    field tt1_tr_trnbr         like tr_trnbr
    field tt1_lot_mov_type     like tr_type
    field tt1_lot_mov_date     like tr_effdate
    field tt1_lot_days_diff    as   int
    field tt1_lot_rsv_pct      as   dec
    field tt1_lot_rsv_amt      like ar_amt
    field tt1_age_qty          like ar_amt    extent 3
    field tt1_age_amt          like ar_amt    extent 3
    field tt1_sum_amt          like ar_amt
    field tt1_inv_stat         like ld_status
    field tt1_item_lot         like ld_lot
    field tt1_item_mov_type    like tr_type
    field tt1_item_mov_date    like tr_effdate
    field tt1_item_days_diff   as   int
    field tt1_item_rsv_pct     as   dec
    field tt1_item_rsv_amt     like ar_amt
    field tt1_spec_rsv_amt     like ar_amt
    /* for summary */
    field tt1_sort_id          as   int
    field tt1_tot_age_qty      like ar_amt    extent 3
    field tt1_tot_exc_amt      like ar_amt

index tt1_part_type
    tt1_part tt1_item_type tt1_sort_id ascending tt1_tr_trnbr descending
index tt1_site_part
     tt1_part tt1_item_type tt1_site tt1_sort_id ascending tt1_tr_trnbr descending .

/* Declare Functions */
FUNCTION plant RETURNS CHAR (INPUT  chrIN   AS CHAR,
                             INPUT  vlen    AS INTEGER,
                             INPUT  align   AS CHAR) FORWARD .

FUNCTION GetInvCat RETURNS CHAR (INPUT  f_item_type   AS CHAR,
                                 INPUT  f_prod_line   AS CHAR,
                                 INPUT  f_lot         AS CHAR,
                                 INPUT  f_um          AS CHAR) FORWARD .

FUNCTION GetSortID RETURNS INT (INPUT  f_item_type   AS CHAR,
                                INPUT  f_move_type   AS CHAR) FORWARD .

/*BEGIN PROGRAM*/
/* Clear Temp-table */
empty temp-table tt1_det.

m_year    = year(as_of_date).
m_month   = month(as_of_date).

/* CREATE REPORT HEADER */
if rpt_type = "D" then do:
    run CREATE-HEADER (input "det", input yes).
end. /* if rpt_type = "D" */
else if rpt_type = "S" then do:
    run CREATE-HEADER (input "sum", input yes).
end. /* else if rpt_type = "S" */
else do:
    run CREATE-HEADER (input "exc", input yes).
end. /* else do: */

/* Main Logic */
/* Get tr_hist of lots > as_of_date */
if as_of_date < today then do:
    for each tr_hist fields (tr_site tr_loc tr_part tr_serial tr_qty_loc)
        where tr_effdate >  as_of_date and tr_domain = global_domain
          and  tr_part >= part and tr_part <= part1
          and  tr_site >= site and tr_site  <= site1
          and  tr_effdate    <> ?
          and  tr_serial     <> ""
          and  tr_qty_loc <> 0 and tr_ship_type = ""
          no-lock use-index tr_eff_trnbr
          break by tr_site by tr_part by tr_serial :
        accumulate tr_qty_loc  (total by tr_serial) .

        if last-of(tr_serial) and (accum total by tr_serial tr_qty_loc) <> 0
        then do:

            find first pt_mstr no-lock where pt_domain = global_domain and pt_part = tr_part no-error.

            if available pt_mstr
                and pt_prod_line >= line       and pt_prod_line <= line1
                and pt_part_type >= part_type  and pt_part_type <= part_type1
                and pt_group     >= part_group and pt_group     <= part_group1
            then do:
                create tt1_det.
                    assign
                           tt1_part = tr_part
                           tt1_lot  = tr_serial
                           tt1_qty_loc = (accum total by tr_serial tr_qty_loc)
                           tt1_qty_oh  = - tt1_qty_loc
                           tt1_added     = pt_added
                           tt1_desc      = pt_desc1 + " " + pt_desc2
                           tt1_part_type = pt_part_type
                           tt1_prod_line = pt_prod_line
                           tt1_um        = pt_um
                           tt1_status    = pt_status
                           no-error.

                    /* get tt1_site and INV status as of date */
                    RUN P-GET-TR IN THIS-PROCEDURE
                            (INPUT tt1_lot,
                             INPUT "",
                             OUTPUT asof_recid) .

                    if avail tr_hist then do:
                        assign
                               tt1_site = tr_site
                               tt1_loc  = tr_loc
                               tt1_inv_stat = tr_status
                               no-error.
                end. /* IF AVAIL tr_hist */
             end. /* if available pt_mstr */
        end. /* if last-of(tr_serial)  */

     end. /* for each tr_hist */
end. /* if as_of_date < today */

for each ld_det no-lock
    where ld_domain = global_domain
      and ld_qty_oh <> 0
      and ld_site >= site and ld_site <= site1
      and ld_part >= part and ld_part <= part1
      use-index ld_loc_p_lot
    break by ld_site by ld_part by ld_lot:

    accumulate ld_qty_oh  (total by ld_lot) .

    if last-of(ld_lot) and (accum total by ld_lot ld_qty_oh) <> 0 then do:
        find first pt_mstr no-lock where pt_domain = global_domain and pt_part = ld_part no-error.
        if available pt_mstr
            and pt_prod_line >= line       and pt_prod_line <= line1
            and pt_part_type >= part_type  and pt_part_type <= part_type1
            and pt_group     >= part_group and pt_group     <= part_group1
        then do:

            find first tt1_det where tt1_lot = ld_lot no-lock no-error.

            if not available tt1_det then do:
                create tt1_det .

                assign
                       tt1_site      = ld_site
                       tt1_loc       = ld_loc
                       tt1_part      = ld_part
                       tt1_added     = pt_added
                       tt1_desc      = pt_desc1 + " " + pt_desc2
                       tt1_part_type = pt_part_type
                       tt1_prod_line = pt_prod_line
                       tt1_um        = pt_um
                       tt1_status    = pt_status
                       tt1_lot       = ld_lot
                       tt1_qty_oh    = (accum total by ld_lot ld_qty_oh)
                       tt1_inv_stat  = ld_status
                       no-error.
            end. /* if not available tt1_det */
            else do:
                if TRUNCATE((accum total by ld_lot ld_qty_oh),3) = TRUNCATE(tt1_qty_loc,3) then delete tt1_det .
                else
                    tt1_qty_oh = (accum total by ld_lot ld_qty_oh) - tt1_qty_loc .
            end. /* else do: */
        end. /* if available pt_mstr */

    end. /* if last-of(ld_lot) */

end. /* for each ld_det */


for each tt1_det no-lock
    break by tt1_part by tt1_item_type:

    find first pl_mstr where pl_domain = global_domain and pl_prod_line = pt_prod_line no-lock no-error.
    if available pl_mstr then assign tt1_entity = pl__chr01.

    tt1_item_type = GetInvCat(tt1_part_type, tt1_prod_line, tt1_lot, tt1_um) .

    RUN P-GET-MOV-DETAIL
        (INPUT tt1_part, INPUT tt1_item_type, INPUT tt1_lot, INPUT tt1_site,
         OUTPUT tt1_lot_mov_type,
         OUTPUT tt1_lot_mov_date,
         OUTPUT tt1_tr_trnbr,
         OUTPUT tt1_lot_days_diff,
         OUTPUT tt1_lot_rsv_pct,
         OUTPUT tt1_gl_cost) .

    /* Delete the tt1_det record > as_of_date */
    if tt1_lot_mov_date = ?  then do:
        delete tt1_det.
        next .
    end.

    tt1_sort_id = GetSortID(tt1_item_type, tt1_lot_mov_type) .

    tt1_value = tt1_gl_cost * tt1_qty_oh .
    tt1_lot_rsv_amt = tt1_value * tt1_lot_rsv_pct * 0.01 .

    if first-of(tt1_item_type) then do:
        ASSIGN
               tt1_item_lot       = tt1_lot
               tt1_item_mov_type  = tt1_lot_mov_type
               tt1_item_mov_date  = tt1_lot_mov_date
               tt1_item_days_diff = tt1_lot_days_diff
               tt1_item_rsv_pct   = tt1_lot_rsv_pct
               tt1_item_rsv_amt   = tt1_lot_rsv_amt
               m_item_lot         = tt1_lot
               m_mov_type         = tt1_lot_mov_type
               m_mov_date         = tt1_lot_mov_date
               m_days_diff        = tt1_lot_days_diff
               m_rsv_pct          = tt1_lot_rsv_pct
               m_rsv_amt          = tt1_lot_rsv_amt
               NO-ERROR.
    end. /* if first-of(tt1_item_type) */
    else do:
        ASSIGN
               tt1_item_lot       = m_item_lot
               tt1_item_mov_type  = m_mov_type
               tt1_item_mov_date  = m_mov_date
               tt1_item_days_diff = m_days_diff
               tt1_item_rsv_pct   = m_rsv_pct
               tt1_item_rsv_amt   = tt1_value * m_rsv_pct * 0.01
               NO-ERROR.
    end. /* else do */

    /* LOCATION STATUS = NNN, THEN 100% RESERVE(Specific Reserve) */
    if tt1_inv_stat = "NNN" then do:
        ASSIGN
               tt1_lot_rsv_amt  = 0
               tt1_item_rsv_amt = 0
               tt1_spec_rsv_amt = tt1_value
               NO-ERROR.
    end.  /* if tt1_inv_stat = "NNN" */

end. /* for each tt1_det detail report end */

if rpt_type = "D" then do:
    for each tt1_det NO-LOCK
        by tt1_part by tt1_item_type by tt1_site by tt1_sort_id :
    /* display detail report */
        put unformatted
            tt1_item_type       FORMAT "X(9)"       /* Type */
            tt1_entity          FORMAT "X(7)"       /* Entity */
            tt1_site            FORMAT "X(9)"       /* Site */
            tt1_loc             FORMAT "X(9)"       /* Location */
            tt1_part            FORMAT "X(19)"      /* Item */
            tt1_desc            FORMAT "X(50)"      /* Description */
            plant(STRING(tt1_added),  14, "r")      /* Item Creation */
            tt1_part_type       FORMAT "X(10)"      /* Item Type */
            tt1_prod_line       FORMAT "X(10)"      /* Prod Line */
            tt1_um              FORMAT "X(3)"       /* UM */
            tt1_status          FORMAT "X(9)"       /* Status */
            tt1_lot             FORMAT "X(19)"      /* Lot(Full digit) */
            plant(STRING(tt1_qty_oh, "->>>,>>>,>>>,>>9.99"), 20, "r")            /* Quantity */
            plant(STRING(tt1_gl_cost, ">>>>,>>>,>>9.99<<<"), 19, "r")            /* GL Cost */
            plant(STRING(tt1_value,  "->>>,>>>,>>>,>>9.99"), 20, "r")            /* Valuation */
            tt1_lot_mov_type    FORMAT "X(23)"                                   /* Last Movemet Type(LOT) */
            plant(STRING(tt1_lot_mov_date),       24, "r")                       /* Last Movement Date(LOT) */
            plant(STRING(tt1_lot_days_diff, "->>>9"),       15, "r")             /* Days Diff(LOT) */
            plant(STRING(tt1_lot_rsv_pct,   "->>>9"),       15, "r")             /* Reserve %(LOT) */
            plant(STRING(tt1_lot_rsv_amt,   "->>>,>>>,>>>,>>9.99"),  29, "r")    /* Slow Moving Reserve Amt(LOT) */
            tt1_item_mov_type   FORMAT "X(24)"                                   /* Last Movemet Type(Item) */
            plant(STRING(tt1_item_mov_date),      25, "r")                       /* Last Movement Date(Item) */
            plant(STRING(tt1_item_days_diff, "->>>9"),      16, "r")             /* Days Diff(Item) */
            plant(STRING(tt1_item_rsv_pct, "->>>9"),        16, "r")             /* Reserve %(Item) */
            plant(STRING(tt1_item_rsv_amt,   "->>>,>>>,>>>,>>9.99"), 30, "r")    /* Slow Moving Reserve Amt(Item) */
            plant(STRING(tt1_spec_rsv_amt,   "->>>,>>>,>>>,>>9.99"), 27, "r")    /* Specific Reserve(NNN)-Item */
            tt1_inv_stat        FORMAT "X(11)"                                   /* Inv Status */
            skip
        .
    end. /* for each tt1_det */
end.  /* if rpt_type = "D" */


/* summary report start */
if rpt_type = "S" then do:
    for each tt1_det no-lock
        break by tt1_part by tt1_item_type:
        ACCUMULATE tt1_qty_oh       (TOTAL BY tt1_item_type).
        ACCUMULATE tt1_value        (TOTAL BY tt1_item_type).
        ACCUMULATE tt1_item_rsv_amt (TOTAL BY tt1_item_type).
        ACCUMULATE tt1_spec_rsv_amt (TOTAL BY tt1_item_type).

        if last-of(tt1_item_type) then do:

            put unformatted
                tt1_item_type       FORMAT "X(9)"       /* Type */
                tt1_entity          FORMAT "X(7)"       /* Entity */
                tt1_part            FORMAT "X(19)"      /* Item */
                tt1_desc            FORMAT "X(50)"      /* Description */
                plant(STRING(tt1_added),  14, "r")      /* Item Creation */
                tt1_part_type       FORMAT "X(10)"      /* Item Type */
                tt1_prod_line       FORMAT "X(10)"      /* Prod Line */
                tt1_um              FORMAT "X(3)"       /* UM */
                tt1_status          FORMAT "X(9)"       /* Status */
                tt1_item_lot        FORMAT "X(19)"      /* Lot(Full digit) */
                plant(STRING(accum total by tt1_item_type tt1_qty_oh, "->>>,>>>,>>>,>>9.99"), 20, "r")      /* Quantity */
                plant(STRING(tt1_gl_cost, ">>>>,>>>,>>9.99<<<"), 19, "r")      /* GL Cost */
                plant(STRING(accum total by tt1_item_type tt1_value,  "->>>,>>>,>>>,>>9.99"), 20, "r")      /* Valuation */
                tt1_item_mov_type   FORMAT "X(18)"      /* Last Movemet Type */
                plant(STRING(tt1_item_mov_date),        19, "r")                /* Last Movement Date */
                plant(STRING(tt1_item_days_diff, "->>>9"),        10, "r")      /* Days Diff */
                plant(STRING(tt1_item_rsv_pct,   "->>>9"),        10, "r")      /* Reserve % */
                plant(STRING(accum total by tt1_item_type tt1_item_rsv_amt, "->>>,>>>,>>>,>>9.99"), 24, "r")      /* Slow Moving Reserve Amt */
                plant(STRING(accum total by tt1_item_type tt1_spec_rsv_amt, "->>>,>>>,>>>,>>9.99"), 22, "r")      /* Specific Reserve(NNN) */
                skip .
        end. /* if last-of(tt1_item_type) */
    end. /* for each tt1_det */
end. /* if rpt_type = "S": summary report end */

if rpt_type = "E" then do:
    for each tt1_det no-lock
        break by tt1_part:
        ACCUMULATE tt1_qty_oh       (TOTAL BY tt1_part).
        ACCUMULATE tt1_value        (TOTAL BY tt1_part).
        ACCUMULATE tt1_item_rsv_amt (TOTAL BY tt1_part).
        ACCUMULATE tt1_spec_rsv_amt (TOTAL BY tt1_part).

        if last-of(tt1_part) then do:

            RUN P-GET-ANL-QTY
                (INPUT  tt1_part, INPUT m_year, INPUT m_month,
                 OUTPUT m_annual_qty) .

            m_tot_excess_qty =  if ((accum total by tt1_part tt1_qty_oh) - m_annual_qty > 0 AND m_annual_qty >= 0)
                               then
                                   (accum total by tt1_part tt1_qty_oh) - m_annual_qty
                               else
                                    0 .

            /* Get excess qty of different period [12,18,24 Mths] */
            do i = 1 to extent(tt1_tot_age_qty) :
                if i <> extent(tt1_tot_age_qty) then do :
                    tt1_tot_age_qty[i] = minimum(m_tot_excess_qty, ABSOLUTE(m_annual_qty / 2)) .
                    m_tot_excess_qty = m_tot_excess_qty - tt1_tot_age_qty[i] .
                end. /* i <> extent(tt1_tot_age_qty) */
                else do:
                    tt1_tot_age_qty[i] = m_tot_excess_qty .
                end. /* else do: */
            end. /* do i = 1 to extent(tt1_tot_age_qty) */

            /* Rsv pct of different age [12,18,24 Mths] is 50%, 75%, 100% */
            tt1_tot_exc_amt = tt1_gl_cost *
                              (tt1_tot_age_qty[1] * 0.5
                               +
                               tt1_tot_age_qty[2] * 0.75
                               +
                               tt1_tot_age_qty[3] * 1).

            put unformatted
                tt1_entity          FORMAT "X(7)"       /* Entity */
                tt1_part            FORMAT "X(19)"      /* Item */
                tt1_desc            FORMAT "X(50)"      /* Description */
                plant(STRING(tt1_added),  14, "r")      /* Item Creation */
                tt1_part_type       FORMAT "X(10)"      /* Item Type */
                tt1_prod_line       FORMAT "X(10)"      /* Prod Line */
                tt1_um              FORMAT "X(3)"       /* UM */
                plant(STRING(accum total by tt1_part tt1_qty_oh, "->>>,>>>,>>>,>>9.99"), 20, "r")      /* Quantity */
                plant(STRING(tt1_gl_cost, ">>>>,>>>,>>9.99<<<"), 19, "r")      /* GL Cost */
                plant(STRING(accum total by tt1_part tt1_value,  "->>>,>>>,>>>,>>9.99"), 20, "r")      /* Valuation */
                plant(STRING(m_annual_qty,       "->>>,>>>,>>>,>>9.99"), 20, "r")      /* Annual Sales Qty */
                plant(STRING(tt1_tot_age_qty[1], "->>>,>>>,>>>,>>9.99"), 20, "r")      /* Excess (12-18 Mths) */
                plant(STRING(tt1_tot_age_qty[2], "->>>,>>>,>>>,>>9.99"), 20, "r")      /* Excess (18-24 Mths) */
                plant(STRING(tt1_tot_age_qty[3], "->>>,>>>,>>>,>>9.99"), 20, "r")      /* Excess (>24 Mths) */
                plant(STRING(tt1_tot_exc_amt,    "->>>,>>>,>>>,>>9.99"), 20, "r")      /* Excess Reserve Amt */
                plant(STRING(accum total by tt1_part tt1_item_rsv_amt, "->>>,>>>,>>>,>>9.99"), 24, "r")      /* Slow Moving Reserve Amt */
                plant(STRING(accum total by tt1_part tt1_spec_rsv_amt, "->>>,>>>,>>>,>>9.99"), 22, "r")      /* Specific Reserve(NNN) */
                skip .

        end. /* if last-of(tt1_part) */
    end. /* for each tt1_det */
end. /* if rpt_type = "S": summary report end */


/* ========================================================================= */
/* ******************************** FUNCTIONS ****************************** */
/* ========================================================================= */

/*===========================================================================*/
FUNCTION Plant RETURNS CHAR (INPUT  chrIN   AS CHAR,
                             INPUT  vlen    AS INTEGER,
                             INPUT  align   AS CHAR):
/*----------------------------------------------------------------------------
  Description:  Output CharIn with an aligned format
-----------------------------------------------------------------------------*/
    DEFINE VARIABLE ileft     AS INTEGER NO-UNDO.
    DEFINE VARIABLE freespace AS INTEGER NO-UNDO.

    chrIN   = SUBSTRING(chrIN, 1, vlen - 1).
    chrIN   = IF chrIN = ? THEN ""
              ELSE STRING(chrIN) NO-ERROR.
    freespace = vlen - length(chrIN,"COLUMN") .
    ileft     = ROUND(freespace / 2 , 0) - 1 .

    CASE align:
      /* Align Center */
      WHEN "c" THEN
        RETURN (FILL(" ",ileft) + chrIN + FILL(" ", freespace - ileft)) .
      /* Align Right */
      WHEN "r" THEN
        RETURN (FILL(" ", freespace - 1) + chrIN + " ") .
    END CASE.
END FUNCTION.

/*===========================================================================*/
FUNCTION GetInvCat RETURNS CHAR (INPUT  f_item_type   AS CHAR,
                                 INPUT  f_prod_line   AS CHAR,
                                 INPUT  f_lot         AS CHAR,
                                 INPUT  f_um          AS CHAR):
/*----------------------------------------------------------------------------
  Description:  Get Inventory Category by item type, prod line, lot, UM
-----------------------------------------------------------------------------*/
    IF f_item_type = "CS"
        OR f_item_type = "SS"
        OR f_prod_line begins "4"
        OR f_prod_line begins "2"
        OR (f_item_type = "F"
            AND f_prod_line begins "1"
            AND LENGTH(f_lot) = 13
            AND f_um = "KG")
        OR (f_item_type = "P"
            AND f_prod_line begins "3")
    THEN RETURN "FG" .

    IF f_item_type = "R" THEN RETURN "RM" .
    IF f_item_type = "W" THEN RETURN "WIP" .

    IF (f_item_type = "F"
        AND f_prod_line begins "1"
        AND LENGTH(f_lot) = 18)
    THEN RETURN "FG" .

    IF (f_item_type = "F"
        AND f_prod_line begins "1"
        AND LENGTH(f_lot) = 13)
    THEN RETURN "WIP" .

    IF (f_item_type = "F"
        AND f_prod_line begins "3"
        AND LENGTH(f_lot) = 18)
    THEN RETURN "FG" .

    IF (f_item_type = "F"
        AND f_prod_line begins "3"
        AND LENGTH(f_lot) = 13)
    THEN RETURN "WIP" .

    IF (f_item_type = "P"
        AND f_prod_line begins "1"
        AND LENGTH(f_lot) = 18)
    THEN RETURN "FG" .

    IF (f_item_type = "P"
        AND f_prod_line begins "1"
        AND LENGTH(f_lot) = 13)
    THEN RETURN "WIP" .

    RETURN "WIP" .

END FUNCTION.

/*===========================================================================*/
FUNCTION GetSortID RETURNS INT (INPUT  f_item_type   AS CHAR,
                                INPUT  f_move_type   AS CHAR):
/*----------------------------------------------------------------------------
  Description:  Return Sort ID by Item Type & Movement Type
-----------------------------------------------------------------------------*/
    IF f_item_type = "RM" THEN DO:
        CASE f_move_type:
            WHEN "ISS-WO" THEN
                RETURN 1 .
            WHEN "ISS-SO" THEN
                RETURN 2.
            WHEN "RCT-PO" THEN
                RETURN 3.
            WHEN "RCT-UNP" THEN
                RETURN 4.
            WHEN "ISS-UNP" THEN
                RETURN 5.
        END CASE.
    END.

    IF f_item_type = "WIP" THEN DO:
        CASE f_move_type:
            WHEN "ISS-SO" THEN
                RETURN 1 .
            WHEN "ISS-WO" THEN
                RETURN 2.
            WHEN "RCT-WO" THEN
                RETURN 3.
            WHEN "RCT-PO" THEN
                RETURN 4.
            WHEN "RCT-UNP" THEN
                RETURN 5.
            WHEN "ISS-UNP" THEN
                RETURN 6.
        END CASE.
    END.

    IF f_item_type = "FG" THEN DO:
        CASE f_move_type:
            WHEN "ISS-SO" THEN
                RETURN 1 .
            WHEN "ISS-WO" THEN
                RETURN 2.
            WHEN "RCT-PO" THEN
                RETURN 3.
            WHEN "RCT-WO" THEN
                RETURN 4.
            WHEN "RCT-UNP" THEN
                RETURN 5.
            WHEN "ISS-UNP" THEN
                RETURN 6.
        END CASE.
    END.

END FUNCTION.


/* ========================================================================= */
/* *************************** Internal Procedures ************************* */
/* ========================================================================= */


/*===========================================================================*/
PROCEDURE P-GET-MOV-DETAIL:
/*----------------------------------------------------------------------------
  Description:  Use item type, lot, site to get movement details(tr_type,
                tr_effdate, tr_trnbr)
-----------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER m_part         LIKE tr_part NO-UNDO .
    DEFINE INPUT  PARAMETER m_type         AS   CHAR FORMAT "x(8)" .
    DEFINE INPUT  PARAMETER m_lot          LIKE ld_lot .
    DEFINE INPUT  PARAMETER m_site         LIKE ld_site .
    DEFINE OUTPUT PARAMETER m_tr_type      LIKE tr_type .
    DEFINE OUTPUT PARAMETER m_tr_effdate   LIKE tr_effdate .
    DEFINE OUTPUT PARAMETER m_tr_trnbr     LIKE tr_trnbr .
    DEFINE OUTPUT PARAMETER m_days_diff    AS   INT .
    DEFINE OUTPUT PARAMETER m_rsv_pct      AS   DEC .
    DEFINE OUTPUT PARAMETER m_stdcost      LIKE sct_cst_tot .

    DEFINE VARIABLE m_recid                AS   RECID NO-UNDO .
    DEFINE VARIABLE trrecno                AS   RECID NO-UNDO .
    DEFINE VARIABLE cst_date               LIKE tr_effdate NO-UNDO .

    /* Reserve Rule - RM */
    IF m_type = "RM" THEN DO:
        RUN P-GET-TR IN THIS-PROCEDURE
            (INPUT m_lot,
             INPUT "ISS-WO",
             OUTPUT m_recid) .

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "ISS-SO",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "RCT-PO",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "RCT-UNP",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "ISS-UNP",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF AVAIL tr_hist THEN DO:
            ASSIGN
                   m_tr_type    = tr_type
                   m_tr_effdate = tr_effdate
                   m_days_diff  = as_of_date - tr_effdate
                   m_tr_trnbr   = tr_trnbr
                   NO-ERROR.
        END. /* IF AVAIL tr_hist */

        IF m_days_diff >= 180 AND m_days_diff < 270
        THEN m_rsv_pct = 25 .

        ELSE IF m_days_diff >= 270 AND m_days_diff < 360
        THEN m_rsv_pct = 50 .

        ELSE IF m_days_diff >= 360
        THEN m_rsv_pct = 100 .

        ELSE m_rsv_pct = 0 .

    END. /* IF m_type = "RM" */

    /* Reserve Rule - WIP */
    IF m_type = "WIP" THEN DO:
        RUN P-GET-TR IN THIS-PROCEDURE
            (INPUT m_lot,
             INPUT "ISS-SO",
             OUTPUT m_recid) .

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "ISS-WO",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "RCT-WO",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "RCT-PO",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "RCT-UNP",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "ISS-UNP",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF AVAIL tr_hist THEN DO:
            ASSIGN
                   m_tr_type    = tr_type
                   m_tr_effdate = tr_effdate
                   m_days_diff  = as_of_date - tr_effdate
                   m_tr_trnbr   = tr_trnbr
                   NO-ERROR.
        END. /* IF AVAIL tr_hist */

            IF m_days_diff >= 90 AND m_days_diff < 180
            THEN m_rsv_pct = 50 .

            ELSE IF m_days_diff >= 180
            THEN m_rsv_pct = 100 .

            ELSE m_rsv_pct = 0 .

    END. /* IF m_type = "WIP" */

    /* Reserve Rule - FG */
    IF m_type = "FG" THEN DO:
        RUN P-GET-TR IN THIS-PROCEDURE
            (INPUT m_lot,
             INPUT "ISS-SO",
             OUTPUT m_recid) .

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "ISS-WO",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "RCT-PO",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "RCT-WO",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "RCT-UNP",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "ISS-UNP",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF NOT avail tr_hist THEN DO:
            RUN P-GET-TR IN THIS-PROCEDURE
                (INPUT m_lot,
                 INPUT "",
                 OUTPUT m_recid) .
        END. /* IF AVAIL tr_hist */

        IF AVAIL tr_hist THEN DO:
            ASSIGN
                   m_tr_type    = tr_type
                   m_tr_effdate = tr_effdate
                   m_days_diff  = as_of_date - tr_effdate
                   m_tr_trnbr   = tr_trnbr
                   NO-ERROR.
        END. /* IF AVAIL tr_hist */

            IF m_days_diff >= 180 AND m_days_diff < 360
            THEN m_rsv_pct = 50 .

            ELSE IF m_days_diff >= 360
            THEN m_rsv_pct = 100 .

            ELSE m_rsv_pct = 0 .

    END. /* IF m_type = "FG" */

    /* Consider rewind items RCT-TR as RCT-WO */
    if m_tr_type = "RCT-TR" then m_tr_type = "RCT-WO" .

    /* FIND THE STANDARD COST AS OF DATE */
    FOR FIRST tr_hist
       FIELDS (tr_domain tr_part tr_effdate tr_site tr_loc tr_ship_type
               tr_qty_loc tr_bdn_std tr_lbr_std tr_mtl_std tr_ovh_std
               tr_price tr_status tr_sub_std tr_trnbr tr_type)
       WHERE tr_domain  = global_domain
       AND   tr_part    =  m_part
       AND   tr_effdate >= as_of_date + 1
       AND   tr_site    =  m_site
       AND   tr_type    =  "CST-ADJ"
    NO-LOCK USE-INDEX tr_part_eff:
    END. /* FOR FIRST TR_HIST */

    IF AVAILABLE tr_hist THEN DO:

       /* GET THE FIRST RECORD ENTERED EVEN IF TR_PART_EFF*/
       /* ISN'T IN TRANSACTION NUMBER SEQUENCE            */

       cst_date = tr_effdate.

       FOR EACH tr_hist
          FIELDS (tr_domain tr_part tr_effdate tr_site tr_loc tr_ship_type
                  tr_qty_loc tr_bdn_std tr_lbr_std tr_mtl_std tr_ovh_std
                  tr_price tr_status tr_sub_std tr_trnbr tr_type)
          WHERE tr_domain = global_domain
          AND   tr_part    = m_part
          AND   tr_effdate = cst_date
          AND   tr_site    = m_site
          AND   tr_type    = "CST-ADJ"
          USE-INDEX tr_part_eff
          BY tr_trnbr:

          trrecno = RECID(tr_hist).
          LEAVE.
       END. /* FOR EACH TR_HIST */

       FOR FIRST tr_hist
          FIELDS (tr_domain tr_part tr_effdate tr_site tr_loc tr_ship_type
                  tr_qty_loc tr_bdn_std tr_lbr_std tr_mtl_std tr_ovh_std
                  tr_price tr_status tr_sub_std tr_trnbr tr_type)
          WHERE RECID(tr_hist) = trrecno NO-LOCK:
       END. /* FOR FIRST TR_HIST */

       m_stdcost    = (tr_mtl_std + tr_lbr_std + tr_ovh_std
                    + tr_bdn_std + tr_sub_std).

       IF tr_price <> m_stdcost THEN
          m_stdcost = m_stdcost - tr_price.

    END. /* if available tr_hist */
    ELSE DO:
        /* GET TOTAL GL COST */
        FIND LAST in_mstr NO-LOCK
            WHERE in_domain = global_domain
              AND in_part = m_part AND in_site = m_site NO-ERROR.
        {gpsct03.i &cost=sct_cst_tot}
        m_stdcost = glxcst .
    END.

END PROCEDURE.  /* P-GET-MOV-DETAIL */

/*===========================================================================*/
PROCEDURE P-GET-TR:
/*----------------------------------------------------------------------------
  Description:  Retrieve latest tr_hist recid by tr_serial
  Version:  1.0
-----------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER m_lot          LIKE tr_serial .
    DEFINE INPUT  PARAMETER m_tr_type      LIKE tr_type .
    DEFINE OUTPUT PARAMETER m_recid        AS   RECID .
    /* Define tr_type excluded list for last movement */
    DEFINE VARIABLE ex_trlist AS CHARACTER
           INITIAL "CYC-RCNT,CYC-CNT,CYC-ERR,RCT-GIT,RCT-DO,ISS-TR,ISS-DO,ISS-GIT,TAG-CNT,RJCT-WO,CST-ADJ,ISS-CHL,RCT-CHL,WIP-ADJ,WO-CLOSE" .

    FOR  EACH tr_hist NO-LOCK
        WHERE tr_domain = global_domain
          AND tr_effdate <= as_of_date
          AND tr_serial = m_lot
          AND (tr_type   = m_tr_type
               OR m_tr_type = "")
          AND (LOOKUP(tr_type, ex_trlist) = 0
                OR
                    (tr_type = "RCT-TR"
                    AND
                    (tr_program = 'xxwowobf.p' OR tr_program = 'xxwowoc1.p'))
                )
        USE-INDEX tr_serial
        BY tr_trnbr DESCENDING:

        m_recid = RECID(tr_hist).
        LEAVE.

    END.

END PROCEDURE.  /* P-GET-TR */

/*===========================================================================*/
/*----------------------------------------------------------------------------
  Description:  Retrieve annual sales qty for certain item
  Version:  1.0
-----------------------------------------------------------------------------*/
PROCEDURE P-GET-ANL-QTY:
    DEFINE INPUT  PARAMETER m_part          LIKE tr_part .
    DEFINE INPUT  PARAMETER yr1             LIKE cph_year .
    DEFINE INPUT  PARAMETER mon1            as   integer format ">9" .
    DEFINE OUTPUT PARAMETER m_annual_qty    LIKE tr_qty_loc .

    DEFINE VARIABLE i       AS   INTEGER.
    DEFINE VARIABLE qty     AS   DECIMAL EXTENT 12 NO-UNDO.
    DEFINE VARIABLE rpt_qty LIKE qty.
    DEFINE VARIABLE yr      LIKE cph_year.
    DEFINE VARIABLE mon     AS   INTEGER FORMAT ">9".

    m_annual_qty = 0 .

    /* SET VARIABLES FOR ROLLING 12 MONTHS */
    IF mon1 = 0 THEN DO:
       mon1 = 12.
       yr1 = yr1 - 1.
    END.
    /* CALCULATE STARTING YEAR AND MONTH */
    yr = yr1 - 1.
    mon = mon1 + 1.
    IF mon1 = 12 THEN DO:
       yr = yr1.
       mon = 1.
    END.

    FOR EACH cph_hist
       WHERE cph_domain = global_domain
       AND cph_part = m_part
       AND   (cph_year     = yr
              OR cph_year  = yr1)
       use-index cph_part :

       /* ACCUMULATES VALUES */
       IF cph_year = yr
       THEN DO i = 1 TO (13 - mon):
          ASSIGN
                 qty[i] = qty[i] + cph_qty[mon + i - 1]
                 NO-ERROR.

       END.

       ELSE IF cph_year =  yr1
              AND   yr  <> yr1
       THEN DO i = (13 - mon1) to 12:
          ASSIGN
                 qty[i] = qty[i] + cph_qty[i - (12 - mon1)]
                 NO-ERROR.
       END.

    END. /* FOR EACH cph_hist */

    /* TOTAL across */
    DO i = 1 TO 12:
       ASSIGN
              m_annual_qty = m_annual_qty  + qty[i]
              NO-ERROR.
    END. /* DO i = 1 TO 12 */

END PROCEDURE.  /* P-GET-ANL-QTY */


/*===========================================================================*/
PROCEDURE CREATE-HEADER:
/*----------------------------------------------------------------------------
  Description:  Create Report Header(Detail/Summary)
  Version:  1.0
-----------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER m_rpt_type AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER withlabel  AS LOGICAL NO-UNDO.

/* CREATE REPORT HEADER */
IF m_rpt_type = "det" THEN DO:
    IF withlabel THEN DO:
    /* 1st LINE OF HEADER */  /* HEADER LABELS */
    PUT UNFORMATTED
        "Type"                                 FORMAT "X(9)"
        "Entity"                               FORMAT "X(7)"
        "Site"                                 FORMAT "X(9)"
        "Location"                             FORMAT "X(9)"
        "Item"                                 FORMAT "X(19)"
        "Description"                          FORMAT "X(50)"
        "Item Creation"                        FORMAT "X(14)"
        "Item Type"                            FORMAT "X(10)"
        "Prod Line"                            FORMAT "X(10)"
        "UM"                                   FORMAT "X(3)"
        "Status"                               FORMAT "X(9)"
        "Lot(Full digit)"                      FORMAT "X(19)"
        "Quantity"                             FORMAT "X(20)"
        "GL Cost"                              FORMAT "X(19)"
        "Valuation"                            FORMAT "X(20)"
        "Last Movemet Type(LOT)"               FORMAT "X(23)"
        "Last Movement Date(LOT)"              FORMAT "X(24)"
        "Days Diff(LOT)"                       FORMAT "X(15)"
        "Reserve %(LOT)"                       FORMAT "X(15)"
        "Slow Moving Reserve Amt(LOT)"         FORMAT "X(29)"
        "Last Movemet Type(Item)"              FORMAT "X(24)"
        "Last Movement Date(Item)"             FORMAT "X(25)"
        "Days Diff(Item)"                      FORMAT "X(16)"
        "Reserve %(Item)"                      FORMAT "X(16)"
        "Slow Moving Reserve Amt(Item)"        FORMAT "X(30)"
        "Specific Reserve(NNN)-Item"           FORMAT "X(27)"
        "Inv Status"                           FORMAT "X(11)"
        SKIP .
    END. /* if l_withlabel  */

    /* 2nd LINE OF HEADER */  /* DASHES ONLY */
    PUT UNFORMATTED
        FILL("-", 8)      FORMAT "X(9)"       /* Type */
        FILL("-", 6)      FORMAT "X(7)"       /* Entity */
        FILL("-", 8)      FORMAT "X(9)"       /* Site */
        FILL("-", 8)      FORMAT "X(9)"       /* Location */
        FILL("-", 18)     FORMAT "X(19)"      /* Item */
        FILL("-", 49)     FORMAT "X(50)"      /* Description */
        FILL("-", 13)     FORMAT "X(14)"      /* Item Creation */
        FILL("-", 9)      FORMAT "X(10)"      /* Item Type */
        FILL("-", 9)      FORMAT "X(10)"      /* Prod Line */
        FILL("-", 2)      FORMAT "X(3)"       /* UM */
        FILL("-", 8)      FORMAT "X(9)"       /* Status */
        FILL("-", 18)     FORMAT "X(19)"      /* Lot(Full digit) */
        FILL("-", 19)     FORMAT "X(20)"      /* Quantity */
        FILL("-", 18)     FORMAT "X(19)"      /* GL Cost */
        FILL("-", 19)     FORMAT "X(20)"      /* Valuation */
        FILL("-", 22)     FORMAT "X(23)"      /* Last Movemet Type(LOT) */
        FILL("-", 23)     FORMAT "X(24)"      /* Last Movement Date(LOT) */
        FILL("-", 14)     FORMAT "X(15)"      /* Days Diff(LOT) */
        FILL("-", 14)     FORMAT "X(15)"      /* Reserve %(LOT) */
        FILL("-", 28)     FORMAT "X(29)"      /* Slow Moving Reserve Amt(LOT) */
        FILL("-", 23)     FORMAT "X(24)"      /* Last Movemet Type(Item) */
        FILL("-", 24)     FORMAT "X(25)"      /* Last Movement Date(Item) */
        FILL("-", 15)     FORMAT "X(16)"      /* Days Diff(Item) */
        FILL("-", 15)     FORMAT "X(16)"      /* Reserve %(Item) */
        FILL("-", 29)     FORMAT "X(30)"      /* Slow Moving Reserve Amt(Item) */
        FILL("-", 26)     FORMAT "X(27)"      /* Specific Reserve(NNN)-Item */
        FILL("-", 10)     FORMAT "X(11)"      /* Inv Status */
        skip.
END. /* if rpt_type = "det" */

ELSE IF m_rpt_type = "sum" THEN DO:
    IF withlabel THEN DO:
    /* 1st LINE OF HEADER */  /* HEADER LABELS */
    PUT UNFORMATTED
        "Type"                                 FORMAT "X(9)"
        "Entity"                               FORMAT "X(7)"
        "Item"                                 FORMAT "X(19)"
        "Description"                          FORMAT "X(50)"
        "Item Creation"                        FORMAT "X(14)"
        "Item Type"                            FORMAT "X(10)"
        "Prod Line"                            FORMAT "X(10)"
        "UM"                                   FORMAT "X(3)"
        "Status"                               FORMAT "X(9)"
        "Lot(Full digit)"                      FORMAT "X(19)"
        "Quantity"                             FORMAT "X(20)"
        "GL Cost"                              FORMAT "X(19)"
        "Valuation"                            FORMAT "X(20)"
        "Last Movemet Type"                    FORMAT "X(18)"
        "Last Movement Date"                   FORMAT "X(19)"
        "Days Diff"                            FORMAT "X(10)"
        "Reserve %"                            FORMAT "X(10)"
        "Slow Moving Reserve Amt"              FORMAT "X(24)"
        "Specific Reserve(NNN)"                FORMAT "X(22)"
        SKIP .
    END. /* if l_withlabel  */

    /* 2nd LINE OF HEADER */  /* DASHES ONLY */
    PUT UNFORMATTED
        FILL("-", 8)      FORMAT "X(9)"       /* Type */
        FILL("-", 6)      FORMAT "X(7)"       /* Entity */
        FILL("-", 18)     FORMAT "X(19)"      /* Item */
        FILL("-", 49)     FORMAT "X(50)"      /* Description */
        FILL("-", 13)     FORMAT "X(14)"      /* Item Creation */
        FILL("-", 9)      FORMAT "X(10)"      /* Item Type */
        FILL("-", 9)      FORMAT "X(10)"      /* Prod Line */
        FILL("-", 2)      FORMAT "X(3)"       /* UM */
        FILL("-", 8)      FORMAT "X(9)"       /* Status */
        FILL("-", 18)     FORMAT "X(19)"      /* Lot(Full digit) */
        FILL("-", 19)     FORMAT "X(20)"      /* Quantity */
        FILL("-", 18)     FORMAT "X(19)"      /* GL Cost */
        FILL("-", 19)     FORMAT "X(20)"      /* Valuation */
        FILL("-", 17)     FORMAT "X(18)"      /* Last Movemet Type */
        FILL("-", 18)     FORMAT "X(19)"      /* Last Movement Date */
        FILL("-", 9)      FORMAT "X(10)"      /* Days Diff */
        FILL("-", 9)      FORMAT "X(10)"      /* Reserve % */
        FILL("-", 23)     FORMAT "X(24)"      /* Slow Moving Reserve Amt */
        FILL("-", 21)     FORMAT "X(22)"      /* Specific Reserve(NNN) */
        SKIP.
END. /* else if rpt_type = "sum" then do: */

ELSE IF m_rpt_type = "exc" THEN DO:

    IF withlabel THEN DO:
    /* 1st LINE OF HEADER */  /* HEADER LABELS */
    PUT UNFORMATTED
        "Entity"                               FORMAT "X(7)"
        "Item"                                 FORMAT "X(19)"
        "Description"                          FORMAT "X(50)"
        "Item Creation"                        FORMAT "X(14)"
        "Item Type"                            FORMAT "X(10)"
        "Prod Line"                            FORMAT "X(10)"
        "UM"                                   FORMAT "X(3)"
        "Quantity"                             FORMAT "X(20)"
        "GL Cost"                              FORMAT "X(19)"
        "Valuation"                            FORMAT "X(20)"
        "Annual Sales Qty"                     FORMAT "X(20)"
        "Excess (12-18 Mths)"                  FORMAT "X(20)"
        "Excess (18-24 Mths)"                  FORMAT "X(20)"
        "Excess (>24 Mths)"                    FORMAT "X(20)"
        "Excess Reserve Amt"                   FORMAT "X(20)"
        "Slow Moving Reserve Amt"              FORMAT "X(24)"
        "Specific Reserve(NNN)"                FORMAT "X(22)"
        SKIP .
    END. /* if l_withlabel  */

    /* 2nd LINE OF HEADER */  /* DASHES ONLY */
    PUT UNFORMATTED
        FILL("-", 6)      FORMAT "X(7)"       /* Entity */
        FILL("-", 18)     FORMAT "X(19)"      /* Item */
        FILL("-", 49)     FORMAT "X(50)"      /* Description */
        FILL("-", 13)     FORMAT "X(14)"      /* Item Creation */
        FILL("-", 9)      FORMAT "X(10)"      /* Item Type */
        FILL("-", 9)      FORMAT "X(10)"      /* Prod Line */
        FILL("-", 2)      FORMAT "X(3)"       /* UM */
        FILL("-", 19)     FORMAT "X(20)"      /* Quantity */
        FILL("-", 18)     FORMAT "X(19)"      /* GL Cost */
        FILL("-", 19)     FORMAT "X(20)"      /* Valuation */
        FILL("-", 19)     FORMAT "X(20)"      /* Annual Sales Qty */
        FILL("-", 19)     FORMAT "X(20)"      /* Excess (12-18 Mths) */
        FILL("-", 19)     FORMAT "X(20)"      /* Excess (18-24 Mths) */
        FILL("-", 19)     FORMAT "X(20)"      /* Excess (>24 Mths) */
        FILL("-", 19)     FORMAT "X(20)"      /* Excess Reserve Amt */
        FILL("-", 23)     FORMAT "X(24)"      /* Slow Moving Reserve Amt */
        FILL("-", 21)     FORMAT "X(22)"      /* Specific Reserve(NNN) */

        SKIP.

END. /* else if rpt_type = "exc" then do: */

END PROCEDURE.  /* CREATE-HEADER */
