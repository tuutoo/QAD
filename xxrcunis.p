/* V8:WebEnabled=No                                                         */
/* V8:RunMode=Character,Windows                                             */
/****************************************************************************/
/* xxrcunis.p    : Shipper Unconfirm                                        */
/* MFG/PRO Ver   : eB2.1 Sp5  - Character                                   */
/* Description   : Shipper Unconfirm                                        */
/* Called From   :                                                          */
/* Called Program:                                                          */
/* Include Files :                                                          */
/* Included in   :                                                          */
/* Database      : qaddb                                                    */
/* Tables read   : tr_hist ld_det                                           */
/* Tables updated:                                                          */
/****************************************************************************/
/* CREATED BY    : Jerry Gu           DATE: 08/28/12  ECO#: 20120810165043  */
/****************************************************************************/
{mfdeclre.i}
ON ASSIGN of tr_hist.tr_lot DO:
    FOR FIRST ld_det NO-LOCK WHERE  ld_domain = global_domain
                               AND  ld_part   = tr_part
                               AND  ld_site   = tr_site
                               AND  ld_loc    = tr_loc
                               AND  ld_lot    = tr_serial :
        ASSIGN ld__chr01 = tr_nbr
	       ld__chr02 = tr_ship_id .
    END.


END.
ON ASSIGN of ld_det.ld_lot DO:
    FOR FIRST tr_hist NO-LOCK WHERE   tr_domain = global_domain
                                AND   tr_type   = "ISS-SO"
                                AND   tr_serial = ld_lot
                                AND   tr_expire <> ?
                                USE-INDEX tr_serial :
        ASSIGN ld_expire = tr_expire.
    END.
END.
{gprun.i ""rcunis.p""}