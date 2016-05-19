{mfdtitle.i}
define variable i       as   integer.
define variable qty     as   decimal extent 12 no-undo.
define variable tot_qty as   decimal no-undo.
define variable rpt_qty like qty.
define variable yr      like cph_year.
define variable yr1     like cph_year.
define variable mon     as   integer format ">9".
define variable mon1    as   integer format ">9".
define variable m_part  like ld_part.

yr1 = 2013 .
mon1 = 6.
m_part = '039-90' .

/* SET VARIABLES FOR ROLLING 12 MONTHS */
if mon1 = 0 then do:
   mon1 = 12.
   yr1 = yr1 - 1.
end.
/* CALCULATE STARTING YEAR AND MONTH */
yr = yr1 - 1.
mon = mon1 + 1.
if mon1 = 12 then do:
   yr = yr1.
   mon = 1.
end.


for each cph_hist
   where cph_domain = global_domain
   and cph_part = m_part
   and   (cph_year     = yr
          or cph_year  = yr1):

   /* ACCUMULATES VALUES */
   if cph_year = yr
   then do i = 1 to (13 - mon):
      ASSIGN
             qty[i]   = qty[i]   + cph_qty[mon   + i - 1]
             NO-ERROR.

   end.

   else if cph_year =  yr1
          and   yr  <> yr1
   then do i = (13 - mon1) to 12:
      ASSIGN
             qty[i]   = qty[i]   + cph_qty[i   - (12 - mon1)]
             NO-ERROR.
   end.

end.

/* TOTAL across */
do i = 1 to 12:
   ASSIGN
          tot_qty    = tot_qty    + qty[i]
          NO-ERROR.
end.

DISPLAY
        tot_qty format "->>>,>>>,>>9.99<<"
        WITH WIDTH 320.