/*Fuction: Output CharIn with aligned format */
FUNCTION plant RETURNS CHAR (INPUT  chrIN AS CHAR,
                             INPUT  vlen    AS INTEGER,
                             INPUT  align   AS CHAR):
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