* @ValidationCode : MjotNTI4MTkzNDI4OkNwMTI1MjoxNjczNTA3OTgxMzAyOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Jan 2023 13:19:41
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.FT.KGDCL.CHARGE

**************************************************************************
*Subroutine Description:
*-----------------------
* PURPOSE: OFS$NEW.COMMAND for "AC.CHARGE.REQUEST,KGDCL" VERSION.
*
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 12/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_GTS.COMMON
*    $INSERT T24.BP I_F.OFS.STATUS.FLAG
    $USING EB.Interface
*    $INSERT T24.BP I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $USING FT.Contract
    $USING EB.SystemTables

    Y.VFUNCTION = EB.SystemTables.getVFunction()
* IF V$FUNCTION EQ 'I' THEN
    IF Y.VFUNCTION EQ 'I' THEN
* Y.ACCT.ACC = R.NEW(FT.CREDIT.ACCT.NO)
        Y.ACCT.ACC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
        IF Y.ACCT.ACC EQ "1122003651200" THEN
          
            OFS$NEW.COMMAND = "AC.CHARGE.REQUEST,KGDCL I F3"
        END
        ELSE IF Y.ACCT.ACC EQ "1122003651231" THEN
            OFS$NEW.COMMAND = "AC.CHARGE.REQUEST,KGDCL1 I F3"
        END
        ELSE IF Y.ACCT.ACC EQ "1122003651236" THEN
            OFS$NEW.COMMAND = "AC.CHARGE.REQUEST,KGDCL2 I F3"
        END
    END
RETURN
END
