* @ValidationCode : MjotODkwNzk3MjMwOkNwMTI1MjoxNjczMTc0MDIxMTY4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jan 2023 16:33:41
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.FT.COMPANY.CHECK
*-----------------------------------------------------------------------------
* Attached to: FUNDS.TRANSFER,ACTR.LCY.KGDCL
*              FUNDS.TRANSFER,ACTR.LCY
**
* Description: VALIDATION ROUTINE

* Modification Overview:
* ----------------------
* 08/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT I_F.VERSION
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

*------------------TAFC TO TAFJ DEV---------------------------------------
    Y.APPLICATION = EB.SystemTables.getApplication()
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()

    IF Y.APPLICATION EQ "FUNDS.TRANSFER" AND Y.PGM.VERSION EQ ",ACTR.LCY" THEN
* EB.SystemTables.getRNew(idx)
        Y.DR.COMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCompCode)
        Y.CR.COMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditCompCode)
        
        IF Y.DR.COMP NE Y.CR.COMP THEN
*            ETEXT="Online Fund Transfer is not allowed in this menu."
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("Online Fund Transfer is not allowed in this menu.")
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN