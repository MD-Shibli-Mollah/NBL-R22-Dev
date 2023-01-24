* @ValidationCode : MjotNjQ3NzAyNjIzOkNwMTI1MjoxNjc0NTUxMjQ0MjcxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 24 Jan 2023 15:07:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.CHECK.CASH.BAL

*-----------------------------------------------------------------------------
* This routine is used as an Input Routine
*-----------------------------------------------------------------------------
* Modification History :
* Developed By         : MD SHIBLI MOLLAH -- FDS
* Date                 : 05TH JAN 2023
*-----------------------------------------------------------------------------
* Modification 2
*
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT I_F.TELLER
    $USING TT.Contract
*    $INSERT I_F.ACCOUNT
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING EB.Foundation
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    R.ACC = ''
    Y.CR.ACC = ''
    Y.CR.AMT = ''
    Y.TT.TR.CODE = ''
    Y.AC.CATEG = ''
    Y.WORKING.BAL = ''

RETURN

OPENFILES:

    EB.DataAccess.Opf(FN.ACC,F.ACC)

RETURN

PROCESS:
*--------

    Y.APPLICATION = EB.SystemTables.getApplication()

    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
        Y.CR.ACC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
        IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount) NE '' THEN
            Y.CR.AMT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
        END ELSE
            Y.CR.AMT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAmount)
        END
* EB.SystemTables.getAf()
        !
        IF Y.CR.ACC[1,8] EQ 'BDT10001' OR Y.CR.ACC[1,8] EQ 'BDT10011' THEN
            GOSUB CASH.AC.VAL
        END
    END

    IF Y.APPLICATION EQ 'TELLER' THEN
        Y.CR.ACC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
        Y.CR.AMT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
        Y.DR.CR.MARKER = EB.SystemTables.getRNew(TT.Contract.Teller.TeDrCrMarker)
* AF = 6
        IF Y.CR.ACC[1,8] EQ 'BDT10001' OR Y.CR.ACC[1,8] EQ 'BDT10011' AND Y.DR.CR.MARKER EQ 'CREDIT' THEN
            GOSUB CASH.AC.VAL
        END
    
        IF EB.SystemTables.getPgmVersion() EQ ',BD.LCY.CASHWDL' THEN
            Y.DR.ACC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
            IF LEFT(Y.DR.ACC,1) EQ '1' THEN
* AF = 18
*                ETEXT = "Customer Account is not allowed."
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext("Customer Account is not allowed.")
                EB.ErrorProcessing.StoreEndError()
            END
        END
    END
RETURN

CASH.AC.VAL:
*-----------
    EB.DataAccess.FRead(FN.ACC,Y.CR.ACC,R.ACC,F.ACC,ERR.AC)
    Y.AC.CATEG = R.ACC<AC.AccountOpening.Account.Category>
    Y.WORKING.BAL  = R.ACC<AC.AccountOpening.Account.WorkingBalance> + Y.CR.AMT
    
    IF Y.AC.CATEG EQ '10001' OR Y.AC.CATEG EQ '10011' THEN
        IF Y.WORKING.BAL GT 0 THEN
*            E = "Insufficient Cash Account Balance"
*            CALL ERR
            EB.SystemTables.setEtext("Insufficient Cash Account Balance")
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END
RETURN
END
