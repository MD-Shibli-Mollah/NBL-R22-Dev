* @ValidationCode : MjotNjUwMjQzNTM2OkNwMTI1MjoxNjczNTAzMTcyMTI0OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Jan 2023 11:59:32
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.PO.MULTIPLE.PO.DELETE
*-------------------------------------------------------------------------------
*Subroutine Description:
*-----------------------
* This check routine triggers if the delete function has been used to delete any
* unauthorised records to delete the corresponding CHEQUES.PRESENTED record.
*-------------------------------------------------------------------------------
*Parameter Description:
*--------- -----------
* N/A
*-------------------------------------------------------------------------------
*Common Variable Description:
*------  ------- -----------
* LCCY = Holds the local currency
* ETEXT = Holds the EB.ERROR id used for displaying the error
*-------------------------------------------------------------------------------
*Called Subroutines:
*------ ------------
* Routine Name:                         Local/Core:    Description:
* STORE.END.ERROR                       Core           Called for displaying the error message
*
*
*-------------------------------------------------------------------------------
* Modification History:
* ------------ --------
*       Date            Name                    Description
*       ----            ----                    -----------
*    25 Jan 2014     Ayush Kumar              Initial Creation
*                   (Sepit Soft Tech Pvt Ltd.)
*   08 JAN 2023     MD Shibli Mollah          RETROFIT from TAFC to TAFJ
*
*-------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT I_F.CHEQUES.PRESENTED
*    $INSERT I_F.CHEQUE.TYPE.ACCOUNT
    $USING CQ.ChqSubmit
    $USING ST.ChqConfig
    $USING ST.ChqIssue
    
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Updates
    $USING EB.Foundation
    $USING EB.TransactionControl
    
    GOSUB INITIALISE
    GOSUB PROCESS

RETURN

*----------
INITIALISE:
*----------
*
* This section is to initialise all the applications and variables.
*

    FN.FUNDS.TRANSFER.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FUNDS.TRANSFER.NAU = ''
    EB.DataAccess.Opf(FN.FUNDS.TRANSFER.NAU,F.FUNDS.TRANSFER.NAU)

    FN.CHEQUES.PRESENTED = 'F.CHEQUES.PRESENTED'
    F.CHEQUES.PRESENTED = ''
    EB.DataAccess.Opf(FN.CHEQUES.PRESENTED,F.CHEQUES.PRESENTED)

    FN.CHEQUE.TYPE.ACCOUNT = 'F.CHEQUE.TYPE.ACCOUNT'
    F.CHEQUE.TYPE.ACCOUNT = ''
    EB.DataAccess.Opf(FN.CHEQUE.TYPE.ACCOUNT,F.CHEQUE.TYPE.ACCOUNT)


*  Y.FT.ID = ID.NEW
    Y.FT.ID = EB.SystemTables.getIdNew()
    R.FT.REC = ''
    Y.FT.ERR = ''
    Y.DR.AC.NO = ''
    Y.CR.AC.NO = ''

    Y.CHQ.PRESENT.ID = ''
    R.CHQ.PRESENT.REC = ''
    Y.CHQ.PRESENT.ERR = ''

    Y.CHQ.TYPE.AC.NO = ''
    R.CHQ.TYPE.AC.REC = ''
    Y.CHQ.TYPE.AC.ERR = ''
    Y.CHQ.TYPE = ''

    Y.APP = 'FUNDS.TRANSFER'
    Y.FLD = 'PR.CHEQUE.NO'
    Y.POS = ''
*    CALL MULTI.GET.LOC.REF(Y.APP,Y.FLD,Y.POS)
    EB.Foundation.MapLocalFields(Y.APP,Y.FLD,Y.POS)
    
    Y.CHQ.POS = Y.POS<1,1>

RETURN

*-------
PROCESS:
*-------
*
* This section is to define actual process and validations.
*

    Y.VERSION = EB.SystemTables.getVFunction()
    IF Y.VERSION EQ 'D' THEN
        ! IF OPERATOR EQ 'PARTHA' OR 'PARTHA1' THEN DEBUG
*  EB.DataAccess.FRead(Fileid, VKey, Rec, FFileid, Er)
        EB.DataAccess.FRead(FN.FUNDS.TRANSFER.NAU,Y.FT.ID,R.FT.REC,F.FUNDS.TRANSFER.NAU,Y.FT.ERR)
*        Y.CR.AC.NO = R.FT.REC<FT.CREDIT.ACCT.NO>
        Y.CR.AC.NO = R.FT.REC<FT.Contract.FundsTransfer.CreditAcctNo>
*        Y.CHQ.NO = R.FT.REC<FT.LOCAL.REF,Y.CHQ.POS>
        Y.CHQ.NO = R.FT.REC<FT.Contract.FundsTransfer.LocalRef,Y.CHQ.POS>
        
        EB.DataAccess.FRead(FN.CHQEQUE.TYPE.ACCOUNT,Y.CR.AC.NO,R.CHQ.TYPE.AC.REC,F.CHEQUE.TYPE.ACCOUNT,Y.CHQ.TYPE.AC.ERR)
*  Y.CHQ.TYPE = R.CHQ.TYPE.AC.REC<CHQ.TYP.CHEQUE.TYPE>
*  ST.ChqIssue.ChequeIssueAccountDelete(Recid, Suffix)
        Y.CHQ.TYPE = R.CHQ.TYPE.AC.REC<CQ.ChqSubmit.ChequeTypeAccount.ChqTypChequeType>
        Y.CHQ.PRESENT.ID = Y.CHQ.TYPE:".":Y.CR.AC.NO : "-" : Y.CHQ.NO
        
        EB.DataAccess.FRead(FN.CHEQUES.PRESENTED,Y.CHQ.PRESENT.ID,R.CHQ.PRESENT.REC,F.CHEQUES.PRESENTED,Y.CHQ.PRESENT.ERR)
        IF R.CHQ.PRESENT.REC THEN
            ! CALL F.DELETE(FN.CHEQUES.PRESENTED,Y.CHQ.PRESENT.ID) *Next Line add by PARTHA
* EB.DataAccess.FDelete(Fileid, VKey)
*  DELETE F.CHEQUES.PRESENTED,Y.CHQ.PRESENT.ID
            EB.DataAccess.FDelete(FN.CHEQUES.PRESENTED,Y.CHQ.PRESENT.ID)
            EB.TransactionControl.JournalUpdate(Y.CHQ.PRESENT.ID)
        END
    END
RETURN

END