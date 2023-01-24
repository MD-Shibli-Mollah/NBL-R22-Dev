* @ValidationCode : MjotMTM0NzQ5NzkyOTpDcDEyNTI6MTY3NDU1MjQ1NTUzNjp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 24 Jan 2023 15:27:35
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.A.CHEQUE.RETURN.ENTRY
*-----------------------------------------------------------------------------
    !-------------------------------------------------
    !This subroutine is used to make a contra entry when
    !a cheque is return
    !developed by -Manik-20100925

    !Changes done by Manikandan V - 20140127 to avoid the GL
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 12/01/2023        - Retrofit     - MD Shibli Mollah - FDS
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    
*    $INSERT I_F.STMT.ENTRY
    $USING AC.EntryCreation
*    $INSERT I_F.CHEQUE.COLLECTION
    $USING CQ.ChqSubmit
* CQ.ChqSubmit.ChequeCollection.ChqColAddlInfo
*    $INSERT I_F.TELLER
    $USING TT.Contract
*    $INSERT I_F.DATES
    $USING EB.Utility
    $USING EB.LocalReferences
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Foundation
    $USING EB.Updates
    $USING AC.API
    $USING EB.TransactionControl
    
    Y.ID.COMPANY = EB.SystemTables.getIdCompany()
    Y.TODAY = EB.SystemTables.getToday()
    
    Y.CHQ.STATUS='CHQ.COLL.STATUS'
    Y.CHQ.STATUS.POS = ''
    EB.LocalReferences.GetLocRef("CHEQUE.COLLECTION",Y.CHQ.STATUS,Y.CHQ.STATUS.POS)

    Y.CHQ.NO ='PR.CHEQUE.NO'
    Y.CHQ.NO.POS = ''
    EB.LocalReferences.GetLocRef("TELLER",Y.CHQ.NO,Y.CHQ.NO.POS)

    Y.CHQ.CURR.STATUS = ''
* Y.CHQ.CURR.STATUS = R.NEW(CHQ.COL.LOCAL.REF)<1,Y.CHQ.STATUS.POS>
    Y.TEMP = EB.SystemTables.getRNew(CQ.ChqSubmit.ChequeCollection.ChqColLocalRef)
    Y.CHQ.CURR.STATUS = Y.TEMP<1,Y.CHQ.STATUS.POS>

    IF Y.CHQ.CURR.STATUS EQ 'RETURNED' THEN
        GOSUB INITIALIZE
        GOSUB RESOLVE.COMMON.LEG.ACCOUNTING
        GOSUB RESOLVE.DEBIT.LEG.ACCOUNTING
        GOSUB RESOLVE.CREDIT.LEG.ACCOUNTING
        GOSUB PERFORM.ACCOUNTING
    END

INITIALIZE:
    !*********!
    ! Initialize Accounting Arrays...
    Y.TT.ID = ''
    REC.TT = ''

    Y.JUL.DATE = ''
* Y.JUL.DATE = RIGHT(R.DATES(EB.DAT.JULIAN.DATE),5)
    
    Y.DAT.JUL.DATE = EB.SystemTables.getRDates(EB.Utility.Dates.DatJulianDate)
    Y.JUL.DATE = RIGHT(Y.DAT.JUL.DATE,5)

    Y.EB.ACC.COMM.ARR = ''
    Y.EB.CR.ARR = ''
    Y.EB.DR.ARR = ''
    Y.EB.ACC.ARR = ''
    Y.CHQ.COLL.AMT = ''
    Y.TR.REF = ''
* Y.TT.ID = R.NEW(CHQ.COL.TXN.ID)
    Y.TT.ID = EB.SystemTables.getRNew(CQ.ChqSubmit.ChequeCollection.ChqColTxnId)

*----------BANK COMMENT------------------------*
*IF Y.TT.ID[3,5] EQ Y.JUL.DATE THEN
*    FN.TT = 'F.TELLER'
* END

*ELSE
*    FN.TT = 'F.TELLER$HIS'
*    Y.TT.ID = Y.TT.ID:";1"
* END
*----------BANK COMMENT---------END---------------*
    FV.TT = ''
    FV.TT.HIS = ''
    FN.TT = 'F.TELLER'
    FN.TT.HIS = 'F.TELLER$HIS'


    EB.DataAccess.Opf(FN.TT,FV.TT)
    EB.DataAccess.FRead(FN.TT,Y.TT.ID,REC.TT,FV.TT,ERR.TT)

    IF ERR.TT THEN
        REC.TT = ''
        Y.TT.ID = Y.TT.ID:";1"
        EB.DataAccess.Opf(FN.TT.HIS,FV.TT.HIS)
        EB.DataAccess.FRead(FN.TT.HIS,Y.TT.ID,REC.TT,FV.TT.HIS,ERR1.TT)
    END

*    Y.CHQ.COLL.AMT = R.NEW(CHQ.COL.AMOUNT)
    Y.CHQ.COLL.AMT = EB.SystemTables.getRNew(CQ.ChqSubmit.ChequeCollection.ChqColAmount)
*    Y.DR.ACCOUNT = R.NEW(CHQ.COL.CREDIT.ACC.NO)
    Y.DR.ACCOUNT = EB.SystemTables.getRNew(CQ.ChqSubmit.ChequeCollection.ChqColCreditAccNo)
*    Y.CR.ACCOUNT =REC.TT<TT.TE.ACCOUNT.1>
    Y.CR.ACCOUNT =REC.TT<TT.Contract.Teller.TeAccountOne>
    !Y.TR.REF = REC.TT<TT.TE.LOCAL.REF,Y.CHQ.NO.POS>
* Y.TR.REF = ID.NEW        ;*** Add by Rayhan for Transaction Ref
   
    Y.TR.REF = EB.SystemTables.getIdNew()
    Y.CHQ.COL.TXN.CODE = EB.SystemTables.getRNew(CQ.ChqSubmit.ChequeCollection.ChqColTxnCode)
    BEGIN CASE
        CASE Y.CHQ.COL.TXN.CODE EQ '93'
            Y.PAID.DR.CODE = '134'
            Y.PAID.CR.CODE = '134'
        CASE Y.CHQ.COL.TXN.CODE EQ '95'
            Y.PAID.DR.CODE = '135'
            Y.PAID.CR.CODE = '135'

        CASE Y.CHQ.COL.TXN.CODE EQ '92'
            Y.PAID.DR.CODE = '136'
            Y.PAID.CR.CODE = '136'
    END CASE
RETURN

RESOLVE.COMMON.LEG.ACCOUNTING:
    !****************************!

    Y.EB.ACC.COMM.ARR<AC.EntryCreation.StmtEntry.SteCompanyCode> = Y.ID.COMPANY
    Y.EB.ACC.COMM.ARR<AC.EntryCreation.StmtEntry.SteTransReference> = Y.TR.REF
    Y.EB.ACC.COMM.ARR<AC.EntryCreation.StmtEntry.SteValueDate> = Y.TODAY
    Y.EB.ACC.COMM.ARR<AC.EntryCreation.StmtEntry.SteBookingDate> = Y.TODAY
    Y.EB.ACC.COMM.ARR<AC.EntryCreation.StmtEntry.SteSystemId> = 'AC'
    Y.EB.ACC.COMM.ARR<AC.EntryCreation.StmtEntry.SteCurrencyMarket> = '1'
    Y.EB.ACC.COMM.ARR<AC.EntryCreation.StmtEntry.SteCurrency> = 'BDT'
    Y.EB.ACC.COMM.ARR<AC.EntryCreation.StmtEntry.SteOurReference> = 'AC-CHEQUE RETURN'
    Y.EB.ACC.COMM.ARR<AC.EntryCreation.StmtEntry.SteAccountOfficer> = 1
RETURN

!***************************!
RESOLVE.DEBIT.LEG.ACCOUNTING:
    !***************************!
    Y.EB.DR.ARR = Y.EB.ACC.COMM.ARR
    Y.EB.DR.ARR<AC.EntryCreation.StmtEntry.SteAmountLcy> = (Y.CHQ.COLL.AMT * -1)
    Y.EB.DR.ARR<AC.EntryCreation.StmtEntry.SteTransactionCode> = Y.PAID.DR.CODE
    Y.EB.DR.ARR<AC.EntryCreation.StmtEntry.SteAccountNumber> = Y.DR.ACCOUNT
RETURN


!****************************!
RESOLVE.CREDIT.LEG.ACCOUNTING:
    !****************************!
    Y.EB.CR.ARR = Y.EB.ACC.COMM.ARR
    Y.EB.CR.ARR<AC.EntryCreation.StmtEntry.SteAmountLcy> = Y.CHQ.COLL.AMT
    Y.EB.CR.ARR<AC.EntryCreation.StmtEntry.SteAccountNumber> = Y.CR.ACCOUNT
    Y.EB.CR.ARR<AC.EntryCreation.StmtEntry.SteTransactionCode> = Y.PAID.CR.CODE
RETURN

!*****************!
PERFORM.ACCOUNTING:
    !*****************!

    Y.EB.ACC.ARR = ''
    Y.EB.ACC.ARR<-1> = LOWER(Y.EB.CR.ARR)
    Y.EB.ACC.ARR<-1> = LOWER(Y.EB.DR.ARR)
    Y.EB.CR.ARR = ''
    Y.EB.DR.ARR = ''
    EB.ERR = ''
    ACC.TYPE = "SAO":@FM:@FM:"UPDATE.ACTIVITY"
*    CALL EB.ACCOUNTING("ACC",ACC.TYPE,Y.EB.ACC.ARR,EB.ERR)
* AC.API.EbAccounting(Pgm, Type, Entries, Forward)  ---TAFJ --- SHIBLI - 20230112
    AC.API.EbAccounting("ACC",ACC.TYPE,Y.EB.ACC.ARR,EB.ERR)
*    CALL JOURNAL.UPDATE('')
    EB.TransactionControl.JournalUpdate('')
    SENSITIVITY = ''
RETURN
END
