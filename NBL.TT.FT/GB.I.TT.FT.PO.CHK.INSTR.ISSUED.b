* @ValidationCode : MjotNDk0MzAyNjUzOkNwMTI1MjoxNjczNDI3MDExNTkxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Jan 2023 14:50:11
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.I.TT.FT.PO.CHK.INSTR.ISSUED
**************************************************************************
*Subroutine Description:
*-----------------------
* PURPOSE: Check Cheque Number parameter set up & Validate check number.
*
*-------------------------------------------------------------------------
*<Description of the arguments>
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 09/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    
*    $INSERT I_F.TELLER
    $USING TT.Contract
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT I_F.CHEQUE.REGISTER
    $USING CQ.ChqSubmit
* CQ.ChqSubmit.ChequeRegister.ChequeRegAuthoriser
*    $INSERT I_F.CHEQUES.PRESENTED
* CQ.ChqSubmit.ChequesPresented.ChqPreRepresentedCount
    $INSERT I_F.PR.H.INSTR.ISSUED
* $INSERT I_F.CHEQUE.TYPE.ACCOUNT
    $INSERT I_F.PR.H.INSTRUMENT
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.LocalReferences

*    GOSUB OPEN.FILES
    GOSUB PROCESS

OPEN.FILES:
*==========

    FN.PR.H.INSTR.ISSUED = 'F.PR.H.INSTR.ISSUED'
    F.PR.H.INSTR.ISSUED = ''
* EB.DataAccess.Opf(YnameIn, YnameOut) -----TAFC TO TAFJ -- Shibli
    EB.DataAccess.Opf(FN.PR.H.INSTR.ISSUED,F.PR.H.INSTR.ISSUED)

*    FN.CHEQUE.REGISTER = 'F':Y.BR.MNE:'.CHEQUE.REGISTER'

    FN.CHEQUE.REGISTER = 'F.CHEQUE.REGISTER'
    F.CHEQUE.REGISTER = ''
    EB.DataAccess.Opf(FN.CHEQUE.REGISTER,F.CHEQUE.REGISTER)

*    FN.CHEQUES.STOPPED = 'F':Y.BR.MNE:'.CHEQUES.STOPPED'

    FN.CHEQUES.STOPPED = 'F.CHEQUES.STOPPED'
    F.CHEQUES.STOPPED = ''
    EB.DataAccess.Opf(FN.CHEQUES.STOPPED,F.CHEQUES.STOPPED)

*    FN.CHEQUES.PRESENTED = 'F':Y.BR.MNE:'.CHEQUES.PRESENTED'

    FN.CHEQUES.PRESENTED = 'F.CHEQUES.PRESENTED'
    F.CHEQUES.PRESENTED = ''
    EB.DataAccess.Opf(FN.CHEQUES.PRESENTED,F.CHEQUES.PRESENTED)

    FN.CHEQUE.TYPE.ACCOUNT = 'F.CHEQUE.TYPE.ACCOUNT'
    F.CHEQUE.TYPE.ACCOUNT = ''
    EB.DataAccess.Opf(FN.CHEQUE.TYPE.ACCOUNT,F.CHEQUE.TYPE.ACCOUNT)

RETURN

PROCESS:
*=======
    Y.CHQ.NO = 'PR.CHEQUE.NO'
    Y.CHQ.POS = ''
    
    Y.ID.COMPANY = EB.SystemTables.getIdCompany()
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
    Y.APPLICATION = EB.SystemTables.getApplication()
    Y.FUNCTION = EB.SystemTables.getVFunction()
    
    
*    EB.LocalReferences.GetLocRef(Y.APPLICATION,Y.CHQ.NO,Y.CHQ.POS)
** Y.CHEQUE.NO = R.NEW(LOCAL.REF.FIELD)<1,Y.CHQ.POS>
*--------------------------------------------------------------------

    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
        EB.LocalReferences.GetLocRef(Y.APPLICATION,Y.CHQ.NO,Y.CHQ.POS)
*---------------LT-----------BANK CLARIFICATION -----------------------------------------
        Y.CHEQ.TEMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
        Y.CHEQUE.NO = Y.CHEQ.TEMP<1,Y.CHQ.POS>
        
        
* IF R.NEW(FT.TRANSACTION.TYPE) EQ 'ACDD' THEN
        IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TransactionType) EQ 'ACDD' THEN
            Y.ID.COMPANY = EB.SystemTables.getIdCompany()
            Y.DEBIT.ACCT = ''
* CALL DBR("PR.H.INSTRUMENT":@FM:INS.DD.ACCOUNT,Y.ID.COMPANY,Y.DEBIT.ACCT)
*     EB.DataAccess.CacheDbr(Filename, FieldNo, Fkey, ReturnField)
*           EB.DataAccess.Dbr(Checkfile1, I1, Enri2)
            EB.DataAccess.Dbr("PR.H.INSTRUMENT":@FM:INS.DD.ACCOUNT,Y.ID.COMPANY,Y.DEBIT.ACCT)
            IF NOT(Y.DEBIT.ACCT) THEN
*                ETEXT = "Intrument Parameter not setup for DD Payable Account in :"ID.COMPANY
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext("Intrument Parameter not setup for DD Payable Account in :"Y.ID.COMPANY)
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
        END ELSE
* Y.DEBIT.ACCT = R.NEW(FT.CREDIT.ACCT.NO)
            Y.DEBIT.ACCT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
        END
        !        Y.CHEQUE.TYPE = R.NEW(FT.CHEQ.TYPE)
*        DEBIT.VAL.DT = R.NEW(FT.DEBIT.VALUE.DATE)
        DEBIT.VAL.DT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
*        Y.REC.STATUS = R.NEW(FT.RECORD.STATUS)
        Y.REC.STATUS = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.RecordStatus)
    END ELSE
* IF R.NEW(TT.TE.TRANSACTION.CODE) EQ 120 THEN
        IF EB.SystemTables.getRNew(TT.Contract.Teller.TeTransactionCode) EQ 120 THEN
            Y.DEBIT.ACCT = ''
            IF Y.PGM.VERSION EQ ',PR.PO.ISSUE' THEN
                EB.DataAccess.Dbr("PR.H.INSTRUMENT":@FM:INS.PO.ACCOUNT,Y.ID.COMPANY,Y.DEBIT.ACCT)
            END ELSE
                EB.DataAccess.Dbr("PR.H.INSTRUMENT":@FM:INS.DD.ACCOUNT,Y.ID.COMPANY,Y.DEBIT.ACCT)
            END
            IF NOT(Y.DEBIT.ACCT) THEN
*                ETEXT = "Intrument Parameter not setup for DD Payable Account in :"ID.COMPANY
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext("Intrument Parameter not setup for DD Payable Account in :"Y.ID.COMPANY)
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
        END ELSE
*            Y.DEBIT.ACCT = R.NEW(TT.TE.ACCOUNT.2)
            !----Change on 26/11/2010-------!
* Y.DEBIT.ACCT = R.NEW(TT.TE.ACCOUNT.1)
            Y.DEBIT.ACCT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
        END
        !       Y.CHEQUE.TYPE = R.NEW(TT.TE.CHEQ.TYPE)
*        DEBIT.VAL.DT = R.NEW(TT.TE.VALUE.DATE.2)
*        Y.REC.STATUS = R.NEW(TT.TE.RECORD.STATUS)
        
        DEBIT.VAL.DT = EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateTwo)
        Y.REC.STATUS = EB.SystemTables.getRNew(TT.Contract.Teller.TeRecordStatus)
    END

*    Y.BR.MNE = ''
*    Y.BR.CO.CODE = ''
*    CALL GET.ACCT.BRANCH(Y.DEBIT.ACCT,Y.BR.MNE,Y.BR.CO.CODE)
*    IF NOT(Y.BR.MNE) THEN Y.BR.MNE = 'BNK'

    GOSUB OPEN.FILES
    Y.CHEQUE.TYPE=""
    IF NOT(Y.CHEQUE.TYPE) THEN
        EB.DataAccess.FRead(FN.CHEQUE.TYPE.ACCOUNT,Y.DEBIT.ACCT,R.CHEQUE.TYPE.ACCOUNT,F.CHEQUE.TYPE.ACCOUNT,CTA.READ.ERR)
        IF NOT(R.CHEQUE.TYPE.ACCOUNT) THEN
*            ETEXT = 'MISSING CHEQUE TYPE'
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext('MISSING CHEQUE TYPE')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END ELSE
* Y.CHEQUE.TYPE = R.CHEQUE.TYPE.ACCOUNT<CHQ.TYP.CHEQUE.TYPE,1>
            Y.CHEQUE.TYPE = R.CHEQUE.TYPE.ACCOUNT<CQ.ChqSubmit.ChequeTypeAccount.ChqTypChequeType,1>
        END
    END

    IF Y.FUNCTION EQ 'I' OR Y.FUNCTION EQ 'C' THEN
        Y.CHQ.REG.ID = Y.CHEQUE.TYPE:'.':Y.DEBIT.ACCT
        EB.DataAccess.FRead(FN.CHEQUE.REGISTER,Y.CHQ.REG.ID,R.CHEQUE.REGISTER,F.CHEQUE.REGISTER,CR.READ.ERR)
        IF NOT(CR.READ.ERR) THEN
* CR.ISSUE.RANGE = R.CHEQUE.REGISTER<CHEQUE.REG.CHEQUE.NOS>
            CR.ISSUE.RANGE = R.CHEQUE.REGISTER<CQ.ChqSubmit.ChequeRegister.ChequeRegChequeNos>
            CR.ISSUE.RANGE.CNT = DCOUNT(CR.ISSUE.RANGE,@VM)
            Y.START.NO = ''
            FOR I = 1 TO CR.ISSUE.RANGE.CNT
                Y.RANGE.FLD = ''
                Y.RANGE.FLD = CR.ISSUE.RANGE<1,I>
                Y.START.NO = Y.CHEQUE.NO
                Y.END.NO = ''
                Y.RESULT = ''
                Y.ERROR = ''
                CALL EB.MAINTAIN.RANGES(Y.RANGE.FLD,Y.START.NO,Y.END.NO,'ENQ',Y.RESULT,Y.ERROR)
                IF Y.RESULT EQ 1 THEN EXIT
            NEXT I
            IF NOT(Y.RESULT) THEN
*                ETEXT = "CHEQUE NUMBER ":Y.CHEQUE.NO:" NOT ISSUED TO THE ACCOUNT ":Y.DEBIT.ACCT
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext("CHEQUE NUMBER ":Y.CHEQUE.NO:" NOT ISSUED TO THE ACCOUNT ":Y.DEBIT.ACCT)
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
*  CR.RET.RANGE = R.CHEQUE.REGISTER<CHEQUE.REG.RETURNED.CHQS>
            CR.RET.RANGE = R.CHEQUE.REGISTER<CQ.ChqSubmit.ChequeRegister.ChequeRegReturnedChqs>
            LOCATE Y.CHEQUE.NO IN CR.RET.RANGE<1,1> SETTING Y.RET.RG.POS THEN
*                ETEXT = "CHEQUE NUMBER ":Y.CHEQUE.NO:" ALREADY CANCELLED"
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext("CHEQUE NUMBER ":Y.CHEQUE.NO:" ALREADY CANCELLED")
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END

            IF Y.PGM.VERSION NE ',PR.PO.ISSUE.AMEND' THEN
                ! IF OPERATOR EQ 'SAYED1' THEN DEBUG
                Y.CHQ.PRESENTED.ID = Y.CHEQUE.TYPE:'.':Y.DEBIT.ACCT:'-':Y.CHEQUE.NO
                CALL F.READ(FN.CHEQUES.PRESENTED,Y.CHQ.PRESENTED.ID,R.CHQ.PRESENT,F.CHEQUES.PRESENTED,CHQ.PRESENT.READ.ERR)
                IF R.CHQ.PRESENT THEN
*                    ETEXT = "CHEQUE NUMBER ":Y.CHEQUE.NO:" ALREADY PRESENTED ON ":R.CHQ.PRESENT<CHQ.PRE.DATE.PRESENTED,1>
*                    CALL STORE.END.ERROR
                    EB.SystemTables.setEtext("CHEQUE NUMBER ":Y.CHEQUE.NO:" ALREADY PRESENTED ON ":R.CHQ.PRESENT<CHQ.PRE.DATE.PRESENTED,1>)
                    EB.ErrorProcessing.StoreEndError()
                    RETURN
                    ! END ELSE     ;* Marked by Partha as required by Tanveer Vai; at the time of issue Cheque should not be in Preseted
                    !     IF NOT(OFS.VAL.ONLY) THEN
                    !         R.CHQ.PRESENT<CHQ.PRE.DATE.PRESENTED,-1> = TODAY
                    !         CALL F.WRITE(FN.CHEQUES.PRESENTED,Y.CHQ.PRESENTED.ID,R.CHQ.PRESENT)
                    !     END
                END
            END

            Y.CHQ.STOPPED.ID = Y.DEBIT.ACCT:'*':Y.CHEQUE.NO
            EB.DataAccess.FRead(FN.CHEQUES.STOPPED,Y.CHQ.STOPPED.ID,R.CHQ.STOPPED,F.CHEQUES.STOPPED,CHQ.STOP.READ.ERR)
            IF R.CHQ.STOPPED THEN
*                ETEXT = "CHEQUE NUMBER ":Y.CHEQUE.NO:" ALREADY STOPPED"
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext("CHEQUE NUMBER ":Y.CHEQUE.NO:" ALREADY STOPPED")
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
            GOSUB CHK.INSTR.ISSUED
        END ELSE
*            ETEXT = "CHEQUE REGISTER NOT AVAILABLE FOR ACCOUNT NUMBER ":Y.DEBIT.ACCT
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("CHEQUE REGISTER NOT AVAILABLE FOR ACCOUNT NUMBER ":Y.DEBIT.ACCT)
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
        RETURN

CHK.INSTR.ISSUED:
*===================
        Y.DEBIT.ACCT = Y.CHEQUE.TYPE:'.':Y.DEBIT.ACCT:'-':Y.CHEQUE.NO
        ! IF Y.REC.STATUS[1,2] EQ 'IN' THEN
        IF Y.FUNCTION EQ 'I' OR Y.FUNCTION EQ 'C' THEN
            EB.DataAccess.FRead(FN.PR.H.INSTR.ISSUED,Y.DEBIT.ACCT,R.INSTR.ISSUED,F.PR.H.INSTR.ISSUED,INSTR.READ.ERR)
            IF R.INSTR.ISSUED THEN
                Y.CHQ.NO.ISSUED = R.INSTR.ISSUED<INS.CHEQUE.NUMBER>
                ! LOCATE Y.CHEQUE.NO IN Y.CHQ.NO.ISSUED<1,1> SETTING Y.CHEQ.POS THEN
                IF Y.CHEQUE.NO EQ Y.CHQ.NO.ISSUED THEN
*                    ETEXT = "CHEQUE NUMBER ":Y.CHEQUE.NO:" ALREADY ISSUED ON ":R.INSTR.ISSUED<INS.DATE.OF.ISSUE>
*                    CALL STORE.END.ERROR
                    EB.SystemTables.setEtext("CHEQUE NUMBER ":Y.CHEQUE.NO:" ALREADY ISSUED ON ":R.INSTR.ISSUED<INS.DATE.OF.ISSUE>)
                    EB.ErrorProcessing.StoreEndError()
                    RETURN
                END
            END
            !           CRT R.INSTR.ISSUED
            !LOCATE Y.CHEQUE.NO IN Y.CHQ.NO.ISSUED<1,1> SETTING Y.CHEQ.POS ELSE
            !   ETEXT = "CHEQUE NUMBER ":Y.CHEQUE.NO:" NOT AVAILABLE IN REGISTER"
            !   CALL STORE.END.ERROR
            !    RETURN
            !END
            !END
            !CALL F.WRITE(FN.PR.H.INSTR.ISSUED,Y.DEBIT.ACCT,R.INSTR.ISSUED)
        END
        RETURN

    END
