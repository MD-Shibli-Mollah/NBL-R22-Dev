* @ValidationCode : MjoxNjQzMzUwMTU3OkNwMTI1MjoxNjczNTAzMTQ3OTAxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Jan 2023 11:59:07
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.I.TT.CHECK.AC.BAL

*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>493</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE ABL.CHECK.AC.BAL
        !-----------------------------------------------------------------------------!
**********Notice Condtion For Savings And SNTD Account
**********Developed By Manik-20100922
**********Specification : It Checks whether Notice is given or not for withdrawal of certain amount
**********                money defined in the ABP.H.NT.PM
        !------------------------------------------------------------------------------!
*<Description of the arguments>
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 12/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************
        $INSERT I_COMMON
        $INSERT I_EQUATE
*    $INSERT I_F.ACCOUNT
*    $INSERT I_F.COMPANY
*    $INSERT I_F.FUNDS.TRANSFER
*    $INSERT I_F.TELLER
*    $INSERT I_F.TRANSACTION
*    $INSERT I_F.TELLER.TRANSACTION

        $USING AC.AccountOpening
        $USING FT.Contract
        $USING TT.Contract
        $USING CQ.ChqSubmit
        $USING EB.Utility
        $USING EB.LocalReferences
        $USING EB.DataAccess
        $USING EB.SystemTables
        $USING EB.ErrorProcessing
        $USING EB.Foundation
        $USING EB.Updates
        $USING EB.API
        $USING ST.ChargeConfig
        $USING ST.Config
        $USING ST.CompanyCreation
        $USING TT.Config
    
        $INSERT I_F.ABL.H.AC.VIOLATION
* $INSERT I_F.ACCT.GROUP.CONDITION
*$INSERT I_F.ABL.H.NOTICE.WITHDRAWAL
        $INSERT I_F.ABP.H.NOTICE.WITHDRAWAL
*$INSERT I_F.ABL.H.NOTICE.PARAMETER
        $INSERT I_F.ABP.H.NT.PM
* $INSERT I_F.AZ.ACCOUNT
   
        Y.APPLICATION = EB.SystemTables.getApplication()
*    IF V$FUNCTION EQ 'I' OR V$FUNCTION EQ 'C' ELSE RETURN
        IF Y.APPLICATION EQ 'FUNDS.TRANSFER' OR Y.APPLICATION EQ 'TELLER' ELSE RETURN

        IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
* IF EB.SystemTables.getRNew(FT.DEBIT.ACCT.NO) MATCHES '3A...' THEN RETURN
* EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo) ; -- SHIBLI - TAFJ
            Y.FT.DEBIT.ACC.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
            IF Y.FT.DEBIT.ACC.NO MATCHES '3A...' THEN RETURN
        END
    
        GOSUB INITIALISE
        GOSUB OPEN.FILES
        GOSUB GET.TRANS.INFO
        GOSUB CHK.CLEARED.BALANCE
        GOSUB CHQ.NO.RESTRICT
        RETURN

INITIALISE:
*==========
        Y.DR.CR.MARKER = ''
        TRANS.CODE.1 = ''
        Y.DEBIT.ACCT = ''
        Y.TRANS.AMT = ''
        Y.TELLER.TRANS = ''
        Y.WORKING.BALANCE = ''
        Y.NOTICE.AMOUNT = ''
        Y.NOTICE.PERIOD = ''
        Y.NOTICE.GRP = ''
        Y.NOTICE.GRP.ID=''
        R.NOTICE.PARAM = ''
        RETURN

OPEN.FILES:
*==========
*    FN.NOTICE.PARAM ='F.ABL.H.NOTICE.PARAMETER'
        FN.NOTICE.PARAM ='F.ABP.H.NT.PM'

        F.NOTICE.PARAM =''
        EB.DataAccess.Opf(FN.NOTICE.PARAM,F.NOTICE.PARAM)

        FN.ACCOUNT = 'F.ACCOUNT'
        F.ACCOUNT = ''
        EB.DataAccess.Opf(FN.ACCOUNT,F.ACCOUNT)

*    FN.NOTICE.WITHDRAWAL = 'F.ABL.H.NOTICE.WITHDRAWAL'
        FN.NOTICE.WITHDRAWAL = 'F.ABP.H.NOTICE.WITHDRAWAL'
        F.NOTICE.WITHDRAWAL = ''
        EB.DataAccess.Opf(FN.NOTICE.WITHDRAWAL,F.NOTICE.WITHDRAWAL)

        FN.ABL.H.AC.VIOLATION = 'F.ABL.H.AC.VIOLATION'
        F.ABL.H.AC.VIOLATION = ''
        EB.DataAccess.Opf(FN.ABL.H.AC.VIOLATION,F.ABL.H.AC.VIOLATION)

        !S - AYUSH - 20140424

*=========== THIS BLOCK IS COMMENTED FOR AZ.ACCOUNT -------------------*
*        Y.APP = 'AZ.ACCOUNT'
*        Y.FLD = 'INSTALMENT.AMT'
*        Y.POS = ''
*       * CALL MULTI.GET.LOC.REF(Y.APP,Y.FLD,Y.POS)
*     EB.LocalReferences.GetLocRef(Y.APP,Y.FLD,Y.POS)
*        Y.INSTALMENT.AMT.POS = Y.POS<1,1>
*        !E
*=========== THIS BLOCK IS COMMENTED FOR AZ.ACCOUNT -----END--------------*
        RETURN

GET.TRANS.INFO:
*==============
        IF APPLICATION EQ 'TELLER' THEN
            Y.DR.CR.MARKER = EB.SystemTables.getRNew(TT.Contract.Teller.TeDrCrMarker)
            IF Y.DR.CR.MARKER = 'DEBIT' THEN
                Y.DEBIT.ACCT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
                Y.TRANS.AMT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
                Y.VALUE.DATE = EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateOne)
            END ELSE
                Y.DEBIT.ACCT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
                Y.TRANS.AMT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalTwo)
                Y.VALUE.DATE = EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateTwo)
            END
            Y.CHEQ.NUMBER=EB.SystemTables.getRNew(TT.Contract.Teller.TeChequeNumber)
            Y.OVERRIDE.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeOverride)
            Y.OVERRIDE.CNT = DCOUNT(Y.OVERRIDE.NO,@VM)
            Y.OVERRIDE.CNT +=1
        END ELSE
            IF APPLICATION EQ 'FUNDS.TRANSFER' THEN
                Y.DEBIT.ACCT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
                Y.TRANS.AMT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
                IF NOT(Y.TRANS.AMT) THEN Y.TRANS.AMT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAmount)
                Y.VALUE.DATE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
                IF NOT(Y.VALUE.DATE) THEN Y.VALUE.DATE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditValueDate)
                Y.CHEQ.NUMBER=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.ChequeNumber)
                Y.OVERRIDE.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Override)
                Y.OVERRIDE.CNT = DCOUNT(Y.OVERRIDE.NO,@VM)
                Y.OVERRIDE.CNT +=1
            END
        END

        !S - AYUSH - 20140424
        !S - SHIBLI - 20230112
**=========== THIS BLOCK IS COMMENTED FOR AZ.ACCOUNT -------------------*
*        IF APPLICATION EQ 'AZ.ACCOUNT' THEN
*            Y.DEBIT.ACCT = EB.SystemTables.getRNew(AZ.REPAY.ACCOUNT)
*            Y.TRANS.AMT = EB.SystemTables.getRNew(AZ.LOCAL.REF)<1,Y.INSTALMENT.AMT.POS>
*            IF NOT(Y.TRANS.AMT) THEN
*                Y.TRANS.AMT = EB.SystemTables.getRNew(AZ.PRINCIPAL)
*            END
*            Y.VALUE.DATE = EB.SystemTables.getRNew(AZ.VALUE.DATE)
*            Y.OVERRIDE.NO = EB.SystemTables.getRNew(AZ.OVERRIDE)
*            Y.OVERRIDE.CNT = DCOUNT(Y.OVERRIDE.NO,@VM)
*            Y.OVERRIDE.CNT +=1
*
*        END
*        !E
*=========== THIS BLOCK IS COMMENTED FOR AZ.ACCOUNT -------END------------*
        RETURN

CHK.CLEARED.BALANCE:
*===================
        !-------For Notice Period-----------!

        DATE.1 = ''
        DATE.2 = Y.VALUE.DATE
        DAYS = 'C'

*------------- EB.DataAccess.FRead(----------- Shibli
        EB.DataAccess.FRead(FN.ACCOUNT,Y.DEBIT.ACCT,R.ACCOUNT,F.ACCOUNT,ACCT.READ.ERR)
        Y.WORKING.BALANCE = R.ACCOUNT<AC.AccountOpening.Account.WorkingBalance> + Y.TRANS.AMT
        Y.NOTICE.GRP = R.ACCOUNT<AC.AccountOpening.Account.ConditionGroup>:R.ACCOUNT<AC.AccountOpening.Account.Currency>
        
        EB.DataAccess.FRead(FN.NOTICE.PARAM,Y.NOTICE.GRP,R.NOTICE.PARAM,F.NOTICE.PARAM,NOTICE.PARAM.ERR)
        IF R.NOTICE.PARAM EQ '' THEN RETURN
        Y.NOTICE.AMOUNT =  R.NOTICE.PARAM<NP.NOTICE.AMOUNT>
        Y.NOTICE.PERIOD  = R.NOTICE.PARAM<NP.NOTICE.DAYS>

*    IF ( Y.NOTICE.GRP EQ Y.NOTICE.GRP.ID ) AND  ( Y.TRANS.AMT GE Y.NOTICE.AMOUNT OR ( Y.TRANS.AMT GT (Y.WORKING.BALANCE/4))) THEN

        IF ( Y.TRANS.AMT GT Y.NOTICE.AMOUNT OR ( Y.TRANS.AMT GT (Y.WORKING.BALANCE/4))) THEN
            EB.DataAccess.FRead(FN.NOTICE.WITHDRAWAL,Y.DEBIT.ACCT,R.NOTICE.WITHDRAWAL,F.NOTICE.WITHDRAWAL,NOTICE.WITH.ERR)
            IF R.NOTICE.WITHDRAWAL THEN
                DATE.1 = R.NOTICE.WITHDRAWAL<NW.NOTICE.DATE>
                CALL CDD ('', DATE.1, DATE.2, DAYS)
                BEGIN CASE
                    CASE DAYS LT Y.NOTICE.PERIOD
                        TEXT = "NO NOTICE GIVEN FOR WITHDRAWAL"
                        CALL STORE.OVERRIDE(Y.OVERRIDE.CNT)
                    CASE DAYS GT 30
                        TEXT = "NO NOTICE GIVEN FOR WITHDRAWAL"
                        CALL STORE.OVERRIDE(Y.OVERRIDE.CNT)
                    CASE 1
*                CALL F.DELETE(FN.NOTICE.WITHDRAWAL,Y.DEBIT.ACCT)
* DELETE F.NOTICE.WITHDRAWAL,Y.DEBIT.ACCT
                        EB.DataAccess.FDelete(FN.NOTICE.WITHDRAWAL, Y.DEBIT.ACCT)
                        EB.TransactionControl.JournalUpdate(Y.DEBIT.ACCT)
                END CASE
            END
            ELSE
                TEXT = "NO NOTICE GIVEN FOR WITHDRAWAL"
                CALL STORE.OVERRIDE(Y.OVERRIDE.CNT)
            END

            RETURN

CHQ.NO.RESTRICT:
*==============
            Y.APP.VER=APPLICATION:PGM.VERSION
            IF Y.APP.VER EQ 'FUNDS.TRANSFER,ACTR.CIBTA.ADV.T24' OR Y.APP.VER EQ 'FUNDS.TRANSFER,ACTR.CIBTA.DR.ADV.T24.A2Z' OR Y.APP.VER EQ 'FUNDS.TRANSFER,ACTR.CIBTA.ADV.T24.ROBI' THEN
                IF RIGHT(ID.COMPANY,4) NE LEFT(Y.DEBIT.ACCT,4) THEN
                    Y.CHQ.NUMBER= EB.SystemTables.getRNew(FT.CHEQUE.NUMBER)
                    IF NOT(Y.CHQ.NUMBER) THEN
                        ETEXT = 'PLEASE ENTER THE CHEQUE NUMBER'
                        CALL STORE.END.ERROR
                    END
                END
            END
            RETURN

        END
