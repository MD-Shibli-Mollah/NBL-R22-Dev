* @ValidationCode : MjoyMDkxMDgzNTUyOkNwMTI1MjoxNjczMTY2Mjc5MzAyOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jan 2023 14:24:39
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.PO.FT.DETAIL.RETRIEVE
*-----------------------------------------------------------------------------
* <Rating>100</Rating>
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.PR.H.INSTR.ISSUED
* $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
* $INSERT I_F.CHEQUE.TYPE.ACCOUNT
    $USING CQ.ChqSubmit
* $INSERT I_F.TELLER
    $USING TT.Contract
    $INSERT I_F.PR.H.INSTRUMENT
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Foundation
    $USING EB.Updates
    $USING EB.ErrorProcessing

    !**************************************************************
    !Collection Amount from PR.H.INSTR.ISSUED for the corresponding
    !**************************************************************

    FN.INS = 'F.PR.H.INSTRUMENT'
    F.INS = ''
    FN.INSTR.ISSUED = 'F.PR.H.INSTR.ISSUED'
    F.INSTR.ISSUED = ''
    EB.Foundation.MapLocalFields('FUNDS.TRANSFER','BRANCH',Y.POS.BRANCH)
*    Y.ID.4 = R.NEW(FT.LOCAL.REF)<1,Y.POS.BRANCH>
    Y.ID.4 = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.POS.BRANCH>
    Y.ID = "BD001": Y.ID.4


* EB.DataAccess.Opf(YnameIn, YnameOut)
    EB.DataAccess.Opf(FN.INS,F.INS)
* EB.DataAccess.Opf(FN.INSTR.ISSUED, F.INSTR.ISSUED)

    EB.DataAccess.FRead(FN.INS,Y.ID,R.INS,F.INS,Y.ERR)

* R.NEW(FT.DEBIT.ACCT.NO) = R.INS<INS.PO.ACCOUNT>
    Y.INS.PO.ACCOUNT = R.INS<INS.PO.ACCOUNT>
    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitAcctNo, Y.INS.PO.ACCOUNT)

    Y.MESSAGE = EB.SystemTables.getMessage()
    IF Y.MESSAGE EQ 'VAL' THEN RETURN
* Y.CHQ.NUMBER=COMI
    Y.CHQ.NUMBER= EB.SystemTables.getComi()
    
    FN.INS.ISS='F.PR.H.INSTR.ISSUED'
    F.INS.ISS = ''
    EB.DataAccess.Opf(FN.INS.ISS,F.INS.ISS)
    Y.ACCOUNT.NUMBER= EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
* Y.CHQ.TYPE = EB.SystemTables.getRNew(FT.CHEQ.TYPE)
    Y.CHQ.TYPE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CheqType)
    Y.INS.ISS.ID = Y.CHQ.TYPE:'.':Y.ACCOUNT.NUMBER:'-':Y.CHQ.NUMBER
    !DEBUG
    EB.DataAccess.FRead(FN.INS.ISS,Y.INS.ISS.ID,R.INS.ISS,F.INS.ISS,Y.ERR)

    IF Y.ERR THEN
*        E='INSTRUMENT NOT ISSUED1'
*        CALL ERR
        EB.SystemTables.setEtext("INSTRUMENT NOT ISSUED1")
        EB.ErrorProcessing.StoreEndError()
    END

    Y.ISS.AMOUNT=R.INS.ISS<INS.ISSUE.AMOUNT>
    Y.PAY.TO = R.INS.ISS<INS.PAY.TO>
*    R.NEW(FT.DEBIT.AMOUNT) = Y.ISS.AMOUNT
    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitAmount, Y.ISS.AMOUNT)
*    R.NEW(FT.PAYMENT.DETAILS) =  Y.PAY.TO
    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.PaymentDetails, Y.PAY.TO)

RETURN

END