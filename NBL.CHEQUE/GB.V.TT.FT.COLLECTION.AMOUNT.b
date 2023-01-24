* @ValidationCode : MjotMTM1ODYzNDUwOTpDcDEyNTI6MTY3MzI0NTc3OTI5MDp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 09 Jan 2023 12:29:39
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.TT.FT.COLLECTION.AMOUNT
*-----------------------------------------------------------------------------
* <Rating>163</Rating>
*-----------------------------------------------------------------------------
**************************************************************************
*Subroutine Description:
*-----------------------
* PURPOSE:
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
    $INSERT I_F.PR.H.INSTR.ISSUED
* $INSERT I_F.CHEQUE.TYPE.ACCOUNT
    $USING CQ.ChqSubmit
   
* $INSERT I_F.TELLER
    $USING FT.Contract
*    $INSERT T24.BP I_F.TELLER
    $USING TT.Contract
    $INSERT I_F.PR.H.INSTRUMENT
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

    !**************************************************************
    !Collection Amount from PR.H.INSTR.ISSUED for the corresponding
    !**************************************************************
    ! IF OPERATOR EQ 'PARTHA' THEN DEBUG

    Y.MESSAGE = EB.SystemTables.getMessage()
    IF Y.MESSAGE EQ 'VAL' THEN RETURN
    Y.COMI = EB.SystemTables.getComi()
    Y.CHQ.NUMBER = Y.COMI
    Y.ACCOUNT.NUMBER =''
    Y.CHQ.TYPE = ''
    FN.INS.ISS='F.PR.H.INSTR.ISSUED'
    F.INS.ISS=''
    R.INS.ISS=''
    Y.ERR=''
    Y.ISS.AMOUNT=''
    Y.PAY.TO = ''
    EB.DataAccess.Opf(FN.INS.ISS,F.INS.ISS)
    
    Y.APPLICATION = EB.SystemTables.getApplication()
    IF Y.APPLICATION EQ 'TELLER' THEN
*        Y.ACCOUNT.NUMBER= R.NEW(TT.TE.ACCOUNT.1)
        Y.ACCOUNT.NUMBER= EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
*        Y.CHQ.TYPE = R.NEW(TT.TE.CHEQ.TYPE)
        Y.CHQ.TYPE = EB.SystemTables.getRNew(TT.Contract.Teller.TeCheqType)

        Y.INS.ISS.ID = Y.CHQ.TYPE:'.':Y.ACCOUNT.NUMBER:'-':Y.CHQ.NUMBER

        EB.DataAccess.FRead(FN.INS.ISS,Y.INS.ISS.ID,R.INS.ISS,F.INS.ISS,Y.ERR)
        IF Y.ERR THEN
*            E='INSTRUMENT NOT ISSUED'
*            CALL ERR
            EB.SystemTables.setEtext('INSTRUMENT NOT ISSUED')
            EB.ErrorProcessing.StoreEndError()
        END

        Y.ISS.AMOUNT=R.INS.ISS<INS.ISSUE.AMOUNT>
        Y.PAY.TO = R.INS.ISS<INS.PAY.TO>
*        R.NEW(TT.TE.AMOUNT.LOCAL.1) = Y.ISS.AMOUNT
*        R.NEW(TT.TE.NARRATIVE.1) =  Y.PAY.TO
        EB.SystemTables.setRNew(TT.Contract.Teller.TeAmountLocalOne, Y.ISS.AMOUNT)
        EB.SystemTables.setRNew(TT.Contract.Teller.TeNarrativeOne, Y.PAY.TO)
    END

    ELSE

*        Y.ACCOUNT.NUMBER= R.NEW(FT.DEBIT.ACCT.NO)
        Y.ACCOUNT.NUMBER= EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)

*        Y.CHQ.TYPE = R.NEW(FT.CHEQ.TYPE)
        Y.CHQ.TYPE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CheqType)

        Y.INS.ISS.ID = Y.CHQ.TYPE:'.':Y.ACCOUNT.NUMBER:'-':Y.CHQ.NUMBER

        EB.DataAccess.FRead(FN.INS.ISS,Y.INS.ISS.ID,R.INS.ISS,F.INS.ISS,Y.ERR)
        IF Y.ERR THEN
*            E='INSTRUMENT NOT ISSUED'
*            CALL ERR
            EB.SystemTables.setEtext('INSTRUMENT NOT ISSUED')
            EB.ErrorProcessing.StoreEndError()
        END

        Y.ISS.AMOUNT=R.INS.ISS<INS.ISSUE.AMOUNT>
        Y.PAY.TO = R.INS.ISS<INS.PAY.TO>
*mod 1 line for BACH integration
        !        R.NEW(FT.CREDIT.AMOUNT) = Y.ISS.AMOUNT
*        R.NEW(FT.DEBIT.AMOUNT) = Y.ISS.AMOUNT
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitAmount, Y.ISS.AMOUNT)
*        R.NEW(FT.PAYMENT.DETAILS) =  Y.PAY.TO
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.PaymentDetails, Y.PAY.TO)
    END

RETURN
END
