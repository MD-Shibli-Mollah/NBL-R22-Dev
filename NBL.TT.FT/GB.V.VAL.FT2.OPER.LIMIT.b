* @ValidationCode : MjotNzYwOTQ0NDQ2OkNwMTI1MjoxNjc0NTUxODY0NTI1OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 24 Jan 2023 15:17:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.VAL.FT2.OPER.LIMIT
    
*-----------------------------------------------------------------------------
* <Rating>50</Rating>
*-----------------------------------------------------------------------------
* Attached to: FUNDS.TRANSFER,BD.OT103.SERIAL.FTHP,
*              FUNDS.TRANSFER,BD.OD.FCY.FTHP
*              FUNDS.TRANSFER,BD.OD.FCY.FTHP
*              FUNDS.TRANSFER,BD.OD.110.FTHP
*			*FUNDS.TRANSFER,BD.OT103.SERIAL.FTHP
**			*FUNDS.TRANSFER,BD.OT103.COVER.FTHP
**			*FUNDS.TRANSFER,BD.OT400.FTHP
**			*FUNDS.TRANSFER,BD.OT400.202.FTHP
**			*FUNDS.TRANSFER,BD.ACTR.FTHP
**
* Description:
* This is a validation routine should draw the information available in
* Mode of operation and Operating Limits of debit account and validate
* the same by comparing with the FUNDS.TRANSFER application.
* If the input in FUNDS.TRANSFER is not satisfying the opeartion
* conditions in Account record, then system will throw an error message.
*-------------------------------------------------------------------------
* Incoming Parameter(s): none
*
* Outgoing Parameter(s): none
*-------------------------------------------------------------------------
* Change Request/Development Ref:
*
* Called By:
*
* CALLS: EB.CURR.CONV - To convert amount to different currency.
*
* Change Request:
*
*-------------------------------------------------------------------------
* Modification Overview:
* ----------------------
* 08/02/2011        - New          - Rashmi K T
* 08/01/2023        - Retrofit     - MD Shibli Mollah - FDS
*-------------------------------------------------------------------------
    $INCLUDE I_COMMON
    $INCLUDE I_EQUATE
*    $INCLUDE I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INCLUDE I_F.FUNDS.TRANSFER
    $USING FT.Contract
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Updates
    $USING EB.Foundation
    $USING EB.ErrorProcessing

    Y.MESSAGE = EB.SystemTables.getMessage()
    IF Y.MESSAGE NE "" THEN RETURN

    GOSUB INIT
    GOSUB PROCESS

RETURN

*-------------------------------------------------------------------------
INIT:
*----
    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ""
    EB.DataAccess.Opf(FN.ACCOUNT,F.ACCOUNT)

    FN.FUNDS.TRANSFER = "F.FUNDS.TRANSFER"
    F.FUNDS.TRANSFER = ""
    EB.DataAccess.Opf(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    !Getting local ref field values

    Y.APPL = "ACCOUNT":@FM:"FUNDS.TRANSFER"
    Y.FIELDS = "MODE.OF.OPER":@VM:"OPERATING.LIMIT":@FM:"MODE.OF.OPER"
    Y.POS = ""
    EB.Updates.MultiGetLocRef(Y.APPL,Y.FIELDS,Y.POS)
    Y.MOD.POS = Y.POS<1,1>
    Y.OP.LIMIT.POS = Y.POS<1,2>
    Y.FT.MOD.POS = Y.POS<2,1>

RETURN
*--------------------------------------------------------------------------
PROCESS:
*-------
    ! Get the debit account number and read the ACCOUNT file to get the
    ! Mode of operation and operating limit

*  EB.SystemTables.getRNew(idx)
*    Y.CREDIT.CCY = EB.SystemTables.getRNew(FT.CREDIT.CURRENCY)
    Y.CREDIT.CCY = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditCurrency)
*    Y.CREDIT.AMT = COMI
    Y.CREDIT.AMT = EB.SystemTables.getComi()
*    Y.ACT.ID = EB.SystemTables.getRNew(FT.DEBIT.ACCT.NO)
    Y.ACT.ID = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)

    EB.DataAccess.FRead(FN.ACCOUNT,Y.ACT.ID,R.ACCOUNT,F.ACCOUNT,Y.ACT.ERR)
    IF R.ACCOUNT THEN
* Y.DEBIT.CCY = R.ACCOUNT<AC.CURRENCY>
        Y.DEBIT.CCY = R.ACCOUNT<AC.AccountOpening.Account.Currency>
        Y.MODE.OF.OPER = R.ACCOUNT<AC.AccountOpening.Account.LocalRef><1,Y.MOD.POS>
        Y.OPER.LIMIT = R.ACCOUNT<AC.AccountOpening.Account.LocalRef><1,Y.OP.LIMIT.POS>
    END
* Y.FT.MODE.OF.OPER = EB.SystemTables.getRNew(FT.LOCAL.REF)<1,Y.FT.MOD.POS>
    Y.FT.MODE.OF.OPER = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.MOD.POS>

    ! Check for the credit currency and debit currency, if not equal then
    ! convert the credit amount to debit currency amount

    IF Y.CREDIT.CCY AND Y.CREDIT.CCY EQ Y.DEBIT.CCY THEN
        Y.DEBIT.AMT = Y.CREDIT.AMT
    END ELSE
        IF Y.CREDIT.CCY AND Y.CREDIT.CCY NE Y.DEBIT.CCY THEN
            
* CALL EB.CURR.CONV(Y.CREDIT.CCY,Y.CREDIT.AMT,Y.DEBIT.CCY,Y.DEBIT.AMT)
            EB.Foundation.CurrConv(Y.CREDIT.CCY,Y.CREDIT.AMT,Y.DEBIT.CCY,Y.DEBIT.AMT)
        END
    END

    ! Check for the debit amount with operating limit and validate
    ! the mode of operation entered in FUNDS.TRANSFER with the ACCOUNT

    LOCATE Y.FT.MODE.OF.OPER IN Y.MODE.OF.OPER<1,1,1> SETTING Y.OP.POS THEN
        Y.OPER.AMT = Y.OPER.LIMIT<1,1,Y.OP.POS>
        IF Y.OPER.LIMIT NE '' THEN
            IF Y.DEBIT.AMT GT Y.OPER.AMT THEN
*                AF = FT.CREDIT.AMOUNT
*                ETEXT = "EB-OPERATING.LIMIT"
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext('EB-OPERATING.LIMIT')
                EB.ErrorProcessing.StoreEndError()
            END
        END
*        !END ELSE
*        !AF = FT.LOCAL.REF
*        !AV = Y.FT.MOD.POS
*        !ETEXT = "EB-MOP.NOT.AVAILABLE":FM:Y.FT.MODE.OF.OPER:VM:Y.ACT.ID
*        !CALL STORE.END.ERROR
    END
RETURN
END
