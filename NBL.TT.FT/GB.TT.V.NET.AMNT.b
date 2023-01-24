* @ValidationCode : MjoxNTI2NjYxMDQ3OkNwMTI1MjoxNjczNDI4NjU4MDY4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Jan 2023 15:17:38
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.TT.V.NET.AMNT
**************************************************************************
*Subroutine Description:
*-----------------------
* PURPOSE: FORMATTING THE AMOUNT VALUE & DEDUCT CHARGE FROM NET AMOUNT.
*
*-------------------------------------------------------------------------
*<Description of the arguments>
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 11/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT T24.BP I_F.FT.COMMISSION.TYPE
    $USING ST.ChargeConfig
*    $INSERT T24.BP I_F.TELLER
    $USING TT.Contract
*    $INSERT T24.BP I_F.ACCOUNT
    $INSERT I_F.BD.H.BRANCH.CODE
    $USING EB.SystemTables

    !DEBUG
* Y.TR.CODE = R.NEW(TT.TE.TRANSACTION.CODE)
    Y.TR.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeTransactionCode)
    IF Y.TR.CODE EQ "212" OR Y.TR.CODE EQ "213" THEN
*        Y.AMOUNT = FMT(R.NEW(TT.TE.AMOUNT.LOCAL.1),"R2")
        Y.AMOUNT = FMT(EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne),"R2")
*        Y.CHARGE = FMT(R.NEW(TT.TE.CHRG.AMT.LOCAL),"R2")
        Y.CHARGE = FMT(EB.SystemTables.getRNew(TT.Contract.Teller.TeChrgAmtLocal),"R2")
        Y.NET.AMNT = Y.AMOUNT - Y.CHARGE
        Y.NET.AMNT.FMT = FMT(Y.NET.AMNT,"R2")
*  R.NEW(TT.TE.AMOUNT.LOCAL.1) = FMT(Y.NET.AMNT,"R2")
        EB.SystemTables.setRNew(TT.Contract.Teller.TeAmountLocalOne, Y.NET.AMNT.FMT)
    END
    
