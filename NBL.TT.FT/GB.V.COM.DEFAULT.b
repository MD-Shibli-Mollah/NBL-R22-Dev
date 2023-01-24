* @ValidationCode : MjoxMTk4ODA4MTQ0OkNwMTI1MjoxNjczMTU1NjYxNzUxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jan 2023 11:27:41
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.V.COM.DEFAULT
*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
* Modification History :
* 08/01/2023 -                      Retrofit   -  MD SHIBLI MOLLAH
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
*  $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
    $USING EB.SystemTables

*   Y.COMMISSION.CODE = COMI
    Y.COMMISSION.CODE = EB.SystemTables.getComi()

    IF Y.COMMISSION.CODE EQ "WAIVE" THEN
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CommissionType, '')
* EB.SystemTables.setRNew(idx, Value)  -----RETROFIT --- Shibli
* FT.Contract.FundsTransfer.CommissionAmt
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CommissionAmt, '')
* FT.Contract.FundsTransfer.TaxAmt
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.TaxAmt, '')
* EB.SystemTables.setRNew(FT.CHARGES.ACCT.NO) = ''
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.ChargesAcctNo, '')
    END

RETURN
END