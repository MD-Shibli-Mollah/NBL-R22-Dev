* @ValidationCode : MjoxMzM3Njk3MzQ2OkNwMTI1MjoxNjczMTcwODI4OTE0OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jan 2023 15:40:28
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.ACCOUNT.CCY
*-----------------------------------------------------------------------------
* <Rating>690</Rating>
*-----------------------------------------------------------------------------
* Modification History :
* Developed By         : MD SHIBLI MOLLAH -- FDS
* Date                 : 08TH JAN 2023
*-----------------------------------------------------------------------------
* Modification 2
*
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT I_F.TELLER
    $USING TT.Contract
*    $INSERT I_F.COMPANY
    $USING ST.CompanyCreation
    $USING EB.DataAccess
    $USING EB.Foundation
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

* Y.ACCOUNT.NO=COMI
    Y.ACCOUNT.NO= EB.SystemTables.getComi()
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
    Y.APPLICATION = EB.SystemTables.getApplication()
    
    Y.VAR=FIELD(Y.PGM.VERSION,'.',3)
    
    IF Y.ACCOUNT.NO[1,2] EQ 'PL' THEN
        Y.AC.CCY = 'BDT'
        IF Y.VAR EQ 'COLLECTION' THEN
            IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
* R.NEW(FT.CREDIT.CURRENCY)=Y.AC.CCY
                EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditCurrency, Y.AC.CCY)
            END
            ELSE
* R.NEW(TT.TE.CURRENCY.2)=Y.AC.CCY
                EB.SystemTables.setRNew(TT.Contract.Teller.TeCurrencyTwo, Y.AC.CCY)
            END
        END
        ELSE
            Y.BR.MNE = ''
            Y.BR.CO.CODE = ''
*     CALL GET.ACCT.BRANCH(Y.ACCOUNT.NO,Y.BR.MNE,Y.BR.CO.CODE)
* AC.AccountOpening.GetAcctBranch(AcctNo, Mnemonic, CoCode)
*   AC.AccountOpening.GetAcctBranch(Y.ACCOUNT.NO,Y.BR.MNE,Y.BR.CO.CODE)
    
*        IF NOT(Y.BR.MNE) THEN
** Y.BR.MNE = R.COMPANY(EB.COM.MNEMONIC)
*        Y.BR.MNE = R.COMPANY(ST.CompanyCreation.Company.EbComMnemonic)
*        FN.ACCOUNT='F':Y.BR.MNE:'.ACCOUNT'
*        F.ACCOUNT=''
*        CALL OPF(FN.ACCOUNT,F.ACCOUNT)
*        READV Y.AC.CCY FROM F.ACCOUNT,Y.ACCOUNT.NO,AC.CURRENCY ELSE
*        END
            BEGIN CASE
                CASE Y.VAR EQ 'ISSUE'
                    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
* R.NEW(FT.DEBIT.CURRENCY)=Y.AC.CCY
                        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitCurrency, Y.AC.CCY)
                    END
                    ELSE
* R.NEW(TT.TE.CURRENCY.1)=Y.AC.CCY
                        EB.SystemTables.setRNew(TT.Contract.Teller.TeCurrencyOne, Y.AC.CCY)
                    END
                CASE Y.VAR EQ 'COLLECTION'
                    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
* R.NEW(FT.CREDIT.CURRENCY)=Y.AC.CCY
                        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditCurrency, Y.AC.CCY)
                    END
                    ELSE
* R.NEW(TT.TE.CURRENCY.2)=Y.AC.CCY
                        EB.SystemTables.setRNew(TT.Contract.Teller.TeCurrencyTwo, Y.AC.CCY)
                    END
            END CASE
        END
        RETURN
    END
