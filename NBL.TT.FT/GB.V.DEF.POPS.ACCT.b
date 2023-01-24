* @ValidationCode : MjotODM3NDQ5NjMxOkNwMTI1MjoxNjczMjQ0MzYwNTQ4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 09 Jan 2023 12:06:00
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.DEF.POPS.ACCT
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
*    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT T24.BP I_F.TELLER
    $USING TT.Contract
    $INSERT I_F.PR.H.INSTRUMENT
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Updates
    $USING EB.Foundation
    $USING EB.LocalReferences

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

INIT:

    FN.INS = 'F.PR.H.INSTRUMENT'
    F.INS = ''
    R.INS = ''
    Y.ERR = ''
    Y.ACCT.NO = ''
    Y.ID = EB.SystemTables.getIdCompany()
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()

    !S - AYUSH - 20140204
* Y.APP = APPLICATION
    Y.APP = EB.SystemTables.getApplication()
    Y.FLD = 'PR.CHEQUE.NO'
    Y.POS = ''
    EB.LocalReferences.GetLocRef(Y.APP,Y.FLD,Y.POSS)
   
    Y.CHQ.NO.POS = Y.POS<1,1>
    !E
RETURN

OPENFILES:

    EB.DataAccess.Opf(FN.INS,F.INS)
RETURN

PROCESS:

* Y.VERSION=PGM.VERSION
    Y.VERSION = EB.SystemTables.getPgmVersion()
    Y.VAR=FIELD(Y.VERSION,'.',3)

    EB.DataAccess.FRead(FN.INS,Y.ID,R.INS,F.INS,Y.ERR)
    Y.INS.PO.ACC = R.INS<INS.PO.ACCOUNT>
    Y.INS.PS.ACC = R.INS<INS.PS.ACCOUNT>
    Y.INS.SDR.ACC = R.INS<INS.SDR.ACCOUNT>

    Y.APPLICATION = EB.SystemTables.getApplication()
    IF Y.APPLICATION EQ 'TELLER' THEN
* Y.TRANS.TYPE = R.NEW(TT.TE.TRANSACTION.CODE) --------TAFC TO TAFJ --- Shibli
        Y.TRANS.TYPE = EB.SystemTables.getRNew(TT.Contract.Teller.TeTransactionCode)

        !S - AYUSH - 20140204
*        Y.CHQ.NO = R.NEW(TT.TE.LOCAL.REF)<1,Y.CHQ.NO.POS>
        Y.CHQ.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.CHQ.NO.POS>
*        R.NEW(TT.TE.CHEQUE.NUMBER) = Y.CHQ.NO
        EB.SystemTables.setRNew(TT.Contract.Teller.TeChequeNumber, Y.CHQ.NO)
        !E
        BEGIN CASE
*            //Sanaullah_20191104
            CASE Y.TRANS.TYPE = '115'
*            R.NEW(TT.TE.ACCOUNT.1) = Y.INS.PO.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PO.ACC)
            CASE Y.TRANS.TYPE = '205'
* R.NEW(TT.TE.ACCOUNT.1) = Y.INS.PO.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PO.ACC)
            CASE Y.TRANS.TYPE = '120'
* R.NEW(TT.TE.ACCOUNT.1) = Y.INS.PO.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PO.ACC)
            CASE Y.TRANS.TYPE = '206'
* R.NEW(TT.TE.ACCOUNT.1) = Y.INS.PO.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PO.ACC)
            CASE Y.TRANS.TYPE = '117'
* R.NEW(TT.TE.ACCOUNT.1) =  Y.INS.PS.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PS.ACC)
            
            CASE Y.TRANS.TYPE = '207'
* R.NEW(TT.TE.ACCOUNT.1) =  Y.INS.PS.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PS.ACC)
            CASE Y.TRANS.TYPE = '118'
* R.NEW(TT.TE.ACCOUNT.1) =  Y.INS.SDR.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PS.ACC)
            CASE Y.TRANS.TYPE = '208'
* R.NEW(TT.TE.ACCOUNT.1) =  Y.INS.SDR.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.SDR.ACC)

                ! CASE Y.TRANS.TYPE = '107'
                ! R.NEW(TT.TE.ACCOUNT.2) =  R.INS<INS.DD.ACCOUNT>

                !CASE Y.TRANS.TYPE = '115'
                !R.NEW(TT.TE.ACCOUNT.2) =  R.INS<INS.DD.ACCOUNT>

            CASE Y.TRANS.TYPE = '116'
* R.NEW(TT.TE.ACCOUNT.1) =  Y.INS.PO.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PO.ACC)

            CASE Y.TRANS.TYPE = '209'
*  R.NEW(TT.TE.ACCOUNT.1) =  Y.INS.PO.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PO.ACC)

            CASE Y.TRANS.TYPE = '49'
*  R.NEW(TT.TE.ACCOUNT.1) =  Y.INS.PO.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PO.ACC)

                !S - AYUSH - 20140325
                IF Y.PGM.VERSION EQ ',PR.SDR.COLLECTION.CLEAR' THEN
* R.NEW(TT.TE.ACCOUNT.1) = Y.INS.SDR.ACC
                    EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.SDR.ACC)
                END
                !E
            CASE Y.TRANS.TYPE = '112'
* R.NEW(TT.TE.ACCOUNT.1) = Y.INS.PS.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PS.ACC)

            CASE Y.TRANS.TYPE = '210'
* R.NEW(TT.TE.ACCOUNT.1) = Y.INS.PS.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.PS.ACC)

            CASE Y.TRANS.TYPE = '119'
* R.NEW(TT.TE.ACCOUNT.1) =  Y.INS.SDR.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.SDR.ACC)

            CASE Y.TRANS.TYPE = '211'
* R.NEW(TT.TE.ACCOUNT.1) =  Y.INS.SDR.ACC
                EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountOne, Y.INS.SDR.ACC)
                !CASE Y.TRANS.TYPE = '108'
                !R.NEW(TT.TE.ACCOUNT.1) =  R.INS<INS.DD.ACCOUNT>

            CASE 1
        END CASE
    END
    ELSE
* Y.TRANS.TYPE = R.NEW(FT.TRANSACTION.TYPE)
        Y.TRANS.TYPE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TransactionType)
        BEGIN CASE

            !        CASE Y.TRANS.TYPE = 'ACPO' OR Y.TRANS.TYPE = 'ACP1'
        CASE Y.TRANS.TYPE = 'ACPO' OR Y.TRANS.TYPE = 'ACP1' OR Y.TRANS.TYPE = "ACOI" OR Y.TRANS.TYPE = "ACSI"

            BEGIN CASE
                CASE Y.VAR EQ 'ISSUE'
* R.NEW(FT.CREDIT.ACCT.NO) = Y.INS.PO.ACC
                    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditAcctNo, Y.INS.PO.ACC)
                CASE Y.VAR EQ 'COLLECTION'
* R.NEW(FT.DEBIT.ACCT.NO)=Y.INS.PO.ACC
                    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitAcctNo, Y.INS.PO.ACC)
            END CASE

        CASE Y.TRANS.TYPE = 'ACPS' OR Y.TRANS.TYPE = 'ACP2'

            BEGIN CASE
                CASE Y.VAR EQ 'ISSUE'
* R.NEW(FT.CREDIT.ACCT.NO) = Y.INS.PS.ACC
                    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditAcctNo, Y.INS.PS.ACC)
                CASE Y.VAR EQ 'COLLECTION'
* R.NEW(FT.DEBIT.ACCT.NO) = Y.INS.PS.ACC
                    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitAcctNo, Y.INS.PS.ACC)
            END CASE

        CASE Y.TRANS.TYPE = 'ACSD' OR Y.TRANS.TYPE = 'ACS1'
            BEGIN CASE
                CASE Y.VAR EQ 'ISSUE'
* R.NEW(FT.CREDIT.ACCT.NO) = Y.INS.SDR.ACC
                    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditAcctNo, Y.INS.SDR.ACC)
                CASE Y.VAR EQ 'COLLECTION'
* R.NEW(FT.DEBIT.ACCT.NO) = Y.INS.SDR.ACC
                    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitAcctNo, Y.INS.SDR.ACC)
            END CASE

        CASE Y.TRANS.TYPE = 'ACDD'
            BEGIN CASE
                CASE Y.VAR EQ 'ISSUE'
* R.NEW(FT.CREDIT.ACCT.NO) = R.INS<INS.DD.ACCOUNT>
                    EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditAcctNo, Y.INS.DD.ACC)
*            CASE Y.VAR EQ 'COLLECTION'   ----BANK Comment----
*                R.NEW(FT.DEBIT.ACCT.NO) = R.INS<INS.DD.ACCOUNT>     ----BANK Comment----
            END CASE

        CASE 1
    END CASE

END

!RETURN

RETURN
END
