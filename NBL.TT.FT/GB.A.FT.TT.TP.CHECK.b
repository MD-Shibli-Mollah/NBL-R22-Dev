* @ValidationCode : MjotNDgyODE1MTY5OkNwMTI1MjoxNjczNTA2MjExNTEzOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Jan 2023 12:50:11
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.A.FT.TT.TP.CHECK

**************************************************************************
*Subroutine Description:
*-----------------------
* PURPOSE: Transaction Profile (TP)Development - BD.H.TRANS.PROFILE , LOCAL TEMPLATE.
*
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 11/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT T24.BP I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INSERT T24.BP I_F.TELLER
    $USING TT.Contract
*    $INSERT T24.BP I_F.DATES
    $USING EB.Utility
*    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $USING FT.Contract
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.OverrideProcessing
    $INSERT I_F.BD.H.TRANS.PROFILE

    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
    Y.APPLICATION = EB.SystemTables.getApplication()
    Y.ID.COMPANY = EB.SystemTables.getIdCompany()

    !IF OPERATOR EQ 'SAYED1' THEN DEBUG
    IF Y.PGM.VERSION THEN
        Y.VERSION1 = Y.APPLICATION : Y.PGM.VERSION
        Y.EXCLD.VERSION='FUNDS.TRANSFER,DATA.UPLOAD.CARD1':@FM:'FUNDS.TRANSFER,DMD.FT.LOAD':@FM:'TELLER,CURR':@FM:'FUNDS.TRANSFER,ED':@FM:'LD.LOANS.AND.DEPOSITS,MAINT.CHG':@FM:'TELLER,OUT.CLEAR.SAME.DAY':@FM:'FUNDS.TRANSFER,OFS':@FM:'FUNDS.TRANSFER,BACHINTRF'
        LOCATE Y.VERSION1 IN Y.EXCLD.VERSION SETTING POS THEN
            RETURN
        END
    END
* EB.SystemTables.getRNew(FT.TFS.REFERENCE)

    Y.FT.TFS.REF = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TfsReference)
    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' AND Y.FT.TFS.REF NE '' THEN RETURN

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    R.ACCOUNT  = ''
    FN.TP='F.BD.H.TRANS.PROFILE'
    F.TP=''
    R.TP=''
    FN.OVR='F.OVERRIDE'
    F.OVR=''
    FN.DTS='F.DATES'
    F.DTS=''

    EB.DataAccess.Opf(EB.FN.ACCOUNT,F.ACCOUNT)
    EB.DataAccess.Opf(FN.TP,F.TP)
    EB.DataAccess.Opf(FN.DTS,F.DTS)

    Y.TP.CATEGORY='1001':@FM:'1003':@FM:'1004':@FM:'6001':@FM:'6002':@FM:'6003':@FM:'6005':@FM:'6006':@FM:'6007':@FM:'6008':@FM:'6009':@FM:'6010':@FM:'6013':@FM:'6017':@FM:'6018':@FM:'6019'


    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
*        Y.OVERRIDE = EB.SystemTables.getRNew(FT.OVERRIDE)
        Y.OVERRIDE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Override)
        Y.DEBIT.AC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
        Y.DEBIT.AMT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
        Y.CREDIT.AC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
        Y.CR.TYPE = Y.CREDIT.AC[1,2]
        Y.CREDIT.AC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
        Y.CHQ.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.ChequeNumber)
        Y.CREDIT.AMT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAmount)
        IF NOT(Y.DEBIT.AMT) AND Y.CREDIT.AMT THEN
            Y.DEBIT.AMT = Y.CREDIT.AMT
        END
    END

    IF APPLICATION EQ 'TELLER' THEN
        Y.OVERRIDE = EB.SystemTables.getRNew(TT.Contract.Teller.TeOverride)
        Y.TT.DR.CR.MARKER = EB.SystemTables.getRNew(TT.Contract.Teller.TeDrCrMarker) ;*** Shibli -FDS
        
        IF Y.TT.DR.CR.MARKER EQ 'DEBIT' THEN
            Y.DEBIT.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
            Y.DEBIT.AMT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)       ;*** Add By Rayhan
            Y.CREDIT.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
        END ELSE
            Y.DEBIT.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
            Y.DEBIT.AMT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalTwo)       ;*** Add By Rayhan
            Y.CREDIT.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
        END
        Y.CHQ.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeChequeNumber)
    END

    OPEN 'NBL.BP' TO F.READREC ELSE NULL

    R.FILE.NAME='TP.BRANCH.txt'
    READ TP.CO.CODE FROM F.READREC,R.FILE.NAME THEN
        LOCATE Y.ID.COMPANY IN TP.CO.CODE SETTING POS THEN
            Y.VER.CHK='TELLER,OUT.CLEAR.OTHER.DAY':@FM:'TELLER,OUT.CLEAR.SAME.DAY':@FM:'TELLER,IN.CLEAR':@FM:'TELLER,OUT.CLEAR.SAME.DAY'
            Y.VERSION1 = Y.APPLICATION : Y.PGM.VERSION
            LOCATE Y.VERSION1 IN Y.VER.CHK SETTING POS ELSE
                EB.DataAccess.FRead(FN.DTS,Y.ID.COMPANY,R.DTS,F.DTS,DTS.ERR)
* Y.DATE= R.DTS<EB.DAT.TODAY>
                Y.DATE= R.DTS<EB.Utility.Dates.DatToday>
                Y.MONTH =Y.DATE[1,6]
                EB.DataAccess.FRead(FN.ACCOUNT,Y.DEBIT.AC,R.ACCOUNT,F.ACCOUNT,AC.ERR)
* R.ACCOUNT<AC.CATEGORY>
                Y.CAT = R.ACCOUNT<AC.AccountOpening.Account.Category>
                LOCATE Y.CAT IN Y.TP.CATEGORY SETTING POS THEN
                    DEFFUN TP.EXTRACT(Y.DEBIT.AC, Y.MONTH, Y.APPLICATION,Y.DEBIT.AMT,Y.CR.DR,Y.N)
                    VIOLATED = TP.EXTRACT(Y.DEBIT.AC, Y.MONTH, Y.APPLICATION,Y.DEBIT.AMT,"DEBIT","")
                    CHANGE @VM TO '/' IN VIOLATED
                    IF VIOLATED NE '' THEN
                        !E='Account :':Y.DEBIT.AC:' ':VIOLATED:' (VC)'
                        !CALL ERR
                        !MESSAGE='REPEAT'
                        !V$ERROR = 1

                        CURR.NO = DCOUNT(Y.OVERRIDE,@VM)
*                        TEXT='Account :':Y.DEBIT.AC:' ':VIOLATED:' (NOW VC OVERRIDE)'
*                        CALL STORE.OVERRIDE(CURR.NO+1)
*                        MESSAGE='REPEAT'
                        EB.SystemTables.setEtext('Account :':Y.DEBIT.AC:' ':VIOLATED:' (NOW VC OVERRIDE)')
                        EB.OverrideProcessing.StoreOverride(CURR.NO+1)
                        EB.SystemTables.setMessage('REPEAT')
        
                
                        RETURN
                    END
                END

                Y.CAT = R.ACCOUNT<AC.AccountOpening.Account.Category>
                EB.DataAccess.FRead(FN.ACCOUNT,Y.CREDIT.AC,R.ACCOUNT,F.ACCOUNT,AC.ERR)
                LOCATE Y.CAT IN Y.TP.CATEGORY SETTING POS THEN
                    VIOLATED = TP.EXTRACT(Y.CREDIT.AC, Y.MONTH, Y.APPLICATION,Y.DEBIT.AMT,"CREDIT","")
                    CHANGE @VM TO '/' IN VIOLATED
                    IF VIOLATED NE '' THEN
                        !E='Account :':Y.CREDIT.AC:' ':VIOLATED: '(VC)'
                        !CALL ERR
                        !MESSAGE='REPEAT'
                        !V$ERROR = 1
                        CURR.NO = DCOUNT(Y.OVERRIDE,@VM)
*                        TEXT='Account :':Y.CREDIT.AC:' ':VIOLATED: '(NOW VC OVERRIDE)'
*                        CALL STORE.OVERRIDE(CURR.NO+1)
*                        MESSAGE='REPEAT'
                        EB.SystemTables.setEtext('Account :':Y.DEBIT.AC:' ':VIOLATED:' (NOW VC OVERRIDE)')
                        EB.OverrideProcessing.StoreOverride(CURR.NO+1)
                        EB.SystemTables.setMessage('REPEAT')
                        RETURN
                    END
                END

            END
        END
    END
RETURN
END
