* @ValidationCode : Mjo5MTUyNTg5NjE6Q3AxMjUyOjE2NzQ1NTE0MzQ2Nzc6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 24 Jan 2023 15:10:34
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.TFS.TP.CHECK
    
*-----------------------------------------------------------------------------
* <Rating>1040</Rating>
*-----------------------------------------------------------------------------
*<Description of the arguments>
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 10/01/2023        - Retrofit     - MD Shibli Mollah - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INSERT I_F.TELLER
    $USING TT.Contract
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $USING TT.TellerFinancialService
*    $INSERT I_F.DRAWINGS
    $USING LC.Contract
*    $INSERT I_F.DATES
    $USING EB.Utility
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.OverrideProcessing
    $USING EB.LocalReferences

    VIOLATED=''
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    R.ACCOUNT  = ''
    CUR.FLAG=0
    FN.DTS='F.DATES'
    F.DTS=''
    
    Y.VFUNCTION = EB.SystemTables.getVFunction()
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
    Y.ID.COMPANY = EB.SystemTables.getIdCompany()
    Y.APPLICATION = EB.SystemTables.getApplication()
    
    EB.DataAccess.Opf(FN.DTS,F.DTS)

    EB.DataAccess.Opf(FN.ACCOUNT,F.ACCOUNT)
    
    IF Y.VFUNCTION EQ 'I' THEN
* R.NEW(TFS.LOCAL.REF)<1,2>=PGM.VERSION
        EB.SystemTables.setRNew(TT.TellerFinancialService.TellerFinancialServices.TfsLockRef, Y.PGM.VERSION)
    END

* Y.CURRENCY=R.NEW(TFS.CURRENCY.DR)
    Y.CURRENCY=EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsCurrencyDr)
*    //Sanaullah_20190912

    IF Y.ID.COMPANY NE "BD0010001" THEN
        IF Y.ID.COMPANY EQ "BD0012043" THEN
            FOR I = 1 TO DCOUNT(Y.CURRENCY,@VM)
                IF Y.CURRENCY<1,I> NE "USD" THEN CUR.FLAG=1
            NEXT I
        END
        ELSE
            FOR I = 1 TO DCOUNT(Y.CURRENCY,@VM)
                IF Y.CURRENCY<1,I> NE 'BDT' THEN CUR.FLAG=1
            NEXT I
        END
        IF CUR.FLAG EQ '1' THEN
            !        E='ONLY BDT CURRENCY ALLOWED'
* E='ONLY LOCAL CURRENCY ALLOWED'
*            CALL ERR
*            V$ERROR = 1
            EB.SystemTables.setEtext('ONLY BDT CURRENCY ALLOWED')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END

    IF Y.VFUNCTION EQ 'A' THEN
* IF (R.NEW(TFS.LOCAL.REF)<1,2> NE '') THEN
        IF (EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsLocalRef)<1,2> NE '') THEN
* IF (R.NEW(TFS.LOCAL.REF)<1,2> NE PGM.VERSION) THEN
            IF (EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsLocalRef)<1,2> NE Y.PGM.VERSION) THEN
*                E="You can't authorized from this version"
*                CALL ERR
*                V$ERROR=1
                EB.SystemTables.setEtext("You can't authorized from this version")
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
        END
    END
* Y.VAL= R.NEW(TFS.REVERSAL.MARK)
    Y.VAL= EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsReversalMark)
    IF Y.VAL<1,1> EQ 'R' THEN RETURN

* IF Y.VFUNCTION EQ 'A' AND R.NEW(TFS.UNDERLYING)<1,1> EQ '' THEN RETURN
    IF Y.VFUNCTION EQ 'A' AND EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsUnderlying)<1,1> EQ '' THEN RETURN

    IF Y.APPLICATION EQ 'TELLER.FINANCIAL.SERVICES' THEN
        Y.TP.CATEGORY='1001':@FM:'1003':@FM:'1004':@FM:'6001':@FM:'6002':@FM:'6003':@FM:'6005':@FM:'6006':@FM:'6007':@FM:'6008':@FM:'6009':@FM:'6010':@FM:'6013':@FM:'6017':@FM:'6018':@FM:'6019'
* CALL F.READ(FN.DTS,ID.COMPANY,R.DTS,F.DTS,DTS.ERR)
    
        CALL EB.DataAccess.FRead(FN.DTS,Y.ID.COMPANY,R.DTS,F.DTS,DTS.ERR)
* Y.DATE= R.DTS<EB.DAT.TODAY>
        Y.DATE= R.DTS<EB.Utility.Dates.DatToday>
        Y.MONTH =Y.DATE[1,6]
        Y.APPLICATION='FUNDS.TRANSFER'
* Y.OVERRIDE=R.NEW(TFS.OVERRIDE)
        Y.OVERRIDE = EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsOverride)

*        Y.DR.AC.LIST=R.NEW(TFS.ACCOUNT.DR)
        Y.DR.AC.LIST = EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsAccountDr)
*        Y.CR.AC.LIST=R.NEW(TFS.ACCOUNT.CR)
        Y.CR.AC.LIST = EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsAccountCr)
*        Y.DR.AC.AMT=R.NEW(TFS.AMOUNT.DR)
        Y.DR.AC.AMT = EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsAmountDr)
*        Y.CR.AC.AMT=R.NEW(TFS.AMOUNT.CR)
        Y.CR.AC.AMT = EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsAmountCr)

        IF Y.VFUNCTION EQ 'I' THEN
            Y.DUP.CHECK=Y.DR.AC.LIST:@VM:Y.CR.AC.LIST
            Y.DUP.CHECK= SORT(Y.DUP.CHECK)
            CHANGE @VM TO @FM IN Y.DUP.CHECK
            FOR I=1 TO DCOUNT(Y.DUP.CHECK,@FM)
                AC.ID=Y.DUP.CHECK<I>
                Y.DUP.CHECK<I>=''
                EB.DataAccess.FRead(N.ACCOUNT,AC.ID,R.ACCOUNT,F.ACCOUNT,ERR.AC)
* AC.CAT=R.ACCOUNT<AC.CATEGORY>
                AC.CAT=R.ACCOUNT<AC.AccountOpening.Account.Category>
                LOCATE AC.CAT IN Y.TP.CATEGORY SETTING POS THEN
                    IF AC.ID[1,2] NE 'PL' AND AC.ID[1,3] NE 'BDT' THEN
                        LOCATE AC.ID IN Y.DUP.CHECK SETTING POS THEN
*                            E='Please use ':AC.ID:' one time in this TFS'
*                            CALL ERR
*                            V$ERROR = 1
                            EB.SystemTables.setEtext('Please use ':AC.ID:' one time in this TFS')
                            EB.ErrorProcessing.StoreEndError()
                            RETURN
                        END
                    END
                END
            NEXT I
        END
        Y.COUNT=DCOUNT(Y.DR.AC.LIST,@VM)
        Y.BREAK.FLAG=0
        FOR KK=1 TO Y.COUNT
            Y.DR.AC=Y.DR.AC.LIST<1,KK>
            Y.DR.AMT=Y.DR.AC.AMT<1,KK>
            EB.DataAccess.FRead(FN.ACCOUNT,Y.DR.AC,R.ACCOUNT,F.ACCOUNT,ERR.AC)
* Y.DR.AC.CAT=R.ACCOUNT<AC.CATEGORY>
            Y.DR.AC.CAT = R.ACCOUNT<AC.AccountOpening.Account.Category>
* LOCATE R.ACCOUNT<AC.CATEGORY> IN Y.TP.CATEGORY SETTING POS THEN
            LOCATE Y.DR.AC.CAT IN Y.TP.CATEGORY SETTING POS THEN
                IF Y.DR.AC[1,2] NE 'PL' AND Y.DR.AC[1,3] NE 'BDT' THEN
                    DEFFUN TP.EXTRACT(Y.DR.AC,Y.MONTH,Y.APPLICATION,Y.DR.AMT,Y.CR.DR,Y.N)
                    VIOLATED = TP.EXTRACT(Y.DR.AC, Y.MONTH, Y.APPLICATION,Y.DR.AMT,"DEBIT","Y")
                    IF VIOLATED NE '' THEN
                        CHANGE @VM TO '/' IN VIOLATED
                        !E='Account :':Y.DR.AC:' ':VIOLATED:' (TFS)'
                        !CALL ERR
                        !MESSAGE='REPEAT'
                        !V$ERROR = 1
                        CURR.NO = DCOUNT(Y.OVERRIDE,@VM)
*                        TEXT='Account :':Y.CREDIT.AC:' ':VIOLATED: '(TFS NEW OVERRIDE)'
*                        CALL STORE.OVERRIDE(CURR.NO+1)
*                        MESSAGE='REPEAT'
                        EB.SystemTables.setEtext('Account :':Y.CREDIT.AC:' ':VIOLATED: '(TFS NEW OVERRIDE)')
                        EB.OverrideProcessing.StoreOverride(CURR.NO+1)
                        EB.SystemTables.setMessage('REPEAT')
                        RETURN
                    END
                END
            END

            !----------------TP CREDIT----------------
            Y.CR.AC=Y.CR.AC.LIST<1,KK>
            Y.CR.AMT=Y.CR.AC.AMT<1,KK>
            EB.DataAccess.FRead(FN.ACCOUNT,Y.CR.AC,R.ACCOUNT,F.ACCOUNT,ERR.AC)
* Y.CR.AC.CAT=R.ACCOUNT<AC.CATEGORY>
            Y.CR.AC.CAT=R.ACCOUNT<AC.AccountOpening.Account.Category>
* LOCATE R.ACCOUNT<AC.CATEGORY> IN Y.TP.CATEGORY SETTING POS THEN
            LOCATE Y.CR.AC.CAT IN Y.TP.CATEGORY SETTING POS THEN
                IF Y.CR.AC[1,2] NE 'PL' AND Y.CR.AC[1,3] NE 'BDT' THEN
                    VIOLATED = TP.EXTRACT(Y.CR.AC,Y.MONTH,Y.APPLICATION,Y.CR.AMT,"CREDIT","Y")
                    IF VIOLATED NE '' THEN
                        CHANGE @VM TO '/' IN VIOLATED
                        !E='Account :':Y.CR.AC:' ':VIOLATED:' (TFS)'
                        !CALL ERR
                        !MESSAGE='REPEAT'
                        !V$ERROR = 1
                        CURR.NO = DCOUNT(Y.OVERRIDE,@VM)
*                        TEXT='Account :':Y.CREDIT.AC:' ':VIOLATED: '(TFS NEW OVERRIDE)'
*                        CALL STORE.OVERRIDE(CURR.NO+1)
*                        MESSAGE='REPEAT'
                        EB.SystemTables.setEtext('Account :':Y.CREDIT.AC:' ':VIOLATED: '(TFS NEW OVERRIDE)')
                        EB.OverrideProcessing.StoreOverride(CURR.NO+1)
                        EB.SystemTables.setMessage('REPEAT')
                        RETURN
                    END
                END
            END
        NEXT KK

    END
    !RETURN
END
