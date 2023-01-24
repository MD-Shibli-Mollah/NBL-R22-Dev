* @ValidationCode : Mjo5ODc4MDkyMzY6Q3AxMjUyOjE2NzMzMzAzOTMzNjQ6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 10 Jan 2023 11:59:53
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.CHECK.AC.LIMIT.TT.FT
    
* Author: Kishor Kumar Saha
* Description : This Routine will Check for Valid LIMIT With Collateral
* Create DATE : 24th July 2022
* Modified by : KISHOR
* Modification DATE : 21st August 2022
*-----------------------------------------------------------------------------
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 10/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_EQUATE
    $INSERT I_COMMON
*    $INSERT I_F.LIMIT
    $USING LI.Config
*    $INSERT I_F.COLLATERAL.RIGHT
    $USING CO.Contract
    $USING CO.Config
*    $INSERT I_F.COLLATERAL
*    $INSERT I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INSERT I_F.TELLER
    $USING TT.Contract
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $USING TT.TellerFinancialService
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:
******
    LIEN.CAT = '6620':@FM:'6621':@FM:'6622':@FM:'6632':@FM:'6633':@FM:'6627':@FM:'6630':@FM:'6631':@FM:'6619':@FM:'6610':@FM:'6611':@FM:'6624':@FM:'6626'
    
    Y.ID.NEW = EB.SystemTables.getIdNew()
    Y.TRN.ID = Y.ID.NEW
RETURN

OPENFILES:
**********
    FN.LIMIT = "F.LIMIT"
    FN.COLLATERAL.RIGHT = "F.COLLATERAL.RIGHT"
    FN.COLLATERAL = "F.COLLATERAL"
    FN.ACC = "F.ACCOUNT"
    CALL OPF(FN.LIMIT,F.LIMIT)
    CALL OPF(FN.COLLATERAL.RIGHT,F.COLLATERAL.RIGHT)
    CALL OPF(FN.COLLATERAL,F.COLLATERAL)
    CALL OPF(FN.ACC,F.ACC)
RETURN

PROCESS:
*********
    Y.ID.CHK = LEFT(Y.TRN.ID,2)
    IF Y.ID.CHK EQ 'FT' THEN
* Y.FT.VALUE = R.NEW(FT.DEBIT.ACCT.NO)
        Y.FT.VALUE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
        Y.FT.CHK = LEFT(Y.FT.VALUE,2)
        IF Y.FT.CHK NE 'BD' AND Y.FT.CHK NE 'PL' THEN
            Y.AC.ID = Y.FT.VALUE
            GOSUB MAIN.PROCESS
        END
    END
    IF Y.ID.CHK EQ 'TT' THEN
* Y.DR.CR.MARKER = R.NEW(TT.TE.DR.CR.MARKER)
        Y.DR.CR.MARKER = EB.SystemTables.getRNew(TT.Contract.Teller.TeDrCrMarker)
        IF Y.DR.CR.MARKER EQ 'DEBIT' THEN
* Y.ACCT.1 = R.NEW(TT.TE.ACCOUNT.1)
            Y.ACCT.1 = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
            Y.ACCT.1.CHK = LEFT(Y.ACCT.1,3)
* Y.ACCT.2 = R.NEW(TT.TE.ACCOUNT.2)
            Y.ACCT.2 = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
            Y.ACCT.2.CHK = LEFT(Y.ACCT.2,3)
            IF Y.ACCT.1.CHK EQ 'BDT' THEN
                Y.AC.ID = Y.ACCT.2
            END
            ELSE
                Y.AC.ID = Y.ACCT.1
            END
            GOSUB MAIN.PROCESS
        END
    END
RETURN

MAIN.PROCESS:
*************
    EB.DataAccess.FRead(FN.ACC,Y.AC.ID,R.AC,F.ACC,AC.ERR)
* Y.CAT = R.AC<AC.CATEGORY>
    Y.CAT = R.AC<AC.AccountOpening.Account.Category>
    Y.VFUNCTION = EB.SystemTables.getVFunction()
    
    IF Y.VFUNCTION EQ 'I' THEN
        IF Y.CAT EQ '1916' THEN
* Y.CUS = R.AC<AC.CUSTOMER>
            Y.CUS = R.AC<AC.AccountOpening.Account.Customer>
* Y.LIMIT.REF = R.AC<AC.LIMIT.REF>
            Y.LIMIT.REF = R.AC<AC.AccountOpening.Account.LimitRef>
            Y.LIMIT.ID = Y.CUS:".000":Y.LIMIT.REF
            
            EB.DataAccess.FRead(FN.LIMIT,Y.LIMIT.ID,R.LIMIT,F.LIMIT,LIMIT.ERR)
            IF (R.LIMIT) THEN
                Y.MISMATCH = 0
*                Y.INTERNAL.AMNT = R.LIMIT<LI.INTERNAL.AMOUNT>
                Y.INTERNAL.AMNT = R.LIMIT<LI.Config.Limit.InternalAmount>
*                Y.COLL.ID = R.LIMIT<LI.COLLATERAL.CODE>
                Y.COLL.ID = R.LIMIT<LI.Config.Limit.CollateralCode>
                Y.COLL.ID.CNT = DCOUNT(Y.COLL.ID,@VM)
                SEL.CMD.COL = " SELECT F.COLLATERAL.RIGHT WITH @ID LIKE ":Y.CUS:"..."
                EB.DataAccess.Readlist(SEL.CMD.COL,SEL.LIST.COL,'',NO.OF.RECORD,ERR.CODE)
                IF NO.OF.RECORD LE 0 THEN
*                    ETEXT = "Please Lien the Collateral First"
*                    CALL STORE.END.ERROR
                    EB.SystemTables.setEtext("Please Lien the Collateral First")
                    EB.ErrorProcessing.StoreEndError()
                END
                LOOP
                    REMOVE Y.COL.RIGHT FROM SEL.LIST.COL SETTING POS
                WHILE Y.COL.RIGHT:POS
                    EB.DataAccess.FRead(FN.COLLATERAL.RIGHT,Y.COL.RIGHT,R.COL.RIGHT,F.COLLATERAL.RIGHT,COL.ERR)
* Y.LIMIT.REF = R.COL.RIGHT<COLL.RIGHT.LIMIT.REFERENCE>
                    Y.LIMIT.REF = R.COL.RIGHT<CO.Contract.CollateralRight.CollRightLimitReference>
                    IF  Y.LIMIT.REF EQ Y.LIMIT.ID THEN
                        Y.COLL.RIGHT.VAL = Y.COL.RIGHT
                        SEL.CMD = " SELECT F.COLLATERAL WITH @ID LIKE ":Y.COLL.RIGHT.VAL:"..."
                        EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.RECORD.COL,ERR.CODE)
                        IF NO.OF.RECORD.COL LE 0 THEN
*                            ETEXT = "Please Open the Collateral"
*                            CALL STORE.END.ERROR
                            EB.SystemTables.setEtext("Please Open the Collateral")
                            EB.ErrorProcessing.StoreEndError()
                        END
                        LOOP
                            REMOVE Y.COL.ID FROM SEL.LIST SETTING POS
                        WHILE Y.COL.ID:POS
                            EB.DataAccess.FRead(FN.COLLATERAL,Y.COL.ID,R.COL,F.COLLATERAL,COL.ERR)
*                            Y.APP.ID = R.COL<COLL.APPLICATION.ID>
                            Y.APP.ID = R.COL<CO.Contract.Collateral.CollApplicationId>
*                            Y.MKT.VAL = R.COL<COLL.NOMINAL.VALUE>
                            Y.MKT.VAL = R.COL<CO.Contract.Collateral.CollNominalValue>
                            EB.DataAccess.FRead(FN.ACC,Y.APP.ID,R.ACC,F.ACC,ACC.ERR)
*                            Y.APP.CAT = R.ACC<AC.CATEGORY>
                            Y.APP.CAT = R.ACC<AC.AccountOpening.Account.Category>
*                            Y.COL.CODE = R.COL<COLL.COLLATERAL.CODE>
                            Y.COL.CODE = R.COL<CO.Contract.Collateral.CollCollateralCode>
                            IF Y.COL.CODE EQ '110' OR Y.COL.CODE EQ '120' THEN
                                Y.TOTAL.AMNT = Y.TOTAL.AMNT + Y.MKT.VAL
                            END
                            !IF Y.COL.CODE NE '110' THEN
                            ELSE
                                LOCATE Y.APP.CAT IN LIEN.CAT SETTING Y.POS THEN
                                    Y.TOTAL.AMNT = Y.TOTAL.AMNT + Y.MKT.VAL
                                END
                                ELSE
                                    Y.TOTAL.AMNT = 0
*                                    ETEXT = "Please Fix Lien Account No First"
*                                    CALL STORE.END.ERROR
                                    EB.SystemTables.setEtext("Please Fix Lien Account No First")
                                    EB.ErrorProcessing.StoreEndError()
                                END
                            END
                            !ELSE
                            !Y.TOTAL.AMNT = Y.TOTAL.AMNT + Y.MKT.VAL
                            !END
                        REPEAT
                    END
                    ELSE
                        Y.MISMATCH = Y.MISMATCH + 1
                    END
                REPEAT
                IF Y.MISMATCH EQ NO.OF.RECORD THEN
*                    ETEXT = "Please Fix Limit Reference in Collateral Right First"
*                    CALL STORE.END.ERROR
                    EB.SystemTables.setEtext("Please Fix Limit Reference in Collateral Right First")
                    EB.ErrorProcessing.StoreEndError()
                END
                IF Y.INTERNAL.AMNT GE Y.TOTAL.AMNT THEN
*                    ETEXT = "Limit Amount Exceeds the Collateral Value"
*                    CALL STORE.END.ERROR
                    EB.SystemTables.setEtext("Limit Amount Exceeds the Collateral Value")
                    EB.ErrorProcessing.StoreEndError()
                END
            END
            ELSE
*                ETEXT = "Please Open Limit First"
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext("Please Open Limit First")
                EB.ErrorProcessing.StoreEndError()
            END
        END
    END
RETURN
END