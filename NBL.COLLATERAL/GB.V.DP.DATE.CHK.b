* @ValidationCode : MjotODAxMjA0NDY1OkNwMTI1MjoxNjczODY5MDc3MzIyOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 16 Jan 2023 17:37:57
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.DP.DATE.CHK
*-------------------------------------------------------------------------
*<Description>
* This is a validation routine which generates ERROR if the User don't input COLLATERAL Local Date Fields with value.
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT I_F.COLLATERAL
    $USING CO.Contract
    $USING EB.Updates
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.Display
    $USING EB.ErrorProcessing
    $USING EB.Foundation

* COLL.TYPE = R.NEW(COLL.COLLATERAL.TYPE)
    COLL.TYPE = EB.SystemTables.getRNew(CO.Contract.Collateral.CollCollateralType)

    IF COLL.TYPE NE 210 THEN RETURN

    Y.APPLICATION       = "COLLATERAL"
    !    FIELD.NAME          = "L.START.DATE":@VM:"L.EXPIRY.DATE":@VM:"DP.S.DATE":@VM:"DP.EX.DATE":@VM:"ACCT.NO":@VM:"LIMIT.REFERENCE"
    FIELD.NAME          = "L.START.DATE":@VM:"L.EXPIRY.DATE":@VM:"DP.S.DATE":@VM:"DP.EX.DATE":@VM:"ACCT.NO":@VM:"LIMIT.REFE"
    FIELD.NAME:= @VM:"LIMIT":@VM:"NAME.OF.STOCK":@VM:"DATE.OF.STOCK":@VM:"QUANTITY":@VM:"VALUE.PER.UNIT":@VM:"MARGIN"
    FIELD.POS           = ""
    
    EB.Updates.MultiGetLocRef(Y.APPLICATION,FIELD.NAME,FIELD.POS)
    
    L.START.DATE.POS  = FIELD.POS<1,1>
    L.EXPIRY.DATE.POS = FIELD.POS<1,2>
    DP.S.DATE.POS  = FIELD.POS<1,3>
    DP.EX.DATE.POS = FIELD.POS<1,4>
    DP.ACCT.NO.POS = FIELD.POS<1,5>
    DP.LIMIT.REFERENCE.POS = FIELD.POS<1,6>
    DP.LIMIT.POS = FIELD.POS<1,7>
    DP.NAME.OF.STOCK.POS = FIELD.POS<1,8>
    DP.DATE.OF.STOCK.POS = FIELD.POS<1,9>
    DP.QUANTITY.POS = FIELD.POS<1,10>
    DP.VALUE.PER.UNIT.POS = FIELD.POS<1,11>
    DP.MARGIN.POS = FIELD.POS<1,12>


    Y.TEMP = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
    Y.LIM.ST.DATE = Y.TEMP<1,L.START.DATE.POS>
    Y.LIM.EX.DATE = Y.TEMP<1,L.EXPIRY.DATE.POS>
    Y.DP.ST.DATE =  Y.TEMP<1,DP.S.DATE.POS>
    Y.DP.EX.DATE =  Y.TEMP<1,DP.EX.DATE.POS>


    IF NOT(Y.TEMP<1,L.START.DATE.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = L.START.DATE.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(L.START.DATE.POS)
        EB.ErrorProcessing.StoreEndError()
    END

    IF NOT(Y.TEMP<1,L.EXPIRY.DATE.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = L.EXPIRY.DATE.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(L.EXPIRY.DATE.POS)
        EB.ErrorProcessing.StoreEndError()
    END

    IF NOT(Y.TEMP<1,DP.S.DATE.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.S.DATE.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.S.DATE.POS)
        EB.ErrorProcessing.StoreEndError()
    END

    IF NOT(Y.TEMP<1,DP.EX.DATE.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.EX.DATE.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.EX.DATE.POS)
        EB.ErrorProcessing.StoreEndError()
    END



    IF Y.DP.ST.DATE GT Y.LIM.EX.DATE THEN
*        AF = COLL.LOCAL.REF
*        AV = DP.S.DATE.POS
*        ETEXT = 'EB-DP start date GT Limit Expiry date'
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-DP start date GT Limit Expiry date")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.S.DATE.POS)
        EB.ErrorProcessing.StoreEndError()
    END


    IF Y.DP.EX.DATE LT Y.DP.ST.DATE THEN
*        AF = COLL.LOCAL.REF
*        AV = DP.EX.DATE.POS
*        ETEXT = 'EB-DP expiry date LT DP start date'
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-DP expiry date LT DP start date")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.EX.DATE.POS)
        EB.ErrorProcessing.StoreEndError()
    END

    IF NOT(Y.TEMP<1,DP.ACCT.NO.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.ACCT.NO.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.ACCT.NO.POS)
        EB.ErrorProcessing.StoreEndError()
    END
    IF NOT(Y.TEMP<1,DP.LIMIT.REFERENCE.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.LIMIT.REFERENCE.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.LIMIT.REFERENCE.POS)
        EB.ErrorProcessing.StoreEndError()
    END
    IF NOT(Y.TEMP<1,DP.LIMIT.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.LIMIT.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.LIMIT.POS)
        EB.ErrorProcessing.StoreEndError()
    END
    IF NOT(Y.TEMP<1,DP.NAME.OF.STOCK.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.NAME.OF.STOCK.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.NAME.OF.STOCK.POS)
        EB.ErrorProcessing.StoreEndError()
    END
    IF NOT(Y.TEMP<1,DP.DATE.OF.STOCK.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.DATE.OF.STOCK.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.DATE.OF.STOCK.POS)
        EB.ErrorProcessing.StoreEndError()
    END
    IF NOT(Y.TEMP<1,DP.QUANTITY.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.QUANTITY.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.QUANTITY.POS)
        EB.ErrorProcessing.StoreEndError()
    END

    IF NOT(Y.TEMP<1,DP.VALUE.PER.UNIT.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.VALUE.PER.UNIT.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.VALUE.PER.UNIT.POS)
        EB.ErrorProcessing.StoreEndError()
    END

    IF NOT(Y.TEMP<1,DP.MARGIN.POS>) THEN
*        ETEXT = "EB-INPUT.MANDATORY"
*        AF    = COLL.LOCAL.REF
*        AV    = DP.MARGIN.POS
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("EB-INPUT.MANDATORY")
        EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
        EB.SystemTables.setAv(DP.MARGIN.POS)
        EB.ErrorProcessing.StoreEndError()
    END

RETURN
END