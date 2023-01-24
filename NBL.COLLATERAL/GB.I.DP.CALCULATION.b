* @ValidationCode : MjoxMzYzOTQyNDgzOkNwMTI1MjoxNjczODYyMTI2NTg0OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 16 Jan 2023 15:42:06
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.DP.CALCULATION
*-------------------------------------------------------------------------
* <Rating>100</Rating>
*-------------------------------------------------------------------------
*-------------------------------------------------------------------------
*<Description>
* Total Number of stock calculation for COLLATERAL
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INCLUDE I_COMMON
    $INCLUDE I_EQUATE
* $INCLUDE I_F.COLLATERAL
    $USING CO.Contract
        
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Foundation
    $USING EB.LocalReferences

* IF R.NEW(COLL.COLLATERAL.TYPE) LT 600 OR R.NEW(COLL.COLLATERAL.TYPE) GT 640 THEN RETURN
    Y.COLL.TYPE = EB.SystemTables.getRNew(CO.Contract.Collateral.CollCollateralType)
    IF Y.COLL.TYPE LT 600 OR Y.COLL.TYPE GT 640 THEN RETURN

    DP.MAR = ''
    DP.AMT.PER.STOCK = ''
    DP.TOT.STK = ''
    DP.TOT.MGN = ''
    Y.TOT.STK.CNT = ''
    Y.TOT.MGN.CNT = ''
    STK.INC = ''
    MGN.INC = ''
    TOT.PER.STOCK = ''
    AMT.MERGIN = ''
    DP.INC = ""
    DP.MAR.CNT=""
    TOT.DR.PWR=""

    Y.APPLICATION="COLLATERAL"
    TOTAL.PER.STOCK="TOTAL.PER.STOCK"
    MARGIN="MARGIN"
    AMOUNT.MARGIN="AMOUNT.MARGIN"
    DRAWING="DRAWING"
    TOT.STOCK="TOT.STOCK"
    TOT.MARGIN="TOT.MARGIN"
    TOT.POWER="TOT.POWER"
    ADDR.GODOWN="ADDR.GODOWN"
    LIMIT="LIMIT"
    QUANTITY="QUANTITY"
    VALUE.PER.UNIT="VALUE.PER.UNIT"
    NAME.OF.STOCK="NAME.OF.STOCK"


    TOTAL.PER.STOCK.POS  = ""
    MARGIN.POS = ""
    AMOUNT.MARGIN.POS  = ""
    DRAWING.POS  = ""
    TOT.STOCK.POS  = ""
    TOT.MARGIN.POS  = ""
    TOT.POWER.POS  = ""
    ADDR.GODOWN.POS  = ""
    LIMIT.POS = ""
    QUANTITY.POS = ""
    VALUE.PER.UNIT.POS = ""
    NAME.OF.STOCK.POS=""

* EB.LocalReferences.GetLocRef(Appl, Fieldname, Pos)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,NAME.OF.STOCK,NAME.OF.STOCK.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,TOTAL.PER.STOCK,TOTAL.PER.STOCK.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,MARGIN,MARGIN.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,AMOUNT.MARGIN,AMOUNT.MARGIN.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,DRAWING,DRAWING.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,TOT.STOCK,TOT.STOCK.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,TOT.MARGIN,TOT.MARGIN.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,TOT.POWER,TOT.POWER.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,ADDR.GODOWN,ADDR.GODOWN.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,LIMIT,LIMIT.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,QUANTITY,QUANTITY.POS)
    EB.LocalReferences.GetLocRef(Y.APPLICATION,VALUE.PER.UNIT,VALUE.PER.UNIT.POS)

* STOCK.CNT=DCOUNT(R.NEW(COLL.LOCAL.REF)<1,NAME.OF.STOCK.POS>,@SM)
    Y.TEMP =  EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
    STOCK.CNT = DCOUNT(Y.TEMP<1,NAME.OF.STOCK.POS>,@SM)

    FOR I=1 TO STOCK.CNT
        TOT.PER.STOCK = Y.TEMP<1,QUANTITY.POS,I> * Y.TEMP<1,VALUE.PER.UNIT.POS,I>
        Y.TEMP<1,TOTAL.PER.STOCK.POS,I> = TOT.PER.STOCK
* R.NEW(COLL.LOCAL.REF)<1,TOTAL.PER.STOCK.POS,I> = TOT.PER.STOCK
* EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef, Y.TEMP)
* AMT.MERGIN = ( TOT.PER.STOCK * R.NEW(COLL.LOCAL.REF)<1,MARGIN.POS,I> ) / 100
        AMT.MERGIN = ( TOT.PER.STOCK * Y.TEMP<1,MARGIN.POS,I> ) / 100
        
*            R.NEW(COLL.LOCAL.REF)<1,AMOUNT.MARGIN.POS,I>  = AMT.MERGIN
        Y.TEMP<1,AMOUNT.MARGIN.POS,I>  = AMT.MERGIN
* EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef, Y.TEMP)
*            R.NEW(COLL.LOCAL.REF)<1,DRAWING.POS,I> = TOT.PER.STOCK  - AMT.MERGIN
        Y.TEMP<1,DRAWING.POS,I> = TOT.PER.STOCK  - AMT.MERGIN
        EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef, Y.TEMP)
            
        DP.TOT.DR.POW = DP.TOT.DR.POW + ( TOT.PER.STOCK - AMT.MERGIN )
        DP.TOT.STK = DP.TOT.STK + TOT.PER.STOCK
        DP.TOT.MGN = DP.TOT.MGN + AMT.MERGIN
        TOT.PER.STOCK=""
        AMT.MERGIN=""

    NEXT

*        R.NEW(COLL.LOCAL.REF)<1,TOT.STOCK.POS> = DP.TOT.STK
    Y.TEMP.REF =  EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
    Y.TEMP.REF<1,TOT.STOCK.POS> = DP.TOT.STK
*        R.NEW(COLL.LOCAL.REF)<1,TOT.MARGIN.POS> = DP.TOT.MGN
    Y.TEMP.REF<1,TOT.MARGIN.POS> = DP.TOT.MGN
* EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef, Y.TEMP.REF)
*        LIMIT.AMT = R.NEW(COLL.LOCAL.REF)<1,LIMIT.POS>
    LIMIT.AMT = Y.TEMP.REF<1,LIMIT.POS>

    IF LIMIT.AMT LT DP.TOT.DR.POW THEN
        TOT.DR.PWR = LIMIT.AMT
    END ELSE
        TOT.DR.PWR = DP.TOT.DR.POW
    END

* R.NEW(COLL.LOCAL.REF)<1,TOT.POWER.POS> = TOT.DR.PWR
    Y.TEMP.REF<1,TOT.POWER.POS> = TOT.DR.PWR
    EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef, Y.TEMP.REF)
    
RETURN

END