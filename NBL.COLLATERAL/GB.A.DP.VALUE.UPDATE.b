* @ValidationCode : MjozODQ5OTk4OTM6Q3AxMjUyOjE2NzM4NjIxMDk0NjU6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 16 Jan 2023 15:41:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.A.DP.VALUE.UPDATE
*-------------------------------------------------------------------------
* <Rating>100</Rating>
*-------------------------------------------------------------------------
*-------------------------------------------------------------------------
*<Description>
* Updating LIMIT's local field Value with COLLATERAL local field value
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_F.COLLATERAL
*    $INSERT I_F.COLLATERAL.RIGHT
    $USING CO.Contract
*    $INSERT I_F.LIMIT
    $USING LI.Config
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Foundation
    $USING EB.LocalReferences
    $USING EB.TransactionControl

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

RETURN

INIT:
    FN.COLL.RGT="F.COLLATERAL.RIGHT"
    F.COLL.RGT=""
    R.COLL.RGT=""
    Y.COLL.RGT.ID=""

    FN.LMT="F.LIMIT"
    F.LMT=""
    R.LMT=""
    Y.LMT.ID=""
    Y.REC.PARENT=""
    Y.CREDIT.LINE=""

    Y.DP.AMT="TOT.POWER"
    Y.DP.AMT.POS=""
    EB.LocalReferences.GetLocRef("COLLATERAL",Y.DP.AMT,Y.DP.AMT.POS)

    Y.DP.STOCK.VAL="DR.STOCK.VALUE"
    Y.DP.STOCK.VAL.POS=""
    EB.LocalReferences.GetLocRef("LIMIT",Y.DP.STOCK.VAL,Y.DP.STOCK.VAL.POS)

RETURN

OPENFILES:

    EB.DataAccess.Opf(FN.COLL.RGT,F.COLL.RGT)
    EB.DataAccess.Opf(FN.LMT,F.LMT)

RETURN


PROCESS:

* Y.COLL.RGT.ID = FIELD(ID.NEW,".",1,1):".":FIELD(ID.NEW,".",2,1)
    Y.ID.NEW = EB.SystemTables.getIdNew()
    Y.COLL.RGT.ID = FIELD(Y.ID.NEW,".",1,1):".":FIELD(Y.ID.NEW,".",2,1)
    EB.DataAccess.FRead(FN.COLL.RGT,Y.COLL.RGT.ID,R.COLL.RGT,F.COLL.RGT,ERR.COLL)
    IF ERR.COLL THEN RETURN
* Y.LMT.ID=R.COLL.RGT<COLL.RIGHT.LIMIT.REFERENCE>
    Y.LMT.ID = R.COLL.RGT<CO.Contract.CollateralRight.CollRightLimitReference>
    
    EB.DataAccess.FRead(FN.LMT,Y.LMT.ID,R.LMT,F.LMT,ERR.LMT)
    IF ERR.LMT THEN RETURN
*    Y.REC.PARENT=R.LMT<LI.RECORD.PARENT>
    Y.REC.PARENT = R.LMT<LI.Config.Limit.RecordParent>
*    Y.CREDIT.LINE=R.LMT<LI.CREDIT.LINE>
    Y.CREDIT.LINE = R.LMT<LI.Config.Limit.CreditLine>

    IF R.LMT THEN
* R.LMT<LI.LOCAL.REF,Y.DP.STOCK.VAL.POS> = R.NEW(COLL.LOCAL.REF)<1,Y.DP.AMT.POS>
        Y.TEMP = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
        Y.DP.AMT.VAR = Y.TEMP<1,Y.DP.AMT.POS>
        R.LMT<LI.Config.Limit.LocalRef, Y.DP.STOCK.VAL.POS>= Y.DP.AMT.VAR
* CALL F.WRITE(FN.LMT,Y.LMT.ID,R.LMT)
        EB.DataAccess.FWrite(FN.LMT,Y.LMT.ID,R.LMT)
        EB.TransactionControl.JournalUpdate(Y.LMT.ID)
    END

    R.LMT = ""
    
    IF R.LMT THEN
        EB.DataAccess.FRead(FN.LMT,Y.REC.PARENT,R.LMT,F.LMT,ERR.LMT)
*        R.LMT<LI.LOCAL.REF,Y.DP.STOCK.VAL.POS> = R.NEW(COLL.LOCAL.REF)<1,Y.DP.AMT.POS>
*        CALL F.WRITE(FN.LMT,Y.REC.PARENT,R.LMT)
        Y.TEMP = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
        Y.DP.AMT.VAR = Y.TEMP<1,Y.DP.AMT.POS>
        R.LMT<LI.Config.Limit.LocalRef, Y.DP.STOCK.VAL.POS>= Y.DP.AMT.VAR
        EB.DataAccess.FWrite(FN.LMT,Y.LMT.ID,R.LMT)
        EB.TransactionControl.JournalUpdate(Y.LMT.ID)
    END
    
    R.LMT = ""
    
    IF R.LMT THEN
        EB.DataAccess.FRead(FN.LMT,Y.CREDIT.LINE,R.LMT,F.LMT,ERR.LMT)
*        R.LMT<LI.LOCAL.REF,Y.DP.STOCK.VAL.POS> = R.NEW(COLL.LOCAL.REF)<1,Y.DP.AMT.POS>
*        CALL F.WRITE(FN.LMT,Y.CREDIT.LINE,R.LMT)
        Y.TEMP = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
        Y.DP.AMT.VAR = Y.TEMP<1,Y.DP.AMT.POS>
        R.LMT<LI.Config.Limit.LocalRef, Y.DP.STOCK.VAL.POS>= Y.DP.AMT.VAR
        EB.DataAccess.FWrite(FN.LMT,Y.LMT.ID,R.LMT)
        EB.TransactionControl.JournalUpdate(Y.LMT.ID)
    END
RETURN
END