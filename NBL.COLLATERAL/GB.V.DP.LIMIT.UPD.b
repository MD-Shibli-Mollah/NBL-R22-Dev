* @ValidationCode : MjotMjY1MzkyODI6Q3AxMjUyOjE2NzM4NzI4Njg3Mzg6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 16 Jan 2023 18:41:08
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.DP.LIMIT.UPD
    
*-------------------------------------------------------------------------
*<Description>
* Update COLLATERAL Local fields with LIMIT Value.
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
    $USING LI.Config
    
    $USING EB.Updates
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.Display
    $USING EB.ErrorProcessing
    $USING EB.Foundation

*    $INCLUDE I_F.LIMIT
*    $INCLUDE I_F.COLLATERAL

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB GET.LOC.MULTI.REF
    GOSUB PROCESS
RETURN

INITIALISE:
***********
    Y.ACC.ID = ''
    R.LIMIT.ERROR = ''

RETURN


OPEN.FILES:
************
    FN.LIMIT = 'F.LIMIT';F.LIMIT = '';R.LIMIT = ''
    EB.DataAccess.Opf(FN.LIMIT,F.LIMIT)

    FN.CUSTOMER = 'F.CUSTOMER';F.CUSTOMER = '';R.CUSTOMER = ''
    EB.DataAccess.Opf(FN.CUSTOMER,F.CUSTOMER)

RETURN

GET.LOC.MULTI.REF:
******************
    Y.APPLICATION       = "COLLATERAL"
*FIELD.NAME          = "LIMIT.REFERENCE":VM:"LIMIT":VM:"L.START.DATE":VM:"L.EXPIRY.DATE"
    FIELD.NAME          = "LIMIT.REFE":@VM:"LIMIT":@VM:"L.START.DATE":@VM:"L.EXPIRY.DATE"
    FIELD.POS           = ""
    EB.Updates.MultiGetLocRef(Y.APPLICATION,FIELD.NAME,FIELD.POS)
    LIMIT.REFERENCE.POS  = FIELD.POS<1,1>
    LIMIT.POS = FIELD.POS<1,2>
    L.START.DATE.POS  = FIELD.POS<1,3>
    L.EXPIRY.DATE.POS = FIELD.POS<1,4>

RETURN

PROCESS:
********

* Y.LIMIT.REF = COMI
    Y.LIMIT.REF = EB.SystemTables.getComi()
    Y.ID.NEW = EB.SystemTables.getIdNew()
    
    Y.LIMIT.ID = FIELD(Y.ID.NEW,'.',1):".":FMT(Y.LIMIT.REF,"R%7"):".01"
    
    EB.DataAccess.FRead(FN.LIMIT,Y.LIMIT.ID,R.LIMIT,F.LIMIT,R.LIMIT.ERROR)
    Y.LI.INTERNAL.AMOUNT = R.LIMIT<LI.Config.Limit.InternalAmount>
    Y.ONLINE.LIMIT.DATE = R.LIMIT<LI.Config.Limit.OnlineLimitDate>
    Y.EXPIRY.DATE = R.LIMIT<LI.Config.Limit.ExpiryDate>
    
*    R.NEW(COLL.LOCAL.REF)<1,LIMIT.POS>  = R.LIMIT<LI.INTERNAL.AMOUNT>
*    R.NEW(COLL.LOCAL.REF)<1,L.START.DATE.POS>  = R.LIMIT<LI.ONLINE.LIMIT.DATE>
*    R.NEW(COLL.LOCAL.REF)<1,L.EXPIRY.DATE.POS>  =  R.LIMIT<LI.EXPIRY.DATE>
    Y.TEMP = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)

    Y.TEMP<1,LIMIT.POS> = Y.LI.INTERNAL.AMOUNT
    Y.TEMP<1,L.START.DATE.POS> = Y.ONLINE.LIMIT.DATE
    Y.TEMP<1,L.EXPIRY.DATE.POS> = Y.EXPIRY.DATE
    EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef, Y.TEMP)
* CALL REBUILD.SCREEN
    EB.Display.RebuildScreen()
RETURN
END