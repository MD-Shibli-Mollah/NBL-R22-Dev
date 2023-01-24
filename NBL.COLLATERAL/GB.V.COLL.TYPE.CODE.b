* @ValidationCode : MjoxNTM1NDg1MTM6Q3AxMjUyOjE2NzM4NjY4MTI1MTY6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 16 Jan 2023 17:00:12
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

*---------------------------------------------------------------------------------------------------------
* If the COLLATERAL.TYPE & COLLATERAL.CODE value are same, then To make the COMPANY field as
* inputable field else its a no inputable field, and after the company is inputted then the
* GROUP,FACE.VALUE and LOT fields from the COMPANY TABLE to be defaulted in the collateral record.
*-------------------------------------------------------------------------------------------------------
*
SUBROUTINE GB.V.COLL.TYPE.CODE

    $INCLUDE I_COMMON
    $INCLUDE I_EQUATE
*    $INCLUDE I_F.COLLATERAL
*    $INCLUDE I_F.COLLATERAL.TYPE
*    $INCLUDE I_F.COLLATERAL.CODE
    $INCLUDE I_F.CO.LOAN.AGAINST.SHARE
    $USING CO.Contract
*    $INSERT I_F.LIMIT
    $USING LI.Config
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Foundation
    $USING EB.LocalReferences
    $USING EB.TransactionControl
    $USING EB.Updates

    GOSUB INIT
    GOSUB PROCESS
    !GOSUB MULTI.GET.LOCAL
INIT:
    Y.COMPANY.CODE=''
    FN.COLLATERAL='F.COLLATERAL'
    F.COLLATERAL=''
    CALL OPF(FN.COLLATERAL,F.COLLATERAL)
    FN.LOAN.AGAINST.SHARE='F.CO.LOAN.AGAINST.SHARE'
    F.LOAN.AGAINST.SHARE=''
    CALL OPF(FN.LOAN.AGAINST.SHARE,F.LOAN.AGAINST.SHARE)
*
PROCESS:
* IF R.NEW(COLL.COLLATERAL.CODE) EQ '230' AND R.NEW(COLL.COLLATERAL.TYPE) EQ '230' THEN
    IF EB.SystemTables.getRNew(CO.Contract.Collateral.CollCollateralCode) EQ '230' AND EB.SystemTables.getRNew(CO.Contract.Collateral.CollCollateralType) EQ '230' THEN
*
        COMPANY.CODE = EB.SystemTables.getComi()
        IF COMPANY.CODE THEN
            GOSUB MULTI.GET.LOCAL
            EB.DataAccess.FRead(FN.LOAN.AGAINST.SHARE,COMPANY.CODE,R.LOAN.AGAINST.SHARE,F.LOAN.AGAINST.SHARE,R.ERR)
*            R.NEW(COLL.LOCAL.REF)<1,Y.GROUP.POS>=R.LOAN.AGAINST.SHARE<CO.LOA.GROUP>
            Y.TEMP = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
            Y.LOA.GROUP = R.LOAN.AGAINST.SHARE<CO.LOA.GROUP>
            Y.TEMP<1,Y.GROUP.POS>= Y.LOA.GROUP
*            R.NEW(COLL.LOCAL.REF)<1,Y.FACE.VALUE.POS>=R.LOAN.AGAINST.SHARE<CO.LOA.FACE.VALUE>
            Y.LOA.FACE.VALUE = R.LOAN.AGAINST.SHARE<CO.LOA.FACE.VALUE>
            Y.TEMP<1,Y.FACE.VALUE.POS>= Y.LOA.FACE.VALUE
*            R.NEW(COLL.LOCAL.REF)<1,Y.LOT.POS>=R.LOAN.AGAINST.SHARE<CO.LOA.LOT>
            Y.LOA.LOT = R.LOAN.AGAINST.SHARE<CO.LOA.LOT>
            Y.TEMP<1,Y.LOT.POS>= Y.LOA.LOT
            EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef, Y.TEMP)
        END
        !ELSE
        !    R.NEW(COLL.LOCAL.REF)<1,Y.COMPANY.CODE.POS>=''
        !    T.LOCREF<Y.COMPANY.CODE.POS,7>='NOINPUT'
        !END
    END
RETURN
*

MULTI.GET.LOCAL:
    FN.APPL='COLLATERAL'
    FIELD.NAME='COMPANY.CODE':@VM:'GROUP':@VM:'FACE.VALUE':@VM:'LOT'
    FLD.POS=''
    EB.Updates.MultiGetLocRef(FN.APPL,FIELD.NAME,FLD.POS)
    Y.COMPANY.CODE.POS=FLD.POS<1,1>
    Y.GROUP.POS=FLD.POS<1,2>
    Y.FACE.VALUE.POS=FLD.POS<1,3>
    Y.LOT.POS=FLD.POS<1,4>

RETURN
