* @ValidationCode : MjotOTk3MjYyMjExOkNwMTI1MjoxNjczNDIxMDA2NjcyOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Jan 2023 13:10:06
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.DEFAULT.BRN

* Modification History:
* ----------------------
* 11/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT I_F.TELLER
    $USING TT.Contract
* $INSERT I_F.COMPANY
    $USING ST.CompanyCreation
    $USING EB.LocalReferences
    $USING EB.DataAccess
    $USING EB.SystemTables

    Y.APP = "TELLER"
    Y.FLD = "BB.ROUTE.CODE"
    Y.POS = ''
    EB.LocalReferences.GetLocRef(Y.APP,Y.FLD,Y.POS)

    FN.COMP = "F.COMPANY"
    FV.COMP = ''
    EB.DataAccess.Opf(FN.COMP,FV.COMP)

    Y.ID.COMP = EB.SystemTables.getIdCompany()
    EB.DataAccess.FRead(FN.COMP,Y.ID.COMP,R.COMP.REC,FV.COMP,ERR1)

*     R.NEW(TT.Contract.Teller.TeLocalRef)<1,Y.POS> = Y.ID.COMP
    Y.TEMP = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
    Y.TEMP<1,Y.ID.COMP>= Y.ID.COMP
    EB.SystemTables.setRNew(TT.Contract.Teller.TeLocalRef, Y.TEMP)
RETURN
END