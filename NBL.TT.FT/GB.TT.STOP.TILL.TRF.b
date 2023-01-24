* @ValidationCode : MjotMTU1ODcwMzQ4MTpDcDEyNTI6MTY3MzI0MDI2MDIzODp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 09 Jan 2023 10:57:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.TT.STOP.TILL.TRF
*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
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
* 08/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************
    $INSERT I_EQUATE
    $INSERT I_COMMON
*    $INSERT I_F.TELLER
    $USING TT.Contract
*    $INSERT I_F.TELLER.ID
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Updates
    $USING EB.Foundation
    $USING EB.ErrorProcessing


    EB.DataAccess.Opf('FBNK.TELLER.ID',F.TILL)

* EB.SystemTables.getRNew(idx) ------TAFC -TAFJ -- Shibli
    Y.TELLER.ID.1 = EB.SystemTables.getRNew(TT.TE.TELLER.ID.1)
    Y.TELLER.ID.2 =EB.SystemTables.getRNew(TT.TE.TELLER.ID.2)
* EB.DataAccess.FRead(        ------TAFC -TAFJ -- Shibli
    EB.DataAccess.FRead('FBNK.TELLER.ID',Y.TELLER.ID.1,R.TILL.1,F.TILL,E.TILL)
    EB.DataAccess.FRead('FBNK.TELLER.ID',Y.TELLER.ID.2,R.TILL.2,F.TILL,E.TILL)

*    Y.USER.1 = R.TILL.1<TT.TID.USER>
    Y.USER.1 = R.TILL.1<TT.Contract.TellerId.TidUser>
*    Y.USER.2 = R.TILL.2<TT.TID.USER>
    Y.USER.2 = R.TILL.1<TT.Contract.TellerId.TidUser>
*    Y.USER = OPERATOR
    Y.USER = EB.SystemTables.getOperator()

    Y.BOOTH.ID.1 = LEFT(RIGHT(Y.USER.1,7),3)
    Y.BOOTH.ID.2 = LEFT(RIGHT(Y.USER.2,7),3)
    Y.USER.ID = LEFT(RIGHT(Y.USER,7),3)

    Y.IS.BOOTH.1 = NUM(Y.BOOTH.ID.1)
    Y.IS.BOOTH.2 = NUM(Y.BOOTH.ID.2)
    Y.IS.USER = NUM(Y.USER.ID)

    IF Y.IS.BOOTH.1 NE Y.IS.BOOTH.2 OR Y.IS.BOOTH.1 NE Y.IS.USER OR Y.IS.BOOTH.2 NE Y.IS.USER  THEN
*        AF = 2
*        ETEXT = "TILL TRANSFER BETWEEN BRANCH AND BOOTH IS NOT POSSIBLE."
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("TILL TRANSFER BETWEEN BRANCH AND BOOTH IS NOT POSSIBLE.")
        EB.ErrorProcessing.StoreEndError()
    END
RETURN
END
