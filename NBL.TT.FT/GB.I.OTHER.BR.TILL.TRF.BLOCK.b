* @ValidationCode : Mjo5NzU2NTYxOTA6Q3AxMjUyOjE2NzM0MjgwNTQxNDQ6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 11 Jan 2023 15:07:34
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.I.OTHER.BR.TILL.TRF.BLOCK
*-----------------------------------------------------------------------------
* <Rating>100</Rating>
*-----------------------------------------------------------------------------
* Subroutine developed to restrict other branch teller
* Developed by Partha, NBL ITD on Janauary 2017
*-------------------------------------------------------------------------
*<Description of the arguments>
* Attached to VERSION.CONTROL of TELLER
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 11/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
*        $INSERT I_F.TELLER
    $USING TT.Contract
*        $INSERT I_F.TELLER.ID
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

    FN.TT.ID = 'F.TELLER.ID'
    F.TT.ID = ''
    EB.DataAccess.Opf(FN.TT.ID,F.TT.ID)
* Y.TXN.REF=ID.NEW
    Y.TXN.REF = EB.SystemTables.getIdNew()
* IF Y.TXN.REF EQ 'TT1830239LH8' THEN RETURN
    !    DEBUG
*        Y.TILL.1=R.NEW(TT.TE.TELLER.ID.1)
    Y.TILL.1 = EB.SystemTables.getRNew(TT.Contract.Teller.TeTellerIdOne)
*        Y.TILL.2=R.NEW(TT.TE.TELLER.ID.2)
    Y.TILL.2 = EB.SystemTables.getRNew(TT.Contract.Teller.TeTellerIdTwo)
            
    IF Y.TILL.1 NE Y.TILL.2 THEN
* Y.COM=ID.COMPANY
        Y.COM = EB.SystemTables.getIdCompany()
        EB.DataAccess.FRead(FN.TT.ID,Y.TILL.1,R.TT.ID,F.TT.ID,ERR.T1)
* Y.T1.COM=R.TT.ID<TT.TID.CO.CODE>
        Y.T1.COM = R.TT.ID<TT.Contract.TellerId.TidCoCode>
        EB.DataAccess.FRead(FN.TT.ID,Y.TILL.2,R.TT.ID,F.TT.ID,ERR.T2)
* Y.T2.COM=R.TT.ID<TT.TID.CO.CODE>
        Y.T2.COM = R.TT.ID<TT.Contract.TellerId.TidCoCode>
        
        IF Y.T1.COM NE Y.T2.COM THEN
*                ETEXT='You can not transfer Till of other Branch'
*                CALL STORE.END.ERROR
            EB.SystemTables.setEtext('You can not transfer Till of other Branch')
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN
END