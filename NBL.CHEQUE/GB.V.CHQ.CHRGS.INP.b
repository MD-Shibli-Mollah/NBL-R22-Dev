* @ValidationCode : MjotMzk0MTI3MTY4OkNwMTI1MjoxNjczMTc4NzM5ODEzOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jan 2023 17:52:19
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.CHQ.CHRGS.INP

*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
**************************************************************************
*Subroutine Description:
*-----------------------
* PURPOSE:The Routine is used to default the charges based on the Cheque Type.
*
*-------------------------------------------------------------------------
*Parameter Description
*--------- -----------
*
* <parameter description if used>
*
*-------------------------------------------------------------------------
*Common Variable Description
*------  ------- -----------
*
*Variable name:   Insert file:          Description:
* TODAY            I_COMMON              Will hold Todays date used for
*                                        writing in flat file
*
*-------------------------------------------------------------------------
*<Description of the arguments>
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 08/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    
*    $INSERT I_F.CHEQUE.ISSUE
* $USING ST.ChqIssue
    $USING CQ.ChqIssue
*    $INSERT I_F.FT.COMMISSION.TYPE
    $USING CG.ChargeConfig
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Updates
    $USING EB.Foundation
    $USING EB.ErrorProcessing

    GOSUB INITILISE
    GOSUB PROCESS

RETURN

*******************
INITILISE:
*******************

    FN.CHQ.ISS = 'F.CHEQUE.ISSUE'
    F.CHQ.ISS = ''
* EB.DataAccess.Opf(YnameIn, YnameOut)
    EB.DataAccess.Opf(FN.CHQ.ISS,F.CHQ.ISS)

    FN.FT.COMM = 'F.FT.COMMISSION.TYPE'
    F.FT.COMM = ''
    EB.DataAccess.Opf(FN.FT.COMM,F.FT.COMM)

RETURN

******************
PROCESS:
******************
    !Y.SB = 'SBCHQCHGS'
    !Y.CD = 'CDCHQCHGS'
    !Y.SND = 'SNDCHQCHGS'

    Y.SB = 'SBCHQCHRGS'
    Y.CD = 'CDCHQCHRGS'
    Y.SND = 'SNDCHQCHRGS'
    Y.CHQ = 'CHQCHARGS'


    Y.ID = EB.SystemTables.getIdNew()
* Y.ID = FIELD(ID.NEW,".",1)

* IF R.NEW(CHEQUE.IS.CHG.CODE) EQ "" THEN
    IF EB.SystemTables.getRNew(CQ.ChqIssue.ChequeIssue.ChequeIsChgCode) EQ "" THEN
        BEGIN CASE
            CASE Y.ID EQ 'SB'
* R.NEW(CHEQUE.IS.CHG.CODE) = Y.SB
                EB.SystemTables.setRNew(CQ.ChqIssue.ChequeIssue.ChequeIsChgCode, Y.SB)
            CASE Y.ID EQ 'CD'
                EB.SystemTables.setRNew(CQ.ChqIssue.ChequeIssue.ChequeIsChgCode, Y.CD)
            CASE Y.ID EQ 'SND'
                EB.SystemTables.setRNew(CQ.ChqIssue.ChequeIssue.ChequeIsChgCode, Y.SND)
* DEFAULT CASE
            CASE 1
                EB.SystemTables.setRNew(CQ.ChqIssue.ChequeIssue.ChequeIsChgCode, Y.CHQ)
        END CASE

*        CALL REFRESH.FIELD(CHEQUE.IS.CHG.CODE,'')
*        CALL REBUILD.SCREEN
    END
RETURN
***************************************************************************************************************
END
