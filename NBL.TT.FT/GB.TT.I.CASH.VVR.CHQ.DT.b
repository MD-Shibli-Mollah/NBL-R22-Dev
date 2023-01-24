* @ValidationCode : MjotMTMyMDA0Mjc0OkNwMTI1MjoxNjc0NTUxMzE5NTIxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 24 Jan 2023 15:08:39
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.TT.I.CASH.VVR.CHQ.DT
    
*-----------------------------------------------------------------------------
* This routine is used as an Input Routine
*-----------------------------------------------------------------------------
* Modification History :
* Developed By         : MD SHIBLI MOLLAH -- FDS
* Date                 : 05TH JAN 2023
*-----------------------------------------------------------------------------
* Modification 2
*
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*    $INSERT I_F.TELLER
    $USING TT.Contract
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
    $USING EB.DataAccess
    $USING EB.Foundation
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.API
    
*-------LOCAL FIELDS--------------------------------* Shibli
    Y.CHQ.DATE = 'CHEQUE.DATE.VD'
    Y.CHQ.DATE.POS = ''

    Y.FT.CHQ.DATE = 'CHEQUE.DATE'
    Y.FT.CHQ.DATE.POS = ''

    Y.CHQ.DT = ''
    Y.CHQ.NO = ''

    Y.APPLICATION = EB.SystemTables.getApplication()

    IF Y.APPLICATION EQ 'TELLER' THEN
        !-----Manik/S------!
*        Y.CHQ.NO = R.NEW(TT.TE.CHEQUE.NUMBER)
*        AF=TT.TE.CHEQUE.NUMBER
        Y.CHQ.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeChequeNumber)
        !----Shibli-------!
        EB.Foundation.MapLocalFields("TELLER",Y.CHQ.DATE,Y.CHQ.DATE.POS)
        Y.CHQ.DT = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.CHQ.DATE.POS>
        GOSUB DATE.VAL
    END

    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
        !-------Manik/S----!
*  Y.CHQ.NO = R.NEW(FT.CHEQUE.NUMBER)
* AF=FT.CHEQUE.NUMBER
        !-------Shibli----!
        Y.CHQ.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
        EB.Foundation.MapLocalFields("FUNDS.TRANSFER",Y.FT.CHQ.DATE,Y.FT.CHQ.DATE.POS)
        Y.CHQ.DT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.CHQ.DATE.POS>
        GOSUB DATE.VAL
    END
DATE.VAL:
********
    IF Y.CHQ.NO NE '' THEN
        IF NOT ( NUM (Y.CHQ.NO)) THEN
*            E="Cheque should be Numeric"
*            CALL ERR
*------------Shibli---------------------
            EB.SystemTables.setEtext('Cheque should be Numeric')
            EB.ErrorProcessing.StoreEndError()

        END
    END

    IF Y.CHQ.DT NE '' THEN
        Y.TODAY = EB.SystemTables.getToday()
*       EB.API.Cdd(Ydate, Ydate2, Ydays, Yregion) ----- Shibli
        EB.API.Cdd(Y.TODAY,Y.CHQ.DT,Y.NO.OF.DAY,'BD')
        Y.STALE.PERIOD = Y.NO.OF.DAY/180
        IF (Y.STALE.PERIOD GT 1) OR (Y.STALE.PERIOD LT -1) OR Y.CHQ.DT GT Y.TODAY THEN
* AF = 1
*            ETEXT = "SORRY CHEQUE DATE IS NOT VALID"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext('SORRY CHEQUE DATE IS NOT VALID')
            EB.ErrorProcessing.StoreEndError()
            RETURN
        END
    END
RETURN

END
