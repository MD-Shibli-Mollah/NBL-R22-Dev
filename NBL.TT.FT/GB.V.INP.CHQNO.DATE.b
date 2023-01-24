* @ValidationCode : MjotMTI1NDIzNjIwOkNwMTI1MjoxNjc0NTUxODkyOTk5OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 24 Jan 2023 15:18:12
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.INP.CHQNO.DATE
    
*-----------------------------------------------------------------------------
* <Rating>885</Rating>
*-----------------------------------------------------------------------------
* Input routine to the FT and TT version
* to check if the cheque number and Cheque Date is Null
* then through an error message.
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT I_F.TELLER
    $USING TT.Contract
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Updates
    $USING EB.ErrorProcessing
*
*-----------------------------------------------------------------------------
*

    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()

    IF Y.PGM.VERSION EQ ',BD.LCY.PAY.CIBIT' THEN RETURN
    IF Y.PGM.VERSION EQ ',PR.SDR.ISSUE.INITIAL' THEN RETURN

    Y.APP = "FUNDS.TRANSFER":@FM:"TELLER"
    Y.FLD = "CHEQUE.DATE":@VM:"PR.CHEQUE.NO":@VM:"DEBIT.AUTHORITY":@FM:"CHEQUE.DATE":@VM:"PR.CHEQUE.NO"
    Y.POS = ""
*

    
    Y.APPLICATION = EB.SystemTables.getApplication()
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
    
    EB.Updates.MultiGetLocRef(Y.APP,Y.FLD,Y.POS)
    Y.FT.CHQ.DATE = Y.POS<1,1>
    Y.FT.CHQ.NUM = Y.POS<1,2>
    Y.FT.LOC.DBA = Y.POS<1,3>
    Y.TT.CHQ.DATE = Y.POS<2,1>
    Y.TT.CHQ.NUM = Y.POS<2,2>
*
    IF Y.APPLICATION EQ 'TELLER' THEN
        IF Y.PGM.VERSION EQ ',ABL.DD.ISSUE' THEN
            Y.CHQ.NO = ''
        END ELSE
* EB.SystemTables.getRNew(idx)
            Y.CHQ.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.CHQ.NUM>
            IF Y.CHQ.NO EQ '' THEN
                Y.CHQ.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeChequeNumber)
            END
            Y.CHQ.DATE = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.CHQ.DATE>
        END
    END ELSE
        IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
            IF Y.PGM.VERSION EQ ',PR.PO.COLLECTION' THEN RETURN
            IF Y.PGM.VERSION EQ ',PR.SDR.COLLECTION' THEN RETURN
            IF Y.PGM.VERSION EQ ',PR.PS.COLLECTION' THEN RETURN
            
            Y.CHQ.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.CHQ.NUM>
            IF Y.CHQ.NO EQ '' THEN
* Y.CHQ.NO = EB.SystemTables.getRNew(FT.CHEQUE.NUMBER)
                Y.CHQ.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.ChequeNumber)
            END
            IF Y.PGM.VERSION EQ ',ABL.DD.ISSUE' THEN
* Y.CHQ.NO = EB.SystemTables.getRNew(FT.CHEQUE.NUMBER)
                Y.CHQ.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.ChequeNumber)
            END
            Y.CHQ.DATE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.CHQ.DATE>
        END

    END
*------------------------------------------
    Y.FT.LOC.DBT.AUTH = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.LOC.DBA>
*------------------------------------------
    IF Y.FT.LOC.DBT.AUTH EQ 'YES' THEN
        IF Y.APPLICATION EQ 'TELLER' THEN
*            EB.SystemTables.getRNew(TT.TE.CHEQUE.NUMBER) = ''
            EB.SystemTables.setRNew(TT.Contract.Teller.TeChequeNumber, '')
            
            Y.TEMP = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
            Y.TEMP<1,Y.TT.CHQ.DATE>= ''
            EB.SystemTables.setRNew(AA.Account.Account.AcLocalRef, Y.TEMP)
            
*  EB.SystemTables.setRNew((TT.Contract.Teller.TeLocalRef)<1,Y.TT.CHQ.DATE>, '')
            
* EB.DataAccess.Dbr(Checkfile1, I1, Enri2)
*  Y.TEMP = EB.SystemTables.getRNew(AA.Account.Account.AcLocalRef)
*        Y.TEMP<1,Y.OLD.LEGACY.ID.POS>=Y.EXLC.COLL.NO

*        EB.SystemTables.setRNew(AA.Account.Account.AcLocalRef, Y.TEMP)
        END
        IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.ChequeNumber, '')
            Y.TEMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
            Y.TEMP<1,Y.FT.CHQ.DATE>= ''
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.LocalRef, Y.TEMP)
* EB.SystemTables.setRNew((FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.CHQ.DATE>, '')
        END
        RETURN
    END ELSE
        IF Y.APPLICATION EQ "TELLER" THEN
            IF Y.CHQ.NO EQ '' AND Y.CHQ.DATE NE '' THEN
*                AF = TT.TE.CHEQUE.NUMBER
*                ETEXT = "EB-FT.TT.NULL.NUM"
                
                EB.SystemTables.setEtext("EB-FT.TT.NULL.NUM")
                EB.ErrorProcessing.StoreEndError()
*                CALL STORE.END.ERROR
            END
            IF Y.CHQ.DATE EQ '' AND Y.CHQ.NO NE '' THEN
*                AF = TT.TE.LOCAL.REF ; AV = Y.TT.CHQ.DATE
*                ETEXT = "EB-FT.TT.NULL.DAT"
                EB.SystemTables.setEtext("EB-FT.TT.NULL.DAT")
                EB.ErrorProcessing.StoreEndError()
*                CALL STORE.END.ERROR
            END
        END
        IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
            IF PGM.VERSION EQ ",ABL.DD.ISSUE" THEN
                IF Y.CHQ.NO EQ '' THEN
*                    AF = FT.CHEQUE.NUMBER
*                    ETEXT = "EB-FT.TT.NULL.NUM"
                    EB.SystemTables.setEtext("EB-FT.TT.NULL.NUM")
                    EB.ErrorProcessing.StoreEndError()
*                    CALL STORE.END.ERROR
                END
                IF Y.CHQ.DATE EQ '' THEN
*                    AF = FT.LOCAL.REF; AV = Y.FT.CHQ.DATE
*                    ETEXT = "EB-FT.TT.NULL.DAT"
*                    CALL STORE.END.ERROR
                    EB.SystemTables.setEtext("EB-FT.TT.NULL.DAT")
                    EB.ErrorProcessing.StoreEndError()
                END
            END ELSE
                IF Y.CHQ.NO EQ '' AND Y.CHQ.DATE NE '' THEN
*                    AF = FT.CHEQUE.NUMBER
*                    ETEXT = "EB-FT.TT.NULL.NUM"
                    EB.SystemTables.setEtext("EB-FT.TT.NULL.NUM")
                    EB.ErrorProcessing.StoreEndError()
*                    CALL STORE.END.ERROR
                END
                IF Y.CHQ.DATE EQ '' AND Y.CHQ.NO NE '' THEN
*                    AF = FT.LOCAL.REF; AV = Y.FT.CHQ.DATE
*                    ETEXT = "EB-FT.TT.NULL.DAT"
                    EB.SystemTables.setEtext("EB-FT.TT.NULL.DAT")
                    EB.ErrorProcessing.StoreEndError()
*                    CALL STORE.END.ERROR
                END
            END
        END
    END
*------------------------------------------

RETURN
END