* @ValidationCode : MjoyMDQ4Nzg5Njc2OkNwMTI1MjoxNjc0NTUxNzc0ODY4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 24 Jan 2023 15:16:14
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.TT.CHK.CLG.ZONE
    
*-----------------------------------------------------------------------------
* <Rating>245</Rating>
*-----------------------------------------------------------------------------
*This routine calculates the CHARGE AMOUNT, VAT AMOUNT

* Modification History :
* Developed By         : MD SHIBLI MOLLAH -- FDS
* Date                 : 05TH JAN 2023
*-----------------------------------------------------------------------------
* Modification 2
*
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT I_F.TELLER
    $USING TT.Contract
    $INSERT I_F.BD.H.BRANCH.CODE
    
* $INSERT I_F.FT.COMMISSION.TYPE
    $USING CG.ChargeConfig
*    $INSERT I_F.TELLER.TRANSACTION
    $USING TT.Config
*    $INSERT I_F.TAX
*    $INSERT I_F.STANDARD.SELECTION
    $USING EB.DataAccess
    $USING EB.Foundation
    $USING EB.SystemTables
    $USING EB.ErrorProcessing


    Y.MESSAGE = EB.SystemTables.getMessage()

    IF Y.MESSAGE EQ 'VAL' THEN RETURN

    GOSUB INIT
    GOSUB PROC
RETURN
****************************************************************
INIT:
****************************************************************
    FN.ABL = "F.BD.H.BRANCH.CODE"
    F.BD.H.BRANCH.CODE = ''
    
*  EB.DataAccess.Opf(YnameIn, YnameOut) -- Shibli ---TAFJ
    EB.DataAccess.Opf(FN.ABL,F.BD.H.BRANCH.CODE)
    R.ABL.REC = ''
    R.ZONE.REC = ''

    FN.TT.TRANS = 'F.TELLER.TRANSACTION'
    F.TELLER.TRANSACTION = ''
    EB.DataAccess.Opf(FN.TT.TRANS,F.TELLER.TRANSACTION)

    FN.FT.COM.TYPE = 'F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE = ''
    EB.DataAccess.Opf(FN.FT.COM.TYPE,F.FT.COMMISSION.TYPE)

    FN.TAX = "F.TAX"
    F.TAX = ""
    EB.DataAccess.Opf(FN.TAX,F.TAX)
    
    Y.COMPANY = EB.SystemTables.getIdCompany()

    Y.BR.LEN = LEN(Y.COMPANY)
    !S

*Y.BR.LEN = Y.BR.LEN - 3
    Y.BR.LEN = Y.BR.LEN - 2
    !E

* Y.TXN.NO = EB.SystemTables.getRNew(TT.TE.TRANSACTION.CODE)
* EB.SystemTables.getRNew(idx) ----TAFJ
    Y.TXN.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeTransactionCode)
    
* EB.DataAccess.FRead(Fileid, VKey, Rec, FFileid, Er) --- TAFJ
    EB.DataAccess.FRead(FN.TT.TRANS,Y.TXN.NO,R.TT.TRANS,F.TELLER.TRANSACTION,Y.TT.TRANS.ERR)
*  Y.CHG.CODE = R.TT.TRANS<TT.TR.CHARGE.CODE>
    Y.CHG.CODE = R.TT.TRANS<TT.Config.TellerTransaction.TrChargeCode>

    EB.DataAccess.FRead(FN.FT.COM.TYPE,Y.CHG.CODE,R.FT.COM.REC,F.FT.COMMISSION.TYPE,Y.FT.COM.ERR)
* Y.UPTO.AMT = R.FT.COM.REC<FT4.UPTO.AMT>
    Y.UPTO.AMT = R.FT.COM.REC<CG.ChargeConfig.FtCommissionType.FtFouUptoAmt>
*    Y.MIN.AMT = R.FT.COM.REC<FT4.MINIMUM.AMT>
    Y.MIN.AMT = R.FT.COM.REC<CG.ChargeConfig.FtCommissionType.FtFouMinimumAmt>
*    Y.TAX.ID = R.FT.COM.REC<FT4.TAX.CODE>
    Y.TAX.ID = R.FT.COM.REC<CG.ChargeConfig.FtCommissionType.FtFouTaxCode>
    Y.AMT.CNT = DCOUNT(Y.UPTO.AMT,@SM)
    Y.AMT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)
    Y.M.AMT = 1
*    Y.VAT.AMT = 2
    Y.CHARGE.AMOUNT = ''

RETURN
**********************************************************
PROC:
**********************************************************

*    Y.ABL.ID = COMI
    Y.ABL.ID = EB.SystemTables.getComi()
*    Y.BR.ID = ID.COMPANY[Y.BR.LEN,-1]
    Y.BR.ID = EB.SystemTables.getIdCompany()[Y.BR.LEN,-1]

    Y.BR.ID = '209':Y.BR.ID
    EB.DataAccess.FRead(FN.ABL,Y.BR.ID,R.ABL.REC,F.BD.H.BRANCH.CODE,ERR1)
    Y.BR.ZONE = R.ABL.REC<ABL.CLG.ZONE>
    !Y.BR.CODE = R.ABL.REC<ABL.CODE>

    EB.DataAccess.FRead(FN.ABL,Y.ABL.ID,R.ZONE.REC,F.BD.H.BRANCH.CODE,ERR1)
    Y.ABL.ZONE = R.ZONE.REC<ABL.CLG.ZONE>

    IF Y.ABL.ZONE EQ 0 OR Y.BR.ZONE EQ 0 THEN
* EB.SystemTables.getRNew(TT.TE.WAIVE.CHARGES) = 'NO'
        EB.SystemTables.setRNew(TT.Contract.Teller.TeWaiveCharges, 'NO')
        GOSUB FT.COMM.CHG
    END ELSE
        IF Y.ABL.ZONE NE Y.BR.ZONE THEN
* EB.SystemTables.getRNew(TT.TE.WAIVE.CHARGES) = 'NO'
            EB.SystemTables.setRNew(TT.Contract.Teller.TeWaiveCharges, 'NO')
            GOSUB FT.COMM.CHG
        END ELSE
            IF Y.ABL.ZONE EQ Y.BR.ZONE THEN
* EB.SystemTables.getRNew(TT.TE.WAIVE.CHARGES) = 'YES'
                EB.SystemTables.setRNew(TT.Contract.Teller.TeWaiveCharges, 'NO')
                GOSUB FT.COMM.CHG
            END
        END
    END
    CALL REBUILD.SCREEN

RETURN
*-------------------------------------------------------------
FT.COMM.CHG:
*-------------------------------------------------------------

    Y.CHG = EB.SystemTables.getRNew(TT.Contract.Teller.TeWaiveCharges)

    IF Y.CHG EQ 'YES' THEN
        Y.CHARGE.AMOUNT = EB.SystemTables.getRNew(TT.Contract.Teller.TeChrgAmtLocal)
        Y.CHG.AMT.CNT = DCOUNT(Y.CHARGE.AMOUNT,@VM)
        IF Y.CHG.AMT.CNT GT 1 THEN
            FOR KK = Y.CHG.AMT.CNT TO 1 STEP-1
****-------------- NEED TO TEST the DELETE operation ---------------------*** SHIBLI --- FDS
                DELETE EB.SystemTables.getRNew(TT.Contract.Teller.TeChrgAmtLocal)<1,KK>
                Y.CHARGE.AMOUNT = EB.SystemTables.getRNew(TT.Contract.Teller.TeChrgAmtLocal)
            NEXT KK
        END

        EB.SystemTables.setRNew(TT.Contract.Teller.TeWaiveCharges, 'YES')
*        EB.SystemTables.setRNew(TT.TE.CHARGE.CODE, '')
        EB.SystemTables.setRNew(TT.Contract.Teller.TeChargeCode, '')
        EB.SystemTables.setRNew(TT.Contract.Teller.TeChargeAccount, '')
        EB.SystemTables.setRNew(TT.Contract.Teller.TeChargeCategory, '')
*  EB.SystemTables.setRNew(TT.TE.CHRG.DR.TXN.CDE, '')
        EB.SystemTables.setRNew(TT.Contract.Teller.TeChrgDrTxnCde, '')
* EB.SystemTables.setRNew(TT.TE.CHRG.CR.TXN.CDE, '')
        EB.SystemTables.setRNew(TT.Contract.Teller.TeChrgCrTxnCde, '')
        EB.SystemTables.setRNew(TT.Contract.Teller.TeChrgAmtLocal, Y.CHARGE.AMOUNT)
        EB.SystemTables.setRNew(TT.Contract.Teller.TeNetAmount, Y.AMT)
    END ELSE
        IF Y.CHG EQ 'NO' THEN
            EB.SystemTables.setRNew(TT.Contract.Teller.TeWaiveCharges, 'NO')
            FOR I = 1 TO Y.AMT.CNT
                IF I GT 1 THEN
                    J = I - 1
                    Y.M.AMT = Y.UPTO.AMT<1,1,J>
                END
                Y.U.AMT = Y.UPTO.AMT<1,1,I>
                IF Y.AMT GE Y.M.AMT AND Y.AMT LE Y.U.AMT THEN
                    Y.CHG.AMT = Y.MIN.AMT<1,1,I>
********* TO CALCULATE VAT FOR COMMISSION AMOUNT**************
                    GOSUB CALC.VAT.AMT
**************************************************************
                    Y.NET.AMT = Y.AMT + Y.CHG.AMT + Y.VAT.AMT
                    Y.CHARGE.AMOUNT = Y.CHG.AMT : @VM : Y.VAT.AMT
                    EB.SystemTables.setRNew(TT.Contract.Teller.TeChrgAmtLocal, Y.CHARGE.AMOUNT)
                    EB.SystemTables.setRNew(TT.Contract.Teller.TeNetAmount, Y.NET.AMT)
                END ELSE
                    K = Y.AMT.CNT -1
                    Y.MAX.AMT = Y.UPTO.AMT<1,1,K>
                    IF Y.AMT GT Y.MAX.AMT THEN
                        Y.CHG.AMT = Y.MIN.AMT<1,1,I>

********* TO CALCULATE VAT FOR COMMISSION AMOUNT**************
                        GOSUB CALC.VAT.AMT
**************************************************************

                        Y.NET.AMT = Y.AMT + Y.CHG.AMT + Y.VAT.AMT
                        Y.CHARGE.AMOUNT = Y.CHG.AMT : @VM : Y.VAT.AMT
                        EB.SystemTables.setRNew(TT.Contract.Teller.TeChrgAmtLocal, Y.CHARGE.AMOUNT)
                        EB.SystemTables.setRNew(TT.Contract.Teller.TeNetAmount, Y.NET.AMT)
                    END
                END
            NEXT I
        END
    END

RETURN
*****************************************************************
CALC.VAT.AMT:
*****************************************************************

    Y.SEL = "SELECT ":FN.TAX: " WITH @ID LIKE ":Y.TAX.ID:"..."
    EB.DataAccess.Readlist(Y.SEL,Y.SEL.LIST,'',Y.SEL.CNT,Y.SEL.ERR)
    Y.TAX = Y.SEL.LIST<Y.SEL.CNT>
    EB.DataAccess.FRead(FN.TAX,Y.TAX,R.TAX.REC,F.TAX,ERR2)
* Y.VAL = R.TAX.REC<EB.TAX.RATE>
    Y.VAL = R.TAX.REC<CG.ChargeConfig.Tax.EbTaxRate>

    Y.VAT.AMT = Y.CHG.AMT * (Y.VAL/100)

RETURN
END