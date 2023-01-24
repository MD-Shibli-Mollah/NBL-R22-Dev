* @ValidationCode : MjoxMTkxNDY2ODAwOkNwMTI1MjoxNjczNDk5OTIwMjQzOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Jan 2023 11:05:20
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.A.PRAN.TT.MESSAGE
*-----------------------------------------------------------------------------
* <Rating>78</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Author: Kishor Kumar Saha
* Description : This Routine will invoke at the time of Authorization
* Create DATE : 26th July 2019

* Modification History :
* RETROFIT By          : MD SHIBLI MOLLAH -- FDS
* Date                 : 10TH JAN 2023
*-----------------------------------------------------------------------------

    $INCLUDE I_EQUATE
    $INCLUDE I_COMMON
* $INCLUDE I_F.TELLER
    $USING TT.Contract
    $USING EB.SystemTables
    $USING EB.LocalReferences
    !DEBUG
* Y.CREDIT.ACCT = R.NEW(TT.TE.ACCOUNT.1)
    Y.CREDIT.ACCT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
    IF Y.CREDIT.ACCT EQ '1106000950853' OR Y.CREDIT.ACCT EQ '1106000040867' OR Y.CREDIT.ACCT EQ '1106000040868' THEN

        OPEN 'PRAN.SMS' TO F.WRITEREC ELSE NULL
        Y.LOG.FILE = 'LOG.txt'
        WRITE 'STARTED' ON F.WRITEREC,Y.LOG.FILE ON ERROR
            CRT 'UNABLE TO WRITE'
        END
        Y.VFUNCTION = EB.SystemTables.getVFunction()
        IF Y.VFUNCTION EQ 'A' THEN
* Y.RECORD.STATUS = R.NEW(TT.TE.RECORD.STATUS)
            Y.RECORD.STATUS = EB.SystemTables.getRNew(TT.Contract.Teller.TeRecordStatus)
            IF Y.RECORD.STATUS EQ 'INAU' THEN
                WRITE 'NEW RECORD ON AUTH':EB.SystemTables.getRNew(1) ON F.WRITEREC,Y.LOG.FILE ON ERROR
                    CRT ''
                END

* CALL GET.LOC.REF("TELLER","DISTRIB.CODE",FIELD.POS.DIS.CODE)
                EB.LocalReferences.GetLocRef("TELLER","DISTRIB.CODE",FIELD.POS.DIS.CODE)
                EB.LocalReferences.GetLocRef("TELLER","DEPOSIT.SLIP",FIELD.POS.DEP.SLIP)
                
* Y.TELLER.ID = ID.NEW
                Y.TELLER.ID = EB.SystemTables.getIdNew()
                Y.REF.CODE = 0
                Y.BANK.CODE = '725129'
*                Y.BR.CODE = R.NEW(TT.TE.CO.CODE)
                Y.BR.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeCoCode)
*                Y.ACC.NUM = R.NEW(TT.TE.ACCOUNT.1)
                Y.ACC.NUM = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
                Y.TRAN.NUM = Y.TELLER.ID
* Y.DIS.CODE = R.NEW(TT.TE.LOCAL.REF)<1,FIELD.POS.DIS.CODE>
                Y.TEMP = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
                Y.DIS.CODE = Y.TEMP<1,FIELD.POS.DIS.CODE>
* Y.DR.CR.MARKER = R.NEW(TT.TE.DR.CR.MARKER)
                Y.DR.CR.MARKER = EB.SystemTables.getRNew(TT.Contract.Teller.TeDrCrMarker)
* Y.CR.AMT = FMT(R.NEW(TT.TE.AMOUNT.LOCAL.1),"R2")
                Y.CR.AMT = FMT(EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne),"R2")
                Y.DR.AMT = 0
* Y.DEP.SLIP = R.NEW(TT.TE.LOCAL.REF)<1,FIELD.POS.DEP.SLIP>
                Y.DEP.SLIP = Y.TEMP<1,FIELD.POS.DEP.SLIP>
* Y.DATE = R.NEW(TT.TE.AUTH.DATE)
                Y.DATE = EB.SystemTables.getRNew(TT.Contract.Teller.TeAuthDate)
                Y.TRAN.DATE = OCONV(ICONV(Y.DATE, 'D-'), 'D4-E')
*                Y.REMARKS = R.NEW(TT.TE.NARRATIVE.2)
                Y.REMARKS = EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)
*                Y.CURRENCY = R.NEW(TT.TE.CURRENCY.1)
                Y.CURRENCY = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrencyOne)
                OUTPUT.STR<-1> = Y.REF.CODE :"|": Y.BANK.CODE :"|": Y.BR.CODE :"|": Y.ACC.NUM :"|": Y.TRAN.NUM :"|": Y.DIS.CODE :"|": Y.CR.AMT :"|": Y.DR.AMT :"|": Y.DEP.SLIP :"|": Y.TRAN.DATE :"|": Y.REMARKS :"|": Y.CURRENCY
                !OPEN 'PRAN.SMS' TO F.WRITEREC ELSE NULL
                W.M.FILE.NAME = Y.TELLER.ID:'.':Y.ACC.NUM:'.':Y.DATE:'.txt'
                WRITE OUTPUT.STR ON F.WRITEREC,W.M.FILE.NAME ON ERROR
                    CRT 'UNABLE TO WRITE'
                END
            END
            IF Y.RECORD.STATUS EQ 'RNAU' THEN
                WRITE 'NEW RECORD ON REVERSE':EB.SystemTables.getRNew(1) ON F.WRITEREC,Y.LOG.FILE ON ERROR
                    CRT ''
                END
                EB.LocalReferences.GetLocRef("TELLER","DISTRIB.CODE",FIELD.POS.DIS.CODE)
                EB.LocalReferences.GetLocRef("TELLER","DEPOSIT.SLIP",FIELD.POS.DEP.SLIP)
                
*                Y.TELLER.ID = ID.NEW
                Y.TELLER.ID = EB.SystemTables.getIdNew()
                Y.REF.CODE = Y.TELLER.ID
                Y.BANK.CODE = '725129'
*                Y.BR.CODE = R.NEW(TT.TE.CO.CODE)
                Y.BR.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeCoCode)
                
*                Y.ACC.NUM = R.NEW(TT.TE.ACCOUNT.1)
                Y.TRAN.NUM = Y.TELLER.ID
                Y.TEMP = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
                Y.DIS.CODE = Y.TEMP<1,FIELD.POS.DIS.CODE>
                Y.DR.CR.MARKER = EB.SystemTables.getRNew(TT.Contract.Teller.TeDrCrMarker)
                Y.CR.AMT = 0
                Y.DR.AMT = FMT(EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne),"R2")
                Y.DEP.SLIP = Y.TEMP<1,FIELD.POS.DEP.SLIP>
                Y.DATE = EB.SystemTables.getRNew(TT.Contract.Teller.TeAuthDate)
                Y.TRAN.DATE = OCONV(ICONV(Y.DATE, 'D-'), 'D4-E')
                Y.REMARKS = EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)
                Y.CURRENCY = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrencyOne)
                OUTPUT.STR<-1> = Y.REF.CODE :"|": Y.BANK.CODE :"|": Y.BR.CODE :"|": Y.ACC.NUM :"|": Y.TRAN.NUM :"|": Y.DIS.CODE :"|": Y.CR.AMT :"|": Y.DR.AMT :"|": Y.DEP.SLIP :"|": Y.TRAN.DATE :"|": Y.REMARKS :"|": Y.CURRENCY
                !OPEN 'PRAN.SMS' TO F.WRITEREC ELSE NULL
                W.M.FILE.NAME = Y.TELLER.ID:'.':Y.ACC.NUM:'.':Y.DATE:'.R.txt'
                WRITE OUTPUT.STR ON F.WRITEREC,W.M.FILE.NAME ON ERROR
                    CRT 'UNABLE TO WRITE'
                END
            END
        END
    END
END
