* @ValidationCode : MjotNDU1ODk3NjA3OkNwMTI1MjoxNjcyODk4NDE2MzE5OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Jan 2023 12:00:16
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.TT.MAX.TRX.CHECK
*-----------------------------------------------------------------------------
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
*    $INSERT I_F.USER
*    $INSERT T24.BP I_F.TELLER.ID
    $USING TT.Contract
    $USING EB.SystemTables
    $USING EB.Security
    $USING EB.DataAccess
    $USING EB.Foundation
    $USING EB.ErrorProcessing
    
    Y.ID.COMPANY = EB.SystemTables.getIdCompany()

    IF Y.ID.COMPANY NE 'BD0011004' THEN RETURN

    FN.TELLER='F.TELLER'
    F.TELLER=''
    
    FN.TELLER.ID='F.TELLER.ID'
    F.TELLER.ID=''
    
    FN.TT.NAU='F.TELLER$NAU'
    F.TT.NAU=''
    
    
    EB.DataAccess.Opf(FN.TELLER,F.TELLER)
    EB.DataAccess.Opf(FN.TELLER.ID, F.TELLER.ID)
    EB.DataAccess.Opf(FN.TT.NAU, F.TT.NAU)

    Y.VFUNCTION = EB.SystemTables.getVFunction()

    IF Y.VFUNCTION NE "I" THEN RETURN
    Y.OPERATOR = EB.SystemTables.getOperator()
    Y.USER = Y.OPERATOR

    SEL.CMD = "SELECT F.TELLER.ID WITH K.USER EQ '" : Y.USER : "'"
    EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR)

    Y.TELLER.ID = FIELD(SEL.LIST,@FM,1)

    EB.DataAccess.FRead(FN.TELLER.ID,Y.TELLER.ID,R.TI,F.TELLER.ID,E.TI)

*    CALL GET.LOC.REF("TELLER.ID","TILL.MAX.TXN.NO",FIELD.POS.NO)
*    CALL GET.LOC.REF("TELLER.ID","TILL.MAX.TXN.AMT",FIELD.POS.AMT)

    FLD.POS = ""
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "TILL.MAX.TXN.NO":@VM:"TILL.MAX.TXN.AMT"
    EB.Foundation.MapLocalFields("TELLER.ID", LOCAL.FIELDS, FLD.POS)
    Y.MAX.TXN.NO.POS= FLD.POS<1,1>
    Y.MAX.TXN.AMT.POS = FLD.POS<1,2>

    Y.TOTAL.LT = R.TI<TT.Contract.Teller.TeLocalRef>
    
    Y.MAX.TXN.NO = Y.TOTAL.LT<1,Y.MAX.TXN.NO.POS>
    Y.MAX.TXN.AMT = Y.TOTAL.LT<1.Y.MAX.TXN.AMT.POS>

*    Y.MAX.TXN.NO = FIELD(Y.LOC.REF,@VM,1)
*    Y.MAX.TXN.AMT = FIELD(Y.LOC.REF,@VM,2)

RETURN

IF Y.ID.COMPANY NE 'BD0011104' THEN RETURN

CNT = 0

SEL.CMD = "SELECT F.TELLER WITH TELLER.ID.1 EQ '" : Y.TELLER.ID : "'"
EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR)
LOOP
    REMOVE TELLER.ID FROM SEL.LIST SETTING TELLER.POS
WHILE TELLER.ID:TELLER.POS
    EB.DataAccess.FRead(FN.TELLER,TELLER.ID,R.TT,F.TELLER,E.TELLER)
    Y.AMT += ABS(R.TT<TT.Contract.Teller.TeAmountLocalOne>)

    CNT++


REPEAT
SEL.CMD = "SELECT FBNK.TELLER$NAU WITH TELLER.ID.1 EQ '" : Y.TELLER.ID : "'"
EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR)
LOOP
    REMOVE TELLER.ID FROM SEL.LIST SETTING TELLER.POS
WHILE TELLER.ID:TELLER.POS
    EB.DataAccess.FRead(FN.TT.NAU,TELLER.ID,R.TT.NAU,F.TT.NAU,E.TT.NAU)
    Y.AMT += ABS(R.TT.NAU<TT.Contract.Teller.TeAmountLocalOne>)

    CNT++


REPEAT

Y.AMT += ABS(EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne))
CNT++

IF Y.AMT GT Y.MAX.TXN.AMT  THEN
*    ETEXT  = 'TELLER MAX TRANSACTION AMOUNT EXCEEDED.'
*    CALL STORE.END.ERROR
    EB.SystemTables.setEtext('TELLER MAX TRANSACTION AMOUNT EXCEEDED.')
    EB.ErrorProcessing.StoreEndError()

END
IF  CNT GT Y.MAX.TXN.NO THEN
*    ETEXT  = 'TELLER MAX TRANSACTION NUMBER EXCEEDED.'
*    CALL STORE.END.ERROR
    EB.SystemTables.setEtext('TELLER MAX TRANSACTION NUMBER EXCEEDED.')
    EB.ErrorProcessing.StoreEndError()

END

RETURN

END
