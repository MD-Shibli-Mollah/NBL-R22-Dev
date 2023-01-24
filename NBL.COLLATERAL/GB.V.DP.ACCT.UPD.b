* @ValidationCode : MjotMTAzNDgzMDQxMjpDcDEyNTI6MTY3Mzg2Nzg5OTk2OTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 16 Jan 2023 17:18:19
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.DP.ACCT.UPD

*-------------------------------------------------------------------------
*<Description>
* Update COLLATERAL local field value with CUSTOMER DATA
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************
    $INCLUDE I_COMMON
    $INCLUDE I_EQUATE
*    $INCLUDE I_F.COLLATERAL
    $USING CO.Contract
*    $INCLUDE I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INCLUDE I_F.CUSTOMER
    $USING ST.Customer
    $USING EB.Updates
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.Display

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB GET.LOC.MULTI.REF
    GOSUB PROCESS
RETURN


INITIALISE:
***********
    Y.ACC.ID = ''
    R.ACCOUNT.ERROR = ''
    R.CUST.ERROR = ''

RETURN


OPEN.FILES:
************
    FN.ACCOUNT = 'F.ACCOUNT';F.ACCOUNT = '';R.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.CUSTOMER = 'F.CUSTOMER';F.CUSTOMER = '';R.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

RETURN

GET.LOC.MULTI.REF:
******************
    Y.APPLICATION       = "COLLATERAL"
    FIELD.NAME          = "ACCT.NO":@VM:"BORROWER":@VM:"CUS.ADDRESS":@VM:"PHONE.NUMBER"
    FIELD.POS           = ""
    CALL MULTI.GET.LOC.REF(Y.APPLICATION,FIELD.NAME,FIELD.POS)
    ACCT.NO.POS  = FIELD.POS<1,1>
    BORROWER.POS = FIELD.POS<1,2>
    CUS.ADDRESS.POS  = FIELD.POS<1,3>
    PHONE.NUMBER.POS = FIELD.POS<1,4>

RETURN

PROCESS:
********

*    Y.ACC.ID = COMI
    Y.ACC.ID = EB.SystemTables.getComi()
*    Y.CUST.ID = FIELD(ID.NEW,'.',1)
    Y.CUST.ID = FIELD(EB.SystemTables.getIdNew(),'.',1)

    EB.DataAccess.FRead(FN.ACCOUNT,Y.ACC.ID,R.ACCOUNT,F.ACCOUNT,R.ACCOUNT.ERROR)
    EB.DataAccess.FRead(FN.CUSTOMER,Y.CUST.ID,R.CUSTOMER,F.CUSTOMER,R.CUST.ERROR)
    Y.ACCOUNT.TITLE.1 = R.ACCOUNT<AC.AccountOpening.Account.AccountTitleOne>
    Y.CUS.ADDR = R.CUSTOMER<ST.Customer.Customer.EbCusStreet,1>:",":R.CUSTOMER<ST.Customer.Customer.EbCusTownCountry,1>:",":R.CUSTOMER<ST.Customer.Customer.EbCusPostCode,1>
    Y.CUS.PHONE = R.CUSTOMER<ST.Customer.Customer.EbCusOffPhone>
    
*    R.NEW(COLL.LOCAL.REF)<1,BORROWER.POS>  = R.ACCOUNT<AC.ACCOUNT.TITLE.1>
*    R.NEW(COLL.LOCAL.REF)<1,CUS.ADDRESS.POS>  =  R.CUSTOMER<EB.CUS.STREET,1>:",":R.CUSTOMER<EB.CUS.TOWN.COUNTRY,1>:",":R.CUSTOMER<EB.CUS.POST.CODE,1>
*    R.NEW(COLL.LOCAL.REF)<1,PHONE.NUMBER.POS>  = R.CUSTOMER<EB.CUS.OFF.PHONE>
*    CALL REBUILD.SCREEN
    Y.TEMP = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
    Y.TEMP<1,BORROWER.POS> = Y.ACCOUNT.TITLE.1
    Y.TEMP<1,CUS.ADDRESS.POS> = Y.CUS.ADDR
    Y.TEMP<1,PHONE.NUMBER.POS> = Y.CUS.PHONE
    EB.SystemTables.setRNew(CO.Contract.Collateral.CollLocalRef, Y.TEMP)
    EB.Display.RebuildScreen()
    
RETURN
END
