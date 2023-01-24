* @ValidationCode : Mjo5MDEzODk0MDpDcDEyNTI6MTY3MzkzNjUzODgwMTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Jan 2023 12:22:18
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.CUS.OWNER.CODE.CORP
*-------------------------------------------------------------------------
*<Description>
* mobile number must be digit, Post code digit only, Business post code digit only

*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT I_F.CUSTOMER
    $USING ST.Customer
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.LocalReferences

    EB.LocalReferences.GetLocRef('CUSTOMER','OWNER.CODE',WC.POS)
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)

    Y.CUS.SMS.1 = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusSmsOne)
    IF NOT(ISDIGIT(Y.CUS.SMS.1)) THEN
        EB.SystemTables.setEtext('Please input only digit in Mobile Number field')
        EB.ErrorProcessing.StoreEndError()
    END
    
    Y.POST.CODE = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusPostCode)
    IF NOT(ISDIGIT(Y.POST.CODE)) THEN
*        ETEXT='Please input only digit in Registerd Post Code field'
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext('Please input only digit in Registerd Post Code field')
        EB.ErrorProcessing.StoreEndError()
    END

* Y.WC = Y.TEMP<1,WC.POS>
    IF NOT(ISDIGIT(Y.TEMP<1,99>)) AND Y.TEMP<1,99> NE '' THEN
*        ETEXT='Please input only digit in Business Post Code field'
*        CALL STORE.END.ERROR

        EB.SystemTables.setEtext('Please input only digit in Business Post Code field')
        EB.ErrorProcessing.StoreEndError()
    END
RETURN
END