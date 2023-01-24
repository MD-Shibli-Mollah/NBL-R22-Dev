* @ValidationCode : MjotNjE2MDM2MTUwOkNwMTI1MjoxNjczOTM2MDM3MTI3OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 17 Jan 2023 12:13:57
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.I.CUS.OWNER.CODE
*-------------------------------------------------------------------------
*<Description>
* if married then spouse name, mobile number must be digit, Ownership type mandatory, Post code digit only, Business post code digit only

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
*    Y.WC = Y.TEMP<1,WC.POS>
*    IF NOT Y.WC THEN
*        !        ETEXT='Please Input Ownership Type Under ADDRESS & OWNERSHIP tab' --- BANK Comment
*        !       CALL STORE.END.ERROR
*    END

    Y.CUS.SMS.1 = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusSmsOne)
    IF NOT(ISDIGIT(Y.CUS.SMS.1)) THEN
*        ETEXT='Please input only digit in Mobile Number field'
*        CALL STORE.END.ERROR
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
    Y.MARITAL.STATUS = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusMaritalStatus)
    IF Y.MARITAL.STATUS EQ 'MARRIED' AND Y.TEMP<1,5> EQ '' THEN
*        ETEXT='Please input Spouse Name'
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext('Please input Spouse Name')
        EB.ErrorProcessing.StoreEndError()
    END
RETURN
END
