* @ValidationCode : MjotMTQyMjc0OTYyNjpDcDEyNTI6MTY3Mzk0NTM5MTIwNjp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Jan 2023 14:49:51
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.CUS.CTR.INCORP.NUM
*-------------------------------------------------------------------------
*<Description>
* Incorporation Number, Incorporation Date, Incorporation District, Incorporation Country is Mandatory
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_F.CUSTOMER
    $USING ST.Customer
    $USING EB.Updates
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

    FN.CUS = 'F.CUSTOMER'
    F.CUS=''
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)
    
    IF Y.TEMP<1,84> EQ 'Public Limited' OR Y.TEMP<1,84> EQ 'Private Limited' OR Y.TEMP<1,84> EQ 'Private' THEN
        IF Y.TEMP<1,89> EQ '' OR Y.TEMP<1,85> EQ '' OR Y.TEMP<1,90> EQ '' OR Y.TEMP<1,91> EQ '' THEN
*            ETEXT='Incorporation Number, Incorporation Date, Incorporation District, Incorporation Country is Mandatory'
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext('Incorporation Number, Incorporation Date, Incorporation District, Incorporation Country is Mandatory')
            EB.ErrorProcessing.StoreEndError()
        END
    END ELSE
    
        Y.TEMP<1,89> = 'Unregistered'
        EB.SystemTables.setRNew(ST.Customer.Customer.EbCusLocalRef, Y.TEMP)
    END
    Y.TEMP<1,78>='Corporate'
    EB.SystemTables.setRNew(ST.Customer.Customer.EbCusLocalRef, Y.TEMP)
RETURN
END