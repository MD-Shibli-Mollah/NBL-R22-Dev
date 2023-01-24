* @ValidationCode : MjoxNTE5NzE4Njg5OkNwMTI1MjoxNjczOTQzOTM4NzIzOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 17 Jan 2023 14:25:38
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.CUS.VAL.WED.DATE
**************************************************************************
* Subroutine Description:
*------------------------
* This validation routine is used to make spouse name as mandatory when wedding
* is entered.
*
*-----------------------------------------------------------------------------
*Common Variable Description
*------  ------- -----------
*
* Variable name:   Insert file:          Description:
* R.NEW            I_COMMON              Holds the current record values
* ETEXT            I_COMMON              Error Varable in the cross validations
* AF               I_COMMON              Current Field number at the field validation
* AV               I_COMMON              Current Value number at the field validation
* COMI             I_COMMON              Holds the current inputted field value

* Modification History:
* ---------------------
* 07/02/2011 -                               New -    Naveen Kumar BN
*                                                     Thesys Technologies
*------------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - MD Shibli Mollah - FDS
*-------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING ST.Customer
    $USING EB.Updates
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

    Y.MESSAGE = EB.SystemTables.getMessage()
    IF Y.MESSAGE EQ "VAL" THEN RETURN

    GOSUB INITIALISE
    GOSUB GET.LOC.REF
    GOSUB PROCESS

RETURN
*----------
INITIALISE:
*----------

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    EB.DataAccess.Opf(FN.CUSTOMER,F.CUSTOMER)

RETURN
*-------------
GET.LOC.REF:
*------------

    Y.APPLICATION = "CUSTOMER"
    FIELD.NAME = "LT.WED.ANIV.DT":@VM:"LT.SPOUSE.NAME"
    FIELD.POS = ""

    EB.Updates.MultiGetLocRef(Y.APPLICATION,FIELD.NAME,FIELD.POS)
    WEDNG.ANIV.DATE.POS = FIELD.POS<1,1>
    SPOUSE.NAME.POS = FIELD.POS<1,2>

RETURN
*--------
PROCESS:
*--------

    Y.WED.DATE = EB.SystemTables.getComi()
    IF Y.WED.DATE THEN
        Y.SP.NAME = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,SPOUSE.NAME.POS>
        
        IF NOT(Y.SP.NAME) THEN
            EB.SystemTables.setEtext('Spouse Name is Mandatory')
*            EB.SystemTables.getAf(ST.Customer.Customer.EbCusLocalRef)
*            EB.SystemTables.getAv(WEDNG.ANIV.DATE.POS)
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN
*----------------------------------------------------------------
END