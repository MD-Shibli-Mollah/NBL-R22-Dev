* @ValidationCode : Mjo4NTY1MjU4NjQ6Q3AxMjUyOjE2NzM5MzA3ODQ4NjU6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 17 Jan 2023 10:46:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.VAL.INS.DATES

*-------------------------------------------------------------------------------
* This routine check the value in INS.COVER.DATE and INS.EXPIRY.DATE local fields
* and validates the record.
*-------------------------------------------------------------------------------
* Modification History :
* --------------------
* 20110210 - New - Naveen Kumar N
*-------------------------------------------------------------------------------
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 17/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************
* Common Variables :
* ----------------
* R.NEW - Holds the current record values
* ETEXT - Error Varable in the cross validations
* AF    - Current Field number at the field validation
* AV    - Current Value number at the field validation
* AS    - Current SUB value number at the field validation
* COMI  - Holds the current inputted field value and used at field validation
*-------------------------------------------------------------------------------

    $INCLUDE I_COMMON
    $INCLUDE I_EQUATE
* $INCLUDE I_F.COLLATERAL
    $USING CO.Contract
    $USING EB.Updates
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.Display
    $USING EB.ErrorProcessing
    $USING EB.Foundation
*-------------------------------------------------------------------------------
    !---------------Main Para-------------------------------------------------------
*
    GOSUB GET.LOC.MULTI.REF
    GOSUB PROCESS
RETURN
*-------------------------------------------------------------------------------
GET.LOC.MULTI.REF:
******************
    ! This Gosub is to get the positions for local reference fields used in thos code
*
    Y.APPLICATION       = "COLLATERAL"
    FIELD.NAME          = "INS.COVER.DATE":@VM:"INS.EXPIRY.DATE"
    FIELD.POS           = ""
    EB.Updates.MultiGetLocRef(Y.APPLICATION,FIELD.NAME,FIELD.POS)
*
    INS.COVER.DATE.POS  = FIELD.POS<1,1>
    INS.EXPIRY.DATE.POS = FIELD.POS<1,2>
*
RETURN
*-------------------------------------------------------------------------------
PROCESS:
********
    ! This gosub is to Check the Expiry Date and Cover Date & also to throw the error
    ! when Expiry Date is less than Cover Date.
*
    INS.EXPIRY.DATE = EB.SystemTables.getComi()
    Y.TEMP = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)
    INS.COVER.DATE = Y.TEMP<1,INS.COVER.DATE.POS>
    
    IF INS.EXPIRY.DATE NE "" AND INS.COVER.DATE NE "" THEN
        IF INS.EXPIRY.DATE LT INS.COVER.DATE THEN
*            ETEXT = "EB-DATE.MISMATCH"
*            AF    = COLL.LOCAL.REF
*            AV    = INS.EXPIRY.DATE.POS
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("EB-DATE.MISMATCH")
            EB.SystemTables.setAf(CO.Contract.Collateral.CollLocalRef)
            EB.SystemTables.setAv(INS.EXPIRY.DATE.POS)
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN
*-------------------------------------------------------------------------------
END
