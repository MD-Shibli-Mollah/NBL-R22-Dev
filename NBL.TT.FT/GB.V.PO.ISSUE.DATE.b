* @ValidationCode : MjotNDA5ODE4MDM3OkNwMTI1MjoxNjczNTE0MjMxNzAxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Jan 2023 15:03:51
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.V.PO.ISSUE.DATE
*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------

    !Issue date validation for today and next working date only
    !Version - FUNDS.TRANSFER,PR.PO.ISSUE
* Modification History :
    
* Retrofit By         : MD SHIBLI MOLLAH -- FDS
* Date                 : 08TH JAN 2023
* ----------------------------NEED to reconsider the next working date if required///
*-----------------------------------------------------------------------------
* Modification 2
*
*-----------------------------------------------------------------------------
    

    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT I_F.DATES
    $USING EB.Utility
* $INSERT I_F.FUNDS.TRANSFER
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
   
* Y.VAL.DATE=COMI
    Y.VAL.DATE = EB.SystemTables.getComi()
    Y.TODAY = EB.SystemTables.getToday()
    
*Y.DATE= R.DATES(EB.DAT.NEXT.WORKING.DAY)
* ---- EB.SystemTables.getRDates(idx)   -------! Shibli --FDS
    Y.DATE = EB.SystemTables.getRDates(EB.Utility.Dates.DatNextWorkingDay)
    
    IF Y.VAL.DATE LT Y.TODAY OR Y.VAL.DATE GT Y.DATE THEN
*        E="Value Date Can only be Today or Next Working date"
*        CALL ERR
        EB.SystemTables.setEtext('Value Date Can be only Today or Next Working date')
        EB.ErrorProcessing.StoreEndError()
    END
RETURN
END
