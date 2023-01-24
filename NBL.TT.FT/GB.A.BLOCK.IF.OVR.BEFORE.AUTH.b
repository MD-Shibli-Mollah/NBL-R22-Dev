* @ValidationCode : MjotMTg3NDA3NDgwMzpDcDEyNTI6MTY3NDU1MTQ5OTE5NTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 24 Jan 2023 15:11:39
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.A.BLOCK.IF.OVR.BEFORE.AUTH
    
*-----------------------------------------------------------------------------
* <Rating>100</Rating>
*-----------------------------------------------------------------------------

*Subroutine Description:
*-----------------------
    !If any override occur, this routine will check whether the user
    !has the specific permission to accept the override or not. If not,
    !this routine will be thorugh an error instead of goes to the INAO
    !status from INAU status
    !Written by: Abu Sayed Date: 20140720
    !-----------------------------------------------------------------------------
**************************************************************************
* Modification History:
* ----------------------
* 12/01/2023        - Retrofit     - MD Shibli Mollah - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    
* $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
* $INSERT I_F.USER
    $USING EB.Security
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Foundation
    $USING EB.Updates

    FN.FT='F.FUNDS.TRANSFER$NAU'
    F.FT=''
    FN.USR='F.USER'
    F.USR=''

    EB.DataAccess.Opf(FN.FT,F.FT)
    EB.DataAccess.Opf(FN.USR,F.USR)

* Y.OVR.MSG=R.NEW(FT.OVERRIDE)
    Y.OVR.MSG = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Override)
    IF Y.OVR.MSG THEN
        Y.VC = DCOUNT(Y.OVR.MSG, @VM)
        LOOP WHILE Y.VC > 0 DO
            Y.CLASS=Y.OVR.MSG<1,Y.VC,2>
            IF Y.CLASS NE '' THEN BREAK
            Y.VC=Y.VC-1
        REPEAT
    END ELSE
        RETURN
    END

    ERROR.FLAG = 'Y'
    IF Y.CLASS THEN
        Y.USR = EB.SystemTables.getOperator()
        
        EB.DataAccess.FRead(FN.USER,Y.USR,R.USR,F.USR,USR.ERR)
* Y.OVR.CLASS=R.USR<EB.USE.OVERRIDE.CLASS>
        Y.OVR.CLASS = R.USR<EB.Security.User.UseOverrideClass>
        
        Y.NO.CLS = DCOUNT(Y.OVR.CLASS, @VM)
        LOOP WHILE Y.NO.CLS >0 DO
            Y.USR.CLS=Y.OVR.CLASS<1,Y.NO.CLS>
            IF Y.USR.CLS EQ Y.CLASS THEN
                ERROR.FLAG='N'
                BREAK
            END
            Y.NO.CLS=Y.NO.CLS-1
        REPEAT

        IF ERROR.FLAG EQ 'Y' THEN
*            ETEXT='Please Contact With Your Manager For Authorized The Record..........!!!'
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext('Please Contact With Your Manager For Authorized The Record..........!!!')
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN
END
