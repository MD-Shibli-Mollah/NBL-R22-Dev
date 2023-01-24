* @ValidationCode : MjotMTY3ODY0Mzc1MjpDcDEyNTI6MTY3MzkzNTE1MjIxNTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Jan 2023 11:59:12
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.CUS.CHECK
*-----------------------------------------------------------------------------
* <Rating>100</Rating>
*-----------------------------------------------------------------------------
* Author: Kishor Kumar Saha
* Description : This Routine will invoke at the time of INPUT
* Create DATE : 19th September 2019

* Modified by : MD SHIBLI MOLLAH
* Modification DATE : 17th JAN 2023
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    
    $USING ST.Customer
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
*    $USING EB.Display
    
    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''

    EB.DataAccess.Opf(FN.CUS,F.CUS)
    
    Y.VFUNCTION = EB.SystemTables.getVFunction()
    IF Y.VFUNCTION EQ 'I' THEN
        Y.CUSTOMER = EB.SystemTables.getComi()
        
        EB.DataAccess.FRead(FN.CUS,Y.CUSTOMER,R.CUS,F.CUS,CUS.ERR)
        Y.SHORT.NAME = R.CUS<ST.Customer.Customer.EbCusShortName>
        !DEBUG
        IF LEN(Y.CUSTOMER) EQ 9 THEN
            IF Y.SHORT.NAME EQ '' THEN
*                TEXT = "Its not a valid Customer ID"
*                CALL REM
                EB.SystemTables.setEtext("Its not a valid Customer ID")
                EB.ErrorProcessing.StoreEndError()
            END
            ELSE IF Y.SHORT.NAME NE '' THEN
*                TEXT = "Name of Customer :" : Y.SHORT.NAME
*                CALL REM
                EB.SystemTables.setEtext( "Name of Customer :" : Y.SHORT.NAME)
                EB.ErrorProcessing.StoreEndError()
            END
        END
        ELSE
*            TEXT = "Its not a valid Customer ID"
*            CALL REM
            EB.SystemTables.setEtext("Its not a valid Customer ID")
            EB.ErrorProcessing.StoreEndError()
        END

*******--------------------------TRACER------------------------------------------------------------------------------
*    WriteData = ' Y.SHORT.NAME: ':Y.SHORT.NAME
*    FileName = 'SHIBLI_NBL_CUS_TEST.txt'
*    FilePath = 'DL.BP'
*    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
*    ELSE
*        CREATE FileOutput ELSE
*        END
*    END
*    WRITESEQ WriteData APPEND TO FileOutput ELSE
*        CLOSESEQ FileOutput
*    END
*    CLOSESEQ FileOutput

* *******--------------------------TRACER---------------------------------------------------------*********************
    END