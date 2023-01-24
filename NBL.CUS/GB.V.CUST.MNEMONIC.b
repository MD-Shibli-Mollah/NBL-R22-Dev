* @ValidationCode : MjotMTU2NTk3MzEwMzpDcDEyNTI6MTY3NDM4NjE2OTAyOTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 22 Jan 2023 17:16:09
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.CUST.MNEMONIC

* <Rating>100</Rating>
*-------------------------------------------------------------------------
*-------------------------------------------------------------------------
*<Description>
* SET MNEMONIC BASED ON PHONE NUMBER & Short Name[1,1]
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 22/01/2023        - Developed By    - MD Shibli Mollah- FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INCLUDE I_COMMON
    $INCLUDE I_EQUATE
*    $INCLUDE T24.BP I_F.CUSTOMER
    $USING ST.Customer
*    $INSERT T24.BP I_F.ACCOUNT
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING EB.SystemTables

    GOSUB INITIALISE
    GOSUB PROCESS

*------------
INITIALISE:
*------------

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    EB.DataAccess.Opf(FN.CUSTOMER,F.CUSTOMER)
RETURN
*------------
PROCESS:
*------------
***SET MNEMONIC------------MNEMONIC with Phone Number)--------------****************
    Y.CUS.NAME.1 = EB.SystemTables.getComi()
    Y.PHONE = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusPhoneOne)
    Y.CUS.TRIM = Y.CUS.NAME.1[1,1]
    Y.CUS.MNE = Y.CUS.TRIM : Y.PHONE
***SET MNEMONIC------------MNEMONIC with Phone Number)-------END-------****************
    EB.SystemTables.setRNew(ST.Customer.Customer.EbCusMnemonic, Y.CUS.MNE)
RETURN
END
