* @ValidationCode : MjotMTQxNDY3NjQyMjpDcDEyNTI6MTY3Mzg1NTgyNTc5OTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 16 Jan 2023 13:57:05
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.PS.CHQ.OVRRD.CHK
*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
* Convert the following overrides to error as per the Business Team requirement and this development based on the core *override generation:
* Override: Cheque not in register

*-------------------------------------------------------------------------
*<Description of the arguments>
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INSERT I_F.PAYMENT.STOP
    $USING CQ.ChqPaymentStop
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Updates


* EB.SystemTables.getV()               -----TAFJ --- Shibli
*    Y.OVERRIDE.VAL = R.NEW(V-9)
    Y.OVERRIDE.VAL = EB.SystemTables.getRNew(EB.SystemTables.getV() - 9)
    Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL, @VM)


    FOR I=1 TO Y.OVRRD.NO
        Y.OVRRD.DETLS = FIELD(Y.OVERRIDE.VAL,@VM,I)
        Y.OVRRD.ID = FIELD(Y.OVRRD.DETLS,'}',1)


        IF (Y.OVRRD.ID='CHQ.NOT.IN.REG') THEN
            GOSUB CHQ.NOT.IN.REG
        END

        IF (Y.OVRRD.ID='AC.CHQ.NO.NOT.IN.REGISTER') THEN
            GOSUB CHQ.NOT.IN.REG
        END

        IF (Y.OVRRD.ID='AC.ACC.NO.NOT.EXIST.IN.CHQ.REGISTER') THEN
            GOSUB CHQ.NOT.IN.REG
        END

    NEXT I

RETURN

CHQ.NOT.IN.REG:

* AF = I
* EB.SystemTables.getAf()
   
*    ETEXT = 'CHEQUE NUMBER NOT IN REG'
*    CALL STORE.END.ERROR
    EB.SystemTables.setEtext('CHEQUE NUMBER NOT IN REG')
    EB.ErrorProcessing.StoreEndError()
RETURN

END