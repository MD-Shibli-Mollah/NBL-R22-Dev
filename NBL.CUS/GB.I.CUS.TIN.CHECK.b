* @ValidationCode : MjoxNzcwNzQ2NTQ5OkNwMTI1MjoxNjczOTM4MTc1MjYzOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 17 Jan 2023 12:49:35
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.CUS.TIN.CHECK

*-------------------------------------------------------------------------
*<Description>
* Subroutine developed for Customer version to check the local field TIN.GIVEN
* if YES then LEGAL.DOC.NAME field will become mandatory. Also depending on the
* input version the local filed BD.CUST.TYPE will become Individual or Corporate
* Developed by Partha, NBL ITD on December 2016 mainly for CTR reporting correction
*-------------------------------------------------------------------------------
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - MD SHIBLI MOLLAH - FDS
*-------------------------------------------------------------------------
**************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE

    $USING ST.Customer
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.LocalReferences

    ! DEBUG
    EB.LocalReferences.GetLocRef('CUSTOMER','TIN.GIVEN',POS)
    EB.LocalReferences.GetLocRef('CUSTOMER','BD.CUST.TYPE',CUS.POS)

    EB.LocalReferences.GetLocRef('CUSTOMER','RETURNSUBMITTED',CUS.RS)
    EB.LocalReferences.GetLocRef('CUSTOMER','NATOFDEPOSITOR',CUS.NOD)
    EB.LocalReferences.GetLocRef('CUSTOMER','ASSESSMENT.YEAR',CUS.AY)
    EB.LocalReferences.GetLocRef('CUSTOMER','RET.SUBMIT.DATE',CUS.RSD)
    EB.LocalReferences.GetLocRef('CUSTOMER','RETSUBDATEBANK',CUS.RSDB)
    EB.LocalReferences.GetLocRef('CUSTOMER','TAX.RETURN.NO',CUS.TRN)
    EB.LocalReferences.GetLocRef('CUSTOMER','RECFUNDAPPAUTH',CUS.RFAA)
    EB.LocalReferences.GetLocRef('CUSTOMER','REASONRETNOTMAN',CUS.RRNM)
    EB.LocalReferences.GetLocRef('CUSTOMER','RECFUNDAPPROVDT',CUS.RFAD)
    
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)

    Y.NATURE.OF.DEPOSITOR = Y.TEMP<1,CUS.NOD>
    Y.ASSESSMENT.YEAR = Y.TEMP<1,CUS.AY>
    Y.RETURN.SUBMIT.DATE = Y.TEMP<1,CUS.RSD>
    Y.RETURN.SUBMIT.DATE.BANK = Y.TEMP<1,CUS.RSDB>
    Y.TAX.RETURN.NO = Y.TEMP<1,CUS.TRN>
    Y.RECONGNIZED.FUND.APPROVAL.AUTH= Y.TEMP<1,CUS.RFAA>
    Y.REASON.RETURN.NOT.MANDATORY = Y.TEMP<1,CUS.RRNM>
    Y.RECONGNIZED.FUND.APPROVAL.DATE= Y.TEMP<1,CUS.RFAD>
    Y.RETURN.SUBMITTED = Y.TEMP<1,CUS.RS>

    IF Y.RETURN.SUBMITTED EQ "" THEN
*        ETEXT = "RETURN SUBMISSION MUST BE YES/NO/RETURN NOT MANDATORY."
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("RETURN SUBMISSION MUST BE YES/NO/RETURN NOT MANDATORY.")
        EB.ErrorProcessing.StoreEndError()
    END
    
    IF Y.NATURE.OF.DEPOSITOR EQ "" THEN
*        ETEXT = "NATURE OF DEPOSITOR IS MANDATORY"
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("NATURE OF DEPOSITOR IS MANDATORY")
        EB.ErrorProcessing.StoreEndError()
    END

    IF Y.NATURE.OF.DEPOSITOR EQ 'RECOGNIZED FUND' THEN
        IF Y.RECONGNIZED.FUND.APPROVAL.AUTH EQ '' OR Y.RECONGNIZED.FUND.APPROVAL.DATE EQ '' THEN
*            AF = 178
*            AV = 116
*            ETEXT = "IF RECONGNIZED FUND THEN APPROVAL AUTHORITY AND APPROVAL DATE OF RECOGNIZED FUND IS MANDATORY"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("IF RECONGNIZED FUND THEN APPROVAL AUTHORITY AND APPROVAL DATE OF RECOGNIZED FUND IS MANDATORY")
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.RETURN.SUBMITTED NE "RETURN NOT MANDATORY" THEN
*            AF = 178
*            AV = CUS.RS
*            ETEXT = "IF RETURN IS NOT MANDATORY REUTRN SUBMITTED FIELD SHOULD BE RETURN NOT MANDATORY"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("IF RETURN IS NOT MANDATORY REUTRN SUBMITTED FIELD SHOULD BE RETURN NOT MANDATORY")
            EB.ErrorProcessing.StoreEndError()
        END
    
    END
    IF Y.NATURE.OF.DEPOSITOR EQ 'RETURN NOT MANDATORY' THEN
        IF Y.REASON.RETURN.NOT.MANDATORY EQ '' THEN
*            AF = 178
*            AV = 117
*            ETEXT = "IF RETURN IS NOT MANDATORY THEN REASON OF THAT IS MANDATORY"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("IF RETURN IS NOT MANDATORY THEN REASON OF THAT IS MANDATORY")
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.RETURN.SUBMITTED NE "RETURN NOT MANDATORY" THEN
*            AF = 178
*            AV = CUS.RS
*            ETEXT = "IF RETURN IS NOT MANDATORY REUTRN SUBMITTED FIELD SHOULD BE RETURN NOT MANDATORY"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("IF RETURN IS NOT MANDATORY REUTRN SUBMITTED FIELD SHOULD BE RETURN NOT MANDATORY")
            EB.ErrorProcessing.StoreEndError()
        END
    END
    
    !IF Y.RETURN.SUBMITTED EQ "YES" THEN
    !   IF Y.RETURN.SUBMIT.DATE EQ "" OR Y.RETURN.SUBMIT.DATE.BANK EQ "" OR Y.TAX.RETURN.NO EQ "" THEN
    !      AF = 178
    !     AV = 113
    !    ETEXT = "IF RETURN IS SUBMITTED THEN SUBMISSION DATES AND RETURN NUMBER IS MANDATORY"
    !   CALL STORE.END.ERROR
    !END
    !END

    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
    IF Y.PGM.VERSION EQ ',BD.INPUT' THEN
        Y.TEMP<1,CUS.POS>='Individual'
        EB.SystemTables.setRNew(ST.Customer.Customer.EbCusLocalRef, Y.TEMP)
    END ELSE
        IF Y.PGM.VERSION EQ ',BD.CORP' THEN
            Y.TEMP<1,CUS.POS>='Corporate'
            EB.SystemTables.setRNew(ST.Customer.Customer.EbCusLocalRef, Y.TEMP)
        END

    END

    Y.TIN.GVN=Y.TEMP<1,POS>
    IF Y.TIN.GVN = 'YES' THEN
* Y.LEG.ID=R.NEW(EB.CUS.LEGAL.DOC.NAME)
        Y.LEG.ID = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLegalDocName)
    
        Y.LEG.LST = CHANGE(Y.LEG.ID,@VM,@FM)
        LOCATE 'TIN' IN Y.LEG.LST SETTING POS1 ELSE
*            ETEXT='TIN.GIVEN is YES put TIN in ID DOC'
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("TIN.GIVEN is YES put TIN in ID DOC")
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN
END