* @ValidationCode : MjoxNzAyODgwMzM1OkNwMTI1MjoxNjczNTEyNzQ0Nzg4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Jan 2023 14:39:04
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.FIELD.DEFAULT
*-----------------------------------------------------------------------------
* <Rating>-50</Rating>
*-----------------------------------------------------------------------------
* Attached to: FUNDS.TRANSFER,BD.ACTR.FTHP
**
* Description: Check PL ACC - 'Invalid Account'

* Change Request/Development Ref:
*
* Change Request:
*
*-------------------------------------------------------------------------
* Modification Overview:
* ----------------------
* 08/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
*        $INSERT I_F.ACCOUNT
*        $INSERT I_F.TELLER
*        $INSERT I_F.FUNDS.TRANSFER
*        $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $USING FT.Contract
    $USING TT.Contract
    $USING AC.AccountOpening
    $USING TT.TellerFinancialService
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Updates
    $USING EB.Display


        
    Y.APPLICATION = EB.SystemTables.getApplication()
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
    Y.TODAY = EB.SystemTables.getToday()

    GOSUB OPN.FILES
    GOSUB INITIALISE
    GOSUB PROCESS
        
RETURN

*

OPN.FILES:
*

    FN.ACCOUNT = 'F.ACCOUNT' ; F.ACCOUNT = ''
        
    EB.DataAccess.Opf(FN.ACCOUNT,F.ACCOUNT)

    FN.TELLER = 'F.TELLER' ; F.TELLER = ''
    EB.DataAccess.Opf(FN.TELLER,F.TELLER)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER' ; F.FUNDS.TRANSFER = ''
    EB.DataAccess.Opf(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.TELLER.FINANCIAL.SERVICES = 'F.TELLER.FINANCIAL.SERVICES' ; F.TELLER.FINANCIAL.SERVICES = ''
    EB.DataAccess.Opf(FN.TELLER.FINANCIAL.SERVICES,F.TELLER.FINANCIAL.SERVICES)

RETURN

*
INITIALISE:
*
    Y.APP = Y.APPLICATION:@FM:'ACCOUNT'
    Y.FLD = 'MODE.OF.OPER':@VM:'OPERATION.MODE':@FM:'MODE.OF.OPER':@VM:'OPERATION.MODE'
    Y.POS = ""

* CALL MULTI.GET.LOC.REF(Y.APP,Y.FLD,Y.POS)
    EB.Updates.MultiGetLocRef(Y.APP,Y.FLD,Y.POS)
    Y.MOD.OF.OPR.POS = Y.POS<1,1>
    Y.OP.MOD.POS = Y.POS<1,2>
    Y.MOP.POS = Y.POS<2,1>
    Y.OPM.POS = Y.POS<2,2>

    Y.AC.ID = EB.SystemTables.getComi()
        
    EB.DataAccess.FRead(FN.ACCOUNT, Y.AC.ID, R.ACCOUNT, F.ACCOUNT, ACC.ERR)
*        Y.MOP = R.ACCOUNT<AC.LOCAL.REF,Y.MOP.POS>
    Y.TEMP = R.ACCOUNT<AC.AccountOpening.Account.LocalRef>
    Y.MOP = Y.TEMP<1,Y.MOP.POS>
*        Y.OPM = R.ACCOUNT<AC.LOCAL.REF,Y.OPM.POS>
    Y.OPM =  Y.TEMP<1,Y.OPM.POS>

    IF Y.MOP NE 'Refer Text Box' THEN
* ------------------ NEED CLARIFICATION----------  SHIBLI -- TAFJ
* T.LOCREF<Y.OP.MOD.POS,7> = 'NOINPUT'
        
    END
RETURN
*
PROCESS:
*
    IF Y.PGM.VERSION EQ ',RD.MISSING.WITHOUT.PENALTY' OR Y.PGM.VERSION EQ ',RD.MISSING' THEN
* R.NEW(FT.PROCESSING.DATE) = TODAY

        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.ProcessingDate, Y.TODAY)
    END

    IF Y.APPLICATION EQ 'TELLER' THEN
        !S
        IF R.ACCOUNT THEN
            !E
*                R.NEW(TT.TE.LOCAL.REF)<1,Y.MOD.OF.OPR.POS> = Y.MOP
*                R.NEW(TT.TE.LOCAL.REF)<1,Y.OP.MOD.POS> = Y.OPM
*                CALL REFRESH.FIELD(TT.TE.LOCAL.REF<1,Y.MOD.OF.OPR.POS>,'')
*                CALL REFRESH.FIELD(TT.TE.LOCAL.REF<1,Y.OP.MOD.POS>,'')

            Y.TEMP.TT = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
            Y.TEMP.TT<1,Y.MOD.OF.OPR.POS> = Y.MOP
            Y.TEMP.TT<1,Y.OP.MOD.POS> = Y.OPM
            Y.TEMP.TT<1,Y.MOD.OF.OPR.POS> = ''
            Y.TEMP.TT<1,Y.OP.MOD.POS> = ''
            EB.SystemTables.setRNew(TT.Contract.Teller.TeLocalRef, Y.TEMP.TT)
*
            !S
        END ELSE
            IF Y.AC.ID[1,2] EQ 'PL' THEN
*                E = 'Invalid Account'
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext('Invalid Account')
                EB.ErrorProcessing.StoreEndError()
                RETURN
            END
        END
        !E
    END
    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
*        R.NEW(FT.LOCAL.REF)<1,Y.MOD.OF.OPR.POS> = Y.MOP
*        R.NEW(FT.LOCAL.REF)<1,Y.OP.MOD.POS> = Y.OPM
*        CALL REFRESH.FIELD(FT.LOCAL.REF<1,Y.MOD.OF.OPR.POS>,'')
*        CALL REFRESH.FIELD(FT.LOCAL.REF<1,Y.OP.MOD.POS>,'')

        Y.TEMP.FT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
        Y.TEMP.FT<1,Y.MOD.OF.OPR.POS> = Y.MOP
        Y.TEMP.FT<1,Y.OP.MOD.POS> = Y.OPM
        Y.TEMP.FT<1,Y.MOD.OF.OPR.POS> = ''
        Y.TEMP.FT<1,Y.OP.MOD.POS> = ''
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.LocalRef, Y.TEMP.FT)
    END
    IF Y.APPLICATION EQ 'TELLER.FINANCIAL.SERVICES' THEN
*        R.NEW(TFS.LOCAL.REF)<1,Y.MOD.OF.OPR.POS> = Y.MOP
*        R.NEW(TFS.LOCAL.REF)<1,Y.OP.MOD.POS> = Y.OPM
*        CALL REFRESH.FIELD(TFS.LOCAL.REF<1,Y.MOD.OF.OPR.POS>,'')
*        CALL REFRESH.FIELD(TFS.LOCAL.REF<1,Y.OP.MOD.POS>,'')
        Y.TEMP.TFS = EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsLocalRef)
        Y.TEMP.TFS<1,Y.MOD.OF.OPR.POS> = Y.MOP
        Y.TEMP.TFS<1,Y.OP.MOD.POS> = Y.OPM
        Y.TEMP.TFS<1,Y.MOD.OF.OPR.POS> = ''
        Y.TEMP.TFS<1,Y.OP.MOD.POS> = ''
        EB.SystemTables.setRNew(TT.TellerFinancialService.TellerFinancialServices.TfsLocalRef, Y.TEMP.TFS)
    END
* CALL REBUILD.SCREEN
    EB.Display.RebuildScreen()
RETURN
END