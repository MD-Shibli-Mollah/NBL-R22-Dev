* @ValidationCode : MjotMTI2NTA3NDMwOkNwMTI1MjoxNjc0NTUxMzY1NjY5OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 24 Jan 2023 15:09:25
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.A.TT.FT.OVERRIDE.LOGGING
    
**************************************************************************
*Subroutine Description:
*-----------------------
* PURPOSE: Write OVERRIDE Recs to FT.EB.OVVERRRIDE.LOG LOCAL TEMPLATE.
*
*-------------------------------------------------------------------------
*<Description of the arguments>
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 11/01/2023        - Retrofit     - MD Shibli Mollah - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INSERT I_F.COMPANY
    $USING ST.CompanyCreation
*    $INSERT I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT I_F.TELLER
    $USING TT.Contract
*    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $USING TT.TellerFinancialService
*    $INSERT I_F.TRANSACTION
    $USING ST.Config
    $INSERT I_F.ABL.H.AC.VIOLATION
    $INSERT I_F.EB.OVERRIDE.LOG
    $INSERT I_GTS.COMMON
    $INSERT I_F.VERSION
    $USING EB.Versions
    $INSERT I_F.ABP.H.NT.PM
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.TransactionControl
  
    Y.APPLICATION = EB.SystemTables.getApplication()
    Y.OPERATOR = EB.SystemTables.getOperator()
    Y.ID.NEW = EB.SystemTables.getIdNew()
    
    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' OR Y.APPLICATION EQ 'TELLER' OR Y.APPLICATION EQ 'TELLER.FINANCIAL.SERVICES' ELSE RETURN


    IF Y.APPLICATION EQ 'FUNDS.TRANSFER' THEN
        IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo) MATCHES '3A...' THEN RETURN
    END

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB GET.TRANS.INFO

RETURN

INITIALISE:
*==========


    Y.OVERRIDE.ID=''
    Y.OVERRIDE=''
    Y.INPUTTER=''
    Y.AUTHORISER=''
    Y.DATE.TIME=''

RETURN

OPEN.FILES:
*==========


    FN.EB.OVERRIDE.LOG = 'F.FT.EB.OVERRIDE.LOG'
    F.EB.OVERRIDE.LOG = ''
    R.EB.OVERRIDE.LOG = ''
    EB.DataAccess.Opf(FN.EB.OVERRIDE.LOG,F.EB.OVERRIDE.LOG)

RETURN

GET.TRANS.INFO:
*==============
    Y.APP= Y.APPLICATION

    BEGIN CASE
        CASE Y.APP EQ 'FUNDS.TRANSFER'
* IF EB.SystemTables.getRNew(FT.OVERRIDE) EQ '' THEN RETURN
            Y.OVERRIDE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Override)
            IF Y.OVERRIDE EQ '' THEN RETURN
    
* Y.OVERRIDE.ID =ID.NEW:'-': EB.SystemTables.getRNew(FT.CURR.NO)
            Y.CURR.NO = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CurrNo)
            Y.OVERRIDE.ID = Y.ID.NEW:'-': Y.CURR.NO
            R.EB.OVERRIDE.LOG<FT.EB.7.APPLICATION> = Y.APPLICATION
            R.EB.OVERRIDE.LOG<FT.EB.7.OVERRIDE> = Y.OVERRIDE
            R.EB.OVERRIDE.LOG<FT.EB.7.RECORD.STATUS>  = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.RecordStatus)
            R.EB.OVERRIDE.LOG<FT.EB.7.CURR.NO> = Y.CURR.NO
            R.EB.OVERRIDE.LOG<FT.EB.7.INPUTTER> = FIELD( EB.SystemTables.getRNew(FT.Contract.FundsTransfer.Inputter),'_',2)
            R.EB.OVERRIDE.LOG<FT.EB.7.DATE.TIME> =EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DateTime)
            R.EB.OVERRIDE.LOG<FT.EB.7.AUTHORISER> = Y.OPERATOR
            R.EB.OVERRIDE.LOG<FT.EB.7.CO.CODE> =EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CoCode)
            R.EB.OVERRIDE.LOG<FT.EB.7.DEPT.CODE> =EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DeptCode)
            R.EB.OVERRIDE.LOG<FT.EB.7.AUDITOR.CODE> =EB.SystemTables.getRNew(FT.Contract.FundsTransfer.AuditorCode)
            R.EB.OVERRIDE.LOG<FT.EB.7.AUDIT.DATE.TIME> =EB.SystemTables.getRNew(FT.Contract.FundsTransfer.AuditDateTime)

        CASE Y.APP EQ 'TELLER'

            Y.OVERRIDE = EB.SystemTables.getRNew(TT.Contract.Teller.TeOverride)
            IF Y.OVERRIDE EQ '' THEN RETURN
            Y.CURR.NO = EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrNo)
            Y.OVERRIDE.ID =Y.ID.NEW:'-': Y.CURR.NO
            R.EB.OVERRIDE.LOG<FT.EB.7.APPLICATION> = Y.APPLICATION
            R.EB.OVERRIDE.LOG<FT.EB.7.OVERRIDE> = EB.SystemTables.getRNew(TT.Contract.Teller.TeOverride)
            R.EB.OVERRIDE.LOG<FT.EB.7.RECORD.STATUS>  = EB.SystemTables.getRNew(TT.Contract.Teller.TeRecordStatus)
            R.EB.OVERRIDE.LOG<FT.EB.7.CURR.NO> =  EB.SystemTables.getRNew(TT.Contract.Teller.TeCurrNo)
            R.EB.OVERRIDE.LOG<FT.EB.7.INPUTTER> = FIELD( EB.SystemTables.getRNew(TT.Contract.Teller.TeInputter),'_',2)
            R.EB.OVERRIDE.LOG<FT.EB.7.DATE.TIME> =EB.SystemTables.getRNew(TT.Contract.Teller.TeDateTime)
            R.EB.OVERRIDE.LOG<FT.EB.7.AUTHORISER> = Y.OPERATOR
            R.EB.OVERRIDE.LOG<FT.EB.7.CO.CODE> =EB.SystemTables.getRNew(TT.Contract.Teller.TeCoCode)
            R.EB.OVERRIDE.LOG<FT.EB.7.DEPT.CODE> =EB.SystemTables.getRNew(TT.Contract.Teller.TeDeptCode)
            R.EB.OVERRIDE.LOG<FT.EB.7.AUDITOR.CODE> =EB.SystemTables.getRNew(TT.Contract.Teller.TeAuditorCode)
            R.EB.OVERRIDE.LOG<FT.EB.7.AUDIT.DATE.TIME> =EB.SystemTables.getRNew(TT.Contract.Teller.TeAuditDateTime)

        CASE Y.APP EQ 'TELLER.FINANCIAL.SERVICES'

            Y.OVERRIDE = EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsOverride)
            IF Y.OVERRIDE EQ '' THEN RETURN
        
            Y.CURR.NO =  EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsCoCode)
            Y.OVERRIDE.ID =Y.ID.NEW:'-': Y.CURR.NO
            R.EB.OVERRIDE.LOG<FT.EB.7.APPLICATION> = Y.APPLICATION
            R.EB.OVERRIDE.LOG<FT.EB.7.OVERRIDE> = EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsOverride)
            R.EB.OVERRIDE.LOG<FT.EB.7.RECORD.STATUS>  = EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsRecordStatus)
            R.EB.OVERRIDE.LOG<FT.EB.7.CURR.NO> =  EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsCurrNo)
            R.EB.OVERRIDE.LOG<FT.EB.7.INPUTTER> = FIELD( EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsInputter),'_',2)
            R.EB.OVERRIDE.LOG<FT.EB.7.DATE.TIME> =EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsDateTime)
            R.EB.OVERRIDE.LOG<FT.EB.7.AUTHORISER> = OPERATOR
            R.EB.OVERRIDE.LOG<FT.EB.7.CO.CODE> =EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsCoCode)
            R.EB.OVERRIDE.LOG<FT.EB.7.DEPT.CODE> =EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsDeptCode)
            R.EB.OVERRIDE.LOG<FT.EB.7.AUDITOR.CODE> =EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsAuditorCode)
            R.EB.OVERRIDE.LOG<FT.EB.7.AUDIT.DATE.TIME> =EB.SystemTables.getRNew(TT.TellerFinancialService.TellerFinancialServices.TfsAuditDateTime)

        CASE 1

            Y.ERROR = 'Error'
    END CASE


    GOSUB CHK.WRITE.VIOLATION
RETURN


CHK.WRITE.VIOLATION:
*===================

    IF R.EB.OVERRIDE.LOG THEN
* WRITE R.EB.OVERRIDE.LOG TO F.EB.OVERRIDE.LOG,Y.OVERRIDE.ID
* EB.DataAccess.FWrite(Fileid, VKey, Rec)   ---TAFC - TAFJ --- Shibli
        EB.DataAccess.FWrite(FN.EB.OVERRIDE.LOG,Y.OVERRIDE.ID,R.EB.OVERRIDE.LOG)
        EB.TransactionControl.JournalUpdate(Y.OVERRIDE.ID)
    END

RETURN
END