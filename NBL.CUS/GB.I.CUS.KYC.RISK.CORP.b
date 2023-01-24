* @ValidationCode : Mjo2Mjk4MTc4NDU6Q3AxMjUyOjE2NzM5NDYzMzA1NDE6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 17 Jan 2023 15:05:30
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.CUS.KYC.RISK.CORP
*-------------------------------------------------------------------------
*<Description>
* Determines overall risk grading for KYC depending on local field values of customer
*-------------------------------------------------------------------------
* Author:                Md. Sanaullah
* Creation Date:         October 04, 2018
* Modification Date:
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT I_F.CUSTOMER
    $USING ST.Customer
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Updates
*
    Y.APP = "CUSTOMER"
    Y.FLD = "LT.OVERALL.RISK":@VM:"LT.CORP.NETWORTH":@VM:"LT.ACCT.OPEN.WAY":@VM:"LT.AMT.CORP":@VM:"LT.NUM.CORP":@VM:"LT.CTXN.AMT.CORP":@VM:"LT.CTXN.NUM.CORP"
*
    EB.Updates.MultiGetLocRef(Y.APP,Y.FLD,Y.POS)
    Y.OVERALL.RISK.POS = Y.POS<1,1>
    Y.CORP.NETWORTH.POS = Y.POS<1,2>
    Y.ACCT.OPEN.WAY.POS = Y.POS<1,3>
    Y.TXN.AMT.POS = Y.POS<1,4>
    Y.TXN.NUM.POS = Y.POS<1,5>
    Y.CTXN.AMT.POS = Y.POS<1,6>
    Y.CTXN.NUM.POS = Y.POS<1,7>
*
    Y.ASSET.RISK.SCORE.5 = "201":@VM:"202":@VM:"203":@VM:"204":@VM:"205":@VM:"206":@VM:"207":@VM:"208":@VM:"209":@VM:"210":@VM:"211":@VM:"212":@VM:"213":@VM:"214":@VM:"215":@VM:"216":@VM:"217":@VM:"218":@VM:"219":@VM:"220":@VM:"221":@VM:"222":@VM:"223":@VM:"224":@VM:"262"
    Y.ASSET.RISK.SCORE.4 = "225":@VM:"226":@VM:"227":@VM:"228":@VM:"229":@VM:"230":@VM:"231":@VM:"232":@VM:"233":@VM:"234":@VM:"235":@VM:"236":@VM:"237":@VM:"238":@VM:"239":@VM:"240":@VM:"241":@VM:"242":@VM:"261"
    Y.ASSET.RISK.SCORE.3 = "243":@VM:"244":@VM:"245":@VM:"246":@VM:"247":@VM:"248":@VM:"249":@VM:"250":@VM:"251":@VM:"252":@VM:"260"
    Y.ASSET.RISK.SCORE.2 = "253":@VM:"254":@VM:"255":@VM:"256":@VM:"257":@VM:"259"
    Y.ASSET.RISK.SCORE.1 = "258"
*
    Y.ASSET.RISK.SCORE = 0
* R.NEW(EB.CUS.RISK.ASSET.TYPE
    Y.CUS.RISK.ASSET.TYPE = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusRiskAssetType)
    
    BEGIN CASE
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.ASSET.RISK.SCORE.5
            Y.ASSET.RISK.SCORE = 5
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.ASSET.RISK.SCORE.4
            Y.ASSET.RISK.SCORE = 4
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.ASSET.RISK.SCORE.3
            Y.ASSET.RISK.SCORE = 3
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.ASSET.RISK.SCORE.2
            Y.ASSET.RISK.SCORE = 2
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.ASSET.RISK.SCORE.1
            Y.ASSET.RISK.SCORE = 1
    END CASE
*
    
* Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.CORP.NETWORTH.POS>
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.CORP.NETWORTH.POS>
    BEGIN CASE
        CASE Y.TEMP EQ "> 1-3 Lac"
            Y.ASSET.RISK.SCORE += 1
        CASE Y.TEMP EQ "above 3 Lac"
            Y.ASSET.RISK.SCORE += 3
    END CASE
*
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.ACCT.OPEN.WAY.POS>
    IF (Y.TEMP EQ 2) OR (Y.TEMP EQ 3) OR (Y.TEMP EQ 4) THEN
        Y.ASSET.RISK.SCORE += 3
    END
*
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.TXN.AMT.POS>
    BEGIN CASE
        CASE Y.TEMP EQ "CD >10-50 Lac" OR Y.TEMP EQ "SB >5-20 Lac"
            Y.ASSET.RISK.SCORE += 1
        CASE Y.TEMP EQ "CD >50 Lac" OR Y.TEMP EQ "SB >20 Lac"
            Y.ASSET.RISK.SCORE += 3
    END CASE
*
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.TXN.NUM.POS>
    BEGIN CASE
        CASE Y.TEMP EQ "CD 101-250" OR Y.TEMP EQ "SB 21-50"
            Y.ASSET.RISK.SCORE += 1
        CASE Y.TEMP EQ "CD >250" OR Y.TEMP EQ "SB >50"
            Y.ASSET.RISK.SCORE += 3
    END CASE
*
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.CTXN.AMT.POS>
    BEGIN CASE
        CASE Y.TEMP EQ "CD >10-25 Lac" OR Y.TEMP EQ "SB >2-7 Lac"
            Y.ASSET.RISK.SCORE += 1
        CASE Y.TEMP EQ "CD >25 Lac" OR Y.TEMP EQ "SB >7 Lac"
            Y.ASSET.RISK.SCORE += 3
    END CASE
*
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.CTXN.NUM.POS>
    BEGIN CASE
        CASE Y.TEMP EQ "CD 16-30" OR Y.TEMP EQ "SB 6-10"
            Y.ASSET.RISK.SCORE += 1
        CASE Y.TEMP EQ "CD >30" OR Y.TEMP EQ "SB >10"
            Y.ASSET.RISK.SCORE += 3
    END CASE
*
    IF Y.ASSET.RISK.SCORE GE 14 THEN
        Y.OVR.RISK = "High-":Y.ASSET.RISK.SCORE
    END ELSE
        IF Y.ASSET.RISK.SCORE GT 0 THEN
            Y.OVR.RISK = "Low-":Y.ASSET.RISK.SCORE
        END
    END
*
* EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.OVERALL.RISK.POS> = Y.OVR.RISK
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)
    Y.TEMP<1,Y.OVERALL.RISK.POS> = Y.OVR.RISK
    EB.SystemTables.setRNew(ST.Customer.Customer.EbCusLocalRef, Y.TEMP)
RETURN
END
