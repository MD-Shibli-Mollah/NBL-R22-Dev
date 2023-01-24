* @ValidationCode : MjotOTQ4OTk2MTI3OkNwMTI1MjoxNjczOTQzNzgxOTM1OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 17 Jan 2023 14:23:01
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.I.CUS.LOC.FLD
*-------------------------------------------------------------------------
*<Description>
* Determines overall risk grading for KYC depending on local field values of customer
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - MD Shibli Mollah - FDS
*-------------------------------------------------------------------------
**************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING ST.Customer
    $USING EB.Updates
    $USING EB.DataAccess
    $USING EB.SystemTables
    
    
*    FN.CUS = 'F.CUSTOMER'
*        F.CUS = ''
*        EB.DataAccess.Opf(FN.CUS,F.CUS)
*        EB.DataAccess.FRead(FN.CUS,Y.CUS.ID,R.CUS,F.CUS,Y.CUS.ERR)

    Y.APP = "CUSTOMER"
    Y.FLD = "LT.OVERALL.RISK":@VM:"LT.CUST.NETWRTH":@VM:"LT.ACT.OPN.WAY":@VM:"LT.MTH.TXN.VAL":@VM:"LT.MTH.TXN.VOL":@VM:"LT.MTH.CTXN.VAL":@VM:"LT.MTH.CTXN.VOL"
    Y.POS =''

*EB.Updates.MultiGetLocRef(ApplArr, FieldnameArr, PosArr)
    EB.Updates.MultiGetLocRef(Y.APP, Y.FLD, Y.POS)
    Y.OVERALL.RISK.POS = Y.POS<1,1>
    Y.CUST.NETWORTH.POS = Y.POS<1,2>
    Y.ACCT.OPEN.WAY.POS = Y.POS<1,3>
    Y.EX.MTH.TXN.VAL.POS = Y.POS<1,4>
    Y.EX.MTH.TXN.VOL.POS = Y.POS<1,5>
    Y.EX.MTH.CTXN.VAL.POS = Y.POS<1,6>
    Y.EX.MTH.CTXN.VOL.POS = Y.POS<1,7>
    !
    Y.CUST.NETWORTH = '' ; Y.ACCT.OPEN.WAY = '' ; Y.MTH.TXN.VAL = '' ; Y.OVR.RISK = '' ; Y.RISK.ASSET.TYPE = '' ; Y.RISK.ASSET.TYPE.LIST.1 = '' ; Y.RISK.ASSET.TYPE.LIST.2 = ''
    Y.MTH.TXN.VOL = '' ; Y.MTH.CTXN.VAL = '' ; Y.MTH.CTXN.VOL = '' ; Y.TOT.OVR.RISK = '' ; Y.RISK.ASSET.TYPE.LIST.3 = '' ; Y.RISK.ASSET.TYPE.LIST.4 = '' ; Y.RISK.ASSET.TYPE.LIST.5 = '' ; Y.RISK.ASSET.TYPE.LIST.6 = ''
    !
    !updated by Md. Sanaullah 20180607
    Y.RISK.ASSET.TYPE.LIST.1<-1> = "101":@VM:"102":@VM:"103":@VM:"104":@VM:"105":@VM:"106":@VM:"107":@VM:"108":@VM:"109":@VM:"110":@VM:"111":@VM:"112":@VM:"113":@VM:"114":@VM:"115":@VM:"151"
    Y.RISK.ASSET.TYPE.LIST.2<-1> = "116":@VM:"117":@VM:"118":@VM:"119":@VM:"120":@VM:"121":@VM:"122":@VM:"123":@VM:"124":@VM:"125":@VM:"126":@VM:"127":@VM:"128":@VM:"150"
    Y.RISK.ASSET.TYPE.LIST.3<-1> = "129":@VM:"130":@VM:"131":@VM:"132":@VM:"133":@VM:"134":@VM:"135":@VM:"136":@VM:"137":@VM:"149"
    Y.RISK.ASSET.TYPE.LIST.4<-1> = "138":@VM:"139":@VM:"140":@VM:"141":@VM:"142":@VM:"143":@VM:"144":@VM:"145":@VM:"146":@VM:"148"
    Y.RISK.ASSET.TYPE.LIST.5<-1> = "147"
    Y.RISK.ASSET.TYPE.LIST.6<-1> = ""
    !
    !updated by Md. Sanaullah 20180607
    
* EB.SystemTables.getRNew(idx)
    Y.CUS.RISK.ASSET.TYPE = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusRiskAssetType)
    BEGIN CASE
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.RISK.ASSET.TYPE.LIST.1
            Y.RISK.ASSET.TYPE = 5
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.RISK.ASSET.TYPE.LIST.2
            Y.RISK.ASSET.TYPE = 4
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.RISK.ASSET.TYPE.LIST.3
            Y.RISK.ASSET.TYPE = 3
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.RISK.ASSET.TYPE.LIST.4
            Y.RISK.ASSET.TYPE = 2
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.RISK.ASSET.TYPE.LIST.5
            Y.RISK.ASSET.TYPE = 1
        CASE Y.CUS.RISK.ASSET.TYPE MATCHES Y.RISK.ASSET.TYPE.LIST.6
            Y.RISK.ASSET.TYPE = 0
    END CASE
    !
* ST.Customer.Customer.EbCusLocalRef
    Y.GET.CUS.NETWORTH.LOC.REF = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.CUST.NETWORTH.POS>
    IF Y.GET.CUS.NETWORTH.LOC.REF = "Upto 1 Lac" OR Y.GET.CUS.NETWORTH.LOC.REF = '' THEN
        Y.CUST.NETWORTH = 0
    END ELSE
        IF Y.GET.CUS.NETWORTH.LOC.REF = "> 1-3 Lac" THEN
            Y.CUST.NETWORTH = 1
        END ELSE
            IF Y.GET.CUS.NETWORTH.LOC.REF = "above 3 Lac" THEN
                Y.CUST.NETWORTH = 3
            END
        END
    END
    !
    Y.CUS.LOC.ACCT.OPEN.WAY = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.ACCT.OPEN.WAY.POS>
    IF Y.CUS.LOC.ACCT.OPEN.WAY = 1 OR Y.CUS.LOC.ACCT.OPEN.WAY = '' THEN
        Y.ACCT.OPEN.WAY = 0
    END ELSE
        IF Y.CUS.LOC.ACCT.OPEN.WAY = 2 THEN
            Y.ACCT.OPEN.WAY = 3
        END ELSE
            IF Y.CUS.LOC.ACCT.OPEN.WAY = 3 THEN
                Y.ACCT.OPEN.WAY = 3
            END ELSE
                IF Y.CUS.LOC.ACCT.OPEN.WAY = 4 THEN
                    Y.ACCT.OPEN.WAY = 3
                END

            END
        END
    END
    !
    Y.CUS.EX.MTH.TXN.VAL = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.EX.MTH.TXN.VAL.POS>
    IF Y.CUS.EX.MTH.TXN.VAL = 'CD 0-10 Lac' OR Y.CUS.EX.MTH.TXN.VAL = 'SB 0-5 Lac' OR Y.CUS.EX.MTH.TXN.VAL = '' THEN
        Y.MTH.TXN.VAL = 0
    END ELSE
        IF Y.CUS.EX.MTH.TXN.VAL = 'CD >10-20 Lac' OR Y.CUS.EX.MTH.TXN.VAL = 'SB >5-10 Lac' THEN
            Y.MTH.TXN.VAL = 1
        END ELSE
            IF Y.CUS.EX.MTH.TXN.VAL = 'CD >20 Lac' OR Y.CUS.EX.MTH.TXN.VAL = 'SB >10 Lac' THEN
                Y.MTH.TXN.VAL = 3
            END
        END
    END
    ! DONE
    Y.CUS.LOC.MTH.TXN.VOL = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.EX.MTH.TXN.VOL.POS>
    
    IF Y.CUS.LOC.MTH.TXN.VOL = 'CD 0-15' OR Y.CUS.LOC.MTH.TXN.VOL = 'SB 0-10' OR Y.CUS.LOC.MTH.TXN.VOL = '' THEN
        Y.MTH.TXN.VOL = 0
    END ELSE
        IF Y.CUS.LOC.MTH.TXN.VOL = 'CD 16-25' OR Y.CUS.LOC.MTH.TXN.VOL = 'SB 11-20' THEN
            Y.MTH.TXN.VOL = 1
        END ELSE
            IF Y.CUS.LOC.MTH.TXN.VOL = 'CD >25' OR Y.CUS.LOC.MTH.TXN.VOL = 'SB >20' THEN
                Y.MTH.TXN.VOL = 3
            END
        END
    END
    !
    Y.CUS.LOC.MTH.CTXN.VAL = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.EX.MTH.CTXN.VAL.POS>
    IF Y.CUS.LOC.MTH.CTXN.VAL = 'CD 0-5 Lac' OR Y.CUS.LOC.MTH.CTXN.VAL = 'SB 0-2 Lac' OR  Y.CUS.LOC.MTH.CTXN.VAL = '' THEN
        Y.MTH.CTXN.VAL = 0
    END ELSE
        IF Y.CUS.LOC.MTH.CTXN.VAL = 'CD >5-10 Lac' OR Y.CUS.LOC.MTH.CTXN.VAL = 'SB >2-5 Lac' THEN
            Y.MTH.CTXN.VAL = 1
        END ELSE
            IF Y.CUS.LOC.MTH.CTXN.VAL = 'CD >10 Lac' OR Y.CUS.LOC.MTH.CTXN.VAL = 'SB >5 Lac' THEN
                Y.MTH.CTXN.VAL = 3
            END
        END
    END
    !
    Y.CUS.LOC.MTH.CTXN.VOL = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)<1,Y.EX.MTH.CTXN.VOL.POS>
    IF Y.CUS.LOC.MTH.CTXN.VOL = 'CD 0-10' OR Y.CUS.LOC.MTH.CTXN.VOL = 'SB 0-5' OR Y.CUS.LOC.MTH.CTXN.VOL = '' THEN
        Y.MTH.CTXN.VOL = 0
    END ELSE
        IF Y.CUS.LOC.MTH.CTXN.VOL = 'CD 11-20' OR Y.CUS.LOC.MTH.CTXN.VOL = 'SB 6-10' THEN
            Y.MTH.CTXN.VOL = 1
        END ELSE
            IF Y.CUS.LOC.MTH.CTXN.VOL = 'CD >20' OR Y.CUS.LOC.MTH.CTXN.VOL = 'SB >10' THEN
                Y.MTH.CTXN.VOL = 3
            END
        END
    END

    ! DONE

    Y.TOT.OVR.RISK = Y.RISK.ASSET.TYPE + Y.CUST.NETWORTH + Y.ACCT.OPEN.WAY + Y.MTH.TXN.VAL + Y.MTH.TXN.VOL + Y.MTH.CTXN.VAL + Y.MTH.CTXN.VOL
    IF Y.RISK.ASSET.TYPE EQ '' THEN
        IF (Y.GET.CUS.NETWORTH.LOC.REF = '') AND (Y.CUS.LOC.ACCT.OPEN.WAY = '') THEN
            IF (Y.CUS.EX.MTH.TXN.VAL = '') AND (Y.CUS.LOC.MTH.TXN.VOL = '') THEN
                IF (Y.CUS.LOC.MTH.CTXN.VAL = '') AND (Y.CUS.LOC.MTH.CTXN.VOL = '') THEN
                    Y.OVR.RISK = ''
                    Y.TOT.OVR.RISK = ''
                END
            END
        END
    END
    IF Y.TOT.OVR.RISK GE 14 THEN
        Y.OVR.RISK = "High"
    END

    IF (Y.TOT.OVR.RISK LT 14) AND (Y.TOT.OVR.RISK NE '') THEN
        Y.OVR.RISK = "Low"
    END

*****SET DATA*****
    Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)
    Y.TEMP<1,Y.OVERALL.RISK.POS> = Y.OVR.RISK
    EB.SystemTables.setRNew(ST.Customer.Customer.EbCusLocalRef, Y.TEMP)

    ! DONE!
RETURN
END
