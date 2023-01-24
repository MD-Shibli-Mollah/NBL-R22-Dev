* @ValidationCode : MjoyNzU5Mjk5ODc6Q3AxMjUyOjE2NzM5NDcxODMzMDc6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 17 Jan 2023 15:19:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.V.CUS.SET.FLD.TO.NOINPUT
*-----------------------------------------------------------------------------
*<Description>
* Set the Local Fields as NO INPUT Fields.
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - MD Shibli Mollah - FDS
*-------------------------------------------------------------------------
**************************************************************************
*
    !
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING ST.Customer
    $USING EB.Updates
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Display
    $USING EB.Versions
    !
    Y.APP = "CUSTOMER"
    Y.FLD = "LT.POL.EX.PRSN":@VM:"LT.SENI.MGMT.APPR":@VM:"LT.SRC.WLTH":@VM:"LT.INTRV.PER":@VM:"LT.REVW.NAME"
    Y.POS = ''

    EB.Updates.MultiGetLocRef(Y.APP,Y.FLD,Y.POS)
    Y.POL.EX.PERSON.POS = Y.POS<1,1>
    Y.SENI.MGMT.APPR.POS = Y.POS<1,2>
    Y.SRC.OF.WEALTH = Y.POS<1,3>
    Y.INTERVIEW.PER = Y.POS<1,4>
    Y.REVIEW.OFF.NAME = Y.POS<1,5>
    !
    IF EB.SystemTables.getComi() EQ 'N' THEN
* EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)

**-------- SET VALUES *****

        Y.TEMP = EB.SystemTables.getRNew(ST.Customer.Customer.EbCusLocalRef)
        Y.TEMP<1,Y.SENI.MGMT.APPR.POS> = ''
        Y.TEMP<1,Y.SRC.OF.WEALTH> = ''
        Y.TEMP<1,Y.INTERVIEW.PER> = ''
        Y.TEMP<1,Y.REVIEW.OFF.NAME> = ''
        EB.SystemTables.setRNew(ST.Customer.Customer.EbCusLocalRef, Y.TEMP)
        
        T.LOCREF<Y.SENI.MGMT.APPR.POS,7> = "NOINPUT"
        T.LOCREF<Y.SRC.OF.WEALTH,7> = "NOINPUT"
        T.LOCREF<Y.INTERVIEW.PER,7> = "NOINPUT"
        T.LOCREF<Y.REVIEW.OFF.NAME,7> = "NOINPUT"
       
        EB.Display.RefreshField(Y.TEMP<1,Y.SENI.MGMT.APPR.POS>,'')
        EB.Display.RefreshField(Y.TEMP<1,Y.SRC.OF.WEALTH>,'')
        EB.Display.RefreshField(Y.TEMP<1,Y.INTERVIEW.PER>,'')
        EB.Display.RefreshField(Y.TEMP<1,Y.REVIEW.OFF.NAME>,'')
    END
    
    IF EB.SystemTables.getComi() EQ 'Y' THEN
        T.LOCREF<Y.SENI.MGMT.APPR.POS,7> = ""
        T.LOCREF<Y.SRC.OF.WEALTH,7> = ""
        T.LOCREF<Y.INTERVIEW.PER,7> = ""
        T.LOCREF<Y.REVIEW.OFF.NAME,7> = ""
* EB.Display.RefreshField(FieldEnri, RefreshFieldNo)
*EB_Display.RefreshField(get(ST_Customer._Customer_EbCusLocalRef, 1, _Y_SENI_MGMT_APPR_POS, 0),"");
*CALL REFRESH.FIELD(EB.CUS.LOCAL.REF<1,Y.SENI.MGMT.APPR.POS>,'')

        EB.Display.RefreshField(Y.TEMP<1,Y.SENI.MGMT.APPR.POS>,'')
        EB.Display.RefreshField(Y.TEMP<1,Y.SRC.OF.WEALTH>,'')
        EB.Display.RefreshField(Y.TEMP<1,Y.INTERVIEW.PER>,'')
        EB.Display.RefreshField(Y.TEMP<1,Y.REVIEW.OFF.NAME>,'')
    END
    !
RETURN
END
