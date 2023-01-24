* @ValidationCode : MjoxMjYwMDQxMDk0OkNwMTI1MjoxNjczODY1NTQyNzg0OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 16 Jan 2023 16:39:02
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.A.UPDATE.CC.STOCK
    
*-------------------------------------------------------------------------
* <Rating>100</Rating>
*-------------------------------------------------------------------------
*-------------------------------------------------------------------------
*<Description>
* Updating EB.CC.STOCK.REGISTER - LOCAL Template
*-------------------------------------------------------------------------
* Modification History:
* ----------------------
* 16/01/2023        - Retrofit     - Shibli - FDS
*-------------------------------------------------------------------------
**************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.CC.STOCK.REGISTER
*    $INSERT T24.BP I_F.COLLATERAL
*    $INSERT T24.BP I_F.COLLATERAL.RIGHT
    $USING CO.Contract
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Updates
    $USING EB.TransactionControl

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

RETURN

INIT:

    FN.CC.STOCK = "F.EB.CC.STOCK.REGISTER"
    F.CC.STOCK=""
    REC.CC.STOCK=""
    Y.CC.ID=""

    FN.COLL.RGHT = "F.COLLATERAL.RIGHT"
    F.COLL.RGHT=""
    REC.COLL.RGHT=""
    Y.COLL.RGHT.ID=""

    STOCK.CNT=""

    OLD.CNT = ""
    NEW.CNT = ""

    Y.APPLICATION       = "COLLATERAL"
    FIELD.NAME         = "NAME.OF.STOCK":@VM:"DATE.OF.STOCK":@VM:"QUANTITY":@VM:"VALUE.PER.UNIT":@VM:"MARGIN":@VM:"DRAWING":@VM:"AMOUNT.MARGIN"
    FIELD.POS           = ""
   
* EB.Updates.MultiGetLocRef(ApplArr, FieldnameArr, PosArr)  -- TAJC TO TAFJ --- Shibli
* CALL MULTI.GET.LOC.REF(Y.APPLICATION,FIELD.NAME,FIELD.POS)
    EB.Updates.MultiGetLocRef(Y.APPLICATION,FIELD.NAME,FIELD.POS)

RETURN

OPENFILES:

    EB.DataAccess.Opf(FN.CC.STOCK,F.CC.STOCK)
    EB.DataAccess.Opf(FN.COLL.RGHT,F.COLL.RGHT)

RETURN

PROCESS:

    NAME.OF.STOC.POS = FIELD.POS<1,1>
    DATE.OF.STOC.POS = FIELD.POS<1,2>
    QUANTITY.POS = FIELD.POS<1,3>
    VALUE.PER.UN.POS = FIELD.POS<1,4>
    MARGIN.POS = FIELD.POS<1,5>
    DRAWING.POS = FIELD.POS<1,6>
    AMOUNT.MARGI.POS = FIELD.POS<1,7>
    
    Y.TODAY = EB.SystemTables.getToday()
    Y.ID.NEW = EB.SystemTables.getIdNew()
    
    Y.COLL.RGHT.ID=FIELD(Y.ID.NEW,".",1,1):".":FIELD(Y.ID.NEW,".",2,1)
    EB.DataAccess.FRead(FN.COLL.RGHT,Y.COLL.RGHT.ID,REC.COLL.RGHT,F.COLL.RGHT,ERR.COLL.RGHT)
* Y.CC.ID = REC.COLL.RGHT<COLL.RIGHT.LIMIT.REFERENCE>
    Y.CC.ID = REC.COLL.RGHT<CO.Contract.CollateralRight.CollRightLimitReference>
    EB.DataAccess.FRead(FN.CC.STOCK,Y.CC.ID,REC.CC.STOCK,F.CC.STOCK,ERR.CC.STOCK)

    IF REC.CC.STOCK THEN
*        OLD.CNT = DCOUNT(EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS>,@SM)
        OLD.CNT = DCOUNT(EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS>,@SM)
*        NEW.CNT = DCOUNT(EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS>,@SM)
        NEW.CNT = DCOUNT(EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS>,@SM)

        BEGIN CASE

            !---------IF OLD STOCK NO AND NEW STOCK NO ARE SAME-------------!

        CASE OLD.CNT EQ NEW.CNT
            Y.STK.VAL=""
* Y.STK.VAL = EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS>
            Y.STK.VAL = EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS>
            FOR J = 1 TO NEW.CNT
                Y.STOCK.NM = ""
                STK.CNT = ""
* Y.STOCK.NM = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS,J>
                Y.STOCK.NM = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS,J>

                LOCATE Y.STOCK.NM IN Y.STK.VAL<1,1,1> SETTING Y.POS1 THEN
                    Y.CC.STK.VAL=""
                    Y.CC.STK.VAL=REC.CC.STOCK<EB.CC.NAME.OF.STOC>
                    BEGIN CASE

                        CASE EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J> GT EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J>

                            !!!!!!!!!!!! STOCK OUT!!!!!!!!!!!

                            LOCATE Y.STOCK.NM IN Y.CC.STK.VAL<1,1> SETTING Y.CC.POS1 THEN
                                STK.CNT = DCOUNT(REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1>,@SM)
                                STK.CNT = STK.CNT + 1
                                REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1,STK.CNT> = Y.TODAY
                                REC.CC.STOCK<EB.CC.IN.QUANTITY,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.OUT.QUANTITY,Y.CC.POS1,STK.CNT> = ( EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J> - EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J>)
                                REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,J>
                                REC.CC.STOCK<EB.CC.BAL.QUANTITY,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J>
                                REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,J>
                                REC.CC.STOCK<EB.CC.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,J>
                                REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,J>
                                REC.CC.STOCK<EB.CC.DRAWING,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,J>
                            END

                        CASE EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J> LT EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J>

                            !!!!!!!!!!!! STOCK IN!!!!!!!!!!!

                            LOCATE Y.STOCK.NM IN Y.CC.STK.VAL<1,1> SETTING Y.CC.POS1 THEN
                                STK.CNT = DCOUNT(REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1>,@SM)
                                STK.CNT = STK.CNT + 1
                                REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1,STK.CNT> = Y.TODAY
                                REC.CC.STOCK<EB.CC.IN.QUANTITY,Y.CC.POS1,STK.CNT> = ( EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J> - EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J>)
                                REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,J>
                                REC.CC.STOCK<EB.CC.OUT.QUANTITY,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.BAL.QUANTITY,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J>
                                REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,J>
                                REC.CC.STOCK<EB.CC.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,J>
                                REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,J>
                                REC.CC.STOCK<EB.CC.DRAWING,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,J>
                            END

                    END CASE

                END

                ELSE

                    REC.CC.STOCK<EB.CC.NAME.OF.STOC,J> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS,J>
                    REC.CC.STOCK<EB.CC.DATE,J,1> = Y.TODAY
                    REC.CC.STOCK<EB.CC.IN.QUANTITY,J,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J>
                    REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,J,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,J>
                    REC.CC.STOCK<EB.CC.OUT.QUANTITY,J,1> = 0
                    REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,J,1> = 0
                    REC.CC.STOCK<EB.CC.BAL.QUANTITY,J,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,J>
                    REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,J,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,J>
                    REC.CC.STOCK<EB.CC.MARGIN,J,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,J>
                    REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,J,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,J>
                    REC.CC.STOCK<EB.CC.DRAWING,J,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,J>

                END
            NEXT
            EB.DataAccess.FWrite(FN.CC.STOCK,Y.CC.ID,REC.CC.STOCK)
            EB.TransactionControl.JournalUpdate(Y.CC.ID)
            
        CASE OLD.CNT LT NEW.CNT

            Y.STK.VAL=""
            Y.STK.VAL=EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS>
            FOR K = 1 TO NEW.CNT
                Y.STOCK.NM = ""
                STK.CNT = ""
                Y.STOCK.NM = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS,K>

                LOCATE Y.STOCK.NM IN Y.STK.VAL<1,1,1> SETTING Y.POS1 THEN

                    Y.CC.STK.VAL=""
                    Y.CC.STK.VAL=REC.CC.STOCK<EB.CC.NAME.OF.STOC>


                    BEGIN CASE

                        CASE EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K> GT EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K>

                            !!!!!!!!!!!! STOCK OUT!!!!!!!!!!!

                            LOCATE Y.STOCK.NM IN Y.CC.STK.VAL<1,1> SETTING Y.CC.POS1 THEN
                                STK.CNT=""
                                STK.CNT = DCOUNT(REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1>,@SM)
                                STK.CNT = STK.CNT + 1
                                REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1,STK.CNT> = Y.TODAY
                                REC.CC.STOCK<EB.CC.IN.QUANTITY,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.OUT.QUANTITY,Y.CC.POS1,STK.CNT> = ( EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K> - EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K>)
                                REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,K>
                                REC.CC.STOCK<EB.CC.BAL.QUANTITY,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K>
                                REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,K>
                                REC.CC.STOCK<EB.CC.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,K>
                                REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,K>
                                REC.CC.STOCK<EB.CC.DRAWING,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,K>
                            END

                        CASE EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K> LT EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K>

                            !!!!!!!!!!!! STOCK IN!!!!!!!!!!!

                            LOCATE Y.STOCK.NM IN Y.CC.STK.VAL<1,1> SETTING Y.CC.POS1 THEN
                                STK.CNT =""
                                STK.CNT = DCOUNT(REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1>,@SM)
                                STK.CNT = STK.CNT + 1
                                REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1,STK.CNT> = Y.TODAY
                                REC.CC.STOCK<EB.CC.IN.QUANTITY,Y.CC.POS1,STK.CNT> = ( EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K> - EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K>)
                                REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,K>
                                REC.CC.STOCK<EB.CC.OUT.QUANTITY,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.BAL.QUANTITY,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K>
                                REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,K>
                                REC.CC.STOCK<EB.CC.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,K>
                                REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,K>
                                REC.CC.STOCK<EB.CC.DRAWING,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,K>
                            END
                    END CASE

                END

                ELSE

                    REC.CC.STOCK<EB.CC.NAME.OF.STOC,K> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS,K>
                    REC.CC.STOCK<EB.CC.DATE,K,1> = Y.TODAY
                    REC.CC.STOCK<EB.CC.IN.QUANTITY,K,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K>
                    REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,K,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,K>
                    REC.CC.STOCK<EB.CC.OUT.QUANTITY,K,1> = 0
                    REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,K,1> = 0
                    REC.CC.STOCK<EB.CC.BAL.QUANTITY,K,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,K>
                    REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,K,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,K>
                    REC.CC.STOCK<EB.CC.MARGIN,K,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,K>
                    REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,K,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,K>
                    REC.CC.STOCK<EB.CC.DRAWING,K,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,K>

                END

            NEXT
            EB.DataAccess.FWrite(FN.CC.STOCK,Y.CC.ID,REC.CC.STOCK)
            EB.TransactionControl.JournalUpdate(Y.CC.ID)
            
        CASE OLD.CNT GT NEW.CNT

            Y.STK.VAL=""
            Y.STK.VAL=EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS>



            FOR LL = 1 TO OLD.CNT
                STK.CNT = ""
                Y.STOCK.NM = ""
                Y.STOCK.NM = EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS,LL>
                LOCATE Y.STOCK.NM IN  Y.STK.VAL<1,1,1> SETTING Y.POS1 THEN

                    Y.CC.STK.VAL=""
                    Y.CC.STK.VAL=REC.CC.STOCK<EB.CC.NAME.OF.STOC>

                    BEGIN CASE

                        CASE EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL> GT EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL>

                            !!!!!!!!!!!! STOCK OUT!!!!!!!!!!!

                            LOCATE Y.STOCK.NM IN Y.CC.STK.VAL<1,1> SETTING Y.CC.POS1 THEN
                                STK.CNT=""
                                STK.CNT = DCOUNT(REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1>,@SM)
                                STK.CNT = STK.CNT + 1
                                REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1,STK.CNT> = Y.TODAY
                                REC.CC.STOCK<EB.CC.IN.QUANTITY,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.OUT.QUANTITY,Y.CC.POS1,STK.CNT> = ( EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,L> - EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL>)
                                REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,LL>
                                REC.CC.STOCK<EB.CC.BAL.QUANTITY,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL>
                                REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,LL>
                                REC.CC.STOCK<EB.CC.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,LL>
                                REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,LL>
                                REC.CC.STOCK<EB.CC.DRAWING,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,LL>
                            END

                        CASE EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL> LT EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL>

                            !!!!!!!!!!!! STOCK IN!!!!!!!!!!!

                            LOCATE Y.STOCK.NM IN Y.CC.STK.VAL<1,1> SETTING Y.CC.POS1 THEN
                                STK.CNT =""
                                STK.CNT = DCOUNT(REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1>,@SM)
                                STK.CNT = STK.CNT + 1
                                REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1,STK.CNT> = Y.TODAY
                                REC.CC.STOCK<EB.CC.IN.QUANTITY,Y.CC.POS1,STK.CNT> = ( EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL> - EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL>)
                                REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,LL>
                                REC.CC.STOCK<EB.CC.OUT.QUANTITY,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,Y.CC.POS1,STK.CNT> = 0
                                REC.CC.STOCK<EB.CC.BAL.QUANTITY,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL>
                                REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,LL>
                                REC.CC.STOCK<EB.CC.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,LL>
                                REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,LL>
                                REC.CC.STOCK<EB.CC.DRAWING,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,LL>
                            END
                    END CASE

                END

                ELSE

                    LOCATE Y.STOCK.NM IN Y.CC.STK.VAL<1,1> SETTING Y.CC.POS1 THEN
                        STK.CNT=""
                        STK.CNT = DCOUNT(REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1>,@SM)
                        STK.CNT = STK.CNT + 1
                        REC.CC.STOCK<EB.CC.DATE,Y.CC.POS1,STK.CNT> = Y.TODAY
                        REC.CC.STOCK<EB.CC.IN.QUANTITY,Y.CC.POS1,STK.CNT> = 0
                        REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,Y.CC.POS1,STK.CNT> = 0
                        REC.CC.STOCK<EB.CC.OUT.QUANTITY,Y.CC.POS1,STK.CNT> = ( EB.SystemTables.getROld(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL> - EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL>)
                        REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,LL>
                        REC.CC.STOCK<EB.CC.BAL.QUANTITY,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,LL>
                        REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,LL>
                        REC.CC.STOCK<EB.CC.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,LL>
                        REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,LL>
                        REC.CC.STOCK<EB.CC.DRAWING,Y.CC.POS1,STK.CNT> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,LL>
                    END

                END

            NEXT
            EB.DataAccess.FWrite(FN.CC.STOCK,Y.CC.ID,REC.CC.STOCK)
            EB.TransactionControl.JournalUpdate(Y.CC.ID)
    END CASE

END

ELSE

    STOCK.CNT = DCOUNT(EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS>,@SM)

    FOR I = 1 TO STOCK.CNT
        REC.CC.STOCK<EB.CC.NAME.OF.STOC,I> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,NAME.OF.STOC.POS,I>
        REC.CC.STOCK<EB.CC.DATE,I,1> = Y.TODAY
        REC.CC.STOCK<EB.CC.IN.QUANTITY,I,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,I>
        REC.CC.STOCK<EB.CC.IN.VAL.PER.UN,I,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,I>
        REC.CC.STOCK<EB.CC.OUT.QUANTITY,I,1> = 0
        REC.CC.STOCK<EB.CC.OUT.VAL.PER.UN,I,1> = 0
        REC.CC.STOCK<EB.CC.BAL.QUANTITY,I,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,QUANTITY.POS,I>
        REC.CC.STOCK<EB.CC.BAL.VAL.PER.UN,I,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,VALUE.PER.UN.POS,I>
        REC.CC.STOCK<EB.CC.MARGIN,I,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,MARGIN.POS,I>
        REC.CC.STOCK<EB.CC.AMT.OF.MARGIN,I,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,AMOUNT.MARGI.POS,I>
        REC.CC.STOCK<EB.CC.DRAWING,I,1> = EB.SystemTables.getRNew(CO.Contract.Collateral.CollLocalRef)<1,DRAWING.POS,I>
        REC.CC.STOCK<EB.CC.COLLATERAL.ID>=Y.ID.NEW
    NEXT

    EB.DataAccess.FWrite(FN.CC.STOCK,Y.CC.ID,REC.CC.STOCK)
    EB.TransactionControl.JournalUpdate(Y.CC.ID)
END

RETURN

END
