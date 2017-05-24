--CREATE TABLE NORTH.X_EBIZ_SHARE AS
  
  Select
  A.YEARMONTH, A.ZKEY, A.DOM_TYPE_II, A.DIVISION_NAME, A.CREDIT_BIN, A.OS_HA, A.EVER_S2S_CUST,
  A.EVER_FOL_CUST, A.EVER_EBIZ_CUST, A.MON, A.FRIENDLY,
  CASE WHEN A.FIRST_S2S_ORDER <= A.YEARMONTH THEN 1 ELSE 0 END S2S_CURRENT_CUST,
  CASE WHEN A.FIRST_FOL_ORDER <= A.YEARMONTH THEN 1 ELSE 0 END FOL_CURRENT_CUST,
  CASE WHEN A.FIRST_EBIZ_ORDER <= A.YEARMONTH THEN 1 ELSE 0 END EBIZ_CURRENT_CUST,
  A.FIRST_FEI_ORDER, A.FIRST_S2S_ORDER, A.CUST_SLS_TM, A.CUST_JOB_SLS,
  A.CUST_SLS_12M, A.CNT_ROLLING_MONTHS,A.BRANCH_SLS_12M,A.BRANCH_SLS_TM,A.CNT_YM,
  MONTHS_BETWEEN(TO_DATE(A.YEARMONTH||'01','YYYYMMDD'), TO_DATE(A.FIRST_FEI_ORDER||'01','YYYYMMDD')) MONTHS_FEI_TENURE
  
  
  from
  (
    Select bch.YEARMONTH, FC.ZKEY, FC.DOM_TYPE_II, FC.DIVISION_NAME, 
    CASE 
    WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 2000 THEN 'LESS THAN 2K'
    WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 15000 THEN '2K TO 15K'
    WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 75000 THEN '15K TO 75K'
    ELSE 'OVER 75K'
    END CREDIT_BIN, FC.OS_HA, Q.EVER_S2S_CUST, Q.EVER_FOL_CUST, Q.EVER_EBIZ_CUST, BCH.MON, FC.FRIENDLY, Q.CNT_YM,
    MIN(MIN(CASE WHEN BCH.TOTAL_ORDERS  > 0 THEN TO_NUMBER(BCH.YEARMONTH) else 999999 end)) over(partition by FC.ZKEY) FIRST_FEI_ORDER,
    min(min(TO_NUMBER(NVL(bch.FIRST_S2S_ORDER,'999999')))) over(partition by FC.ZKEY) FIRST_S2S_ORDER,
    min(min(TO_NUMBER(NVL(BCH.FIRST_ORDER,'999999')))) over(partition by FC.ZKEY) FIRST_FOL_ORDER,
    min(min(TO_NUMBER(
      CASE WHEN NVL(BCH.FIRST_ORDER,'999999') = '999999' AND NVL(bch.FIRST_S2S_ORDER,'999999') = '999999' THEN '999999'
      WHEN NVL(BCH.FIRST_ORDER,'999999') > NVL(bch.FIRST_S2S_ORDER,'999999') THEN NVL(bch.FIRST_S2S_ORDER,'999999') 
      ELSE NVL(BCH.FIRST_ORDER,'999999') end
    ))) over(partition by FC.ZKEY) FIRST_EBIZ_ORDER,
    sum(NVL(bch.NET_SALES,0)) CUST_SLS_TM,
    SUM(NVL(BCH.NET_JOB_SALES,0)) CUST_JOB_SLS,
    SUM(SUM(NVL(BCH.NET_SALES,0))) OVER(PARTITION BY FC.ZKEY ORDER BY to_date(BCH.YEARMONTH || '01', 'yyyymmdd') RANGE BETWEEN INTERVAL '11' MONTH PRECEDING AND INTERVAL '0' MONTH PRECEDING) AS CUST_SLS_12M,
    SUM(COUNT(DISTINCT CASE WHEN NVL(BCH.TOTAL_ORDERS,0) > 0 THEN BCH.YEARMONTH END)) OVER(PARTITION BY FC.ZKEY ORDER BY to_date(BCH.YEARMONTH || '01', 'yyyymmdd') RANGE BETWEEN INTERVAL '100' MONTH PRECEDING AND INTERVAL '0' MONTH PRECEDING) AS CNT_ROLLING_MONTHS,
    SUM(SUM(NVL(BCH.NET_SALES,0))) OVER(PARTITION BY BCH.ACCOUNT_NAME ORDER BY to_date(BCH.YEARMONTH || '01', 'yyyymmdd') RANGE BETWEEN INTERVAL '11' MONTH PRECEDING AND INTERVAL '0' MONTH PRECEDING) AS BRANCH_SLS_12M,
    SUM(SUM(NVL(BCH.NET_SALES,0))) OVER(PARTITION BY BCH.ACCOUNT_NAME ORDER BY to_date(BCH.YEARMONTH || '01', 'yyyymmdd') RANGE BETWEEN INTERVAL '0' MONTH PRECEDING AND INTERVAL '0' MONTH PRECEDING) AS BRANCH_SLS_TM
    
  
    
    from NORTH.BRANCH_CUSTOMER_HISTORY bch
    JOIN (SELECT FC123.ACCOUNT_NAME||FC123.MAIN_CUSTOMER_NK ZKEY, MAX(fc123.DOM_TYPE_II) DOM_TYPE_II, MAX(FC123.DIVISION_NAME) DIVISION_NAME, MAX(NVL(fc123.CREDIT_LIMIT,0)) CREDIT_LIMIT, MAX(FC123.OS_HA) OS_HA, MAX(FC123.PRICE_COLUMN) PRICE_COLUMN, MAX(FC123.FRIENDLY_PROFILE) FRIENDLY 
          FROM NORTH.FERGUSON_CUSTOMERS FC123
          WHERE FC123.CUSTOMER_TYPE not in ('0_BUYING_GROUP', 'CASH', 'E_EMPLOY', 'E_ENDUSER', 'E_ETAILER', 'EMPLOYEE', 'EXPORT', 'GOVT_AGENT', 'GOVT_STATE_EDUC', 'I_MISCELL', 'INDELECTRONIC', 'INDEQUIP', 'INDMISC', 'INDTRANSPOR', 'N_OEMFABDOM', 'N_OEMFABEXP', 'N_OEMMACDOM', 'N_OEMMACEXP', 'O_EXPORTER', 'O_FARM', 'O_OTHER', 'OTHER', 'T_CHURCH', 'U_REFUSE', 'UNKNOWN')
          GROUP BY FC123.ACCOUNT_NAME||FC123.MAIN_CUSTOMER_NK) FC ON FC.ZKEY = BCH.ACCOUNT_NAME||BCH.MAIN_CUSTOMER_NK
    join 
    (
          SELECT BCH.ACCOUNT_NAME||BCH.MAIN_CUSTOMER_NK AS ZKEY,
          CASE WHEN MAX(TO_NUMBER(NVL(bch.FIRST_S2S_ORDER,'0'))) = 0 THEN 0 ELSE 1 END EVER_S2S_CUST,
          CASE WHEN MAX(TO_NUMBER(NVL(BCH.FIRST_ORDER,'0'))) = 0 THEN 0 ELSE 1 END EVER_FOL_CUST,
          CASE WHEN MAX(TO_NUMBER(NVL(BCH.FIRST_ORDER,'0'))) + MAX(TO_NUMBER(NVL(bch.FIRST_S2S_ORDER,'0'))) = 0 THEN 0 ELSE 1 END EVER_EBIZ_CUST,
          COUNT(DISTINCT BCH.YEARMONTH) CNT_YM,
          SUM(BCH.NET_SALES) FEI_SLS,
          SUM(BCH.TOTAL_ORDERS) TOT_ORDERS,
          SUM(BCH.TOTAL_LINES) TOT_LINES
          FROM NORTH.BRANCH_CUSTOMER_HISTORY BCH
          WHERE bch.CUSTOMER_TYPE not in ('0_BUYING_GROUP', 'CASH', 'E_EMPLOY', 'E_ENDUSER', 'E_ETAILER', 'EMPLOYEE', 'EXPORT', 'GOVT_AGENT', 'GOVT_STATE_EDUC', 'I_MISCELL', 'INDELECTRONIC', 'INDEQUIP', 'INDMISC', 'INDTRANSPOR', 'N_OEMFABDOM', 'N_OEMFABEXP', 'N_OEMMACDOM', 'N_OEMMACEXP', 'O_EXPORTER', 'O_FARM', 'O_OTHER', 'OTHER', 'T_CHURCH', 'U_REFUSE', 'UNKNOWN')
          --AND BCH.YEARMONTH = 201511
          GROUP BY BCH.ACCOUNT_NAME||BCH.MAIN_CUSTOMER_NK
  
    ) Q on q.zkey = fc.zkey
    
    group by
    bch.YEARMONTH, FC.ZKEY, FC.DOM_TYPE_II, FC.DIVISION_NAME, 
    CASE 
    WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 2000 THEN 'LESS THAN 2K'
    WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 15000 THEN '2K TO 15K'
    WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 75000 THEN '15K TO 75K'
    ELSE 'OVER 75K'
    END, FC.OS_HA, Q.EVER_S2S_CUST, Q.EVER_FOL_CUST, Q.EVER_EBIZ_CUST,BCH.MON,FC.FRIENDLY, BCH.ACCOUNT_NAME, Q.CNT_YM
  ) A