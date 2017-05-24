SELECT
C.YEARMONTH, C.ZKEY, C.DOM_TYPE_II, C.DIVISION_NAME, C.CREDIT_BIN, C.OS_HA, C.EVER_S2S_CUST, C.ACCOUNT_NAME, 
C.PROPLUS, C.PROPLUS_CURRENT_CUST,
C.EVER_FOL_CUST, C.EVER_EBIZ_CUST, C.MON, C.FRIENDLY, C.S2S_CURRENT_CUST, C.FOL_CURRENT_CUST, 
C.EBIZ_CURRENT_CUST, C.FIRST_FEI_ORDER, C.FIRST_S2S_ORDER, C.FIRST_FOL_ORDER, C.FIRST_EBIZ_ORDER, 
C.CUST_SLS_TM, C.CUST_GP_TM, C.CUST_JOB_SLS, C.CNT_ROLLING_MONTHS, C.BRANCH_SLS_TM, C.BRANCH_GP_TM, 
C.CNT_YM, C.TREND, 
C.EBIZ_CURRENT_CUST*(1 + C.TREND - C.EBIZ_TREND) AS EBIZ_TREND,
C.FOL_CURRENT_CUST*(1 + C.TREND - C.FOL_TREND) AS FOL_TREND,
C.S2S_CURRENT_CUST*(1 + C.TREND - C.S2S_TREND) AS S2S_TREND,
C.PROPLUS_CURRENT_CUST*(1 + C.TREND - C.PP_TREND) AS PP_TREND
--C.MAX_TREND,
--C.FILTER_ALWAYS_EBIZ

FROM
(
  SELECT
  B.YEARMONTH, B.ZKEY, B.DOM_TYPE_II, B.DIVISION_NAME, B.CREDIT_BIN, B.OS_HA, B.EVER_S2S_CUST, B.ACCOUNT_NAME, 
  B.PROPLUS, B.PROPLUS_CURRENT_CUST,
  B.EVER_FOL_CUST, B.EVER_EBIZ_CUST, B.MON, B.FRIENDLY, B.S2S_CURRENT_CUST, B.FOL_CURRENT_CUST, 
  B.EBIZ_CURRENT_CUST, B.FIRST_FEI_ORDER, B.FIRST_S2S_ORDER, B.FIRST_FOL_ORDER, B.FIRST_EBIZ_ORDER, 
  B.CUST_SLS_TM, B.CUST_GP_TM, B.CUST_JOB_SLS, B.CNT_ROLLING_MONTHS, B.BRANCH_SLS_TM, B.BRANCH_GP_TM, 
  B.CNT_YM, B.TREND,
  MAX(MAX(case when B.FIRST_EBIZ_ORDER = B.YEARMONTH THEN B.TREND ELSE 0 END)) OVER(PARTITION BY B.ZKEY) EBIZ_TREND,
  MAX(MAX(case when B.FIRST_FOL_ORDER = B.YEARMONTH THEN B.TREND ELSE 0 END)) OVER(PARTITION BY B.ZKEY) FOL_TREND,
  MAX(MAX(case when B.FIRST_S2S_ORDER = B.YEARMONTH THEN B.TREND ELSE 0 END)) OVER(PARTITION BY B.ZKEY) S2S_TREND,
  MAX(MAX(case when B.PP_YM = B.YEARMONTH THEN B.TREND ELSE 0 END)) OVER(PARTITION BY B.ZKEY) PP_TREND,
  MAX(MAX(B.TREND)) OVER(PARTITION BY B.ZKEY) MAX_TREND,
  MAX(MAX(CASE 
          WHEN B.TREND = 1 and B.EBIZ_CURRENT_CUST = 1 THEN 1
          WHEN B.TREND = 2 and B.EBIZ_CURRENT_CUST = 1 THEN 1
          ELSE 0
          END)) OVER(PARTITION BY B.ZKEY) FILTER_ALWAYS_EBIZ
  
  FROM
  (
    Select
    A.YEARMONTH, A.ZKEY, A.DOM_TYPE_II, A.DIVISION_NAME, A.CREDIT_BIN, A.OS_HA, A.EVER_S2S_CUST, a.ACCOUNT_NAME,
    A.PROPLUS,A.PROPLUS_CURRENT_CUST,A.PP_YM,
    A.EVER_FOL_CUST, 
    CASE WHEN A.EVER_S2S_CUST + A.EVER_FOL_CUST > 0 THEN 1 ELSE 0 END AS EVER_EBIZ_CUST, 
    A.MON, A.FRIENDLY,
    CASE WHEN A.FIRST_S2S_ORDER <= A.YEARMONTH THEN 1 ELSE 0 END S2S_CURRENT_CUST,
    CASE WHEN A.FIRST_FOL_ORDER <= A.YEARMONTH THEN 1 ELSE 0 END FOL_CURRENT_CUST,
    CASE WHEN A.FIRST_EBIZ_ORDER <= A.YEARMONTH THEN 1 ELSE 0 END EBIZ_CURRENT_CUST,
 
    A.FIRST_FEI_ORDER, A.FIRST_S2S_ORDER,A.FIRST_FOL_ORDER,A.FIRST_EBIZ_ORDER, A.CUST_SLS_TM, 
    A.CUST_SLS_TM - CUST_COGS_TM AS CUST_GP_TM,
    A.CUST_JOB_SLS, A.CNT_ROLLING_MONTHS,
    --A.BRANCH_SLS_TM,
    --A.BRANCH_SLS_TM - A.BRANCH_COGS_TM AS BRANCH_GP_TM,
    BRANCH_SLS.TOT_SLS AS BRANCH_SLS_TM,
    BRANCH_SLS.GP AS BRANCH_GP_TM,
    A.CNT_YM,
    ROW_NUMBER() OVER(PARTITION BY A.ZKEY ORDER BY A.YEARMONTH ASC) AS TREND
    
    
    from
    (
      Select bch.YEARMONTH, FC.ZKEY, FC.DOM_TYPE_II, FC.DIVISION_NAME, BCH.ACCOUNT_NAME,
      CASE 
      WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 2000 THEN 'LESS THAN 2K'
      WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 15000 THEN '2K TO 15K'
      WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 75000 THEN '15K TO 75K'
      ELSE 'OVER 75K'
      END CREDIT_BIN, FC.OS_HA, 
      
      max(CASE WHEN BCH.PRO_PLUS_ACCEPTANCE_DATE IS NOT NULL THEN 1 ELSE 0 END) PROPLUS,
      max(TO_CHAR(BCH.PRO_PLUS_ACCEPTANCE_DATE,'YYYYMM')) PP_YM,
      max(CASE WHEN TO_CHAR(BCH.PRO_PLUS_ACCEPTANCE_DATE,'YYYYMM') <= BCH.YEARMONTH THEN 1 ELSE 0 END) PROPLUS_CURRENT_CUST,
      CASE WHEN MAX(MAX(TO_NUMBER(NVL(bch.FIRST_S2S_ORDER,'0')))) OVER(PARTITION BY FC.ZKEY) = 0 THEN 0 ELSE 1 END EVER_S2S_CUST,
      CASE WHEN MAX(MAX(TO_NUMBER(NVL(BCH.FIRST_ORDER,'0')))) OVER(PARTITION BY FC.ZKEY) = 0 THEN 0 ELSE 1 END EVER_FOL_CUST,
      SUM(COUNT(DISTINCT BCH.YEARMONTH)) OVER(PARTITION BY FC.ZKEY) CNT_YM,
      
      BCH.MON, FC.FRIENDLY, 
      MIN(MIN(CASE WHEN BCH.TOTAL_ORDERS  > 0 THEN TO_NUMBER(BCH.YEARMONTH) else 999999 end)) over(partition by FC.ZKEY) FIRST_FEI_ORDER,
      min(min(TO_NUMBER(NVL(bch.FIRST_S2S_ORDER,'999999')))) over(partition by FC.ZKEY) FIRST_S2S_ORDER,
      min(min(TO_NUMBER(NVL(BCH.FIRST_ORDER,'999999')))) over(partition by FC.ZKEY) FIRST_FOL_ORDER,
      min(min(TO_NUMBER(
        CASE WHEN NVL(BCH.FIRST_ORDER,'999999') = '999999' AND NVL(bch.FIRST_S2S_ORDER,'999999') = '999999' THEN '999999'
        WHEN NVL(BCH.FIRST_ORDER,'999999') > NVL(bch.FIRST_S2S_ORDER,'999999') THEN NVL(bch.FIRST_S2S_ORDER,'999999') 
        ELSE NVL(BCH.FIRST_ORDER,'999999') end
      ))) over(partition by FC.ZKEY) FIRST_EBIZ_ORDER,
      sum(NVL(bch.NET_SALES,0)) CUST_SLS_TM,
      sum(NVL(bch.ALL_COGS,0)) CUST_COGS_TM,
      SUM(NVL(BCH.NET_JOB_SALES,0)) CUST_JOB_SLS,
      SUM(COUNT(DISTINCT CASE WHEN NVL(BCH.TOTAL_ORDERS,0) > 0 THEN BCH.YEARMONTH END)) OVER(PARTITION BY FC.ZKEY ORDER BY to_date(BCH.YEARMONTH || '01', 'yyyymmdd') RANGE BETWEEN INTERVAL '100' MONTH PRECEDING AND INTERVAL '0' MONTH PRECEDING) AS CNT_ROLLING_MONTHS
      --SUM(SUM(NVL(BCH.NET_SALES,0))) OVER(PARTITION BY BCH.ACCOUNT_NAME, bch.YEARMONTH) AS BRANCH_SLS_TM,
      --SUM(SUM(NVL(bch.ALL_COGS,0))) OVER(PARTITION BY BCH.ACCOUNT_NAME, bch.YEARMONTH) AS BRANCH_COGS_TM
    
      
      from NORTH.BRANCH_CUSTOMER_HISTORY bch
      JOIN (
              SELECT FC123.ACCOUNT_NAME||FC123.MAIN_CUSTOMER_NK ZKEY, MAX(fc123.DOM_TYPE_II) DOM_TYPE_II, MAX(FC123.DIVISION_NAME) DIVISION_NAME, MAX(NVL(fc123.CREDIT_LIMIT,0)) CREDIT_LIMIT, MAX(FC123.OS_HA) OS_HA, MAX(FC123.PRICE_COLUMN) PRICE_COLUMN, MAX(FC123.FRIENDLY_PROFILE) FRIENDLY 
              FROM NORTH.FERGUSON_CUSTOMERS FC123
              JOIN AAI8260.MATCHED_ZKEYS_EBIZ@DWFEI MZK ON MZK.ZKEY = FC123.ACCOUNT_NAME||FC123.MAIN_CUSTOMER_NK
              
              WHERE FC123.CUSTOMER_TYPE not in ('0_BUYING_GROUP', 'CASH', 'E_EMPLOY', 'E_ENDUSER', 'E_ETAILER', 'EMPLOYEE', 'EXPORT', 'GOVT_AGENT', 'GOVT_STATE_EDUC', 'I_MISCELL', 'INDELECTRONIC', 'INDEQUIP', 'INDMISC', 'INDTRANSPOR', 'N_OEMFABDOM', 'N_OEMFABEXP', 'N_OEMMACDOM', 'N_OEMMACEXP', 'O_EXPORTER', 'O_FARM', 'O_OTHER', 'OTHER', 'T_CHURCH', 'U_REFUSE', 'UNKNOWN')
              --and FC123.ACCOUNT_NAME||FC123.MAIN_CUSTOMER_NK = 'AUSTINHVAC10005'
              
              AND MZK.RN1 < .25
            --AND MZK.RN2 < .25
            --AND MZK.RN3 < .25
            --AND MZK.RN4 < .25
            --AND MZK.RN5 < .25
            --AND MZK.RN6 < .25
            --AND MZK.RN7 < .25
            --AND MZK.RN8 < .25
            --AND MZK.RN9 < .25
            --AND MZK.RN10 < .25	
          
              
              GROUP BY FC123.ACCOUNT_NAME||FC123.MAIN_CUSTOMER_NK
            ) FC ON FC.ZKEY = BCH.ACCOUNT_NAME||BCH.MAIN_CUSTOMER_NK
      
      WHERE NVL(bch.NET_SALES,0) >= 1
      and NVL(bch.ALL_COGS,0) >= 1
      and NVL(bch.NET_SALES,0) > NVL(bch.ALL_COGS,0)
      --and bch.ACCOUNT_NAME||bch.MAIN_CUSTOMER_NK = 'AUSTINHVAC10005'
      
      group by
      bch.YEARMONTH, FC.ZKEY, FC.DOM_TYPE_II, FC.DIVISION_NAME, BCH.ACCOUNT_NAME,
      CASE 
      WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 2000 THEN 'LESS THAN 2K'
      WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 15000 THEN '2K TO 15K'
      WHEN TO_NUMBER(FC.CREDIT_LIMIT) < 75000 THEN '15K TO 75K'
      ELSE 'OVER 75K'
      END, FC.OS_HA, BCH.MON,FC.FRIENDLY, BCH.ACCOUNT_NAME
    ) A
    JOIN
    (
        select
        BCH.YEARMONTH,
        BCH.ACCOUNT_NAME,
        SUM(BCH.NET_SALES) TOT_SLS,
        SUM(BCH.NET_SALES - BCH.ALL_COGS) GP
        from NORTH.BRANCH_CUSTOMER_HISTORY BCH
        WHERE NVL(bch.NET_SALES,0) >= 1
        and NVL(bch.ALL_COGS,0) >= 1
        and NVL(bch.NET_SALES,0) > NVL(bch.ALL_COGS,0)
        GROUP BY
        BCH.YEARMONTH,
        BCH.ACCOUNT_NAME
    ) BRANCH_SLS ON A.ACCOUNT_NAME = BRANCH_SLS.ACCOUNT_NAME AND A.YEARMONTH = BRANCH_SLS.YEARMONTH
    
    WHERE A.CUST_SLS_TM >= 1

  ) B
  GROUP BY
  B.YEARMONTH, B.ZKEY, B.DOM_TYPE_II, B.DIVISION_NAME, B.CREDIT_BIN, B.OS_HA, B.EVER_S2S_CUST, B.ACCOUNT_NAME,
  B.PROPLUS, B.PROPLUS_CURRENT_CUST,
  B.EVER_FOL_CUST, B.EVER_EBIZ_CUST, B.MON, B.FRIENDLY, B.S2S_CURRENT_CUST, B.FOL_CURRENT_CUST, 
  B.EBIZ_CURRENT_CUST, B.FIRST_FEI_ORDER, B.FIRST_S2S_ORDER, B.FIRST_FOL_ORDER, B.FIRST_EBIZ_ORDER, 
  B.CUST_SLS_TM, B.CUST_GP_TM, B.CUST_JOB_SLS, B.CNT_ROLLING_MONTHS, B.BRANCH_SLS_TM, B.BRANCH_GP_TM, 
  B.CNT_YM, B.TREND
) C

WHERE C.MAX_TREND > 5
AND C.FILTER_ALWAYS_EBIZ < 1


