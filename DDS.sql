DECLARE

  @MSO_IN_AREAS AS VARCHAR(MAX),
  @MSO_OUT_AREAS AS VARCHAR(MAX),
  @LY_REPLEN_AREAS AS VARCHAR(MAX),
  @CS_REPLEN_AREAS AS VARCHAR(MAX),
  @VNA_REPLEN_AREAS AS VARCHAR(MAX),
  @CASE_PICK_AREAS AS VARCHAR(MAX),
  @VNA_PICK_AREAS AS VARCHAR(MAX),
  @LAYER_PICK_AREAS AS VARCHAR(MAX),
  @MSO_RECEIPT_TRKREF AS VARCHAR(MAX),
  @MSO_TRKREF AS VARCHAR(MAX),
  @VNA_AREA_LIST_OUT AS VARCHAR(MAX),
  @MSO_OPRCOD_AREAS AS VARCHAR(MAX),
  @VNA_AREA_LIST AS VARCHAR(MAX),
  @CASE_ARECOD_OR AS VARCHAR(MAX),
  @CASE_ARECOD_OUT AS VARCHAR(MAX),
  @CASE_ARECOD_IN AS VARCHAR(MAX),
  @now DATETIME,
  @from DATETIME,
  -- added this
  @OUTBOUND_ORD_LINE AS VARCHAR(MAX),
	@OUTBOUND_SHIP_LINE AS VARCHAR(MAX),
	@INBOUND_RCV_LINE AS VARCHAR(MAX);
 
	


SET 
	@now = dbo.getdate2();
SET 
	@from = DATEADD(HOUR, -12, @now);
SET
  @MSO_IN_AREAS = 'HOFFMSOIN';
SET
  @MSO_OUT_AREAS = 'HOFFMSOOUT';
SET
  @LY_REPLEN_AREAS = 'NONE';
SET
  @CS_REPLEN_AREAS = 'PW-C-SL, Z0-C-SL';
SET
  @VNA_REPLEN_AREAS = 'PW-C-SL-E, Z0-C-SL-E';
SET
  @CASE_PICK_AREAS = 'PW-C-SL, Z0-C-SL';
SET
  @VNA_PICK_AREAS = 'PW-C-SL-E, Z0-C-SL-E';
SET
  @VNA_AREA_LIST = 'VNA-AREA-LIST';
SET
  @LAYER_PICK_AREAS = 'NONE';
SET
  @MSO_RECEIPT_TRKREF = 'GB01';
SET
  @MSO_TRKREF = 'DL01';
SET
  @MSO_OPRCOD_AREAS = 'WOPCK';
SET
  @CASE_ARECOD_OR = 'LEDO';
SET
  @CASE_ARECOD_OUT = 'HD-LY';
SET
  @CASE_ARECOD_IN = 'C-SL, B-PKVNA, C-DYNAMIC';
-- added this
SET 
	@OUTBOUND_ORD_LINE = 'prodwms.ord_line';
SET 
	@OUTBOUND_SHIP_LINE = 'prodwms.shipment_line';
SET 
	@INBOUND_RCV_LINE = 'prodwms.rcvlin';



SELECT TOP 1 wh.wh_id,
     -- ROUND(DECODE(ISNULL(mso.req_pals, 0), 0, 0, (ISNULL(mso.pck_pals, 0) / ISNULL(mso.req_pals, 0)) *100), 2) AS "MSO CFR%",
			ROUND (CASE WHEN COALESCE(mso.req_pals, 0)= 0 
				THEN 0
				ELSE (COALESCE(mso.pck_pals, 0) / COALESCE(mso.req_pals, 0)) *100
				END,
			2) AS [MSO CFR%],
			ISNULL(prc_ords.ontime_wkords, 0) AS [Proc ord cmplt ontime],
			ISNULL(prc_ords.ontime_wkords_prcnt, 0) AS [%prc ord ontime],
			ISNULL(load_trlr.cnt_car_moves, 0) AS [Shipments Loaded],
			ISNULL(ltl.ltl_ship_prv_day, 0) AS [LTL Shipments],
			ISNULL(loaded_loads.cnt_load_lods, 0) AS [Pallets Loaded],
			ISNULL(loaded_loads.cnt_load_cs, 0) AS [Cases Loaded],
			ISNULL(inb_trks_ret.inb_prc_trlr_returns, 0) AS [Returns Unloaded],
			ISNULL(inb_trks.inb_prc_trlr, 0) AS [Receipts Unloaded],
			ISNULL(unld_pals.cnt_unld_lods, 0) AS [Pallets Unloaded],
			ISNULL(unld_pals.unld_CASEs, 0) AS [Cases Unloaded],
			ISNULL(no_inb_show.no_show_inb, 0) AS [Inbound no shows],
			ISNULL(no_inb_show.late_show_inb, 0) AS [Inbound no shows - late],
			ISNULL(apal_scores.apal_scores, 0) AS [Zero APAL Scores],
			ISNULL(replen.time_to_comp_replen, 0) AS [Time Complet Replen],
			ISNULL(canpcks.can_rea_pcks, 0) AS [# Cancl AND re-allocat],
			--ISNULL(lost_pals.lost_pallets, 0) AS [# of lost pals],
			--ISNULL(lost_pals.wrked_lost_pallets, 0) AS [# of worked lost pals],
			ISNULL(lost_inv_del.lost_inv_del, 0) AS [# lost inv delet],
			ISNULL(inv_trn.tot_trans, 0) AS [# of Inv Transactions],
			ISNULL(cntbacks.cnt_audits, 0) AS [COUNTback Lock ups],
			ISNULL(cntbacks.cntbck_lock_up_prcnt, 0) AS [COUNTback Lock-up %],
			ISNULL(cs_pck.all_cs_pcks, 0) AS [Total Case Pick Cases],
			ISNULL(cs_pck.cs, 0) AS [Cases Picked - CP],
			ISNULL(cs_pck.vna_cs, 0) AS [Cases Picked - VNA],
			ISNULL(cs_pck.ly_cs, 0) AS [Cases Picked - LP],
			ISNULL(cs_pck.all_cs_pck_pals, 0) AS [Total Case Pick Pallets],
			ISNULL(cs_pck.cs_pals, 0) AS [CP Pallets],
			ISNULL(cs_pck.vna_pals, 0) AS [VNA Pallets],
			ISNULL(cs_pck.ly_pals, 0) AS [LP Pallets],
			--ISNULL(mso.pck_pals, 0) + ' / ' + ISNULL(mso.req_pals, 0) AS [MSO],	
			ISNULL(replen1.lp_rpln_pal, 0) AS [Replen Pallets - LY],
			ISNULL(replen1.lp_rpln_cs, 0) AS [Replen Cases - LY],
			ISNULL(replen1.cp_rpln_pal, 0) AS [Replen Pallets - CS],
			ISNULL(replen1.cp_rpln_cs, 0) AS [Replen Cases - CS],
			ISNULL(stg_pals.stg_pals, 0) AS [Staged Pallets],
			ISNULL(full_pal_shp.full_pal_shp, 0) AS [Full Pallets Loaded],
			ISNULL(trlr_moves.trlr_moves, 0) AS [Trailer Moves],
			ISNULL(brudi_pals.brudi_pals, 0) AS [Brudi Pallets],
			ISNULL(fr_mso.part_pals, 0) AS [Partials From MSO],
			ISNULL(fr_mso.full_pals, 0) AS [Full Pals From MSO],
			ISNULL(tblpckwrk_3.to_mso, 0) AS [To MSO]


			FROM prodwms.wh

			LEFT		--Line 117 to 132
					JOIN		
        /*Get work orders processed on time*/		
        (SELECT ISNULL(ontime_wkords, 0) AS ontime_wkords,		
                ISNULL(ROUND((ontime_wkords / tot_wkords) * 100, 2), 0) AS ontime_wkords_prcnt,		
                wh_id		
           FROM (SELECT COUNT('x') tot_wkords,		
                        SUM(CASE WHEN clsdte >= sch_enddte THEN 1		
                                 ELSE 0		
                            END) AS ontime_wkords,		
                        wh_id		
                   FROM prodwms.wkohdr		
                  WHERE sch_enddte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')		
                    AND sch_enddte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')	
                  GROUP BY wh_id
				  ) AS Sub
				   )AS prc_ords		
				   
     ON wh.wh_id = prc_ords.wh_id



	    
       LEFT	JOIN (				 -- Line 205 -242
	   SELECT COUNT(distinct rt.trknum) AS inb_prc_trlr,
                rt.wh_id
           FROM prodwms.rcvtrk rt
          WHERE rt.clsdte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND rt.clsdte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
            and rt.trknum not like '0015%'
            and rt.trkref != @MSO_TRKREF
          GROUP BY rt.wh_id
		  ) AS inb_trks
     ON wh.wh_id = inb_trks.wh_id
   LEFT
   JOIN (SELECT COUNT(distinct rt.trknum) AS inb_prc_trlr_returns,
                rt.wh_id
           FROM prodwms.rcvtrk rt
          WHERE rt.clsdte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND rt.clsdte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
            and rt.trknum like '0015%'
            and rt.trkref != @MSO_TRKREF
          GROUP BY rt.wh_id
		  ) AS inb_trks_ret
     ON wh.wh_id = inb_trks_ret.wh_id
   LEFT
   JOIN (SELECT COUNT(distinct cm.car_move_id) AS cnt_car_moves,
                cm.wh_id
           FROM prodwms.shipment s,
                prodwms.cardtl cd,
                prodwms.stop,
                prodwms.car_move cm,
                prodwms.trlr
          WHERE s.carcod = cd.carcod
            AND s.srvlvl = cd.srvlvl
            AND s.stop_id = stop.stop_id
            AND stop.car_move_id = cm.car_move_id
            and cm.trlr_id = trlr.trlr_id
            AND cd.cartyp not in ('S')
            AND trlr.close_dte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')	
            AND trlr.close_dte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY cm.wh_id
		  ) AS load_trlr
     ON wh.wh_id = load_trlr.wh_id

   

	   LEFT JOIN (			 -- 265 to 337
	   SELECT ISNULL(COUNT(DISTINCT cm.car_move_id), 0) AS ltl_ship_prv_day,
                s.wh_id
           FROM prodwms.shipment s,
                prodwms.cardtl cd,
                prodwms.stop,
                prodwms.car_move cm,
                prodwms.trlr
          WHERE s.carcod = cd.carcod
            AND s.srvlvl = cd.srvlvl
            AND s.stop_id = stop.stop_id
            AND stop.car_move_id = cm.car_move_id
            and cm.trlr_id = trlr.trlr_id
            AND cd.cartyp in ('L', 'X')
            AND trlr.close_dte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND trlr.close_dte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY s.wh_id
		  ) 
		  As ltl
     ON wh.wh_id = ltl.wh_id
   LEFT
   JOIN (SELECT COUNT(distinct dlytrn.lodnum) cnt_load_lods,
                CEILING(SUM((dlytrn.trnqty * 1.0) / pfv.untcas)) AS cnt_load_cs,
			dlytrn.wh_id
           FROM prodwms.dlytrn,
                prodwms.prtftp_view pfv,
                prodwms.aremst a
          WHERE dlytrn.wh_id = pfv.wh_id
            AND dlytrn.prtnum = pfv.prtnum
            AND dlytrn.prt_client_id = pfv.prt_client_id
            AND pfv.defftp_flg = 1
            AND dlytrn.to_arecod = a.arecod
            AND dlytrn.wh_id = a.wh_id
            AND a.shpflg = 1
            AND dlytrn.tostol like 'TRL%'
            AND actcod = 'TRLR_LOAD'
            AND trndte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND trndte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY dlytrn.wh_id
		  ) AS loaded_loads
     ON wh.wh_id = loaded_loads.wh_id


      LEFT
   JOIN (
   SELECT ISNULL(CEILING(SUM((trnqty * 1.0) / untcas)), 0) AS unld_CASEs,
        COUNT(DISTINCT lodnum) AS cnt_unld_lods,
                wh_id
        FROM
        (SELECT d.wh_id,
                d.trnqty,
               pfv.untcAS,
               d.lodnum,
               MAX(
                CASE 
                    WHEN CONVERT(VARCHAR(8), d.trndte, 108) < '06:00:00' 
                        THEN CAST(CONVERT(DATE, DATEADD(DAY, -1, d.trndte)) AS DATE)
                    ELSE CAST(CONVERT(DATE, d.trndte) AS DATE)
                END ) AS rptdt
          FROM prodwms.dlytrn d
        JOIN prodwms.prtftp_view pfv ON d.wh_id = pfv.wh_id 
                                    AND d.prtnum = pfv.prtnum 
                                    AND d.prt_client_id = pfv.prt_client_id
        JOIN prodwms.aremst a ON d.fr_arecod = a.arecod 
                              AND d.wh_id = a.wh_id
        WHERE pfv.defftp_flg = 1
          AND d.wh_id = pfv.wh_id
          AND (
              (a.expflg = 1 
               AND d.oprcod IN ('URC', 'UID') 
               AND d.actcod IN ('RCV', 'ASN_TRUST', 'ASN_NONTRUST'))
              OR 
              (d.oprcod IN ('URC', 'UID') 
               AND d.actcod IN ('CRSDCK', 'PCKSTEAL') 
               AND (d.fr_arecod LIKE 'RDTS%' OR d.fr_arecod LIKE 'EXPR%'))
          )
                AND trndte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
                AND trndte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
        GROUP BY d.wh_id,
              d.lodnum,
              d.trnqty,
              pfv.untcAS
			  ) as sub
 GROUP BY wh_id
 ) AS unld_pals
     ON wh.wh_id = unld_pals.wh_id


	    LEFT JOIN (			--460 to 503
			select wh_id,
                sum(no_show_inb) no_show_inb,
                sum(late_show_inb) late_show_inb
           from (SELECT rt.wh_id,
                        rt.trknum,
                        CASE WHEN (min(trlr.arrdte) >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00') or min(trlr.arrdte) is null) then 1
                             else 0
                        end as no_show_inb,
                        CASE WHEN min(trlr.arrdte) >= FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00') then 1
                             else 0
                        end as late_show_inb
                   FROM prodwms.rcvtrk rt
                   left
                   JOIN prodwms.trlr
                     ON rt.trlr_id = trlr.trlr_id
                  WHERE rt.expdte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
                    AND rt.expdte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
                   and rt.trkref != @MSO_TRKREF
                  GROUP BY wh_id,
                        rt.trknum
						) AS sub
          group by wh_id
		  ) AS no_inb_show
     ON wh.wh_id = no_inb_show.wh_id


   LEFT
   JOIN (SELECT COUNT(distinct cm.car_move_id) AS apal_scores,
                cm.wh_id
           FROM prodwms.car_move cm,
                prodwms.trlr
          WHERE cm.trlr_id = trlr.trlr_id
            and cm.vc_palletctl_flg = 1
            AND cm.vc_sol_score = 0
            AND trlr.close_dte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND trlr.close_dte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY cm.wh_id
		  ) AS apal_scores
     ON wh.wh_id = apal_scores.wh_id

   LEFT
   JOIN (
   
   SELECT ROUND(ISNULL(SUM(actual_seconds) * 1.0 / COUNT(*), 0) / 60, 2) AS time_to_comp_replen,
			 wh_id
           FROM prodwms.kvi_summary
          WHERE jobcodeid Like '%RPLN%'
            AND stop_time >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND stop_time < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY wh_id
		  ) AS replen
     ON wh.wh_id = replen.wh_id


	    LEFT JOIN (			--580 to 599
   SELECT COUNT('x') AS can_rea_pcks,
                wh_id
           FROM prodwms.canpck
          WHERE cancod IN (SELECT codval
                             FROM prodwms.cancod
                            WHERE reaflg = 1)
            AND cANDte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND cANDte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY wh_id
		  ) AS canpcks
     ON wh.wh_id = canpcks.wh_id

  

		/*LEFT JOIN (   -- need to optimize
    SELECT 
        SUM(CASE WHEN tostol LIKE '%LOST%' THEN 1 ELSE 0 END) AS lost_pallets,
        SUM(CASE WHEN frstol LIKE '%LOST%' THEN 1 ELSE 0 END) AS wrked_lost_pallets,
        wh_id
    FROM prodwms.dlytrn
    WHERE trndte >= FORMAT(DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00') 
      AND trndte < FORMAT(dbo.getdate2(), 'yyyy/MM/dd HH:00')
    GROUP BY wh_id
) AS lost_pals 
ON wh.wh_id = lost_pals.wh_id */

--optimized version (from 336)
LEFT JOIN (
    SELECT
        wh_id,
        SUM(CASE WHEN tostol LIKE '%LOST%' THEN 1 ELSE 0 END) AS lost_pallets,
        SUM(CASE WHEN frstol LIKE '%LOST%' THEN 1 ELSE 0 END) AS wrked_lost_pallets
    FROM prodwms.dlytrn
    WHERE trndte >= @from AND trndte < @now
    AND (tostol LIKE '%LOST%' OR frstol LIKE '%LOST%')
    GROUP BY wh_id
) AS lost_pals ON wh.wh_id = lost_pals.wh_id



	    LEFT JOIN (			--612 to 630
   SELECT count(distinct dlytrn.lodnum) lost_inv_del,
                dlytrn.wh_id
           FROM prodwms.dlytrn

		             WHERE actcod = 'INVDEL'
            AND oprcod = 'INVADJ'
            AND frstol like '%LOST%'
            AND trndte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND trndte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
            AND client_id is not null
          GROUP BY dlytrn.wh_id
		  ) AS lost_inv_del
     ON wh.wh_id = lost_inv_del.wh_id


	 			
	LEFT JOIN ( --631 to 714
    SELECT 
        ISNULL(a.cnt_del_loads, 0) + ISNULL(b.cnt_add_loads, 0) AS tot_trans,
        wh.wh_id
    FROM prodwms.wh
    LEFT JOIN (
        SELECT 
            COUNT(*) AS cnt_del_loads,
            wh_id
        FROM prodwms.dlytrn
        WHERE actcod = 'INVDEL'
          AND oprcod = 'INVADJ'
          AND trndte >= FORMAT(DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
          AND trndte < FORMAT(dbo.getdate2(), 'yyyy/MM/dd HH:00')
          AND client_id IS NOT NULL
        GROUP BY wh_id
    ) AS a ON wh.wh_id = a.wh_id

    LEFT JOIN (
        SELECT 
            COUNT(*) AS cnt_add_loads,
            wh_id
        FROM prodwms.dlytrn
        WHERE actcod = 'IDNTFY_AJ'
          AND oprcod = 'INVADJ'
          AND trndte >= FORMAT(DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
          AND trndte < FORMAT(dbo.getdate2(), 'yyyy/MM/dd HH:00')
        GROUP BY wh_id
    ) AS b ON wh.wh_id = b.wh_id
) AS inv_trn ON wh.wh_id = inv_trn.wh_id



   LEFT JOIN (
    SELECT 
        cnt_audits AS cnt_audits,
        CASE 
            WHEN CASE_picks = 0 THEN 0
            ELSE ROUND((cnt_audits * 100.0 / CASE_picks), 2)
        END AS cntbck_lock_up_prcnt,
        wh.wh_id
    FROM prodwms.wh



           LEFT
           JOIN (SELECT COUNT('x') AS cnt_audits,
                        wh_id
                   FROM prodwms.cnthst
                  WHERE cntdte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')	
                    AND cntdte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
                    AND cnttyp = 'CB'
                  GROUP BY wh_id
				  ) AS a
             ON wh.wh_id = a.wh_id


           LEFT
           JOIN (
		   select count('x') as case_picks,
                        wh_id
                   from prodwms.pckwrk_view
                  where list_id is not null
                    and lodlvl = 'S'
                    and appqty > 0
                    and pckdte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
                    AND pckdte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
                  GROUP BY wh_id
				  ) AS b
             ON wh.wh_id = b.wh_id
			 ) AS cntbacks  
     ON wh.wh_id = cntbacks.wh_id


   LEFT JOIN (
   select req_pals.wh_id,
                ISNULL(req_pals.req_pals, 0) req_pals,
               ISNULL(pck_pals.pck_pals, 0) pck_pals
           from (select sum(pals) req_pals,
                        wh_id
                   from ((select count(distinct pwv.wrkref) pals,
                                 pwv.wh_id
                            from prodwms.wkohdr
                           INNER
                            JOIN prodwms.pckwrk_view pwv
                              ON wkohdr.wkonum = pwv.wkonum
                             and wkohdr.wkorev = pwv.wkorev
                             and wkohdr.wh_id = pwv.wh_id
                             and wkohdr.client_id = pwv.client_id
                           WHERE pwv.list_id is null
                             and FORMAT(wkohdr.sch_begdte, 'yyyy/MM/dd') = FORMAT (dbo.getdate2(), 'yyyy/MM/dd')
                           GROUP BY pwv.wh_id)
                         union
                         (select count(distinct pwv.list_id) pals,
                                 pwv.wh_id
                            from prodwms.wkohdr
                           INNER
                            JOIN prodwms.pckwrk_view pwv
                              ON wkohdr.wkonum = pwv.wkonum
                             and wkohdr.wkorev = pwv.wkorev
                             and wkohdr.wh_id = pwv.wh_id
                             and wkohdr.client_id = pwv.client_id
                           WHERE pwv.list_id is not null
                             and FORMAT(wkohdr.sch_begdte, 'yyyy/MM/dd') = FORMAT (dbo.getdate2(), 'yyyy/MM/dd')
                           GROUP BY pwv.wh_id)) AS tabpckwrk
                  GROUP BY wh_id
				  ) AS req_pals

				  ---

				             LEFT JOIN (				-- 715 to 747
							 select count(distinct iv.lodnum) pck_pals,
                        iv.wh_id
                   from prodwms.wkohdr
                  INNER
                   JOIN prodwms.pckwrk_view pwv
                     ON wkohdr.wkonum = pwv.wkonum
                    and wkohdr.wkorev = pwv.wkorev
                    and wkohdr.wh_id = pwv.wh_id
                    and wkohdr.client_id = pwv.client_id
                  INNER
                   JOIN (select lodnum,
                                wrkref,
                                wh_id
                           from prodwms.inventory_view
                          where wrkref is not null
                          GROUP BY lodnum,
                                wrkref,
                                wh_id) iv
                     ON pwv.wrkref = iv.wrkref
                    and pwv.wh_id = iv.wh_id
                  INNER
                   JOIN prodwms.dlytrn
                     ON iv.lodnum = dlytrn.lodnum
                    AND iv.wh_id = dlytrn.wh_id
                  where FORMAT(wkohdr.sch_begdte, 'YYYY-MM-DD') = FORMAT(dbo.getdate2(), 'YYYY-MM-DD')
                    and dlytrn.trndte <= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
                    and dlytrn.trndte > FORMAT(DATEADD(DAY,-3,dbo.getdate2()), 'yyyy/MM/dd HH:00')
                    and dlytrn.to_arecod = @MSO_IN_AREAS
                    AND dlytrn.oprcod = @MSO_OPRCOD_AREAS
                  GROUP BY iv.wh_id
				  ) AS pck_pals
             on req_pals.wh_id = pck_pals.wh_id
			 ) AS mso
     ON wh.wh_id = mso.wh_id




				  --
				     LEFT JOIN (		-- 748 to 864
					 select pwv.wh_id,
                CEILING(SUM(pwv.dtl_appqty / pwv.untcas)) all_cs_pcks,
                Count(distinct pwv.list_id) AS all_cs_pck_pals,
                CEILING(SUM(CASE
							WHEN pwv.srcare = vna_arecod.vna_arecod THEN(pwv.dtl_appqty / pwv.untcas)
                              ELSE 0
                         END)) AS vna_cs,
                COUNT(distinct (CASE
									WHEN pwv.srcare = vna_arecod.vna_arecod 
									THEN pwv.list_id
                                     ELSE null
                                END)) AS vna_pals,
                ROUND(
					(CEILING(SUM(CASE
								WHEN pwv.srcare = vna_arecod.vna_arecod 
								THEN(pwv.dtl_appqty / pwv.untcas)
                                     ELSE 0
                                END)) * 100.0)
								/ CEILING(SUM(pwv.dtl_appqty / pwv.untcas)),
								2) AS vna_prcnt,

                CEILING(SUM(CASE
								WHEN pwv.srcare != ISNULL(vna_arecod.vna_arecod, 'a1z') 
									AND pwv.pck_uom = 'LY' 
									THEN(pwv.dtl_appqty / pwv.untcas)
                              ELSE 0
                         END)) AS ly_cs,

                COUNT(Distinct CASE 
								WHEN pwv.srcare <> ISNULL(vna_arecod.vna_arecod, 'a1z') 
								AND pwv.pck_uom = 'LY' THEN 
								pwv.list_id
                                     ELSE null
                                END) AS ly_pals,

                ROUND(
					(CEILING(SUM(CASE
								WHEN pwv.srcare != ISNULL(vna_arecod.vna_arecod, 'a1z')
								AND pwv.pck_uom = 'LY' THEN(pwv.dtl_appqty / pwv.untcas)
                                     ELSE 0
                                END)) * 100.0) / 
								CEILING(SUM(pwv.dtl_appqty / pwv.untcas)),
								2) AS ly_prcnt,


                CEILING(SUM(CASE 
							WHEN pwv.srcare <> ISNULL(vna_arecod.vna_arecod, 'a1z') 
								AND pftpd.cas_flg = 1 
								THEN(pwv.dtl_appqty / pwv.untcas)
                              ELSE 0
                         END)) AS cs,


                COUNT(Distinct CASE 
							WHEN pwv.srcare <> ISNULL(vna_arecod.vna_arecod, 'a1z') 
							AND pftpd.cas_flg = 1 
							THEN pwv.list_id
                                     ELSE null
                                END) AS cs_pals
           from Prodwms.pckwrk_view pwv


          INNER
           JOIN prodwms.prtftp_dtl pftpd
             ON pwv.wh_id = pftpd.wh_id
            and pwv.prtnum = pftpd.prtnum
            and pwv.prt_client_id = pftpd.prt_client_id
            and pwv.ftpcod = pftpd.ftpcod
            and pwv.pck_uom = pftpd.uomcod
			
			      LEFT -- need to update to site
           JOIN (select uc_param_val vna_arecod,
                        wh_id
                   from prodwms.uc_custom_parameters
                  where uc_param_var = @VNA_AREA_LIST) vna_arecod
             ON pwv.wh_id = vna_arecod.wh_id 


			           where pwv.lodlvl = 'S'
            and pwv.pck_uom is not null
            and pwv.appqty > 0
            and pwv.pckdte >= FORMAT(DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            and pwv.pckdte < FORMAT(dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY pwv.wh_id
		  ) as cs_pck
     ON wh.wh_id = cs_pck.wh_id

   

   LEFT JOIN (select count(distinct (CASE WHEN to_arecod = @CASE_ARECOD_OR or to_arecod = @CASE_ARECOD_OUT THEN lodnum
                                     ELSE NULL
                                END)) lp_rpln_pal,
                sum(CASE WHEN to_arecod = @CASE_ARECOD_OR or to_arecod = @CASE_ARECOD_OUT THEN trnqty / untcas
                         ELSE 0
                    END) lp_rpln_cs,
                count(distinct (CASE WHEN to_arecod in (@CASE_ARECOD_IN) THEN lodnum
                                     ELSE NULL
                                END)) cp_rpln_pal,
                sum(CASE WHEN to_arecod in (@CASE_ARECOD_IN) THEN trnqty / untcas

		                         ELSE 0
                    END) cp_rpln_cs,
                dlytrn.wh_id
           from prodwms.dlytrn,
                prodwms.prtftp_view pfv
          WHERE dlytrn.wh_id = pfv.wh_id
            AND dlytrn.prtnum = pfv.prtnum
            AND dlytrn.prt_client_id = pfv.prt_client_id
            AND pfv.defftp_flg = 1


			            and to_arecod in (@CASE_ARECOD_OR, @CASE_ARECOD_IN)  


					              and actcod != 'PL_XFR' and fr_arecod = 'RDTS'
            and trndte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND trndte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY dlytrn.wh_id) replen1
     ON wh.wh_id = replen1.wh_id
   
   
   LEFT
   JOIN (select count(distinct lodnum) stg_pals,
                dlytrn.wh_id
           from prodwms.dlytrn,
                prodwms.aremst a
          where dlytrn.to_arecod = a.arecod
            and dlytrn.wh_id = a.wh_id
            and a.stgflg = 1
            and oprcod = 'PCK'
            and trndte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND trndte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY dlytrn.wh_id
		  ) AS stg_pals
     on wh.wh_id = stg_pals.wh_id


   LEFT JOIN (
    SELECT 
        SUM(cnt) AS full_pal_shp,
        wh_id
    FROM (
        SELECT 
            1 AS cnt,
            dlytrn.lodnum,
            dlytrn.wh_id
        FROM 
            prodwms.dlytrn
        JOIN 
            prodwms.prtftp_view pfv 
            ON dlytrn.wh_id = pfv.wh_id 
            AND dlytrn.prtnum = pfv.prtnum 
            AND dlytrn.prt_client_id = pfv.prt_client_id
        WHERE 
            pfv.defftp_flg = 1
            AND dlytrn.actcod = 'TRLR_LOAD'
            AND dlytrn.trndte >= FORMAT(DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND dlytrn.trndte < FORMAT(dbo.getdate2(), 'yyyy/MM/dd HH:00')
        GROUP BY 
            dlytrn.lodnum,
            dlytrn.wh_id
        HAVING 
            MAX(pfv.untpal) = MAX(dlytrn.trnqty)
    ) AS inner_q
    GROUP BY 
        wh_id
) AS full_pal_shp ON wh.wh_id = full_pal_shp.wh_id


   LEFT JOIN (			
   select count('x') trlr_moves,
                wh_id
           from prodwms.trlract
          where actcod = 'TMOVE'
            and trndte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND trndte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
          GROUP BY wh_id
		  ) AS trlr_moves
     ON wh.wh_id = trlr_moves.wh_id


	    LEFT JOIN (				--889 to 910
		select wh_id,
                sum(brudi_pals) brudi_pals
           from ((select count(distinct list_id) brudi_pals,
                         wh_id
                    from prodwms.pckwrk_view
                   where asset_typ != 'CHEP'
                     and lodlvl != 'L'
                     and pckdte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
                     AND pckdte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
                   group by wh_id
				   ) union
                 (select count(distinct wrkref) brudi_pals,
                         wh_id
                    from prodwms.pckwrk_view
                   where asset_typ != 'CHEP'
                     and lodlvl = 'L'
                     and pckdte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
                     AND pckdte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
                   GROUP BY wh_id)) as tblpckwrk_1
          GROUP BY wh_id
		  ) as brudi_pals
     ON wh.wh_id = brudi_pals.wh_id


	    LEFT JOIN (				-- 911 to 975
			select wh_id,
                count(distinct lodnum) AS fr_mso,
                count(distinct part_pals) AS part_pals,
                count(distinct full_pals) AS full_pals
           from (
					select dlytrn.wh_id,
                         dlytrn.lodnum,
                         case 
							when sum(dlytrn.trnqty) != max(pfv.untpal) then dlytrn.lodnum
                              else null
                         end part_pals,
                         case 
							when sum(dlytrn.trnqty) = max(pfv.untpal) then dlytrn.lodnum
                              else null
                         end full_pals
                    from prodwms.dlytrn
                   INNER
                    JOIN prodwms.prtftp_view pfv
                      ON dlytrn.wh_id = pfv.wh_id
                     and dlytrn.prtnum = pfv.prtnum
                     and dlytrn.prt_client_id = pfv.prt_client_id
                     and pfv.defftp_flg = 1
                   where fr_arecod = @MSO_OUT_AREAS
                     and actcod != 'ATELOCCLRAUTO'
                     and trndte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
                     AND trndte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
                   GROUP BY dlytrn.wh_id,
                         dlytrn.lodnum
                 union

					select dlytrn.wh_id,
                         dlytrn.lodnum,
                         case when sum(dlytrn.trnqty) != max(pfv.untpal) then dlytrn.lodnum
                              else null
                         end part_pals,
                         case when sum(dlytrn.trnqty) = max(pfv.untpal) then dlytrn.lodnum
                              else null
                         end full_pals
                    from prodwms.dlytrn

             INNER JOIN prodwms.prtftp_view pfv
                      ON dlytrn.wh_id = pfv.wh_id
                     and dlytrn.prtnum = pfv.prtnum
                     and dlytrn.prt_client_id = pfv.prt_client_id
                     and pfv.defftp_flg = 1
                   where fr_arecod = @MSO_OUT_AREAS
                     and actcod != 'ATELOCCLRAUTO'
                     and trndte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
                     AND trndte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
                     AND actcod = 'UPCK'
                   GROUP BY dlytrn.wh_id,
                         dlytrn.lodnum
							) AS tblprtftp_3
					GROUP BY wh_id
						) AS fr_mso
					ON wh.wh_id = fr_mso.wh_id

 -- Change here

        LEFT JOIN (
            SELECT DISTINCT 
                ISNULL(pwv.wh_id, '') AS wh_id,
                ISNULL(a.bldg_id, '') AS bldg_id,
                ISNULL(pwv.client_id, '') AS original_client_id,
                ISNULL(COALESCE(ol.client_id, sl.client_id, rl.client_id, pwv.client_id), '') AS final_client_id,

                -- Inject your client_id logic here
                CASE 
                    WHEN a.bldg_id = 'B1' THEN '1707'
                    WHEN a.bldg_id = 'BZ0' THEN 'C411'
                    WHEN a.bldg_id = 'BPW' THEN 'D180'
                    ELSE '----'
                END AS mapped_client_id

            FROM prodwms.pckwrk_view pwv
            LEFT JOIN prodwms.aremst a 
                ON pwv.wh_id = a.wh_id
            LEFT JOIN prodwms.ord_line ol 
                ON pwv.client_id = ol.client_id 
                AND pwv.wh_id = ol.wh_id
            LEFT JOIN prodwms.shipment_line sl 
                ON pwv.client_id = sl.client_id 
                AND pwv.wh_id = sl.wh_id
            LEFT JOIN prodwms.rcvlin rl 
                ON pwv.client_id = rl.client_id 
                AND pwv.wh_id = rl.wh_id
            WHERE pwv.pckdte >= @from
              AND pwv.pckdte < @now
        ) AS client_bldg_info
        ON wh.wh_id = client_bldg_info.wh_id


 -- End Here

   LEFT JOIN (
			select wh_id,
                count(distinct lodnum) to_mso
           from prodwms.dlytrn
          WHERE to_arecod IN (SELECT value FROM STRING_SPLIT(@MSO_IN_AREAS, ',')) --where to_arecod = 'HOFFMSOIN' //prev
            and trndte >= FORMAT (DATEADD(HOUR, -12, dbo.getdate2()), 'yyyy/MM/dd HH:00')
            AND trndte < FORMAT (dbo.getdate2(), 'yyyy/MM/dd HH:00')
            and exists(select 'x'
                         from prodwms.pckwrk_view pwv
                        where dlytrn.lodnum = pwv.pallet_id
                          and dlytrn.wh_id = pwv.wh_id
                          and pwv.wkonum is not null)
          GROUP BY wh_id) AS tblpckwrk_3
     ON wh.wh_id = tblpckwrk_3.wh_id
     
     
END
GO