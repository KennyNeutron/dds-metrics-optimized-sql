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
  @OUTBOUND_ORD_LINE AS VARCHAR(MAX),
  @OUTBOUND_SHIP_LINE AS VARCHAR(MAX),
  @INBOUND_RCV_LINE AS VARCHAR(MAX);

SET @now = dbo.getdate2();
SET @from = DATEADD(HOUR, -12, @now);
SET @MSO_IN_AREAS = 'HOFFMSOIN';
SET @MSO_OUT_AREAS = 'HOFFMSOOUT';
SET @LY_REPLEN_AREAS = 'NONE';
SET @CS_REPLEN_AREAS = 'PW-C-SL, Z0-C-SL';
SET @VNA_REPLEN_AREAS = 'PW-C-SL-E, Z0-C-SL-E';
SET @CASE_PICK_AREAS = 'PW-C-SL, Z0-C-SL';
SET @VNA_PICK_AREAS = 'PW-C-SL-E, Z0-C-SL-E';
SET @VNA_AREA_LIST = 'VNA-AREA-LIST';
SET @LAYER_PICK_AREAS = 'NONE';
SET @MSO_RECEIPT_TRKREF = 'GB01';
SET @MSO_TRKREF = 'DL01';
SET @MSO_OPRCOD_AREAS = 'WOPCK';
SET @CASE_ARECOD_OR = 'LEDO';
SET @CASE_ARECOD_OUT = 'HD-LY';
SET @CASE_ARECOD_IN = 'C-SL, B-PKVNA, C-DYNAMIC';
SET @OUTBOUND_ORD_LINE = 'prodwms.ord_line';
SET @OUTBOUND_SHIP_LINE = 'prodwms.shipment_line';
SET @INBOUND_RCV_LINE = 'prodwms.rcvlin';

SELECT TOP 1 
    base.wh_id,
    ROUND(CASE WHEN COALESCE(mso.req_pals, 0) = 0 THEN 0
               ELSE (COALESCE(mso.pck_pals, 0) / COALESCE(mso.req_pals, 0)) * 100
          END, 2) AS [MSO CFR%],
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
    ISNULL(lost_pals.lost_pallets, 0) AS [# of lost pals],
    ISNULL(lost_pals.wrked_lost_pallets, 0) AS [# of worked lost pals],
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

FROM (
    SELECT wh_id, bldg_id,
    CASE 
        WHEN bldg_id = 'B1' THEN '1707'
        WHEN bldg_id = 'BPW' THEN 'D180'
        WHEN bldg_id = 'BZ0' THEN 'C411'
        ELSE '----'
    END client_id
    FROM prodwms.bldg_mst 
    GROUP BY wh_id, bldg_id
) base

LEFT JOIN (
    SELECT ISNULL(ontime_wkords, 0) AS ontime_wkords,
           ISNULL(ROUND((ontime_wkords / tot_wkords) * 100, 2), 0) AS ontime_wkords_prcnt,
           wh_id
    FROM (
        SELECT COUNT('x') tot_wkords,
               SUM(CASE WHEN clsdte >= sch_enddte THEN 1 ELSE 0 END) AS ontime_wkords,
               wh_id
        FROM prodwms.wkohdr
        WHERE sch_enddte >= @from AND sch_enddte < @now
        GROUP BY wh_id
    ) AS Sub
) AS prc_ords ON base.wh_id = prc_ords.wh_id

LEFT JOIN (
    SELECT COUNT(distinct rt.trknum) AS inb_prc_trlr, rt.wh_id
    FROM prodwms.rcvtrk rt
    WHERE rt.clsdte >= @from AND rt.clsdte < @now
      AND rt.trknum NOT LIKE '0015%'
      AND rt.trkref != @MSO_TRKREF
    GROUP BY rt.wh_id
) AS inb_trks ON base.wh_id = inb_trks.wh_id

LEFT JOIN (
    SELECT COUNT(distinct rt.trknum) AS inb_prc_trlr_returns, rt.wh_id
    FROM prodwms.rcvtrk rt
    WHERE rt.clsdte >= @from AND rt.clsdte < @now
      AND rt.trknum LIKE '0015%'
      AND rt.trkref != @MSO_TRKREF
    GROUP BY rt.wh_id
) AS inb_trks_ret ON base.wh_id = inb_trks_ret.wh_id

LEFT JOIN (
    SELECT COUNT(distinct cm.car_move_id) AS cnt_car_moves, cm.wh_id
    FROM prodwms.shipment s
    INNER JOIN prodwms.cardtl cd ON s.carcod = cd.carcod AND s.srvlvl = cd.srvlvl
    INNER JOIN prodwms.stop ON s.stop_id = stop.stop_id
    INNER JOIN prodwms.car_move cm ON stop.car_move_id = cm.car_move_id
    INNER JOIN prodwms.trlr ON cm.trlr_id = trlr.trlr_id
    WHERE cd.cartyp NOT IN ('S')
      AND trlr.close_dte >= @from AND trlr.close_dte < @now
    GROUP BY cm.wh_id
) AS load_trlr ON base.wh_id = load_trlr.wh_id

LEFT JOIN (
    SELECT COUNT(DISTINCT cm.car_move_id) AS ltl_ship_prv_day, s.wh_id
    FROM prodwms.shipment s
    INNER JOIN prodwms.cardtl cd ON s.carcod = cd.carcod AND s.srvlvl = cd.srvlvl
    INNER JOIN prodwms.stop ON s.stop_id = stop.stop_id
    INNER JOIN prodwms.car_move cm ON stop.car_move_id = cm.car_move_id
    INNER JOIN prodwms.trlr ON cm.trlr_id = trlr.trlr_id
    WHERE cd.cartyp IN ('L', 'X')
      AND trlr.close_dte >= @from AND trlr.close_dte < @now
    GROUP BY s.wh_id
) AS ltl ON base.wh_id = ltl.wh_id

LEFT JOIN (
    SELECT COUNT(distinct dlytrn.lodnum) cnt_load_lods,
           CEILING(SUM((dlytrn.trnqty * 1.0) / pfv.untcas)) AS cnt_load_cs,
           dlytrn.wh_id
    FROM prodwms.dlytrn
    INNER JOIN prodwms.prtftp_view pfv ON dlytrn.wh_id = pfv.wh_id 
        AND dlytrn.prtnum = pfv.prtnum 
        AND dlytrn.prt_client_id = pfv.prt_client_id
    INNER JOIN prodwms.aremst a ON dlytrn.to_arecod = a.arecod AND dlytrn.wh_id = a.wh_id
    WHERE pfv.defftp_flg = 1
      AND a.shpflg = 1
      AND dlytrn.tostol LIKE 'TRL%'
      AND actcod = 'TRLR_LOAD'
      AND trndte >= @from AND trndte < @now
    GROUP BY dlytrn.wh_id
) AS loaded_loads ON base.wh_id = loaded_loads.wh_id

LEFT JOIN (
    SELECT CEILING(SUM((trnqty * 1.0) / untcas)) AS unld_CASEs,
           COUNT(DISTINCT lodnum) AS cnt_unld_lods,
           wh_id
    FROM (
        SELECT d.wh_id, d.trnqty, pfv.untcAS, d.lodnum
        FROM prodwms.dlytrn d
        INNER JOIN prodwms.prtftp_view pfv ON d.wh_id = pfv.wh_id 
            AND d.prtnum = pfv.prtnum 
            AND d.prt_client_id = pfv.prt_client_id
        INNER JOIN prodwms.aremst a ON d.fr_arecod = a.arecod AND d.wh_id = a.wh_id
        WHERE pfv.defftp_flg = 1
          AND ((a.expflg = 1 AND d.oprcod IN ('URC', 'UID') AND d.actcod IN ('RCV', 'ASN_TRUST', 'ASN_NONTRUST'))
               OR (d.oprcod IN ('URC', 'UID') AND d.actcod IN ('CRSDCK', 'PCKSTEAL') 
                   AND (d.fr_arecod LIKE 'RDTS%' OR d.fr_arecod LIKE 'EXPR%')))
          AND trndte >= @from AND trndte < @now
    ) as sub
    GROUP BY wh_id
) AS unld_pals ON base.wh_id = unld_pals.wh_id

LEFT JOIN (
    SELECT wh_id, SUM(no_show_inb) no_show_inb, SUM(late_show_inb) late_show_inb
    FROM (
        SELECT rt.wh_id, rt.trknum,
               CASE WHEN (MIN(trlr.arrdte) >= @from OR MIN(trlr.arrdte) IS NULL) THEN 1 ELSE 0 END as no_show_inb,
               CASE WHEN MIN(trlr.arrdte) >= @now THEN 1 ELSE 0 END as late_show_inb
        FROM prodwms.rcvtrk rt
        LEFT JOIN prodwms.trlr ON rt.trlr_id = trlr.trlr_id
        WHERE rt.expdte >= @from AND rt.expdte < @now AND rt.trkref != @MSO_TRKREF
        GROUP BY wh_id, rt.trknum
    ) AS sub
    GROUP BY wh_id
) AS no_inb_show ON base.wh_id = no_inb_show.wh_id

LEFT JOIN (
    SELECT COUNT(distinct cm.car_move_id) AS apal_scores, cm.wh_id
    FROM prodwms.car_move cm
    INNER JOIN prodwms.trlr ON cm.trlr_id = trlr.trlr_id
    WHERE cm.vc_palletctl_flg = 1
      AND cm.vc_sol_score = 0
      AND trlr.close_dte >= @from AND trlr.close_dte < @now
    GROUP BY cm.wh_id
) AS apal_scores ON base.wh_id = apal_scores.wh_id

LEFT JOIN (
    SELECT ROUND(ISNULL(SUM(actual_seconds) * 1.0 / COUNT(*), 0) / 60, 2) AS time_to_comp_replen, wh_id
    FROM prodwms.kvi_summary
    WHERE jobcodeid LIKE '%RPLN%'
      AND stop_time >= @from AND stop_time < @now
    GROUP BY wh_id
) AS replen ON base.wh_id = replen.wh_id

LEFT JOIN (
    SELECT COUNT('x') AS can_rea_pcks, wh_id
    FROM prodwms.canpck
    WHERE cancod IN (SELECT codval FROM prodwms.cancod WHERE reaflg = 1)
      AND cANDte >= @from AND cANDte < @now
    GROUP BY wh_id
) AS canpcks ON base.wh_id = canpcks.wh_id

LEFT JOIN (
    SELECT wh_id,
           SUM(CASE WHEN tostol LIKE '%LOST%' THEN 1 ELSE 0 END) AS lost_pallets,
           SUM(CASE WHEN frstol LIKE '%LOST%' THEN 1 ELSE 0 END) AS wrked_lost_pallets
    FROM prodwms.dlytrn
    WHERE trndte >= @from AND trndte < @now
      AND (tostol LIKE '%LOST%' OR frstol LIKE '%LOST%')
    GROUP BY wh_id
) AS lost_pals ON base.wh_id = lost_pals.wh_id

LEFT JOIN (
    SELECT COUNT(distinct dlytrn.lodnum) lost_inv_del, dlytrn.wh_id
    FROM prodwms.dlytrn
    WHERE actcod = 'INVDEL' AND oprcod = 'INVADJ' AND frstol LIKE '%LOST%'
      AND trndte >= @from AND trndte < @now AND client_id IS NOT NULL
    GROUP BY dlytrn.wh_id
) AS lost_inv_del ON base.wh_id = lost_inv_del.wh_id

LEFT JOIN (
    SELECT ISNULL(a.cnt_del_loads, 0) + ISNULL(b.cnt_add_loads, 0) AS tot_trans, base_wh.wh_id
    FROM (SELECT DISTINCT wh_id FROM prodwms.bldg_mst) base_wh
    LEFT JOIN (
        SELECT COUNT(*) AS cnt_del_loads, wh_id
        FROM prodwms.dlytrn
        WHERE actcod = 'INVDEL' AND oprcod = 'INVADJ'
          AND trndte >= @from AND trndte < @now AND client_id IS NOT NULL
        GROUP BY wh_id
    ) AS a ON base_wh.wh_id = a.wh_id
    LEFT JOIN (
        SELECT COUNT(*) AS cnt_add_loads, wh_id
        FROM prodwms.dlytrn
        WHERE actcod = 'IDNTFY_AJ' AND oprcod = 'INVADJ'
          AND trndte >= @from AND trndte < @now
        GROUP BY wh_id
    ) AS b ON base_wh.wh_id = b.wh_id
) AS inv_trn ON base.wh_id = inv_trn.wh_id

LEFT JOIN (
    SELECT cnt_audits AS cnt_audits,
           CASE WHEN CASE_picks = 0 THEN 0 
                ELSE ROUND((cnt_audits * 100.0 / CASE_picks), 2) 
           END AS cntbck_lock_up_prcnt,
           base_wh.wh_id
    FROM (SELECT DISTINCT wh_id FROM prodwms.bldg_mst) base_wh
    LEFT JOIN (
        SELECT COUNT('x') AS cnt_audits, wh_id
        FROM prodwms.cnthst
        WHERE cntdte >= @from AND cntdte < @now AND cnttyp = 'CB'
        GROUP BY wh_id
    ) AS a ON base_wh.wh_id = a.wh_id
    LEFT JOIN (
        SELECT COUNT('x') as case_picks, wh_id
        FROM prodwms.pckwrk_view
        WHERE list_id IS NOT NULL AND lodlvl = 'S' AND appqty > 0
          AND pckdte >= @from AND pckdte < @now
        GROUP BY wh_id
    ) AS b ON base_wh.wh_id = b.wh_id
) AS cntbacks ON base.wh_id = cntbacks.wh_id

LEFT JOIN (
    SELECT req_pals.wh_id,
           ISNULL(req_pals.req_pals, 0) req_pals,
           ISNULL(pck_pals.pck_pals, 0) pck_pals
    FROM (
        SELECT SUM(pals) req_pals, wh_id
        FROM (
            (SELECT COUNT(distinct pwv.wrkref) pals, pwv.wh_id
             FROM prodwms.wkohdr
             INNER JOIN prodwms.pckwrk_view pwv ON wkohdr.wkonum = pwv.wkonum 
                 AND wkohdr.wkorev = pwv.wkorev AND wkohdr.wh_id = pwv.wh_id 
                 AND wkohdr.client_id = pwv.client_id
             WHERE pwv.list_id IS NULL AND FORMAT(wkohdr.sch_begdte, 'yyyy/MM/dd') = FORMAT(@now, 'yyyy/MM/dd')
             GROUP BY pwv.wh_id)
            UNION
            (SELECT COUNT(distinct pwv.list_id) pals, pwv.wh_id
             FROM prodwms.wkohdr
             INNER JOIN prodwms.pckwrk_view pwv ON wkohdr.wkonum = pwv.wkonum 
                 AND wkohdr.wkorev = pwv.wkorev AND wkohdr.wh_id = pwv.wh_id 
                 AND wkohdr.client_id = pwv.client_id
             WHERE pwv.list_id IS NOT NULL AND FORMAT(wkohdr.sch_begdte, 'yyyy/MM/dd') = FORMAT(@now, 'yyyy/MM/dd')
             GROUP BY pwv.wh_id)
        ) AS tabpckwrk
        GROUP BY wh_id
    ) AS req_pals
    LEFT JOIN (
        SELECT COUNT(distinct iv.lodnum) pck_pals, iv.wh_id
        FROM prodwms.wkohdr
        INNER JOIN prodwms.pckwrk_view pwv ON wkohdr.wkonum = pwv.wkonum 
            AND wkohdr.wkorev = pwv.wkorev AND wkohdr.wh_id = pwv.wh_id 
            AND wkohdr.client_id = pwv.client_id
        INNER JOIN (
            SELECT lodnum, wrkref, wh_id
            FROM prodwms.inventory_view
            WHERE wrkref IS NOT NULL
            GROUP BY lodnum, wrkref, wh_id
        ) iv ON pwv.wrkref = iv.wrkref AND pwv.wh_id = iv.wh_id
        INNER JOIN prodwms.dlytrn ON iv.lodnum = dlytrn.lodnum AND iv.wh_id = dlytrn.wh_id
        WHERE FORMAT(wkohdr.sch_begdte, 'YYYY-MM-DD') = FORMAT(@now, 'YYYY-MM-DD')
          AND dlytrn.trndte <= @from AND dlytrn.trndte > DATEADD(DAY,-3,@now)
          AND dlytrn.to_arecod = @MSO_IN_AREAS AND dlytrn.oprcod = @MSO_OPRCOD_AREAS
        GROUP BY iv.wh_id
    ) AS pck_pals ON req_pals.wh_id = pck_pals.wh_id
) AS mso ON base.wh_id = mso.wh_id

LEFT JOIN (
    SELECT pwv.wh_id,
           CEILING(SUM(pwv.dtl_appqty / pwv.untcas)) all_cs_pcks,
           Count(distinct pwv.list_id) AS all_cs_pck_pals,
           CEILING(SUM(CASE WHEN pwv.srcare = vna_arecod.vna_arecod 
                           THEN (pwv.dtl_appqty / pwv.untcas) ELSE 0 END)) AS vna_cs,
           COUNT(distinct (CASE WHEN pwv.srcare = vna_arecod.vna_arecod 
                               THEN pwv.list_id ELSE null END)) AS vna_pals,
           CEILING(SUM(CASE WHEN pwv.srcare != ISNULL(vna_arecod.vna_arecod, 'a1z') 
                               AND pwv.pck_uom = 'LY' 
                           THEN (pwv.dtl_appqty / pwv.untcas) ELSE 0 END)) AS ly_cs,
           COUNT(Distinct CASE WHEN pwv.srcare <> ISNULL(vna_arecod.vna_arecod, 'a1z') 
                              AND pwv.pck_uom = 'LY' THEN pwv.list_id ELSE null END) AS ly_pals,
           CEILING(SUM(CASE WHEN pwv.srcare <> ISNULL(vna_arecod.vna_arecod, 'a1z') 
                               AND pftpd.cas_flg = 1 
                           THEN (pwv.dtl_appqty / pwv.untcas) ELSE 0 END)) AS cs,
           COUNT(Distinct CASE WHEN pwv.srcare <> ISNULL(vna_arecod.vna_arecod, 'a1z') 
                              AND pftpd.cas_flg = 1 THEN pwv.list_id ELSE null END) AS cs_pals
    FROM Prodwms.pckwrk_view pwv
    INNER JOIN prodwms.prtftp_dtl pftpd ON pwv.wh_id = pftpd.wh_id 
        AND pwv.prtnum = pftpd.prtnum AND pwv.prt_client_id = pftpd.prt_client_id 
        AND pwv.ftpcod = pftpd.ftpcod AND pwv.pck_uom = pftpd.uomcod
    LEFT JOIN (
        SELECT uc_param_val vna_arecod, wh_id
        FROM prodwms.uc_custom_parameters
        WHERE uc_param_var = @VNA_AREA_LIST
    ) vna_arecod ON pwv.wh_id = vna_arecod.wh_id
    WHERE pwv.lodlvl = 'S' AND pwv.pck_uom IS NOT NULL AND pwv.appqty > 0
      AND pwv.pckdte >= @from AND pwv.pckdte < @now
    GROUP BY pwv.wh_id
) as cs_pck ON base.wh_id = cs_pck.wh_id

LEFT JOIN (
    SELECT count(distinct (CASE WHEN to_arecod = @CASE_ARECOD_OR or to_arecod = @CASE_ARECOD_OUT 
                               THEN lodnum ELSE NULL END)) lp_rpln_pal,
           sum(CASE WHEN to_arecod = @CASE_ARECOD_OR or to_arecod = @CASE_ARECOD_OUT 
                   THEN trnqty / untcas ELSE 0 END) lp_rpln_cs,
           count(distinct (CASE WHEN to_arecod in (@CASE_ARECOD_IN) 
                               THEN lodnum ELSE NULL END)) cp_rpln_pal,
           sum(CASE WHEN to_arecod in (@CASE_ARECOD_IN) 
                   THEN trnqty / untcas ELSE 0 END) cp_rpln_cs,
           dlytrn.wh_id
    FROM prodwms.dlytrn
    INNER JOIN prodwms.prtftp_view pfv ON dlytrn.wh_id = pfv.wh_id 
        AND dlytrn.prtnum = pfv.prtnum AND dlytrn.prt_client_id = pfv.prt_client_id
    WHERE pfv.defftp_flg = 1
      AND to_arecod in (@CASE_ARECOD_OR, @CASE_ARECOD_IN)
      AND actcod != 'PL_XFR' AND fr_arecod = 'RDTS'
      AND trndte >= @from AND trndte < @now
    GROUP BY dlytrn.wh_id
) replen1 ON base.wh_id = replen1.wh_id

LEFT JOIN (
    SELECT COUNT(distinct lodnum) stg_pals, dlytrn.wh_id
    FROM prodwms.dlytrn
    INNER JOIN prodwms.aremst a ON dlytrn.to_arecod = a.arecod AND dlytrn.wh_id = a.wh_id
    WHERE a.stgflg = 1 AND oprcod = 'PCK'
      AND trndte >= @from AND trndte < @now
    GROUP BY dlytrn.wh_id
) AS stg_pals ON base.wh_id = stg_pals.wh_id

LEFT JOIN (
    SELECT SUM(cnt) AS full_pal_shp, wh_id
    FROM (
        SELECT 1 AS cnt, dlytrn.lodnum, dlytrn.wh_id
        FROM prodwms.dlytrn
        INNER JOIN prodwms.prtftp_view pfv ON dlytrn.wh_id = pfv.wh_id 
            AND dlytrn.prtnum = pfv.prtnum AND dlytrn.prt_client_id = pfv.prt_client_id
        WHERE pfv.defftp_flg = 1 AND dlytrn.actcod = 'TRLR_LOAD'
          AND dlytrn.trndte >= @from AND dlytrn.trndte < @now
        GROUP BY dlytrn.lodnum, dlytrn.wh_id
        HAVING MAX(pfv.untpal) = MAX(dlytrn.trnqty)
    ) AS inner_q
    GROUP BY wh_id
) AS full_pal_shp ON base.wh_id = full_pal_shp.wh_id

LEFT JOIN (
    SELECT COUNT('x') trlr_moves, wh_id
    FROM prodwms.trlract
    WHERE actcod = 'TMOVE'
      AND trndte >= @from AND trndte < @now
    GROUP BY wh_id
) AS trlr_moves ON base.wh_id = trlr_moves.wh_id

LEFT JOIN (
    SELECT wh_id, SUM(brudi_pals) brudi_pals
    FROM (
        (SELECT COUNT(distinct list_id) brudi_pals, wh_id
         FROM prodwms.pckwrk_view
         WHERE asset_typ != 'CHEP' AND lodlvl != 'L'
           AND pckdte >= @from AND pckdte < @now
         GROUP BY wh_id)
        UNION
        (SELECT COUNT(distinct wrkref) brudi_pals, wh_id
         FROM prodwms.pckwrk_view
         WHERE asset_typ != 'CHEP' AND lodlvl = 'L'
           AND pckdte >= @from AND pckdte < @now
         GROUP BY wh_id)
    ) as tblpckwrk_1
    GROUP BY wh_id
) as brudi_pals ON base.wh_id = brudi_pals.wh_id

LEFT JOIN (
    SELECT wh_id,
           COUNT(distinct lodnum) AS fr_mso,
           COUNT(distinct part_pals) AS part_pals,
           COUNT(distinct full_pals) AS full_pals
    FROM (
        SELECT dlytrn.wh_id, dlytrn.lodnum,
               CASE WHEN SUM(dlytrn.trnqty) != MAX(pfv.untpal) THEN dlytrn.lodnum ELSE null END part_pals,
               CASE WHEN SUM(dlytrn.trnqty) = MAX(pfv.untpal) THEN dlytrn.lodnum ELSE null END full_pals
        FROM prodwms.dlytrn
        INNER JOIN prodwms.prtftp_view pfv ON dlytrn.wh_id = pfv.wh_id 
            AND dlytrn.prtnum = pfv.prtnum AND dlytrn.prt_client_id = pfv.prt_client_id 
            AND pfv.defftp_flg = 1
        WHERE fr_arecod = @MSO_OUT_AREAS AND actcod != 'ATELOCCLRAUTO'
          AND trndte >= @from AND trndte < @now
        GROUP BY dlytrn.wh_id, dlytrn.lodnum
        UNION
        SELECT dlytrn.wh_id, dlytrn.lodnum,
               CASE WHEN SUM(dlytrn.trnqty) != MAX(pfv.untpal) THEN dlytrn.lodnum ELSE null END part_pals,
               CASE WHEN SUM(dlytrn.trnqty) = MAX(pfv.untpal) THEN dlytrn.lodnum ELSE null END full_pals
        FROM prodwms.dlytrn
        INNER JOIN prodwms.prtftp_view pfv ON dlytrn.wh_id = pfv.wh_id 
            AND dlytrn.prtnum = pfv.prtnum AND dlytrn.prt_client_id = pfv.prt_client_id 
            AND pfv.defftp_flg = 1
        WHERE fr_arecod = @MSO_OUT_AREAS AND actcod != 'ATELOCCLRAUTO'
          AND trndte >= @from AND trndte < @now AND actcod = 'UPCK'
        GROUP BY dlytrn.wh_id, dlytrn.lodnum
    ) AS tblprtftp_3
    GROUP BY wh_id
) AS fr_mso ON base.wh_id = fr_mso.wh_id

LEFT JOIN (
    SELECT wh_id, COUNT(distinct lodnum) to_mso
    FROM prodwms.dlytrn
    WHERE to_arecod IN (SELECT value FROM STRING_SPLIT(@MSO_IN_AREAS, ','))
      AND trndte >= @from AND trndte < @now
      AND EXISTS(SELECT 'x' FROM prodwms.pckwrk_view pwv 
                 WHERE dlytrn.lodnum = pwv.pallet_id AND dlytrn.wh_id = pwv.wh_id 
                   AND pwv.wkonum IS NOT NULL)
    GROUP BY wh_id
) AS tblpckwrk_3 ON base.wh_id = tblpckwrk_3.wh_id

GO