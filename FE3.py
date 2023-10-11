import duckdb

# Connect to DuckDB (creates an in-memory database)
con = duckdb.connect()

# Load data from CSV file into a DuckDB table
con.execute("CREATE TABLE bank_data AS SELECT * FROM read_csv_auto('~/buckets/b1/datasets/competencia_02.csv.gz');")


con.execute(create or replace table competencia_02 as
SELECT
  *
, regr_slope(ctrx_quarter, cliente_antiguedad) over ventana_6 as ctrx_quarter_slope_6, lag(ctrx_quarter, 1) over ventana_6 as ctrx_quarter_lag_1, lag(ctrx_quarter, 2) over ventana_6 as ctrx_quarter_lag_2, lag(ctrx_quarter, 3) over ventana_6 as ctrx_quarter_lag_3, lag(ctrx_quarter, 4) over ventana_6 as ctrx_quarter_lag_4, lag(ctrx_quarter, 5) over ventana_6 as ctrx_quarter_lag_5, lag(ctrx_quarter, 6) over ventana_6 as ctrx_quarter_lag_6, ctrx_quarter - lag(ctrx_quarter, 1) over ventana_6 as ctrx_quarter_delta_1_lag, ctrx_quarter - lag(ctrx_quarter, 2) over ventana_6 as ctrx_quarter_delta_2_lag, ctrx_quarter - lag(ctrx_quarter, 3) over ventana_6 as ctrx_quarter_delta_3_lag, ctrx_quarter - lag(ctrx_quarter, 4) over ventana_6 as ctrx_quarter_delta_4_lag, ctrx_quarter - lag(ctrx_quarter, 5) over ventana_6 as ctrx_quarter_delta_5_lag, ctrx_quarter - lag(ctrx_quarter, 6) over ventana_6 as ctrx_quarter_delta_6_lag, avg(ctrx_quarter) over ventana_6 as ctrx_quarter_media_6, max(ctrx_quarter) over ventana_6 as ctrx_quarter_max_6, min(ctrx_quarter) over ventana_6 as ctrx_quarter_min_6
, regr_slope(mpasivos_margen, cliente_antiguedad) over ventana_6 as mpasivos_margen_slope_6, lag(mpasivos_margen, 1) over ventana_6 as mpasivos_margen_lag_1, lag(mpasivos_margen, 2) over ventana_6 as mpasivos_margen_lag_2, lag(mpasivos_margen, 3) over ventana_6 as mpasivos_margen_lag_3, lag(mpasivos_margen, 4) over ventana_6 as mpasivos_margen_lag_4, lag(mpasivos_margen, 5) over ventana_6 as mpasivos_margen_lag_5, lag(mpasivos_margen, 6) over ventana_6 as mpasivos_margen_lag_6, mpasivos_margen - lag(mpasivos_margen, 1) over ventana_6 as mpasivos_margen_delta_1_lag, mpasivos_margen - lag(mpasivos_margen, 2) over ventana_6 as mpasivos_margen_delta_2_lag, mpasivos_margen - lag(mpasivos_margen, 3) over ventana_6 as mpasivos_margen_delta_3_lag, mpasivos_margen - lag(mpasivos_margen, 4) over ventana_6 as mpasivos_margen_delta_4_lag, mpasivos_margen - lag(mpasivos_margen, 5) over ventana_6 as mpasivos_margen_delta_5_lag, mpasivos_margen - lag(mpasivos_margen, 6) over ventana_6 as mpasivos_margen_delta_6_lag, avg(mpasivos_margen) over ventana_6 as mpasivos_margen_media_6, max(mpasivos_margen) over ventana_6 as mpasivos_margen_max_6, min(mpasivos_margen) over ventana_6 as mpasivos_margen_min_6
, regr_slope(mautoservicio, cliente_antiguedad) over ventana_6 as mautoservicio_slope_6, lag(mautoservicio, 1) over ventana_6 as mautoservicio_lag_1, lag(mautoservicio, 2) over ventana_6 as mautoservicio_lag_2, lag(mautoservicio, 3) over ventana_6 as mautoservicio_lag_3, lag(mautoservicio, 4) over ventana_6 as mautoservicio_lag_4, lag(mautoservicio, 5) over ventana_6 as mautoservicio_lag_5, lag(mautoservicio, 6) over ventana_6 as mautoservicio_lag_6, mautoservicio - lag(mautoservicio, 1) over ventana_6 as mautoservicio_delta_1_lag, mautoservicio - lag(mautoservicio, 2) over ventana_6 as mautoservicio_delta_2_lag, mautoservicio - lag(mautoservicio, 3) over ventana_6 as mautoservicio_delta_3_lag, mautoservicio - lag(mautoservicio, 4) over ventana_6 as mautoservicio_delta_4_lag, mautoservicio - lag(mautoservicio, 5) over ventana_6 as mautoservicio_delta_5_lag, mautoservicio - lag(mautoservicio, 6) over ventana_6 as mautoservicio_delta_6_lag, avg(mautoservicio) over ventana_6 as mautoservicio_media_6, max(mautoservicio) over ventana_6 as mautoservicio_max_6, min(mautoservicio) over ventana_6 as mautoservicio_min_6
, regr_slope(ctarjeta_debito_transacciones, cliente_antiguedad) over ventana_6 as ctarjeta_debito_transacciones_slope_6, lag(ctarjeta_debito_transacciones, 1) over ventana_6 as ctarjeta_debito_transacciones_lag_1, lag(ctarjeta_debito_transacciones, 2) over ventana_6 as ctarjeta_debito_transacciones_lag_2, lag(ctarjeta_debito_transacciones, 3) over ventana_6 as ctarjeta_debito_transacciones_lag_3, lag(ctarjeta_debito_transacciones, 4) over ventana_6 as ctarjeta_debito_transacciones_lag_4, lag(ctarjeta_debito_transacciones, 5) over ventana_6 as ctarjeta_debito_transacciones_lag_5, lag(ctarjeta_debito_transacciones, 6) over ventana_6 as ctarjeta_debito_transacciones_lag_6, ctarjeta_debito_transacciones - lag(ctarjeta_debito_transacciones, 1) over ventana_6 as ctarjeta_debito_transacciones_delta_1_lag, ctarjeta_debito_transacciones - lag(ctarjeta_debito_transacciones, 2) over ventana_6 as ctarjeta_debito_transacciones_delta_2_lag, ctarjeta_debito_transacciones - lag(ctarjeta_debito_transacciones, 3) over ventana_6 as ctarjeta_debito_transacciones_delta_3_lag, ctarjeta_debito_transacciones - lag(ctarjeta_debito_transacciones, 4) over ventana_6 as ctarjeta_debito_transacciones_delta_4_lag, ctarjeta_debito_transacciones - lag(ctarjeta_debito_transacciones, 5) over ventana_6 as ctarjeta_debito_transacciones_delta_5_lag, ctarjeta_debito_transacciones - lag(ctarjeta_debito_transacciones, 6) over ventana_6 as ctarjeta_debito_transacciones_delta_6_lag, avg(ctarjeta_debito_transacciones) over ventana_6 as ctarjeta_debito_transacciones_media_6, max(ctarjeta_debito_transacciones) over ventana_6 as ctarjeta_debito_transacciones_max_6, min(ctarjeta_debito_transacciones) over ventana_6 as ctarjeta_debito_transacciones_min_6
, regr_slope(mtransferencias_emitidas, cliente_antiguedad) over ventana_6 as mtransferencias_emitidas_slope_6, lag(mtransferencias_emitidas, 1) over ventana_6 as mtransferencias_emitidas_lag_1, lag(mtransferencias_emitidas, 2) over ventana_6 as mtransferencias_emitidas_lag_2, lag(mtransferencias_emitidas, 3) over ventana_6 as mtransferencias_emitidas_lag_3, lag(mtransferencias_emitidas, 4) over ventana_6 as mtransferencias_emitidas_lag_4, lag(mtransferencias_emitidas, 5) over ventana_6 as mtransferencias_emitidas_lag_5, lag(mtransferencias_emitidas, 6) over ventana_6 as mtransferencias_emitidas_lag_6, mtransferencias_emitidas - lag(mtransferencias_emitidas, 1) over ventana_6 as mtransferencias_emitidas_delta_1_lag, mtransferencias_emitidas - lag(mtransferencias_emitidas, 2) over ventana_6 as mtransferencias_emitidas_delta_2_lag, mtransferencias_emitidas - lag(mtransferencias_emitidas, 3) over ventana_6 as mtransferencias_emitidas_delta_3_lag, mtransferencias_emitidas - lag(mtransferencias_emitidas, 4) over ventana_6 as mtransferencias_emitidas_delta_4_lag, mtransferencias_emitidas - lag(mtransferencias_emitidas, 5) over ventana_6 as mtransferencias_emitidas_delta_5_lag, mtransferencias_emitidas - lag(mtransferencias_emitidas, 6) over ventana_6 as mtransferencias_emitidas_delta_6_lag, avg(mtransferencias_emitidas) over ventana_6 as mtransferencias_emitidas_media_6, max(mtransferencias_emitidas) over ventana_6 as mtransferencias_emitidas_max_6, min(mtransferencias_emitidas) over ventana_6 as mtransferencias_emitidas_min_6
, regr_slope(ctransferencias_emitidas, cliente_antiguedad) over ventana_6 as ctransferencias_emitidas_slope_6, lag(ctransferencias_emitidas, 1) over ventana_6 as ctransferencias_emitidas_lag_1, lag(ctransferencias_emitidas, 2) over ventana_6 as ctransferencias_emitidas_lag_2, lag(ctransferencias_emitidas, 3) over ventana_6 as ctransferencias_emitidas_lag_3, lag(ctransferencias_emitidas, 4) over ventana_6 as ctransferencias_emitidas_lag_4, lag(ctransferencias_emitidas, 5) over ventana_6 as ctransferencias_emitidas_lag_5, lag(ctransferencias_emitidas, 6) over ventana_6 as ctransferencias_emitidas_lag_6, ctransferencias_emitidas - lag(ctransferencias_emitidas, 1) over ventana_6 as ctransferencias_emitidas_delta_1_lag, ctransferencias_emitidas - lag(ctransferencias_emitidas, 2) over ventana_6 as ctransferencias_emitidas_delta_2_lag, ctransferencias_emitidas - lag(ctransferencias_emitidas, 3) over ventana_6 as ctransferencias_emitidas_delta_3_lag, ctransferencias_emitidas - lag(ctransferencias_emitidas, 4) over ventana_6 as ctransferencias_emitidas_delta_4_lag, ctransferencias_emitidas - lag(ctransferencias_emitidas, 5) over ventana_6 as ctransferencias_emitidas_delta_5_lag, ctransferencias_emitidas - lag(ctransferencias_emitidas, 6) over ventana_6 as ctransferencias_emitidas_delta_6_lag, avg(ctransferencias_emitidas) over ventana_6 as ctransferencias_emitidas_media_6, max(ctransferencias_emitidas) over ventana_6 as ctransferencias_emitidas_max_6, min(ctransferencias_emitidas) over ventana_6 as ctransferencias_emitidas_min_6
, regr_slope(mpayroll, cliente_antiguedad) over ventana_6 as mpayroll_slope_6, lag(mpayroll, 1) over ventana_6 as mpayroll_lag_1, lag(mpayroll, 2) over ventana_6 as mpayroll_lag_2, lag(mpayroll, 3) over ventana_6 as mpayroll_lag_3, lag(mpayroll, 4) over ventana_6 as mpayroll_lag_4, lag(mpayroll, 5) over ventana_6 as mpayroll_lag_5, lag(mpayroll, 6) over ventana_6 as mpayroll_lag_6, mpayroll - lag(mpayroll, 1) over ventana_6 as mpayroll_delta_1_lag, mpayroll - lag(mpayroll, 2) over ventana_6 as mpayroll_delta_2_lag, mpayroll - lag(mpayroll, 3) over ventana_6 as mpayroll_delta_3_lag, mpayroll - lag(mpayroll, 4) over ventana_6 as mpayroll_delta_4_lag, mpayroll - lag(mpayroll, 5) over ventana_6 as mpayroll_delta_5_lag, mpayroll - lag(mpayroll, 6) over ventana_6 as mpayroll_delta_6_lag, avg(mpayroll) over ventana_6 as mpayroll_media_6, max(mpayroll) over ventana_6 as mpayroll_max_6, min(mpayroll) over ventana_6 as mpayroll_min_6
, regr_slope(cpayroll_trx, cliente_antiguedad) over ventana_6 as cpayroll_trx_slope_6, lag(cpayroll_trx, 1) over ventana_6 as cpayroll_trx_lag_1, lag(cpayroll_trx, 2) over ventana_6 as cpayroll_trx_lag_2, lag(cpayroll_trx, 3) over ventana_6 as cpayroll_trx_lag_3, lag(cpayroll_trx, 4) over ventana_6 as cpayroll_trx_lag_4, lag(cpayroll_trx, 5) over ventana_6 as cpayroll_trx_lag_5, lag(cpayroll_trx, 6) over ventana_6 as cpayroll_trx_lag_6, cpayroll_trx - lag(cpayroll_trx, 1) over ventana_6 as cpayroll_trx_delta_1_lag, cpayroll_trx - lag(cpayroll_trx, 2) over ventana_6 as cpayroll_trx_delta_2_lag, cpayroll_trx - lag(cpayroll_trx, 3) over ventana_6 as cpayroll_trx_delta_3_lag, cpayroll_trx - lag(cpayroll_trx, 4) over ventana_6 as cpayroll_trx_delta_4_lag, cpayroll_trx - lag(cpayroll_trx, 5) over ventana_6 as cpayroll_trx_delta_5_lag, cpayroll_trx - lag(cpayroll_trx, 6) over ventana_6 as cpayroll_trx_delta_6_lag, avg(cpayroll_trx) over ventana_6 as cpayroll_trx_media_6, max(cpayroll_trx) over ventana_6 as cpayroll_trx_max_6, min(cpayroll_trx) over ventana_6 as cpayroll_trx_min_6
, regr_slope(mcomisiones_mantenimiento, cliente_antiguedad) over ventana_6 as mcomisiones_mantenimiento_slope_6, lag(mcomisiones_mantenimiento, 1) over ventana_6 as mcomisiones_mantenimiento_lag_1, lag(mcomisiones_mantenimiento, 2) over ventana_6 as mcomisiones_mantenimiento_lag_2, lag(mcomisiones_mantenimiento, 3) over ventana_6 as mcomisiones_mantenimiento_lag_3, lag(mcomisiones_mantenimiento, 4) over ventana_6 as mcomisiones_mantenimiento_lag_4, lag(mcomisiones_mantenimiento, 5) over ventana_6 as mcomisiones_mantenimiento_lag_5, lag(mcomisiones_mantenimiento, 6) over ventana_6 as mcomisiones_mantenimiento_lag_6, mcomisiones_mantenimiento - lag(mcomisiones_mantenimiento, 1) over ventana_6 as mcomisiones_mantenimiento_delta_1_lag, mcomisiones_mantenimiento - lag(mcomisiones_mantenimiento, 2) over ventana_6 as mcomisiones_mantenimiento_delta_2_lag, mcomisiones_mantenimiento - lag(mcomisiones_mantenimiento, 3) over ventana_6 as mcomisiones_mantenimiento_delta_3_lag, mcomisiones_mantenimiento - lag(mcomisiones_mantenimiento, 4) over ventana_6 as mcomisiones_mantenimiento_delta_4_lag, mcomisiones_mantenimiento - lag(mcomisiones_mantenimiento, 5) over ventana_6 as mcomisiones_mantenimiento_delta_5_lag, mcomisiones_mantenimiento - lag(mcomisiones_mantenimiento, 6) over ventana_6 as mcomisiones_mantenimiento_delta_6_lag, avg(mcomisiones_mantenimiento) over ventana_6 as mcomisiones_mantenimiento_media_6, max(mcomisiones_mantenimiento) over ventana_6 as mcomisiones_mantenimiento_max_6, min(mcomisiones_mantenimiento) over ventana_6 as mcomisiones_mantenimiento_min_6
, regr_slope(mcomisiones_otras, cliente_antiguedad) over ventana_6 as mcomisiones_otras_slope_6, lag(mcomisiones_otras, 1) over ventana_6 as mcomisiones_otras_lag_1, lag(mcomisiones_otras, 2) over ventana_6 as mcomisiones_otras_lag_2, lag(mcomisiones_otras, 3) over ventana_6 as mcomisiones_otras_lag_3, lag(mcomisiones_otras, 4) over ventana_6 as mcomisiones_otras_lag_4, lag(mcomisiones_otras, 5) over ventana_6 as mcomisiones_otras_lag_5, lag(mcomisiones_otras, 6) over ventana_6 as mcomisiones_otras_lag_6, mcomisiones_otras - lag(mcomisiones_otras, 1) over ventana_6 as mcomisiones_otras_delta_1_lag, mcomisiones_otras - lag(mcomisiones_otras, 2) over ventana_6 as mcomisiones_otras_delta_2_lag, mcomisiones_otras - lag(mcomisiones_otras, 3) over ventana_6 as mcomisiones_otras_delta_3_lag, mcomisiones_otras - lag(mcomisiones_otras, 4) over ventana_6 as mcomisiones_otras_delta_4_lag, mcomisiones_otras - lag(mcomisiones_otras, 5) over ventana_6 as mcomisiones_otras_delta_5_lag, mcomisiones_otras - lag(mcomisiones_otras, 6) over ventana_6 as mcomisiones_otras_delta_6_lag, avg(mcomisiones_otras) over ventana_6 as mcomisiones_otras_media_6, max(mcomisiones_otras) over ventana_6 as mcomisiones_otras_max_6, min(mcomisiones_otras) over ventana_6 as mcomisiones_otras_min_6
, regr_slope(mcomisiones, cliente_antiguedad) over ventana_6 as mcomisiones_slope_6, lag(mcomisiones, 1) over ventana_6 as mcomisiones_lag_1, lag(mcomisiones, 2) over ventana_6 as mcomisiones_lag_2, lag(mcomisiones, 3) over ventana_6 as mcomisiones_lag_3, lag(mcomisiones, 4) over ventana_6 as mcomisiones_lag_4, lag(mcomisiones, 5) over ventana_6 as mcomisiones_lag_5, lag(mcomisiones, 6) over ventana_6 as mcomisiones_lag_6, mcomisiones - lag(mcomisiones, 1) over ventana_6 as mcomisiones_delta_1_lag, mcomisiones - lag(mcomisiones, 2) over ventana_6 as mcomisiones_delta_2_lag, mcomisiones - lag(mcomisiones, 3) over ventana_6 as mcomisiones_delta_3_lag, mcomisiones - lag(mcomisiones, 4) over ventana_6 as mcomisiones_delta_4_lag, mcomisiones - lag(mcomisiones, 5) over ventana_6 as mcomisiones_delta_5_lag, mcomisiones - lag(mcomisiones, 6) over ventana_6 as mcomisiones_delta_6_lag, avg(mcomisiones) over ventana_6 as mcomisiones_media_6, max(mcomisiones) over ventana_6 as mcomisiones_max_6, min(mcomisiones) over ventana_6 as mcomisiones_min_6
, regr_slope(mcaja_ahorro, cliente_antiguedad) over ventana_6 as mcaja_ahorro_slope_6, lag(mcaja_ahorro, 1) over ventana_6 as mcaja_ahorro_lag_1, lag(mcaja_ahorro, 2) over ventana_6 as mcaja_ahorro_lag_2, lag(mcaja_ahorro, 3) over ventana_6 as mcaja_ahorro_lag_3, lag(mcaja_ahorro, 4) over ventana_6 as mcaja_ahorro_lag_4, lag(mcaja_ahorro, 5) over ventana_6 as mcaja_ahorro_lag_5, lag(mcaja_ahorro, 6) over ventana_6 as mcaja_ahorro_lag_6, mcaja_ahorro - lag(mcaja_ahorro, 1) over ventana_6 as mcaja_ahorro_delta_1_lag, mcaja_ahorro - lag(mcaja_ahorro, 2) over ventana_6 as mcaja_ahorro_delta_2_lag, mcaja_ahorro - lag(mcaja_ahorro, 3) over ventana_6 as mcaja_ahorro_delta_3_lag, mcaja_ahorro - lag(mcaja_ahorro, 4) over ventana_6 as mcaja_ahorro_delta_4_lag, mcaja_ahorro - lag(mcaja_ahorro, 5) over ventana_6 as mcaja_ahorro_delta_5_lag, mcaja_ahorro - lag(mcaja_ahorro, 6) over ventana_6 as mcaja_ahorro_delta_6_lag, avg(mcaja_ahorro) over ventana_6 as mcaja_ahorro_media_6, max(mcaja_ahorro) over ventana_6 as mcaja_ahorro_max_6, min(mcaja_ahorro) over ventana_6 as mcaja_ahorro_min_6
, regr_slope(mcuentas_saldo, cliente_antiguedad) over ventana_6 as mcuentas_saldo_slope_6, lag(mcuentas_saldo, 1) over ventana_6 as mcuentas_saldo_lag_1, lag(mcuentas_saldo, 2) over ventana_6 as mcuentas_saldo_lag_2, lag(mcuentas_saldo, 3) over ventana_6 as mcuentas_saldo_lag_3, lag(mcuentas_saldo, 4) over ventana_6 as mcuentas_saldo_lag_4, lag(mcuentas_saldo, 5) over ventana_6 as mcuentas_saldo_lag_5, lag(mcuentas_saldo, 6) over ventana_6 as mcuentas_saldo_lag_6, mcuentas_saldo - lag(mcuentas_saldo, 1) over ventana_6 as mcuentas_saldo_delta_1_lag, mcuentas_saldo - lag(mcuentas_saldo, 2) over ventana_6 as mcuentas_saldo_delta_2_lag, mcuentas_saldo - lag(mcuentas_saldo, 3) over ventana_6 as mcuentas_saldo_delta_3_lag, mcuentas_saldo - lag(mcuentas_saldo, 4) over ventana_6 as mcuentas_saldo_delta_4_lag, mcuentas_saldo - lag(mcuentas_saldo, 5) over ventana_6 as mcuentas_saldo_delta_5_lag, mcuentas_saldo - lag(mcuentas_saldo, 6) over ventana_6 as mcuentas_saldo_delta_6_lag, avg(mcuentas_saldo) over ventana_6 as mcuentas_saldo_media_6, max(mcuentas_saldo) over ventana_6 as mcuentas_saldo_max_6, min(mcuentas_saldo) over ventana_6 as mcuentas_saldo_min_6
, regr_slope(mtarjeta_visa_consumo, cliente_antiguedad) over ventana_6 as mtarjeta_visa_consumo_slope_6, lag(mtarjeta_visa_consumo, 1) over ventana_6 as mtarjeta_visa_consumo_lag_1, lag(mtarjeta_visa_consumo, 2) over ventana_6 as mtarjeta_visa_consumo_lag_2, lag(mtarjeta_visa_consumo, 3) over ventana_6 as mtarjeta_visa_consumo_lag_3, lag(mtarjeta_visa_consumo, 4) over ventana_6 as mtarjeta_visa_consumo_lag_4, lag(mtarjeta_visa_consumo, 5) over ventana_6 as mtarjeta_visa_consumo_lag_5, lag(mtarjeta_visa_consumo, 6) over ventana_6 as mtarjeta_visa_consumo_lag_6, mtarjeta_visa_consumo - lag(mtarjeta_visa_consumo, 1) over ventana_6 as mtarjeta_visa_consumo_delta_1_lag, mtarjeta_visa_consumo - lag(mtarjeta_visa_consumo, 2) over ventana_6 as mtarjeta_visa_consumo_delta_2_lag, mtarjeta_visa_consumo - lag(mtarjeta_visa_consumo, 3) over ventana_6 as mtarjeta_visa_consumo_delta_3_lag, mtarjeta_visa_consumo - lag(mtarjeta_visa_consumo, 4) over ventana_6 as mtarjeta_visa_consumo_delta_4_lag, mtarjeta_visa_consumo - lag(mtarjeta_visa_consumo, 5) over ventana_6 as mtarjeta_visa_consumo_delta_5_lag, mtarjeta_visa_consumo - lag(mtarjeta_visa_consumo, 6) over ventana_6 as mtarjeta_visa_consumo_delta_6_lag, avg(mtarjeta_visa_consumo) over ventana_6 as mtarjeta_visa_consumo_media_6, max(mtarjeta_visa_consumo) over ventana_6 as mtarjeta_visa_consumo_max_6, min(mtarjeta_visa_consumo) over ventana_6 as mtarjeta_visa_consumo_min_6
, regr_slope(mprestamos_personales, cliente_antiguedad) over ventana_6 as mprestamos_personales_slope_6, lag(mprestamos_personales, 1) over ventana_6 as mprestamos_personales_lag_1, lag(mprestamos_personales, 2) over ventana_6 as mprestamos_personales_lag_2, lag(mprestamos_personales, 3) over ventana_6 as mprestamos_personales_lag_3, lag(mprestamos_personales, 4) over ventana_6 as mprestamos_personales_lag_4, lag(mprestamos_personales, 5) over ventana_6 as mprestamos_personales_lag_5, lag(mprestamos_personales, 6) over ventana_6 as mprestamos_personales_lag_6, mprestamos_personales - lag(mprestamos_personales, 1) over ventana_6 as mprestamos_personales_delta_1_lag, mprestamos_personales - lag(mprestamos_personales, 2) over ventana_6 as mprestamos_personales_delta_2_lag, mprestamos_personales - lag(mprestamos_personales, 3) over ventana_6 as mprestamos_personales_delta_3_lag, mprestamos_personales - lag(mprestamos_personales, 4) over ventana_6 as mprestamos_personales_delta_4_lag, mprestamos_personales - lag(mprestamos_personales, 5) over ventana_6 as mprestamos_personales_delta_5_lag, mprestamos_personales - lag(mprestamos_personales, 6) over ventana_6 as mprestamos_personales_delta_6_lag, avg(mprestamos_personales) over ventana_6 as mprestamos_personales_media_6, max(mprestamos_personales) over ventana_6 as mprestamos_personales_max_6, min(mprestamos_personales) over ventana_6 as mprestamos_personales_min_6
FROM competencia_02
WINDOW ventana_6 AS (PARTITION BY numero_de_cliente ORDER BY foto_mes ROWS BETWEEN 6 PRECEDING AND CURRENT ROW))


con.execute(copy competencia_02 to '~/buckets/b1/datasets/competencia_02_FE.csv.gz' (FORMAT CSV, HEADER))

con.close()

quit()

