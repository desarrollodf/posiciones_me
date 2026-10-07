import bcchapi
import zeep
import pandas as pd
from zeep.helpers import serialize_object
import datetime as dt
import numpy as np
from time import sleep
import sys
import os

user = os.environ["BCCH_USER"]
pw = os.environ["BCCH_PASSWORD"]

siete = bcchapi.Siete(user, pw)

# Función para crear tablas agregadas

hasta = pd.Timestamp.today().strftime("%Y-%m-%d")
desde = (pd.Timestamp.today() - pd.DateOffset(years=2)).strftime("%Y-%m-%d")

def series_bcentral(diccionario, frec='ME', var=0, desde=None, hasta=None, observed='last'):
    """Descarga series desde el BCCh y devuelve DataFrame largo con columna Fecha (datetime)."""
    from time import sleep
    import pandas as pd
    import numpy as np

    datos = {}
    for nombre, codigo in diccionario.items():
        print(f'Descargando {nombre}: {codigo}', flush=True)
      
        try:
            respuesta = siete.cuadro(
                series=[codigo],
                nombres=[nombre],
                variacion=var,
                frecuencia=frec,
                desde=desde,
                hasta=hasta,
                observed=observed
            )
        except TypeError:
            respuesta = siete.cuadro(
                series=[codigo],
                nombres=[nombre],
                variacion=var,
                frecuencia=frec,
                desde=desde,
                hasta=hasta,
                observado=observed
            )

        # convertir a DataFrame si no lo es
        if isinstance(respuesta, pd.DataFrame):
            df_resp = respuesta.copy()
        else:
            df_resp = pd.DataFrame(respuesta)

        # aplanar MultiIndex de columnas si existiera
        if isinstance(df_resp.columns, pd.MultiIndex):
            df_resp.columns = ['_'.join([str(x) for x in col if x is not None]) for col in df_resp.columns]

        # 1) Si el índice ya es datetime -> tomar la columna de valores de forma segura
        idx_is_dt = pd.api.types.is_datetime64_any_dtype(df_resp.index) or pd.api.types.is_datetime64_any_dtype(df_resp.index.astype('object', copy=False))
        if not idx_is_dt:
            try:
                idx_conv = pd.to_datetime(df_resp.index)
                if not idx_conv.isna().all():
                    df_resp.index = idx_conv
                    idx_is_dt = True
            except Exception:
                idx_is_dt = False

        if idx_is_dt:
            if nombre in df_resp.columns:
                serie = df_resp[nombre]
            else:
                numeric_cols = df_resp.select_dtypes(include=[np.number]).columns.tolist()
                if numeric_cols:
                    serie = df_resp[numeric_cols[0]]
                elif df_resp.shape[1] >= 1:
                    serie = df_resp.iloc[:, 0]
                else:
                    raise ValueError(f"No hay columnas válidas en la respuesta para {codigo!r}")
            serie = serie.astype(float, errors='ignore').rename(nombre)
            datos[nombre] = serie.copy()
            sleep(1)
            continue

        # 2) Si la fecha está en una columna, detectarla
        date_col = next((c for c in df_resp.columns if 'date' in str(c).lower() or 'fecha' in str(c).lower()), None)
        if date_col is None:
            for c in df_resp.columns:
                try:
                    pd.to_datetime(df_resp[c].dropna().iloc[:5])
                    date_col = c
                    break
                except Exception:
                    continue

        # detectar columna de valor
        value_col = None
        if nombre in df_resp.columns:
            value_col = nombre
        else:
            numeric_cols = df_resp.select_dtypes(include=[np.number]).columns.tolist()
            if numeric_cols:
                numeric_cols = [c for c in numeric_cols if c != date_col]
            if numeric_cols:
                value_col = numeric_cols[0]
            else:
                candidates = [c for c in df_resp.columns if c != date_col]
                if candidates:
                    value_col = candidates[0]

        if date_col is None or value_col is None:
            raise ValueError(f"No se pudo parsear la respuesta para la serie {codigo!r} (date_col={date_col}, value_col={value_col})")

        df_resp[date_col] = pd.to_datetime(df_resp[date_col], errors='coerce')
        serie = df_resp.set_index(date_col)[value_col].astype(float, errors='ignore').rename(nombre)
        datos[nombre] = serie
        sleep(1)

    # construir DataFrame ancho a partir de las series (índice datetime)
    df = pd.DataFrame(datos)
    df.index.name = 'Fecha'
    df_largo = df.reset_index().melt(id_vars='Fecha', var_name='Serie', value_name='Valor')
    df_largo = df_largo.set_index('Fecha')
    return df_largo

# Montos vigentes de no residentes en forwards USDCLP

series_posnet_nores_plazo = {
    'Hasta 7 días': 'F099.DER.STO.Z.40.N.NR.NET.NDF.MMUSD.CLPUSD.C.P17.0.D',
    '8-35 días': 'F099.DER.STO.Z.40.N.NR.NET.NDF.MMUSD.CLPUSD.C.P835.0.D',
    '36-95 días': 'F099.DER.STO.Z.40.N.NR.NET.NDF.MMUSD.CLPUSD.C.P3695.0.D',
    '96-185 días': 'F099.DER.STO.Z.40.N.NR.NET.NDF.MMUSD.CLPUSD.C.P96185.0.D',
    '186-370 días': 'F099.DER.STO.Z.40.N.NR.NET.NDF.MMUSD.CLPUSD.C.P186370.0.D',
    'Más de 1 año': 'F099.DER.STO.Z.40.N.NR.NET.NDF.MMUSD.CLPUSD.C.MA01.0.D'
}
posnet_nores_plazo = series_bcentral(
    series_posnet_nores_plazo,
    frec='D',
    desde=desde,
    hasta=hasta
).dropna()

# Versión modificada
posnet_1 = posnet_nores_plazo[
  posnet_nores_plazo['Serie'].isin(['Hasta 7 días', '8-35 días'])
].groupby('Fecha')[['Valor']].sum().assign(Serie='Hasta 35 días')

posnet_2 = posnet_nores_plazo[
  posnet_nores_plazo['Serie'].isin(['36-95 días', '96-185 días'])
].groupby('Fecha')[['Valor']].sum().assign(Serie='36-185 días')

posnet_3 = posnet_nores_plazo[
  posnet_nores_plazo['Serie'].isin(['186-370 días', 'Más de 1 año'])
].groupby('Fecha')[['Valor']].sum().assign(Serie='186 días o más')

posnet_nores_plazo_mod = pd.concat([posnet_1, posnet_2, posnet_3])
posnet_nores_plazo_mod.to_excel('datos.xlsx', index=True)

print('datos.xlsx actualizado correctamente')
print(f'Última fecha: {posnet_nores_plazo_mod.index.max()}')
print(f'Filas generadas: {len(posnet_nores_plazo_mod):,}')
