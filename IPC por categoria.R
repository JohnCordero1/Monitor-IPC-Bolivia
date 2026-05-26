# ==============================================================================
#  MONITOR IPC BOLIVIA
#  El Script lee los Excels, calcula IPC por divisiones y exporta un HTML interactivo.
#
#  USO:
#    1. En RStudio: source("IPC por categoria.R")
#    2. Genera "monitor_ipc_bolivia.html"
#
# ==============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(jsonlite)

cat("─────────────────────────────────────────────\n")
cat("  Monitor IPC Bolivia — generando HTML...\n")
cat("─────────────────────────────────────────────\n\n")

# ==============================================================================
# 1. CARGA Y LIMPIEZA
# ==============================================================================

ipc_raw  <- read_excel("D:/Usuario/Desktop/Inflación/IPC producto.xlsx",     col_names = FALSE)
pond_raw <- read_excel("D:/Usuario/Desktop/Inflación/Ponderaciones IPC.xlsx", col_names = FALSE)

col_names  <- as.character(ipc_raw[1, ])
month_cols <- col_names[3:length(col_names)]
n_meses    <- length(month_cols)

cat(sprintf("  ✔ Archivos cargados: %d meses detectados (%s → %s)\n\n",
            n_meses, month_cols[1], month_cols[n_meses]))

# IPC General (fila 2)
ipc_general_vals <- as.numeric(ipc_raw[2, 3:ncol(ipc_raw)])

# Productos (fila 3 en adelante)
ipc_prod <- ipc_raw[3:nrow(ipc_raw), ]
colnames(ipc_prod) <- col_names
ipc_prod <- ipc_prod |>
  mutate(
    CODIGO = str_replace_all(CODIGO, "\u00a0", "") |> trimws(),
    DIV    = substr(CODIGO, 1, 2)
  )

# Ponderaciones
pond <- pond_raw[3:nrow(pond_raw), ]
colnames(pond) <- c("CODIGO", "DESCRIPCION", "PONDERADOR")
pond <- pond |>
  mutate(
    CODIGO     = str_replace_all(CODIGO, "\u00a0", "") |> trimws(),
    PONDERADOR = as.numeric(PONDERADOR)
  )

ipc_prod <- ipc_prod |>
  left_join(pond |> select(CODIGO, PONDERADOR), by = "CODIGO") |>
  mutate(across(all_of(month_cols), as.numeric))

# ==============================================================================
# 2. PARSEO DE FECHAS
# ==============================================================================

meses_es <- c(
  "ene"  = "01", "feb"  = "02", "mar"  = "03", "abr"  = "04",
  "may"  = "05", "jun"  = "06", "jul"  = "07", "ago"  = "08",
  "sept" = "09", "oct"  = "10", "nov"  = "11", "dic"  = "12"
)

parsear_fecha <- function(f) {
  partes <- strsplit(tolower(f), "-")[[1]]
  mes    <- meses_es[partes[1]]
  anio   <- ifelse(nchar(partes[2]) == 2, paste0("20", partes[2]), partes[2])
  as.Date(paste0(anio, "-", mes, "-01"))
}

fechas     <- as.Date(sapply(month_cols, parsear_fecha), origin = "1970-01-01")
fechas_iso <- format(fechas, "%Y-%m-%d")

# ==============================================================================
# 3. IPC POR DIVISIÓN
# ==============================================================================

divisiones_info <- list(
  "01" = "Alimentos y Bebidas No Alcohólicas",
  "02" = "Bebidas Alcohólicas y Tabaco",
  "03" = "Prendas de Vestir y Calzados",
  "04" = "Vivienda y Servicios Básicos",
  "05" = "Muebles, Bienes y Servicios Domésticos",
  "06" = "Salud",
  "07" = "Transporte",
  "08" = "Comunicaciones",
  "09" = "Recreación y Cultura",
  "10" = "Educación",
  "11" = "Alimentos y Bebidas Fuera del Hogar",
  "12" = "Bienes y Servicios Diversos"
)

calcular_ipc_div <- function(df_div) {
  w      <- df_div$PONDERADOR
  w[is.na(w)] <- 0
  w_norm <- w / sum(w) * 100
  mat    <- as.matrix(df_div[, month_cols])
  as.numeric(colSums(mat * w_norm, na.rm = TRUE) / 100)
}

ipc_div_list  <- list()
pond_div_list <- list()

for (div in names(divisiones_info)) {
  sub <- ipc_prod |> filter(DIV == div)
  pond_div_list[[div]] <- sum(sub$PONDERADOR, na.rm = TRUE)
  ipc_div_list[[div]]  <- if (nrow(sub) == 0) rep(NA_real_, n_meses) else calcular_ipc_div(sub)
  cat(sprintf("  ✔ Div %s  %-48s  %3d productos  pond=%.2f%%\n",
              div, divisiones_info[[div]], nrow(sub), pond_div_list[[div]]))
}
cat("\n")

# ==============================================================================
# 4. FUNCIONES DE INFLACIÓN
# ==============================================================================

calc_mensual <- function(s) {
  r <- rep(NA_real_, length(s))
  for (i in 2:length(s))
    if (!is.na(s[i]) && !is.na(s[i-1]) && s[i-1] != 0)
      r[i] <- (s[i] / s[i-1] - 1) * 100
  r
}

calc_acumulada <- function(s, fechas) {
  anios <- as.integer(format(fechas, "%Y"))
  meses <- as.integer(format(fechas, "%m"))
  r     <- rep(NA_real_, length(s))
  for (i in seq_along(s)) {
    idx_dic <- which(anios == (anios[i] - 1) & meses == 12)
    if (length(idx_dic) > 0 && !is.na(s[idx_dic[1]]) && s[idx_dic[1]] != 0)
      r[i] <- (s[i] / s[idx_dic[1]] - 1) * 100
  }
  r
}

calc_12meses <- function(s) {
  n <- length(s); r <- rep(NA_real_, n)
  if (n > 12)
    r[13:n] <- ifelse(s[1:(n-12)] != 0, (s[13:n] / s[1:(n-12)] - 1) * 100, NA_real_)
  r
}


# Calcular inflaciones para el IPC general
inf_gen_mensual  <- calc_mensual(ipc_general_vals)
inf_gen_acumulada <- calc_acumulada(ipc_general_vals, fechas)
inf_gen_12m       <- calc_12meses(ipc_general_vals)

# Calcular inflaciones para cada división (resultado: listas de vectores)
inf_div_mensual  <- lapply(ipc_div_list, calc_mensual)
inf_div_acumulada <- lapply(ipc_div_list, function(x) calc_acumulada(x, fechas))
inf_div_12m       <- lapply(ipc_div_list, calc_12meses)

# Inflación general
df_gen <- data.frame(
  fecha = fechas,
  ipc   = ipc_general_vals,
  mensual = calc_mensual(ipc_general_vals),
  acumulada = calc_acumulada(ipc_general_vals, fechas),
  doce_meses = calc_12meses(ipc_general_vals)
)

# Para cada división, se crea un data frame o una lista anidada
df_div <- list()
for (div in names(ipc_div_list)) {
  ipc_vec <- ipc_div_list[[div]]
  df_div[[div]] <- data.frame(
    fecha = fechas,
    ipc = ipc_vec,
    mensual = calc_mensual(ipc_vec),
    acumulada = calc_acumulada(ipc_vec, fechas),
    doce_meses = calc_12meses(ipc_vec)
  )
}


# ==============================================================================
# 5. EMPAQUETAR JSON
# ==============================================================================

# Convierte NAs a NULL para JSON
na_null <- function(x) lapply(x, function(v) if (is.na(v)) NULL else v)

datos_json <- toJSON(list(
  fechas      = fechas_iso,
  labels      = month_cols,
  ipc_gen     = round(ipc_general_vals, 6),
  ipc_div     = lapply(ipc_div_list,  function(s) na_null(round(s, 6))),
  pond_div    = pond_div_list,
  div_nombres = divisiones_info
), auto_unbox = TRUE, null = "null", na = "null")

cat("  ✔ Datos empaquetados\n\n")

# ==============================================================================
# 6. GENERAR HTML
# ==============================================================================
# Mapeo de abreviaturas a nombre con primera letra mayúscula
meses_map <- c(
  "ene" = "Ene", "feb" = "Feb", "mar" = "Mar", "abr" = "Abr",
  "may" = "May", "jun" = "Jun", "jul" = "Jul", "ago" = "Ago",
  "sep" = "Sep", "sept" = "Sep", "oct" = "Oct", "nov" = "Nov", "dic" = "Dic"
)

# Formatear el último mes para mostrar en cabecera
ultimo_mes_raw <- month_cols[n_meses]
partes <- strsplit(ultimo_mes_raw, "-")[[1]]
mes_abr <- partes[1]   # "abr"
anio_corto <- partes[2] # "26"
anio_largo <- ifelse(nchar(anio_corto) == 2, paste0("20", anio_corto), anio_corto)
mes_formateado <- meses_map[tolower(mes_abr)]
ultimo_mes <- paste(mes_formateado, anio_largo)

# Formatear el primer mes
primer_mes_raw <- month_cols[1]
partes_prim <- strsplit(primer_mes_raw, "-")[[1]]
mes_prim_abr <- partes_prim[1]
anio_prim_corto <- partes_prim[2]
anio_prim_largo <- ifelse(nchar(anio_prim_corto) == 2, paste0("20", anio_prim_corto), anio_prim_corto)
mes_prim_formateado <- meses_map[tolower(mes_prim_abr)]
primer_mes <- paste(mes_prim_formateado, anio_prim_largo)

n_idx      <- n_meses - 1   # índice máximo para slider (base 0)

html <- paste0(
  '<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Monitor IPC Bolivia</title>
<script src="https://cdnjs.cloudflare.com/ajax/libs/plotly.js/2.27.0/plotly.min.js"></script>
<style>
@import url("https://fonts.googleapis.com/css2?family=Inter:opsz,wght@14..32,300;14..32,400;14..32,500;14..32,600&family=JetBrains+Mono:wght@400;500&display=swap");
:root{
  --bg:#ffffff;
  --surf:#f8fafc;
  --surf2:#ffffff;
  --bd:#e2e8f0;
  --bd2:#cbd5e1;
  --tx:#0f172a;
  --mu:#475569;
  --mu2:#64748b;
  --ac:#2c6e9e;
  --ac2:#3b82f6;
  --gr:#10b981;
  --rd:#ef4444;
  --yl:#f59e0b;
  --shadow:0 1px 3px rgba(0,0,0,0.05),0 1px 2px rgba(0,0,0,0.03);
}
*{box-sizing:border-box;margin:0;padding:0}
html,body{height:100%;overflow:hidden}
body{font-family:"Inter",system-ui,-apple-system,sans-serif;background:var(--bg);color:var(--tx);font-size:13px;line-height:1.4}

.hdr{height:56px;display:flex;align-items:center;justify-content:space-between;
  padding:0 24px;background:var(--surf);border-bottom:1px solid var(--bd);flex-shrink:0}
.hdr-l{display:flex;align-items:baseline;gap:12px}
.hdr-title{font-size:16px;font-weight:600;letter-spacing:-0.2px;color:#0f172a}
.hdr-sub{font-size:12px;color:var(--mu)}
.badge{background:#eef2ff;border:1px solid #cbd5e1;color:var(--ac2);font-size:11px;font-weight:500;padding:4px 12px;border-radius:24px;font-family:"JetBrains Mono",monospace;max-width:360px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}

.app{display:flex;height:calc(100vh - 56px)}

/* sidebar */
.sb{width:264px;min-width:264px;background:var(--surf);border-right:1px solid var(--bd);display:flex;flex-direction:column;overflow:hidden}
.sb-top{padding:16px 16px 12px;border-bottom:1px solid var(--bd);flex-shrink:0}
.sb-lbl{font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:0.5px;color:var(--mu);margin-bottom:10px}
.qrow{display:flex;gap:8px;margin-bottom:12px}
.qbtn{flex:1;padding:6px 0;background:var(--surf2);border:1px solid var(--bd2);color:var(--mu2);border-radius:8px;font-size:12px;font-weight:500;cursor:pointer;transition:all 0.15s}
.qbtn:hover{background:var(--bd);color:var(--tx)}
.peso-box{background:#f1f5f9;border:1px solid var(--bd);border-radius:10px;padding:10px 12px}
.peso-row{display:flex;justify-content:space-between;align-items:center}
.peso-lbl{font-size:11px;font-weight:500;color:var(--mu)}
.peso-num{font-family:"JetBrains Mono",monospace;font-size:15px;font-weight:600;color:var(--ac)}
.peso-warn{font-size:11px;color:var(--yl);margin-top:6px;display:none}
.chips{overflow-y:auto;flex:1;padding:12px 10px 16px;display:flex;flex-direction:column;gap:6px}
.chip{display:flex;align-items:center;gap:10px;padding:8px 10px;border-radius:10px;border:1px solid var(--bd);background:var(--surf2);cursor:pointer;transition:all 0.15s}
.chip:hover{background:#f1f5f9;border-color:var(--bd2)}
.chip.on{border-color:var(--ac);background:#f0f9ff}
.cnum{min-width:28px;height:22px;border-radius:6px;background:#e2e8f0;color:#334155;font-size:11px;font-weight:700;font-family:"JetBrains Mono",monospace;display:flex;align-items:center;justify-content:center}
.chip.on .cnum{background:var(--ac);color:white}
.ctxt{font-size:12px;font-weight:500;color:var(--mu);flex:1}
.chip.on .ctxt{color:var(--tx)}
.cpct{font-size:10px;font-family:"JetBrains Mono",monospace;color:var(--mu2)}
.chip.on .cpct{color:var(--ac2)}

/* content */
.cnt{flex:1;display:flex;flex-direction:column;padding:16px 20px;gap:14px;overflow:hidden;min-width:0}

/* kpis */
.kpis{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;flex-shrink:0}
.kpi{background:var(--surf);border:1px solid var(--bd);border-radius:12px;padding:12px 14px;box-shadow:var(--shadow)}
.kpi-lbl{font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:0.5px;color:var(--mu);margin-bottom:6px}
.kpi-v{font-family:"JetBrains Mono",monospace;font-size:22px;font-weight:600;color:var(--tx);line-height:1.2}
.kpi-v.up{color:var(--rd)} .kpi-v.dn{color:var(--gr)}
.kpi-s{font-size:11px;color:var(--mu);margin-top:4px}

/* toolbar */
.tbar{display:flex;align-items:center;gap:12px;flex-shrink:0;flex-wrap:wrap}
.bgrp{display:flex;background:var(--surf2);border:1px solid var(--bd);border-radius:10px;padding:3px;gap:4px}
.tbtn{background:transparent;border:none;color:var(--mu);padding:5px 14px;border-radius:7px;font-size:12px;font-weight:500;cursor:pointer;transition:all 0.15s}
.tbtn:hover{color:var(--tx);background:#f1f5f9} 
.tbtn.on{background:#e2e8f0;color:#0f172a}
.rbox{display:flex;align-items:center;gap:8px;background:var(--surf2);border:1px solid var(--bd);border-radius:10px;padding:4px 12px}
.rlbl{font-size:12px;color:var(--mu)}
input[type=range]{-webkit-appearance:none;width:100px;height:4px;background:#cbd5e1;border-radius:4px;outline:none}
input[type=range]::-webkit-slider-thumb{-webkit-appearance:none;width:14px;height:14px;border-radius:50%;background:var(--ac);cursor:pointer;box-shadow:0 1px 2px rgba(0,0,0,0.1)}
.rv{font-family:"JetBrains Mono",monospace;font-size:12px;color:var(--ac);min-width:36px}
.spacer{flex:1}
.src{font-size:11px;color:var(--mu);font-style:normal}

/* plots */
.plots{display:grid;grid-template-columns:1fr 1fr;gap:14px;flex:1;min-height:0;transition:all 0.18s ease;}
.pcard{background:var(--surf);border:1px solid var(--bd);border-radius:14px;padding:10px 10px 4px;display:flex;flex-direction:column;min-height:0;box-shadow:var(--shadow);transition:opacity .18s ease,transform .18s ease}
.pc-h{display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;flex-shrink:0}
.pc-t{font-size:12px;font-weight:600;text-transform:uppercase;letter-spacing:0.3px;color:var(--mu)}
.pc-s{font-size:11px;color:var(--mu2)}
.pc-b{flex:1;min-height:0;width:100%}

::-webkit-scrollbar{width:5px;height:5px}
::-webkit-scrollbar-track{background:#f1f5f9}
::-webkit-scrollbar-thumb{background:#cbd5e1;border-radius:3px}
</style>
</head>
<body>

<div class="hdr">
  <div class="hdr-l">
    <span class="hdr-title">Monitor IPC Bolivia</span>
    <span class="hdr-sub">INE &middot; ', primer_mes, ' &ndash; ', ultimo_mes, '</span>
  </div>
  <span class="badge" id="badge">IPC General (100.00%)</span>
</div>

<div class="app">
  <div class="sb">
    <div class="sb-top">
      <div class="sb-lbl">Composici&oacute;n del &iacute;ndice</div>
      <div class="qrow">
        <button class="qbtn" onclick="selAll()">Todas</button>
        <button class="qbtn" onclick="selNone()">Limpiar</button>
      </div>
      <div class="peso-box">
        <div class="peso-row">
          <span class="peso-lbl">Peso seleccionado</span>
          <span class="peso-num" id="peso-num">100.0%</span>
        </div>
        <div class="peso-warn" id="peso-warn">&#9888; &Iacute;ndice parcial &mdash; no equivale al IPC General</div>
      </div>
    </div>
    <div class="chips" id="chips"></div>
  </div>

  <div class="cnt">
    <div class="kpis">
      <div class="kpi"><div class="kpi-lbl">&Uacute;ltimo IPC</div>
        <div class="kpi-v" id="k-ipc">&mdash;</div><div class="kpi-s" id="k-ipc-s">&mdash;</div></div>
      <div class="kpi"><div class="kpi-lbl">Inflaci&oacute;n Mensual</div>
        <div class="kpi-v" id="k-men">&mdash;</div><div class="kpi-s" id="k-men-s">m&aacute;s reciente</div></div>
      <div class="kpi"><div class="kpi-lbl">Inflaci&oacute;n Acumulada</div>
        <div class="kpi-v" id="k-acu">&mdash;</div><div class="kpi-s" id="k-acu-s">vs. dic a&ntilde;o anterior</div></div>
      <div class="kpi"><div class="kpi-lbl">Inflaci&oacute;n 12 Meses</div>
        <div class="kpi-v" id="k-12m">&mdash;</div><div class="kpi-s" id="k-12m-s">variaci&oacute;n interanual</div></div>
    </div>

    <div class="tbar">
      <div class="bgrp">
        <button class="tbtn viewbtn on" onclick="setView(\'both\',this)">Ambos</button>
        <button class="tbtn viewbtn" onclick="setView(\'ipc\',this)">IPC</button>
        <button class="tbtn viewbtn" onclick="setView(\'inf\',this)">Inflación</button>
      </div>
      <div class="bgrp" id="inf-grp">
        <button class="tbtn on"  data-t="todas"     onclick="setTipo(this)">Todas</button>
        <button class="tbtn"     data-t="Mensual"   onclick="setTipo(this)">Mensual</button>
        <button class="tbtn"     data-t="Acumulada" onclick="setTipo(this)">Acumulada</button>
        <button class="tbtn"     data-t="A12Meses"  onclick="setTipo(this)">12 Meses</button>
      </div>
      <div class="rbox">
        <span class="rlbl">Desde</span>
        <input type="range" id="r-ini" min="0" max="', n_idx, '" value="0"       oninput="setRng()">
        <span class="rv" id="r-ini-v">2018</span>
      </div>
      <div class="rbox">
        <span class="rlbl">Hasta</span>
        <input type="range" id="r-fin" min="0" max="', n_idx, '" value="', n_idx, '" oninput="setRng()">
        <span class="rv" id="r-fin-v">2026</span>
      </div>
      <div class="spacer"></div>
    </div>

    <div class="plots">
      <div class="pcard" id="card-ipc">
        <div class="pc-h"><span class="pc-t">&Iacute;ndice de Precios al Consumidor</span>
          <span class="pc-s">Base 2016 = 100</span></div>
        <div class="pc-b" id="pl-ipc"></div>
      </div>
      <div class="pcard" id="card-inf">
        <div class="pc-h"><span class="pc-t">Inflaci&oacute;n (%)</span>
          <span class="pc-s" id="inf-hint">Mensual &middot; Acumulada &middot; 12 meses</span></div>
        <div class="pc-b" id="pl-inf"></div>
      </div>
    </div>
  </div>
</div>

<script>
const D = ', datos_json, ';
const POND_TOT = Object.values(D.pond_div).reduce((a,b)=>a+b,0);
const DIVS     = Object.keys(D.div_nombres);
const CLRS     = {Mensual:"#d6bfbb",Acumulada:"#5ea5ce",A12Meses:"#26456e"};

let sel  = new Set(DIVS);
let tipo = "todas";
let rIni = 0;
let rFin = D.fechas.length - 1;

// chips
function initChips(){
  const c=document.getElementById("chips");
  DIVS.forEach(div=>{
    const pct=(D.pond_div[div]/POND_TOT*100).toFixed(2);
    const el=document.createElement("div");
    el.className="chip on"; el.dataset.div=div;
    el.innerHTML=`<span class="cnum">${div}</span><span class="ctxt">${D.div_nombres[div]}</span><span class="cpct">${pct}%</span>`;
    el.addEventListener("click",()=>toggle(div,el));
    c.appendChild(el);
  });
}
function toggle(div,el){sel.has(div)?sel.delete(div):sel.add(div);el.classList.toggle("on",sel.has(div));update();}
function selAll(){DIVS.forEach(d=>sel.add(d));document.querySelectorAll(".chip").forEach(e=>e.classList.add("on"));update();}
function selNone(){sel.clear();document.querySelectorAll(".chip").forEach(e=>e.classList.remove("on"));update();}

// calcular IPC compuesto
function calcIPC(ds){
  const n=D.fechas.length;
  if(!ds.length) return new Array(n).fill(null);
  const pw=ds.map(d=>D.pond_div[d]), pt=pw.reduce((a,b)=>a+b,0);
  const pn=pw.map(p=>p/pt*100);
  const r=new Array(n).fill(0);
  ds.forEach((div,i)=>D.ipc_div[div].forEach((v,t)=>{if(v!==null)r[t]+=v*pn[i]/100;}));
  return r;
}

// inflaciones
function calcInf(ipc){
  const n=ipc.length,fechas=D.fechas;
  const men=new Array(n).fill(null);
  for(let i=1;i<n;i++) if(ipc[i]!=null&&ipc[i-1]!=null&&ipc[i-1]!==0) men[i]=(ipc[i]/ipc[i-1]-1)*100;

  const acu=new Array(n).fill(null);
  const años=fechas.map(f=>+f.slice(0,4)), meses=fechas.map(f=>+f.slice(5,7));
  for(let i=0;i<n;i++){
    const idx=fechas.map((_,j)=>años[j]===(años[i]-1)&&meses[j]===12?j:-1).filter(x=>x>=0);
    if(idx.length){const b=ipc[idx.at(-1)];if(b) acu[i]=(ipc[i]/b-1)*100;}
  }

  const m12=new Array(n).fill(null);
  for(let i=12;i<n;i++) if(ipc[i]!=null&&ipc[i-12]!=null&&ipc[i-12]!==0) m12[i]=(ipc[i]/ipc[i-12]-1)*100;

  return{men,acu,m12};
}

// rango y tipo
function setRng(){
  rIni=+document.getElementById("r-ini").value;
  rFin=+document.getElementById("r-fin").value;
  if(rIni>rFin){const t=rIni;rIni=rFin;rFin=t;}
  document.getElementById("r-ini-v").textContent=D.fechas[rIni].slice(0,4);
  document.getElementById("r-fin-v").textContent=D.fechas[rFin].slice(0,4);
  render();
}
function setTipo(btn){
  document.querySelectorAll("#inf-grp .tbtn").forEach(b=>b.classList.remove("on"));
  btn.classList.add("on"); tipo=btn.dataset.t;
  const h={todas:"mensual \u00b7 acumulada \u00b7 12 meses",Mensual:"variaci\u00f3n mes a mes",
           Acumulada:"respecto a dic a\u00f1o anterior",A12Meses:"variaci\u00f3n interanual"};
  document.getElementById("inf-hint").textContent=h[tipo]||"";
  render();
}
function setView(v,btn){

  document.querySelectorAll(".viewbtn")
    .forEach(b=>b.classList.remove("on"));

  if(btn) btn.classList.add("on");

  const plots = document.querySelector(".plots");
  const ipc   = document.getElementById("card-ipc");
  const inf   = document.getElementById("card-inf");

  // reset
  ipc.style.opacity = "1";
  inf.style.opacity = "1";

  ipc.style.visibility = "visible";
  inf.style.visibility = "visible";

  ipc.style.position = "relative";
  inf.style.position = "relative";

  ipc.style.pointerEvents = "auto";
  inf.style.pointerEvents = "auto";

  if(v==="both"){

    plots.style.gridTemplateColumns = "1fr 1fr";

    ipc.style.height = "";
    inf.style.height = "";
  }

  if(v==="ipc"){

    plots.style.gridTemplateColumns = "1fr";

    inf.style.opacity = "0";
    inf.style.visibility = "hidden";
    inf.style.position = "absolute";
    inf.style.pointerEvents = "none";
  }

  if(v==="inf"){

    plots.style.gridTemplateColumns = "1fr";

    ipc.style.opacity = "0";
    ipc.style.visibility = "hidden";
    ipc.style.position = "absolute";
    ipc.style.pointerEvents = "none";
  }

  requestAnimationFrame(()=>{
    requestAnimationFrame(()=>{
      Plotly.Plots.resize("pl-ipc");
      Plotly.Plots.resize("pl-inf");
    });
  });
}

// update
function update(){updatePeso();updateBadge();render();}

function updatePeso(){
  const ds=[...sel],tot=ds.reduce((a,d)=>a+D.pond_div[d],0),pct=(tot/POND_TOT*100).toFixed(2);
  document.getElementById("peso-num").textContent=pct+"%";
  document.getElementById("peso-warn").style.display=(pct>0&&pct<99.5)?"block":"none";
}
function updateBadge(){
  const ds=[...sel];
  if(!ds.length){document.getElementById("badge").textContent="Sin selecci\u00f3n";return;}
  const tot=ds.reduce((a,d)=>a+D.pond_div[d],0),pct=(tot/POND_TOT*100).toFixed(2);
  const lbl=ds.length===DIVS.length?"IPC General (100.00%)":
    ds.length===1?`${D.div_nombres[ds[0]]} (${pct}%)`:`${ds.length} divisiones (${pct}%)`;
  document.getElementById("badge").textContent=lbl;
}

function fmtF(iso){return new Date(iso+"T12:00:00").toLocaleDateString("es-BO",{month:"short",year:"numeric"});}
function lastNN(arr){for(let i=arr.length-1;i>=0;i--)if(arr[i]!==null)return{v:arr[i],i};return null;}

function updateKPIs(ipc,men,acu,m12){
  const setK=(id,sid,v,sub)=>{
    const el=document.getElementById(id);
    if(v===null){el.textContent="\u2014";el.className="kpi-v";return;}
    const isInf=Math.abs(v)<500;
    el.textContent=isInf?(v>=0?"+":"")+v.toFixed(2)+"%":v.toFixed(4);
    el.className="kpi-v"+(isInf?(v>0?" up":v<0?" dn":""):"");
    if(sid&&sub!==undefined)document.getElementById(sid).textContent=sub;
  };
  const lu=lastNN(ipc),lm=lastNN(men),la=lastNN(acu),l12=lastNN(m12);
  setK("k-ipc","k-ipc-s",lu?lu.v:null,lu?fmtF(D.fechas[lu.i]):"\u2014");
  setK("k-men","k-men-s",lm?lm.v:null,lm?fmtF(D.fechas[lm.i]):"m\u00e1s reciente");
  setK("k-acu","k-acu-s",la?la.v:null,la?"a "+fmtF(D.fechas[la.i]):"\u2014");
  setK("k-12m","k-12m-s",l12?l12.v:null,l12?"a "+fmtF(D.fechas[l12.i]):"\u2014");
}

const LAY = {
  paper_bgcolor: "#ffffff",
  plot_bgcolor: "#ffffff",
  margin: { l: 46, r: 10, t: 6, b: 15 },
  font: { family: "Inter, system-ui", color: "#475569", size: 11 },
  xaxis: { gridcolor: "#e2e8f0", linecolor: "#cbd5e1", zerolinecolor: "#cbd5e1", tickfont: { size: 10 }, hoverformat:"%b %Y"},
  yaxis: { gridcolor: "#e2e8f0", linecolor: "#cbd5e1", zerolinecolor: "#cbd5e1", tickfont: { size: 10 } },
  hovermode: "x unified",
  hoverlabel: { bgcolor: "#ffffff", bordercolor: "#cbd5e1", font: { family: "JetBrains Mono", size: 11, color: "#0f172a" } },
  legend: { bgcolor: "rgba(0,0,0,0)", font: { size: 11, color: "#475569" }, orientation: "h", y: -0.05, x: 0.15 }
};
const CFG={displayModeBar:false,responsive:true};

function render(){
  const ds=[...sel];
  const ipc=calcIPC(ds);
  const {men,acu,m12}=calcInf(ipc);
  const slc=a=>a.slice(rIni,rFin+1);
  const fec=slc(D.fechas),lbl=slc(D.labels);
  updateKPIs(ipc,men,acu,m12);

  // IPC
  Plotly.react("pl-ipc",[{
    x:fec,y:slc(ipc),type:"scatter",mode:"lines",name:"IPC",showlegend:false,
    line:{color:"#26456e",width:2},text:lbl,
    hovertemplate:"IPC: <b>%{y:.4f}</b><extra></extra>"
  }],Object.assign({},LAY,{showlegend:false,yaxis:Object.assign({},LAY.yaxis,{tickformat:".2f"})}),CFG);

  // Inflaciones
  const traces=[];
  const mk=(y,n,c)=>({x:fec,y:slc(y),type:"scatter",mode:"lines",name:n,
    line:{color:c,width:1.8},text:lbl,
    hovertemplate:`${n}: <b>%{y:.2f}%</b><extra></extra>`});
  if(tipo==="todas"||tipo==="Mensual")   traces.push(mk(men,"Mensual",   CLRS.Mensual));
  if(tipo==="todas"||tipo==="Acumulada") traces.push(mk(acu,"Acumulada", CLRS.Acumulada));
  if(tipo==="todas"||tipo==="A12Meses") traces.push(mk(m12,"12 Meses",  CLRS.A12Meses));
  const shapes=[{type:"line",xref:"x",yref:"y",x0:fec[0],x1:fec.at(-1),y0:0,y1:0,
    line:{color:"#2a3a55",width:1,dash:"dot"}}];
  Plotly.react("pl-inf",traces,
    Object.assign({},LAY,{showlegend:false,shapes,yaxis:Object.assign({},LAY.yaxis,{tickformat:".2f",ticksuffix:"%"})}),CFG);
}

initChips();
update();
window.addEventListener("resize",()=>{Plotly.Plots.resize("pl-ipc");Plotly.Plots.resize("pl-inf");});
</script>
</body>
</html>')

# ==============================================================================
# 7. GUARDAR
# ==============================================================================

archivo <- "monitor_ipc_bolivia.html"
writeLines(html, archivo, useBytes = FALSE)

cat(sprintf("  ✔ Archivo generado: %s  (%.0f KB)\n\n", archivo, file.size(archivo)/1024))
cat("─────────────────────────────────────────────\n")
cat("  Listo! Puedes abrir el HTML en cualquier navegador.\n")
cat("  Para actualizar solo debe agregar meses a los Excels\n")
cat("  y volver a correr este script: source(\"IPC por categoria.R\")\n")
cat("─────────────────────────────────────────────\n")
