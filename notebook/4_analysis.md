Analysis - spatial chemical mixture
================
Thursday, 23<sup>rd</sup> May 2024

## Data import

The initial phase of any data modeling endeavor necessitates the
meticulous acquisition of essential resources. This critical step
involves the importation of both specialized libraries and the raw
functions files themselves.

Beyond core libraries and data files, our setup will incorporate two
additional resources. Firstly, custom R functions will translate raw
geographical points into designated regions, adding spatial context.
Secondly, we’ll import Amazon S3 access keys, acting as the key to
unlock our data treasure chest stored in the cloud. These additional
files ensure a well-equipped environment for our data modeling journey.

``` r
source("spatial_library.R")
source("key_amazon.R")
```

## Geographical data

To enrich our spatial analysis, we’ll import a crucial dataset: the
geographical boundaries of Regione Lombardia’s 86 health districts. This
data comes in GeoJSON format, a popular and convenient choice for
representing geographical features. GeoJSON allows R to seamlessly
process the data, providing valuable insights into the spatial
distribution of our target variable. By incorporating this data, we can
explore potential geographical trends and relationships within the
health districts of Regione Lombardia.

``` r
district =  geojson_read("https://www.dati.lombardia.it/resource/9n45-7bpc.geojson",
                         what = "sp")
dt_district = tidy(district)
dt_district = data.table(dt_district)
dt_district = merge(dt_district,
                    district@data[,c("objectid_1","codice_ats",
                                     "distretto","descrizion")],
                    by.x = "id", 
                    by.y = "objectid_1")
dt_district$id = as.numeric(dt_district$id)
```

## Hospitalization data

Our data delves into healthcare utilization across Regione Lombardia’s
86 health districts for the year 2019. This dataset provides a detailed
breakdown of hospitalizations categorized by geographical location. To
gain deeper insights into specific healthcare needs, we’ll strategically
subselect the data. We’ll focus on hospitalizations related to
respiratory and cardiac issues, effectively filtering out other types of
admissions. Once this filtering is complete, we’ll embark on a crucial
step: grouping the data by health district. This process will involve
aggregating the number of respiratory and cardiac hospitalizations
within each district. By calculating the ratio of these combined
admissions to the total number of hospitalizations in each district, we
can unveil informative patterns. This analysis will expose variations in
the prevalence of respiratory and cardiac conditions across the health
districts of Regione Lombardia, uncovering potential areas of concern or
highlighting districts with lower hospitalization rates.

``` r
url_hospit = paste0("s3://envbran/maps/hospitalization.arrow")
hospit = arrow::read_ipc_file(url_hospit)
hospit = hospit[anno == 2019]

lh = hospit[descrizione_acc_diagnosi %in% c("MALATTIE DEL SISTEMA RESPIRATORIO") &
              descrizione_acc_intervento %in% c("PROCEDURE DIAGNOSTICHE E TERAPEUTICHE VARIE") &
                                          codice_mdc == "04"][, .(h_lungs = sum(ricoveri_do)),
                                                              by = .(latitudine_norm,longitudine_norm)]
hh = hospit[descrizione_acc_diagnosi %in% c("MALATTIE DEL SISTEMA CIRCOLATORIO") &
              descrizione_acc_intervento %in% c("PROCEDURE DIAGNOSTICHE E TERAPEUTICHE VARIE") &
              codice_mdc == "05"][, .(h_heart = sum(ricoveri_do)),
                                  by = .(latitudine_norm,longitudine_norm)]
h = hospit[descrizione_acc_intervento %in% c("PROCEDURE DIAGNOSTICHE E TERAPEUTICHE VARIE"), 
                                      .(h_all = sum(ricoveri_do)),
                                      by = .(latitudine_norm,longitudine_norm)]

DTh = Reduce(function(...) merge(..., all = TRUE, by = c("latitudine_norm",
                                                         "longitudine_norm")), 
             list(h,hh,lh))
setnames(DTh,c("latitudine_norm","longitudine_norm"), c("lat","lng"))
DTh = map_point(district, DTh, "district_id")
DTh = DTh[,.(h_all = sum(h_all,na.rm = T),
             h_heart = sum(h_heart,na.rm = T),
             h_lungs = sum(h_lungs,na.rm = T)), by = district_id]
```

## Water quality

To investigate potential environmental influences on health outcomes,
we’ll leverage the power of AWS S3 and the analytical capabilities of
DuckDB. DuckDB, a fast and efficient SQL database engine, resides within
our AWS S3 storage. By connecting to this database, we can extract
crucial data related to water quality across various sampling points
throughout the Lombardia region. This data will be meticulously filtered
to focus on the concentration levels of three specific elements in the
underground water: lead, manganese, and sulfates. These elements are
known to have potential health risks if present in high concentrations.

``` r
duckdb_con = DBI::dbConnect(duckdb::duckdb())

DT = open_dataset("s3://envbran/WQ/WQ_underground.arrow",format = "arrow") 
DT = DT |> to_duckdb(table_name = "test", 
                     con = duckdb_con)

WC = DT |>
  filter(anno == "2019") |>
  as.data.table() 

WC[, lat := unlist(WC$lat)]
WC[, lng := unlist(WC$lng)]

WC = map_point(district, WC, "district_id")
WC[, valore_numerico := as.numeric(gsub(",",".",valore_numerico))]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
WC[, valore_numerico := ifelse(segno == "<", 
                               valore_numerico/sqrt(2), valore_numerico)]

WC = WC[parametro %in% c("Manganese","Piombo","Solfati")
   ,.(valore_numerico = mean(valore_numerico,na.rm=T)), by = .(parametro,district_id)]

wq = dcast(WC, district_id ~ parametro)
```

    ## Using 'valore_numerico' as value column. Use 'value.var' to override

# Spatial statistical model

By establishing a connection between the water quality data and the
previously analyzed hospitalization data (with a focus on respiratory
and cardiac issues), we can embark on a fascinating exploration. We’ll
be investigating a potential correlation: is there a link between
elevated levels of these chemicals in the groundwater and the number of
hospitalizations for respiratory and cardiac issues within each health
district? This analysis could yield valuable insights. If a correlation
is identified, it could suggest a potential environmental influence on
public health within specific districts.

Poisson spatial regression model with a mixture: we have aggregated
count data $y_i$ at $86$ locations, and we expect that neighboring
locations will have similar counts. With a Poisson likelihood:

$$y_i \sim \text{Poisson}(\text{exp}(\beta_0 + \mathcal{T}_{i} \beta_1 + \varphi_i)) \quad \text{with} \quad
\mathcal{T}_i = \sum_{j = 1}^Mw_jx_j$$

where $\mathcal{T}_i$ is toxicity index defined by a weighted sum of the
metals involved while $\varphi_i$ is a spatial adjustment (→ see
`2_stan_model.md`).

We’ll build a map of connections between districts (adjacency matrix),
merge hospitalization and water quality data, calculate standardized
hospitalization ratios, and categorize contaminant levels by dividing
them into ten groups (deciles). These steps prepare us to analyze the
relationship between water quality and health outcomes.

``` r
district_nb = poly2nb(district,snap=0.0002)
district_nb = nb2mat(district_nb)
A = rowSums(district_nb != 0) * district_nb 

dt = merge(DTh, wq,
           by = c("district_id"),
           all = T)
dt$district_id = as.numeric(as.character(dt$district_id))
dt = dt[, lapply(.SD, function(x) replace(x, is.na(x), median(x, na.rm = TRUE)))]
dt[,':='(ratio_heart = 100*round(h_heart/h_all,2),
         ratio_lungs = 100*round(h_lungs/h_all,2),
         Manganese_q = 10*ecdf(Manganese)(Manganese),
         Solfati_q = 10*ecdf(Solfati)(Solfati),
         Piombo_q = 10*ecdf(Piombo)(Piombo))]
```

Before diving into Stan’s NUTS algorithm, data needs cleaning and
wrangling. Think of it as tidying a room for analysis. Stan (Bayesian
framework) uses NUTS to explore the probability of model parameters
given the data, ultimately revealing hidden patterns

``` r
check_cmdstan_toolchain(fix = TRUE)
file = "../stan/poisson_m/poisson_w.stan"
mod = cmdstan_model(file)

x = as.matrix(dt[,.(Manganese_q,Piombo_q,Solfati_q)])
W = A

full_d = list(n = NROW(dt), p = 3, X = x, 
              y = dt$h_heart, W = A)

fit = mod$sample(
  data = full_d,
  chains = 4,
  thin = 1,
  iter_warmup = 500,
  iter_sampling = 1000,
  parallel_chains = 4,
  refresh = 100,
  max_treedepth = 20
)
```

``` r
fit$summary(variables = c("we","Dalp","beta0","beta1"),
  posterior::default_summary_measures()[1:4],
  quantiles = ~ quantile2(., probs = c(0.025, 0.975)),
  posterior::default_convergence_measures()) |> 
  mutate(variable = c("Manganese","Lead","Sulfates","Dw_Mn","Dw_Pb","DW_Sulf","Intercept","Slope")) |> 
  mutate_if(is.numeric, round, 2) |> 
  kbl(caption = "MCMC 4 chains - Sampling: 1000 - Warmup: 500 - Thinning: 1") |>
  kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="color: black; font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
MCMC 4 chains - Sampling: 1000 - Warmup: 500 - Thinning: 1
</caption>
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
mad
</th>
<th style="text-align:right;">
q2.5
</th>
<th style="text-align:right;">
q97.5
</th>
<th style="text-align:right;">
rhat
</th>
<th style="text-align:right;">
ess_bulk
</th>
<th style="text-align:right;">
ess_tail
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Manganese
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
598.87
</td>
<td style="text-align:right;">
686.34
</td>
</tr>
<tr>
<td style="text-align:left;">
Lead
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
0.29
</td>
<td style="text-align:right;">
0.21
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
852.33
</td>
<td style="text-align:right;">
938.62
</td>
</tr>
<tr>
<td style="text-align:left;">
Sulfates
</td>
<td style="text-align:right;">
0.39
</td>
<td style="text-align:right;">
0.37
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
731.99
</td>
<td style="text-align:right;">
1098.26
</td>
</tr>
<tr>
<td style="text-align:left;">
Dw_Mn
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.58
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
2.57
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1263.55
</td>
<td style="text-align:right;">
1127.27
</td>
</tr>
<tr>
<td style="text-align:left;">
Dw_Pb
</td>
<td style="text-align:right;">
1.04
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
2.69
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1678.68
</td>
<td style="text-align:right;">
1893.32
</td>
</tr>
<tr>
<td style="text-align:left;">
DW_Sulf
</td>
<td style="text-align:right;">
1.14
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
3.01
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1395.65
</td>
<td style="text-align:right;">
1379.81
</td>
</tr>
<tr>
<td style="text-align:left;">
Intercept
</td>
<td style="text-align:right;">
6.20
</td>
<td style="text-align:right;">
6.24
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
4.71
</td>
<td style="text-align:right;">
7.47
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
537.61
</td>
<td style="text-align:right;">
773.04
</td>
</tr>
<tr>
<td style="text-align:left;">
Slope
</td>
<td style="text-align:right;">
0.25
</td>
<td style="text-align:right;">
0.24
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
1.01
</td>
<td style="text-align:right;">
491.26
</td>
<td style="text-align:right;">
656.74
</td>
</tr>
</tbody>
</table>
