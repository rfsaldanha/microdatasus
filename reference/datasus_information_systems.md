# Consult supported DataSUS information systems

Lists every canonical value accepted by the `information_system`
argument of
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).
Names and file acronyms follow the DataSUS transfer portal. Operational
metadata comes directly from the same registry used for file discovery,
and SINAN aliases come from the registry shared with
[`process_sinan()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinan.md)
and
[`fetch_tabwin_dictionary()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_tabwin_dictionary.md).

## Usage

``` r
datasus_information_systems()
```

## Value

A tibble with 93 rows and eight columns:

- `information_system`:

  Preferred identifier accepted by the API.

- `system`:

  Source system: SIM, SIH, SINASC, CNES, SIA, or SINAN.

- `name`:

  Human-readable Portuguese name.

- `file_acronym`:

  Acronym used in DataSUS DBC file names.

- `periodicity`:

  Publication interval, `"year"` or `"month"`.

- `geography`:

  File coverage, `"state"` or `"national"`.

- `minimum_date`:

  Earliest date supported by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

- `aliases`:

  List-column of accepted alternative identifiers.

## References

[DataSUS file transfer
portal](https://datasus.saude.gov.br/transferencia-de-arquivos/)

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
[`process_sinan()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinan.md),
[`fetch_tabwin_dictionary()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_tabwin_dictionary.md)

## Examples

``` r
systems <- datasus_information_systems()
systems[, c("information_system", "system", "name")]
#> # A tibble: 93 × 3
#>    information_system system name                                     
#>    <chr>              <chr>  <chr>                                    
#>  1 SIM-DO             SIM    Declarações de óbito                     
#>  2 SIM-DOFET          SIM    Declarações de óbitos fetais             
#>  3 SIM-DOEXT          SIM    Declarações de óbitos por causas externas
#>  4 SIM-DOINF          SIM    Declarações de óbitos infantis           
#>  5 SIM-DOMAT          SIM    Declarações de óbitos maternos           
#>  6 SIH-RD             SIH    AIH reduzida                             
#>  7 SIH-RJ             SIH    AIH rejeitadas                           
#>  8 SIH-SP             SIH    Serviços profissionais                   
#>  9 SIH-ER             SIH    AIH rejeitadas com código de erro        
#> 10 SINASC             SINASC Declarações de nascidos vivos            
#> # ℹ 83 more rows
systems[systems$system == "SINAN", ]
#> # A tibble: 58 × 8
#>    information_system            system name  file_acronym periodicity geography
#>    <chr>                         <chr>  <chr> <chr>        <chr>       <chr>    
#>  1 SINAN-ACIDENTE-POR-ANIMAIS-P… SINAN  Acid… ANIM         year        national 
#>  2 SINAN-ATENDIMENTO-ANTIRRABICO SINAN  Aten… ANTR         year        national 
#>  3 SINAN-AIDS-EM-ADULTOS         SINAN  AIDS… AIDA         year        national 
#>  4 SINAN-AIDS-EM-CRIANCAS        SINAN  AIDS… AIDC         year        national 
#>  5 SINAN-BOTULISMO               SINAN  Botu… BOTU         year        national 
#>  6 SINAN-COLERA                  SINAN  Cóle… COLE         year        national 
#>  7 SINAN-COQUELUCHE              SINAN  Coqu… COQU         year        national 
#>  8 SINAN-DENGUE                  SINAN  Deng… DENG         year        national 
#>  9 SINAN-DIFTERIA                SINAN  Dift… DIFT         year        national 
#> 10 SINAN-DOENCA-DE-CREUTZFELDT-… SINAN  Doen… DCRJ         year        national 
#> # ℹ 48 more rows
#> # ℹ 2 more variables: minimum_date <date>, aliases <list>
```
