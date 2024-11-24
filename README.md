# Sveitarfélög Íslands - Shiny App

Þetta Shiny forrit sýnir gögn um fermetraverð, mannfjölda og herbergjafjölda fyrir sveitarfélög á Íslandi.

## Hvernig á að keyra verkefnið

1. **Afritaðu verkefnið**:
   Klónaðu þetta verkefni úr geymslunni eða halaðu niður skjölunum.

2. **Settu upp nauðsynlega pakka**:
   Keyrðu skrána `install_packages.R` til að setja upp alla nauðsynlega R-pakka:
   ```bash
   Rscript install_packages.R

3. **Settu upp gagnagrunnsupplýsingar**: Búðu til config.yml skrá með upplýsingum um gagnagrunninn:
database:
  dbname: "railway"
  host: "junction.proxy.rlwy.net"
  port: 11050
  user: "lesenda_adgangur"

Búðu til .Renviron skrá og settu hana svona upp með lykilorði að gagnagrunninum: 

PG_PASSWORD=þitt_lykilorð
  
4. **Keyrsla**: Keyrðu forritið app.R

**Kröfur**
R (mælt með nýjustu útgáfu RStudio)