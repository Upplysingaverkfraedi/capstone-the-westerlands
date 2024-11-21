-- gagnagrunnur fyrir kaupskrá, sótt af hagstofu, aðaltafla
CREATE TABLE IF NOT EXISTS kaupskra (
  faerslunumer int4,  
  emnr int4 NOT NULL,
  skjalanumer bpchar(13) PRIMARY KEY,  -- einkvæmt
  fastnum int4 NOT NULL,
  heimilisfang varchar(255),
  postnr NUMERIC,
  heinum int4,
  svfn int4 NOT NULL,
  sveitarfelag VARCHAR(255),
  utgdag TIMESTAMP,
  thinglystdags TIMESTAMP,
  kaupverd int4 NOT NULL CHECK (kaupverd > 0),
  fasteignamat int4,
  fasteignamat_gildandi NUMERIC,
  fyrirhugad_fasteignamat NUMERIC,
  brunabotamat_gildandi NUMERIC,
  byggar bpchar(4),
  fepilog VARCHAR(50),
  einflm NUMERIC CHECK (einflm > 0),
  lod_flm NUMERIC,
  lod_flmein VARCHAR(255),
  fjherb NUMERIC,
  tegund varchar(255),
  fullbuid INTEGER CHECK (fullbuid IN (0, 1)),
  onothaefur_samningur INTEGER CHECK (onothaefur_samningur IN (0, 1))
);

-- Add index to optimize joins and filters
CREATE INDEX IF NOT EXISTS idx_kaupskra_sveitarfelag_utgdag 
ON kaupskra (sveitarfelag, utgdag);

-- Mannfjöldi sveitarfélaga, sótt af hagstofu
CREATE TABLE IF NOT EXISTS mannfjoldi (
    sveitarfelag VARCHAR(50),
    aldur VARCHAR(50),
    ar BIGINT,
    kyn VARCHAR(50),
    mannfjoldi BIGINT
);

-- Add index to optimize joins
CREATE INDEX IF NOT EXISTS idx_mannfjoldi_sveitarfelag_ar 
ON mannfjoldi (sveitarfelag, ar);

-- Hjálpartafla fyrir reikninga
CREATE TABLE IF NOT EXISTS sveitarfelog_upplysingar (
    sveitarfelag VARCHAR(255),
    ar INT4,
    tegund VARCHAR(255),
    heildarverd NUMERIC,
    heildarfermetrar NUMERIC,
    fjoldi_eigna INT4,
    medal_fermetraverd NUMERIC,
    mannfjoldi BIGINT
);

-- hjálpartafla fyrir póstnúmer
CREATE TABLE IF NOT EXISTS samantekt_postnumer (
    sveitarfelag VARCHAR(255),
    postnr INT4,
    ar INT4,
    tegund VARCHAR(255),
    heildarverd NUMERIC,
    heildarfermetrar NUMERIC,
    fjoldi_eigna INT4,
    medal_fermetraverd NUMERIC
);

-- hjálpartafla fyrir herbergi
CREATE TABLE IF NOT EXISTS samantekt_herbergi (
    sveitarfelag VARCHAR(255),
    ar INT4,
    tegund VARCHAR(255),
    fjherb INT4,
    heildarverd NUMERIC,
    heildarfermetrar NUMERIC,
    fjoldi_eigna INT4,
    medal_fermetraverd NUMERIC
);

-- Hjálpartafla fyrir hlutfall tegunda
CREATE TABLE IF NOT EXISTS samantekt_tegund_eigna (
    sveitarfelag VARCHAR(255),
    ar INT,
    property_type VARCHAR(255),
    total_properties INT
);

-- Búum til sýndartöflu sem hreinsar gögnin í kaupskra
CREATE OR REPLACE VIEW v_kaupskra_hreinsuð AS
SELECT 
    sveitarfelag,
    EXTRACT(YEAR FROM utgdag) AS ar,
    postnr::INT4 AS postnr,
    fjherb::INT4 as fjherb,
    kaupverd,
    einflm,
    tegund
FROM kaupskra
WHERE fullbuid = 1 AND onothaefur_samningur = 0;

-- Búum til CTE fyrir sveitarfélög
WITH sveitarfelag_reiknad AS (
    SELECT 
        k.sveitarfelag,
        k.ar,
        k.tegund,
        SUM(k.kaupverd) AS heildarverd,
        SUM(k.einflm) AS heildarfermetrar,
        COUNT(*) AS fjoldi_eigna,
        SUM(k.kaupverd) / NULLIF(SUM(k.einflm), 0) AS medal_fermetraverd,
        m.mannfjoldi
    FROM v_kaupskra_hreinsuð k
    INNER JOIN mannfjoldi m
    ON k.sveitarfelag = m.sveitarfelag
       AND k.ar = m.ar
    GROUP BY k.sveitarfelag, k.ar, k.tegund, m.mannfjoldi
)
INSERT INTO sveitarfelog_upplysingar (
    sveitarfelag, ar, tegund, heildarverd, heildarfermetrar, fjoldi_eigna, medal_fermetraverd, mannfjoldi
)
SELECT * FROM sveitarfelag_reiknad;

-- Gerum CTE fyrir postnumersútreikninga
WITH postnumer_reiknad AS (
    SELECT 
        k.sveitarfelag,
        k.postnr,
        k.ar,
        k.tegund,
        SUM(k.kaupverd) AS heildarverd,
        SUM(k.einflm) AS heildarfermetrar,
        COUNT(*) AS fjoldi_eigna,
        SUM(k.kaupverd) / NULLIF(SUM(k.einflm), 0) AS medal_fermetraverd
    FROM v_kaupskra_hreinsuð k
    GROUP BY k.sveitarfelag, k.postnr, k.ar, k.tegund
)
INSERT INTO samantekt_postnumer (
    sveitarfelag, postnr, ar, tegund, heildarverd, heildarfermetrar, fjoldi_eigna, medal_fermetraverd
)
SELECT * FROM postnumer_reiknad;

-- Gerum CTE fyrir útreikninga fyrir fjölda herbergja
WITH herbergi_reiknad AS (
    SELECT 
        k.sveitarfelag,
        k.ar,
        k.tegund,
        k.fjherb,
        SUM(k.kaupverd) AS heildarverd,
        SUM(k.einflm) AS heildarfermetrar,
        COUNT(*) AS fjoldi_eigna,
        SUM(k.kaupverd) / NULLIF(SUM(k.einflm), 0) AS medal_fermetraverd
    FROM v_kaupskra_hreinsuð k
    GROUP BY k.sveitarfelag, k.ar, k.tegund, k.fjherb
)
INSERT INTO samantekt_herbergi (
    sveitarfelag, ar, tegund, fjherb, heildarverd, heildarfermetrar, fjoldi_eigna, medal_fermetraverd
)
SELECT * FROM herbergi_reiknad;

-- Fyllum samantekt_tegund_eigna með gögnum
INSERT INTO samantekt_tegund_eigna (sveitarfelag, ar, property_type, total_properties)
SELECT 
    sveitarfelag,
    EXTRACT(YEAR FROM utgdag) AS ar,
    tegund AS property_type,
    COUNT(*) AS total_properties
FROM 
    kaupskra
WHERE 
    fullbuid = 1 
    AND onothaefur_samningur = 0
GROUP BY 
    sveitarfelag, 
    EXTRACT(YEAR FROM utgdag), 
    tegund;
