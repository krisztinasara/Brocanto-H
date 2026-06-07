library(tidyverse)

# FUNKCIÓK ---------------------------------------------------------------------

# Adatelőkészítő funkció a GJ adattáblához
GJ_trial_fill = function(df, index_var, target_var, index, target) {
  idx = match(df[[index_var]], index)
  matched = !is.na(idx)
  df[[target_var]][matched] = target[idx[matched]]
  return(df)
}

# Hagyományos d' számítás funkció
#
# A d'-ot két arányból számoljuk:
#   - hit rate (HR): a célingerekre adott helyes "igen" válaszok aránya
#   - false alarm rate (FAR): a nem-targetekre adott "igen" válaszok aránya
# Majd mindkettőt z-score-rá alakítjuk és kivonjuk egymásból:
#   d' = z(HR) - z(FAR)
# Magasabb d' = jobb diszkrimináció, 0 körüli érték = véletlen szintű teljesítmény.
#
# Paraméterek:
#   df: a bemeneti adattábla(rész)
#   tnt_var: oszlopnév (idézőjelek nélkül), ami a "target vs. nontarget"
#            kategóriát tartalmazza (pl. trial_type)
#   response_var: oszlopnév (szintén idézőjel nélkül), a résztvevő válasza
#                 (Likert szám)
#   target_name: a tnt_var azon értéke, ami a célingert jelöli (pl. "seen_high_freq")
#   nontarget_name: a tnt_var azon értéke, ami a nem-célingert jelöli (pl. "illegal")
#   target_min: a Likert skálán az a minimális érték, amitől felfelé a választ
#               "igen, target"-ként kódoljuk (pl. 1-6 skálán 4 jelenti azt,
#               hogy 4, 5, 6 = "target", 1, 2, 3 = "nontarget")
#
d_prime = function(df, tnt_var, response_var, target_name, nontarget_name, target_min) {
  tryCatch({
    # Találatok száma: hány olyan target trial volt, ahol a válasz >= target_min
    # (azaz a résztvevő helyesen "igen, target"-nek jelölte a célingert)
    hits = df |> filter({{ tnt_var }} == target_name & {{ response_var }} >= target_min) |> nrow()
    # Téves riasztások száma: hány olyan nem target trial volt, ahol a résztvevő
    # mégis "igen, target"-nek jelölte (tehát rosszul döntött)
    false_alarms = df |> filter({{ tnt_var }} == nontarget_name & {{ response_var }} >= target_min) |> nrow()
    # Az összes target trial száma (ez lesz a "hit rate" nevezője)
    n_signal = df |> filter({{ tnt_var }} == target_name) |> nrow()
    # Az összes nontarget trial száma (ez lesz a "false alarm rate" nevezője)
    n_noise = df |> filter({{ tnt_var }} == nontarget_name) |> nrow()
    # Arányok kiszámítása
    hit_rate = hits / n_signal
    false_alarm_rate = false_alarms / n_noise
    # Szélsőérték-korrekció:
    # A qnorm(0) = -Inf és qnorm(1) = +Inf, ami tönkretenné a d' értékét.
    # Ha egy résztvevő tökéletesen teljesít (hit rate = 1) vagy semmit sem ismer
    # fel (false alarm rate = 0), akkor 0.99-re ill. 0.01-re módosítjuk az
    # értéket. Ez a Macmillan & Kaplan (1985) klippelő korrekció rögzített
    # küszöbökkel. Ezzel a d' maximuma itt qnorm(0.99) - qnorm(0.01) ≈ 4.65.
    if (hit_rate == 1) hit_rate = 0.99
    if (hit_rate == 0) hit_rate = 0.01
    if (false_alarm_rate == 0) false_alarm_rate = 0.01
    if (false_alarm_rate == 1) false_alarm_rate = 0.99
    # A két arányt z-pontszámra alakítjuk, és kivonjuk egymásból
    d_prime = qnorm(hit_rate) - qnorm(false_alarm_rate)
    # Ha valamiért mégis NaN, NA vagy Inf jönne ki, akkor NA-t adunk vissza
    if (!is.finite(d_prime)) return(NA_real_)
    return(d_prime)
  }, error = function(e) NA_real_)
}

# Likert skálás érzékenységi index számítás funkció
#
# Ez a függvény a hagyományos (bináris) d' gradált változata. A logika azonos
# (z(HR) - z(FAR)), de itt nem dichotomizáljuk a Likert válaszokat egy küszöb
# alapján, hanem a teljes válaszskálát használjuk:
#   - "hit rate" analóg = a target trialeken kapott összes pontszám,
#     osztva a maximálisan elérhető pontszámmal (n_target * scale_max).
#   - "false alarm rate" analóg = ugyanez a nontarget trialeken.
# Mivel a Likert skála 1-től 6-ig terjed (és nem 0-tól kezdődik), minden választ
# eltolunk scale_min-nel, hogy a [0, 1] tartományba essen a kiszámolt arány.
# Pl. 1-6 skálán a válaszokat átalakítjuk [0, 0.2, 0.4, 0.6, 0.8, 1]-re.
#
# Paraméterek:
#   df: a bemeneti adattábla(rész)
#   tnt_var: target/nontarget kategória oszlopa (idézőjel nélkül)
#   response_var: Likert válasz oszlopa (idézőjel nélkül)
#   target_name: a target címkéje
#   nontarget_name: a nem-target címkéje
#   scale_min: a Likert skála legkisebb értéke (pl. 1)
#   scale_max: a Likert skála legnagyobb értéke (pl. 6)
#
likert_sensitivity = function(df, tnt_var, response_var, target_name, nontarget_name, scale_min, scale_max) {
  tryCatch({
    # A skála teljes terjedelme, pl. 1-6 esetén 6 - 1 = 5;
    # ezzel osztunk normalizáláskor
    scale_range = scale_max - scale_min
    # Két vektorba gyűjtjük a célinger és a nem-célinger trialeken adott
    # Likert válaszokat
    target_responses = df |> filter({{ tnt_var }} == target_name) |> pull({{ response_var }})
    nontarget_responses = df |> filter({{ tnt_var }} == nontarget_name) |> pull({{ response_var }})
    # Normalizálás [0, 1] tartományra:
    # - target_responses - scale_min: minden választ módosítunk úgy, hogy a
    #   skála legkisebb értéke 0 legyen (1-6 skálából 0-5)
    # - length(target_responses) * scale_range: az elérhető maximális összpontszám
    #   az eltolt skálán (n trial * 5 az 1-6 skála esetén)
    # Az eredmény egy [0, 1]intervallumba eső arány
    hit_rate = sum(target_responses - scale_min) / (length(target_responses) * scale_range)
    false_alarm_rate = sum(nontarget_responses - scale_min) / (length(nontarget_responses) * scale_range)
    # Ugyanaz a szélsőérték-korrekció, mint a d_prime()-ban:
    if (hit_rate == 1) hit_rate = 0.99
    if (hit_rate == 0) hit_rate = 0.01
    if (false_alarm_rate == 0) false_alarm_rate = 0.01
    if (false_alarm_rate == 1) false_alarm_rate = 0.99
    # z-score-ok különbsége
    likert_sensitivity = qnorm(hit_rate) - qnorm(false_alarm_rate)
    # Try catch
    if (!is.finite(likert_sensitivity)) return(NA_real_)
    return(likert_sensitivity)
  }, error = function(e) NA_real_)
}

# ADATOK ELŐKÉSZÍTÉSE ----------------------------------------------------------

# Ez a kódrészlet csak előkészíti az adattáblákat, lefuttatható úgy, ahogy van.
# A working directoryt valószínűleg be kell állítani.

data = read_csv("results/BrocantoHPilot1MorphData.csv")

GJ = data |>
  filter(task_type == "GJ") |>
  arrange(participant, trial_id_global) |>
  mutate(
    response = as.numeric(response),
    legality = case_when(legality == "legal" ~ 1, legality == "illegal" ~ 0)
    ) |>
  select(participant_unique_id, trial_id_global, target_sentence, trial_type, legality, response)

GJ_s2_l1 = GJ$trial_type[11:20]
GJ_s2_l1_i = GJ$trial_id_global[11:20] - 10
GJ_s2_l2 = GJ$trial_type[31:40]
GJ_s2_l2_i = GJ$trial_id_global[31:40] - 10
GJ_s3_l1 = GJ$trial_type[151:160]
GJ_s3_l1_i = GJ$trial_id_global[151:160] - 10
GJ_s3_l2 = GJ$trial_type[171:180]
GJ_s3_l2_i = GJ$trial_id_global[171:180] - 10

for (GJ_block in list(list(GJ_s2_l1, GJ_s2_l1_i), list(GJ_s2_l2, GJ_s2_l2_i), list(GJ_s3_l1, GJ_s3_l1_i), list(GJ_s3_l2, GJ_s3_l2_i))) {
  GJ = GJ_trial_fill(GJ, "trial_id_global", "trial_type", GJ_block[[2]], GJ_block[[1]])
}

chunk = data |>
  filter(task_type == "Chunk") |>
  mutate(
    response = as.numeric(response),
    legality = case_when(legality == "legal" ~ 1, legality == "illegal" ~ 0)
    ) |>
  select(participant_unique_id, trial_id_global, target_sentence, trial_type, legality, response)

# SZÁMOLÁS ---------------------------------------------------------------------
