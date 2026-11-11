# Group NYC 311 complaint_type values into 9 broad, policy-relevant clusters.
#
# The taxonomy below covers ALL 123 distinct complaint_type values in the file --
# not a sample. Assignments were made by reading, for each complaint type, its
# handling agency plus its most common `descriptor` and `location_type` values,
# which is what disambiguates the vague labels (see notes inline).
#
# Outputs
#   complaint_type_clusters.csv  - the mapping itself (one row per complaint type)
#   nyc_311_clustered.csv        - every request with its cluster assigned
#
# The script FAILS LOUDLY if any complaint type in the data is unmapped, or if the
# mapping names a type that does not exist in the data. That guard is the point:
# it means the mapping cannot silently drift out of sync with the dataset.

library(readr); library(dplyr)

csv_path <- file.path("data", "nyc_311_sample.csv")

# ---------------------------------------------------------------------------
# The taxonomy. Note that complaint_type is CASE-SENSITIVE in this data: HPD
# writes its housing codes in caps ("PLUMBING", "ELEVATOR") while DOB uses title
# case ("Plumbing", "Elevator"). They are distinct strings and both are listed.
# ---------------------------------------------------------------------------
clusters <- list(

  # HPD housing-maintenance codes + DOB construction/occupancy enforcement +
  # the DOHMH complaints that are really conditions inside a dwelling.
  "Housing & Building Conditions" = c(
    "HEAT/HOT WATER", "UNSANITARY CONDITION", "PLUMBING", "PAINT/PLASTER",
    "DOOR/WINDOW", "WATER LEAK", "GENERAL", "FLOORING/STAIRS", "ELECTRIC",
    "APPLIANCE", "SAFETY", "OUTSIDE BUILDING", "ELEVATOR",
    "General Construction/Plumbing", "Elevator", "Building/Use",
    "Special Projects Inspection Team (SPIT)", "Real Time Enforcement",
    "Emergency Response Team (ERT)", "Boilers", "Plumbing", "Electrical",
    "Cranes and Derricks", "Mold", "Indoor Air Quality", "Indoor Sewage",
    "Construction Lead Dust", "School Maintenance"
  ),

  # Kept as its own cluster: noise is ~23% of all requests and is a distinct
  # policy domain (NYPD response vs DEP construction/equipment enforcement).
  "Noise" = c(
    "Noise - Residential", "Noise - Street/Sidewalk", "Noise - Commercial",
    "Noise - Vehicle", "Noise", "Noise - Helicopter", "Noise - Park",
    "Noise - House of Worship"
  ),

  # Complaints about how vehicles are used or left, i.e. enforcement against a
  # vehicle -- as opposed to the condition of the roadway itself.
  "Traffic, Parking & Vehicles" = c(
    "Illegal Parking", "Blocked Driveway", "Abandoned Vehicle",
    "Derelict Vehicles", "Traffic", "Bike/Roller/Skate Chronic",
    "Bike/Roller/Skate", "E-Scooter", "Abandoned Bike"
  ),

  # The built streetscape: pavement, signals, signage, street furniture.
  "Streets & Public Infrastructure" = c(
    "Street Condition", "Street Light Condition", "Traffic Signal Condition",
    "Sidewalk Condition", "Curb Condition", "Street Sign - Damaged",
    "Street Sign - Missing", "Street Sign - Dangling", "Highway Condition",
    "Highway Sign - Damaged", "Bus Stop Shelter Complaint",
    "Broken Parking Meter", "Public Toilet", "Outdoor Dining"
  ),

  # DSNY collection, cleanliness and illegal-disposal enforcement.
  "Sanitation & Waste" = c(
    "Dirty Condition", "Illegal Dumping", "Missed Collection", "Graffiti",
    "Residential Disposal Complaint", "Obstruction", "Street Sweeping Complaint",
    "Sanitation Worker or Vehicle Complaint", "Dead Animal",
    "Litter Basket Request", "Litter Basket Complaint", "Dumpster Complaint",
    "Lot Condition", "Illegal Posting", "Snow or Ice", "Seasonal Collection"
  ),

  # DEP water/sewer network plus contamination and air-quality hazards.
  "Water, Sewer & Environmental Hazards" = c(
    "Water System", "Sewer", "Water Quality", "Water Conservation",
    "Drinking Water", "Lead", "Air Quality", "Hazardous Materials",
    "Oil or Gas Spill"
  ),

  # DPR: the urban forest and park facilities, including street trees.
  "Parks, Trees & Green Space" = c(
    "Damaged Tree", "Overgrown Tree/Branches", "Dead/Dying Tree",
    "New Tree Request", "Root/Sewer/Sidewalk Condition", "Illegal Tree Damage",
    "Uprooted Stump", "Wood Pile Remaining", "Maintenance or Facility",
    "Violation of Park Rules", "Animal in a Park"
  ),

  # Street-level disorder and vulnerability -- the complaints that get a police
  # or outreach response rather than an inspection.
  "Public Safety & Homelessness" = c(
    "Encampment", "Homeless Person Assistance", "Drug Activity",
    "Non-Emergency Police Matter", "Panhandling", "Drinking",
    "Illegal Fireworks", "Animal-Abuse"
  ),

  # DOHMH health/vector control + DCWP business regulation + TLC for-hire trade.
  "Public Health & Consumer Protection" = c(
    "Rodent", "Food Establishment", "Mobile Food Vendor", "Food Poisoning",
    "Day Care", "Consumer Complaint", "Vendor Enforcement", "Smoking or Vaping",
    "Standing Water", "Unsanitary Animal Pvt Property", "Unleashed Dog",
    "Harboring Bees/Wasps", "Unsanitary Pigeon Condition",
    "Illegal Animal Kept as Pet", "Pet Sale", "For Hire Vehicle Complaint",
    "Taxi Complaint", "Lost Property", "Taxi Report", "For Hire Vehicle Report"
  )
)

mapping <- stack(clusters) %>%
  transmute(complaint_type = as.character(values),
            cluster        = as.character(ind))

# ---------------------------------------------------------------------------
# Apply and validate
# ---------------------------------------------------------------------------
raw <- read_csv(csv_path, col_types = cols(.default = col_character()))

in_data    <- sort(unique(raw$complaint_type))
unmapped   <- setdiff(in_data, mapping$complaint_type)
phantom    <- setdiff(mapping$complaint_type, in_data)
duplicated <- mapping$complaint_type[duplicated(mapping$complaint_type)]

if (length(unmapped))   stop("Unmapped complaint types: ",   paste(unmapped, collapse = " | "))
if (length(phantom))    stop("Mapping names absent types: ", paste(phantom, collapse = " | "))
if (length(duplicated)) stop("Type in two clusters: ",       paste(duplicated, collapse = " | "))

cat("All", length(in_data), "distinct complaint types mapped to",
    length(clusters), "clusters; no gaps, no duplicates.\n\n")

labelled <- raw %>% left_join(mapping, by = "complaint_type")
stopifnot(!any(is.na(labelled$cluster)))   # belt and braces

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
mapping_out <- mapping %>%
  left_join(raw %>% count(complaint_type, name = "n_requests"), by = "complaint_type") %>%
  left_join(raw %>% group_by(complaint_type) %>%
              summarise(top_agency = names(sort(table(agency), decreasing = TRUE))[1],
                        top_descriptor = names(sort(table(descriptor), decreasing = TRUE))[1],
                        .groups = "drop"),
            by = "complaint_type") %>%
  arrange(cluster, desc(n_requests))

write_csv(mapping_out, file.path("output", "complaint_type_clusters.csv"))
write_csv(labelled,    file.path("output", "nyc_311_clustered.csv"))

summary_tbl <- labelled %>%
  count(cluster, name = "requests") %>%
  mutate(share = requests / sum(requests)) %>%
  arrange(desc(requests))

print(as.data.frame(summary_tbl), digits = 3)
cat("\nDistinct complaint types per cluster:\n")
print(as.data.frame(mapping %>% count(cluster, name = "n_types") %>% arrange(desc(n_types))))
cat("\nsaved output/complaint_type_clusters.csv and output/nyc_311_clustered.csv\n")
