# FIRST CYCLE

# Add column run to identify the runs in the enrolments data
cyber.security.3_enrolments = cyber.security.3_enrolments |> mutate(run = 3)
cyber.security.4_enrolments = cyber.security.4_enrolments |> mutate(run = 4)
cyber.security.5_enrolments = cyber.security.5_enrolments |> mutate(run = 5)
cyber.security.6_enrolments = cyber.security.6_enrolments |> mutate(run = 6)
cyber.security.7_enrolments = cyber.security.7_enrolments |> mutate(run = 7)

# Add column run to identify the runs in the archetype survey response data
cyber.security.3_archetype.survey.responses = cyber.security.3_archetype.survey.responses |> mutate(run = 3)
cyber.security.4_archetype.survey.responses = cyber.security.4_archetype.survey.responses |> mutate(run = 4)
cyber.security.5_archetype.survey.responses = cyber.security.5_archetype.survey.responses |> mutate(run = 5)
cyber.security.6_archetype.survey.responses = cyber.security.6_archetype.survey.responses |> mutate(run = 6)
cyber.security.7_archetype.survey.responses = cyber.security.7_archetype.survey.responses |> mutate(run = 7)

# Append all enrolments data frames
enrolments = bind_rows(cyber.security.3_enrolments) |>
  bind_rows(cyber.security.4_enrolments) |>
  bind_rows(cyber.security.5_enrolments) |>
  bind_rows(cyber.security.6_enrolments) |>
  bind_rows(cyber.security.7_enrolments)

# Append all archetype survey responses data frames
archetype_survey_responses = bind_rows(cyber.security.3_archetype.survey.responses) |>
  bind_rows(cyber.security.4_archetype.survey.responses) |>
  bind_rows(cyber.security.5_archetype.survey.responses) |>
  bind_rows(cyber.security.6_archetype.survey.responses) |>
  bind_rows(cyber.security.7_archetype.survey.responses)

# Ensure that all roles in the enrolments dataframe are learners 
enrolments = enrolments |> filter(role == "learner")

# Remove archetypes that are not classified in the 7 archetypes
archetype_survey_responses = archetype_survey_responses |> filter(archetype %in% c("Vitalisers", "Explorers", "Fixers", "Advancers", "Preparers", "Flourishers", "Hobbyists"))

# Join archetype survey responses and enrolments dataframes by learner id and run
enrolments_archetype = left_join(archetype_survey_responses, enrolments, by = c("learner_id", "run"))

# Drop duplicates 
enrolments_archetype = enrolments_archetype |> distinct(learner_id, run, .keep_all = T)

# Drop unnecessary columns which are not needed for both cycles
enrolments_archetype = enrolments_archetype |> select(learner_id, archetype, run, purchased_statement_at, gender, age_range:detected_country)

# Add column to transform the certificate purchase into a categorical variable (yes, no)
enrolments_archetype = enrolments_archetype |> mutate(
  purchased_certificate = if_else(purchased_statement_at != "", "Yes", "No"),
  .after = purchased_statement_at
)

# Create dataframe that counts the archetypes per run
archetypes_count = enrolments_archetype |> dplyr::summarise(count = n(), .by = c(archetype, run))

# Add the count for the missing tuple (Preparers for the third run) which is 0
archetypes_count = archetypes_count |> dplyr::add_row(archetype = "Preparers", run = 3, count = 0)


# SECOND CYCLE 

# Drop duplicates between the tuples learner id and archetype because we are not discriminating by runs in this cycle
enrolments_archetype_2 = enrolments_archetype |> distinct(learner_id, archetype, .keep_all = T)

# Drop unnecessary columns which are not needed for this cycle
enrolments_archetype_2 = enrolments_archetype_2 |> select(archetype, gender:employment_status)

# Select the two most common archetypes (Vitalisers and Explorers) AND remove Unknown values for the columns needed in the analysis
enrolments_archetype_2 = enrolments_archetype_2 |>
  filter(archetype %in% c('Vitalisers', 'Explorers'))  |>
  filter(
    gender != 'Unknown' &
      employment_status != 'Unknown' &
      highest_education_level != 'Unknown' &
      age_range != 'Unknown'
  )

# Change the not_working value to unemployed in the employment status column
enrolments_archetype_2 = enrolments_archetype_2 |> mutate(employment_status = replace(employment_status, employment_status == "not_working", "unemployed"))

# Create dataframe that counts the archetypes per employment status for a specific format needed for a plot
employment_status = enrolments_archetype_2 |> dplyr::summarise(count = n(), .by = c(archetype, employment_status))