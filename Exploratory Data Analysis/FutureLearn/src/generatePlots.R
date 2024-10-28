library('ProjectTemplate')
load.project()

# Plots - First cycle
theme_update(plot.title = element_text(hjust = 0.5, size = 12))

archetype_plot = ggplot(archetypes_count, aes(x = run, y = count, colour = archetype)) +
  geom_line() +
  geom_point() +
  ggtitle("Number of learner archetypes between 3rd and 7th run") +
  labs(y = "Number of learners", x = "Run", colour = "Learner Archetype") 

certificate_plot = ggplot(enrolments_archetype, aes(x = archetype, fill = purchased_certificate)) + 
  facet_grid(. ~ run) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar() +
  ggtitle("Certificate Purchase by archetype between the 3rd and 7th run") +
  labs(y = "Number of learners", x = "Learner Archetype", fill = "Certificate Purchase")

# Plots - Second cycle

gender_plot = ggplot(enrolments_archetype_2, aes(x = archetype, fill = gender)) + 
  geom_bar(position = "dodge") +
  ggtitle("Distribution of gender by learner archetype") +
  labs(y = "Number of learners", x = "Learner Archetype", fill = "Gender")

employment_plot = ggplot(enrolments_archetype_2, aes(x = archetype, fill = employment_status)) + 
  geom_bar() +
  ggtitle("Distribution of employment status by learner archetype") +
  labs(y = "Number of learners", x = "Learner Archetype", fill = "Employment Status")

employment_plot_2 = ggballoonplot(
  employment_status, x = "archetype", y = "employment_status",
  fill = "count", show.label = TRUE, font.label = list(size = 9, color = "white")
  ) +
  ggtitle("Frequency of employment status by learner archetype") +
  labs(fill = "Frequency") +
  guides(size = FALSE)

employment_plot_3 = ggplot(enrolments_archetype_2, aes(x = archetype, fill = employment_status), main = "Title") + 
  facet_grid(. ~ gender) +
  geom_bar() +
  ggtitle("Distribution of employment status by archetype vs gender") +
  labs(x = "Learner Archetype", y = "Number of learners", fill = "Employment Status") +
  scale_fill_discrete(labels = c("Full time student", "Looking for work", "Retired",
                                 "Self employed", "Unemployed", "Working Full Time", 
                                 "Working Part Time"))

education_plot = ggplot(enrolments_archetype_2, aes(x = archetype, fill = highest_education_level)) + 
  geom_bar(position = "fill") +
  ggtitle("Distribution of highest education level by learner archetype") +
  labs(x = "Learner Archetype", y = "Percentage of learners", fill = "Highest Education Level") +
  scale_fill_discrete(labels = c("Less than secondary", "Professional", "Secondary", 
                                 "Tertiary", "Undergrad", "PhD", "Masters"))

age_range_plot = ggplot(enrolments_archetype_2, aes(x = archetype, fill = age_range)) + 
  facet_grid(. ~ gender) +
  geom_bar(position = "dodge") +
  ggtitle("Distribution of the age range by learner archetype vs gender") +
  labs(x = "Learner Archetype", y = "Number of learners", fill = "Age Range")