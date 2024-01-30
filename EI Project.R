library(readxl)
data = read_excel("/home/pradnya/Downloads/Emotional Intelligence (Responses) (1).xlsx")
View(data)
summary(data)
data = data[,-1] # removing timestamp
View(data)
# As 'Age' is the char column I want to convert
data$Age = as.integer(data$Age)
summary(data)
# Print the column names directly
print(colnames(data))

data = data[,-(9:33)] # removing the 30 questions of the scale
View(data)
print(colnames(data))
# 25 columns out of which 5 are emotional answers and rest 20 are variables

comment('
Parameters That I want to use:
[1] "Age"
[2] "Gender"
[3] "Your Blood group"
[4] "Emotional Awareness"
[5] "Managing one’s emotions"
[6] "Self motivation"
[7] "Empathy items"
[8] "Coaching other’s emotions"
[10] "Family type:"    # joint nuclear
[17] "What is your dietary preference?"
')

data = data[, -c(12:15, 16, 18:25,9,11)]
View(data) # Data is ready temporary with types of columns desired
print(colnames(data))

########  Understanding the data ########
# checking individual variables 

summary(data$Age)
"'''
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   8.00   20.00   21.00   21.49   22.00   55.00 
'''"

table(data$Gender)
"'''
Bisexual   Female     Male 
       1      123       79 
'''"
table(data$`Your Blood group`)

#Checking scores 
variables <- data[, c("Emotional Awareness", "Managing one’s emotions", 
                      "Self motivation", "Empathy items", 
                      "Coaching other’s emotions")]
View(variables)
summary(variables)


#checking family type
table(data$`Family type:`)
"'''
  Joint Nuclear 
     57     146
'''"

# Meditate
table(data$`Do you meditate on a daily basis?`)

# Diet
table(data$`What is your dietary preference?`)
# Mapping for dietary preferences to veg or nonveg
diet_mapping <- c(
  "Both" = "nonveg",
  "Crow biriyani" = "nonveg",
  "Eggetarian" = "nonveg",
  "Keto" = "nonveg",
  "Mathura" = "veg",
  "No preference as such, avoid red meat" = "veg",
  "Non-vegetarian" = "nonveg",
  "Pure veg" = "veg",
  "Veg Nonveg" = "nonveg",
  "Vegan" = "veg",
  "Vegetarian" = "veg"
)

# Re-codee the values in the 'What is your dietary preference?' column
data$`What is your dietary preference?` <- diet_mapping[data$`What is your dietary preference?`]

# Display the updated table
diet_counts <- table(data$`What is your dietary preference?`)
pie(diet_counts, labels = paste(names(diet_counts), ": ", diet_counts), 
    col = rainbow(length(diet_counts)), main = "Dietary Preferences",
    cex.main = 1.2, cex = 0.8)

# Add a legend
legend("topright", legend = names(diet_counts), fill = rainbow(length(diet_counts)), 
       title = "Diet Type", cex = 0.8)

############################### analysis ##########################


## gender wise variables relation

library(ggplot2)
library(dplyr)
library(tidyr)
# data being inadequate for bisexual and for simplicity removing that obs
# Subset relevant columns
your_data <- data[data$Gender != 'Bisexual', ]
View(your_data)
table(your_data$Gender)
selected_variables <- your_data[, c("Gender", "Emotional Awareness", "Managing one’s emotions", 
                               "Self motivation", "Empathy items", "Coaching other’s emotions")]

# Reshape the data for ggplot
selected_variables_long <- selected_variables %>%
  pivot_longer(cols = -c(Gender), names_to = "Variable", values_to = "Value")

# Plot gender-wise density distributions for selected variables
ggplot(selected_variables_long, aes(x = Value, fill = Gender)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  labs(title = "Gender-wise Distribution of Scoring Parameters",
       x = "Value", fill = "Gender") +
  theme_minimal()



#### blood group wise parameters

# Subset relevant columns
selected_variables_blood <- data[, c("Age","Your Blood group", "Emotional Awareness", "Managing one’s emotions", 
                                     "Self motivation", "Empathy items", "Coaching other’s emotions")]

selected_variables_blood_long <- selected_variables_blood %>%
  pivot_longer(cols = -c(`Your Blood group`, Age), names_to = "Variable", values_to = "Value")

# Plot blood group-wise comparison for each variable
ggplot(selected_variables_blood_long, aes(x = `Your Blood group`, y = Value, fill = `Your Blood group`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  labs(title = "Blood Group-wise Comparison of Scoring Parameters",
       x = "Blood Group", y = "Value", fill = "Blood Group") +
  theme_minimal()


### to check how age wise the variables behave

selected_variables_age <- data[, c("Age", "Emotional Awareness", "Managing one’s emotions", 
                                   "Self motivation", "Empathy items", "Coaching other’s emotions")]

# Convert 'Age' to numeric (assuming it's stored as a factor)
selected_variables_age$Age <- as.numeric(as.character(selected_variables_age$Age))

# Reshape the data for ggplot
selected_variables_age_long <- selected_variables_age %>%
  pivot_longer(cols = -c(Age), names_to = "Variable", values_to = "Value")

# Plot age group-wise comparison for each variable
ggplot(selected_variables_age_long, aes(x = factor(Age), y = Value, fill = factor(Age))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  labs(title = "Age Group-wise Comparison of Selected Variables",
       x = "Age Group", y = "Value", fill = "Age Group") +
  theme_minimal()




'How does dietary preference influence the scoring parameters; 
People eating veg or nonveg have more self control ie managing one’s emotion.
Understanding these emotional deficiencies can provide a means to subsequently 
improve the quality of consumption decisions.So.. should you be vegetarian or a 
nonvegetarian?!!! '
## How Dietary preference effect the Scoring Parameters
print(colnames(data))
# List of scoring parameters
scoring_parameters <- c("Emotional Awareness", "Managing one’s emotions", "Self motivation", "Empathy items", "Coaching other’s emotions")

# Create box plots for each scoring parameter by dietary preference
for (param in scoring_parameters) {
  plot_title <- paste(param, "by Dietary Preference")
  
  p <- data %>%
    ggplot(aes(x = `What is your dietary preference?`, y = !!sym(param))) +
    geom_boxplot() +
    labs(title = plot_title) +
    theme_minimal()
  
  print(p)
}

selected_variablesdiet <- data[, c("What is your dietary preference?", "Emotional Awareness", "Managing one’s emotions", 
                                    "Self motivation", "Empathy items", "Coaching other’s emotions")]

# Reshape the data for ggplot
selected_variables_long <- selected_variablesdiet %>%
  pivot_longer(cols = -c(`What is your dietary preference?`), names_to = "Variable", values_to = "Value")

# Plot gender-wise density distributions for selected variables
ggplot(selected_variables_long, aes(x = Value, fill = `What is your dietary preference?`)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  labs(title = "Dietary preference Distribution of Scoring Parameters",
       x = "Value", fill = "What is your dietary preference?") +
  theme_minimal()




##How does family type influence the Emotional Parameters? 
print(colnames(data))
table(data$`Family type:`)
emotional_parameters <- c("Emotional Awareness", "Managing one’s emotions", 
                          "Self motivation", "Empathy items", 
                          "Coaching other’s emotions")

# Create box plots in a loop
for (param in emotional_parameters) {
  plot_title <- paste(param, "by Family Type")
  
  p <- data %>%
    ggplot(aes(x = `Family type:`, y = !!sym(param))) +
    geom_boxplot() +
    labs(title = plot_title) +
    theme_minimal()
  
  print(p)
  # Kruskal-Wallis test
  kruskal_results <- kruskal.test(data[[param]], data$`Family type:`)
  print(kruskal_results)
}
#plotting density
selected_variables_fam <- data[, c("Family type:", "Emotional Awareness", "Managing one’s emotions", 
                                   "Self motivation", "Empathy items", "Coaching other’s emotions")]

# Reshape the data for ggplot
selected_variables_long <- selected_variables_fam %>%
  pivot_longer(cols = -c(`Family type:`), names_to = "Variable", values_to = "Value")

# Plot gender-wise density distributions for selected variables
ggplot(selected_variables_long, aes(x = Value, fill = `Family type:`)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  labs(title = "Family Type Distribution of Scoring Parameters",
       x = "Value", fill = "Family type:") +
  theme_minimal()

