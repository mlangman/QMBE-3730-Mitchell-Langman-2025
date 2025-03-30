#Part1
#1
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(readxl)) install.packages("readxl", dependencies = TRUE)

library(ggplot2)
library(readxl)

print(getwd())


print(list.files())

df <- read_excel("/Users/mitchelllangman/Downloads/wages (1).xlsx")  # Update the path as needed

str(df)
head(df)

ggplot(df, aes(x = Age, y = Wage)) +
  geom_point(color = "blue") +
  labs(title = "Wage vs Age", x = "Age", y = "Wage") +
  theme_minimal()

linear_model <- lm(Wage ~ Age, data = df)
summary(linear_model)

quadratic_model <- lm(Wage ~ poly(Age, 2), data = df)
summary(quadratic_model)

anova(linear_model, quadratic_model)

ggplot(df, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +  # Linear fit
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "green", se = FALSE) +  # Quadratic fit
  labs(title = "Linear vs Quadratic Fit", x = "Age", y = "Wage") +
  theme_minimal()


#2
multi_model <- lm(Wage ~ Age + Educ, data = df)

summary(multi_model)

interaction_model <- lm(Wage ~ Age * Educ, data = df)
summary(interaction_model)

anova(multi_model, interaction_model)

library(ggplot2)
ggplot(df, aes(x = Age, y = Wage, color = factor(Educ))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Wage vs Age by Education Level", x = "Age", y = "Wage", color = "Education Level") +
  theme_minimal()

#3
quad_model <- lm(Wage ~ Age + I(Age^2) + Educ, data = df)

summary(quad_model)

linear_model <- lm(Wage ~ Age + Educ, data = df)  # Linear model for comparison
anova(linear_model, quad_model)  # Model comparison

library(ggplot2)

ggplot(df, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Quadratic fit
  labs(title = "Quadratic Wage vs Age Relationship", x = "Age", y = "Wage") +
  theme_minimal()

#4
quad_model <- lm(Wage ~ Age + I(Age^2) + Educ, data = df)
new_data <- data.frame(Age = c(30, 50, 70), 
                       Educ = c(16, 16, 16))
predicted_wages <- predict(quad_model, new_data)
print(predicted_wages)
results <- data.frame(Age = new_data$Age, 
                      Educ = new_data$Educ, 
                      Predicted_Wage = predicted_wages)
print(results)

#5
quad_model <- lm(Wage ~ Age + I(Age^2) + Educ, data = df)

b1 <- coef(quad_model)["Age"]
b2 <- coef(quad_model)["I(Age^2)"]

optimal_age <- -b1 / (2 * b2)
print(optimal_age)

#Part2
#1
install.packages("readxl")  # For reading Excel files
install.packages("ggplot2") # For visualization

library(readxl)
library(ggplot2)

df <- read_excel(file.choose())  # Opens a file picker

print(colnames(df))


ggplot(df, aes(x = Beds, y = Rent)) +
  geom_point() +
  ggtitle("Rent vs Beds") +
  xlab("Number of Beds") +
  ylab("Rent")

ggplot(df, aes(x = Baths, y = Rent)) +
  geom_point() +
  ggtitle("Rent vs Baths") +
  xlab("Number of Baths") +
  ylab("Rent")

ggplot(df, aes(x = Sqft, y = Rent)) +
  geom_point() +
  ggtitle("Rent vs Sqft") +
  xlab("Square Footage") +
  ylab("Rent")

ggplot(df, aes(x = log(Sqft), y = Rent)) +
  geom_point() +
  ggtitle("Rent vs Log(Sqft)") +
  xlab("Log(Sqft)") +
  ylab("Rent")

#2
df$Log_Sqft <- log(df$Sqft)

model <- lm(Rent ~ Beds + Baths + Log_Sqft, data = df)

summary(model)

new_rental <- data.frame(Beds = 3, Baths = 2, Log_Sqft = log(1600))

predicted_rent <- predict(model, newdata = new_rental)
print(predicted_rent)

