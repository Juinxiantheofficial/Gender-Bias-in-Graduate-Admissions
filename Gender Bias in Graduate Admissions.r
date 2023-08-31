
# Load UCBAdmissions dataset
data(UCBAdmissions)
# Print dataset to console
print(UCBAdmissions)

# Load broom package
library(broom)
# Convert UCBAdmissions to tidy format
ucb_tidy <- tidy(UCBAdmissions)

# Print tidy dataset to console
print(ucb_tidy)

library(testthat)
library(IRkernel.testthat)

soln_ucb_tidy <- tidy(UCBAdmissions)

run_tests({
    
    test_that("packages are loaded", {
        
        expect_true("broom" %in% .packages(), info = "Did you load the `broom` package?")
    
    })
    
    test_that("data is loaded and formatted correctly", {
        
        expect_true(exists("UCBAdmissions"),
                     info = "Did you load in the `UCBAdmissions` dataset with the `data()` function?")
        expect_identical(ucb_tidy, soln_ucb_tidy, info = "Did you tidy `UCBAdmissions` with the `tidy()` function?")

        
    })
})

# Load the dplyr library
library(dplyr)

# Aggregate over department
ucb_tidy_aggregated <- ucb_tidy %>% group_by(Admit,Gender) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(Admit =='Admitted')

# Print aggregated dataset
print(ucb_tidy_aggregated)

soln_ucb_tidy_aggregated <- soln_ucb_tidy %>% 
  group_by(Admit, Gender) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(Admit == "Admitted")

run_tests({
    test_that("packages are loaded", {
        expect_true("dplyr" %in% .packages(), info = "Did you load the `dplyr` package?")
    })
    
    test_that("data is prepared correctly", {
        expect_false("Dept" %in% colnames(ucb_tidy_aggregated), info = "Did you group by `Admit` and `Gender`?")
        expect_equal(soln_ucb_tidy_aggregated$n, ucb_tidy_aggregated$n, info = "Did you sum over `n` in the `summarize` function?")
        expect_equal(soln_ucb_tidy_aggregated$prop, ucb_tidy_aggregated$prop, info = "Did you calculate `prop` by dividing `n` by its sum?")
        expect_false("Rejected" %in% ucb_tidy_aggregated$Admit, info = "Did you filter for `Admitted` students only?")
    })
})

# Load the ggplot2 and scales packages
library(ggplot2)
library(scales)

# Prepare the bar plot
gg_bar <- ucb_tidy_aggregated %>% 
    ggplot(aes(x = Gender, y = prop, fill = Gender)) +
    geom_col() +
    geom_text(aes(label = percent(prop)), vjust = -1) +
    labs(title = "Acceptance rate of male and female applicants",
         subtitle = "University of California, Berkeley (1973)",
         y = "Acceptance rate") +
    scale_y_continuous(labels = percent , limits = c(0,0.5)) +
    guides(fill = FALSE)

# Print the bar plot
print(gg_bar)

soln_gg_bar <- soln_ucb_tidy_aggregated %>% 
    ggplot(aes(x = Gender, y = prop, fill = Gender)) +
    geom_col() +
    geom_text(aes(label = percent(prop)), vjust = -1) +
    labs(title = "Acceptance rate of male and female applicants",
         subtitle = "University of California, Berkeley (1973)",
         y = "Acceptance rate") +
    scale_y_continuous(labels = percent, limits = c(0, 0.5)) +
    guides(fill = FALSE)

run_tests({
    
    test_that("packages are loaded", {
        
        expect_true("ggplot2" %in% .packages(), info = "Did you load the `ggplot2` package?")
        expect_true("scales" %in% .packages(), info = "Did you load the `scales` package?")
        
    })
    
    test_that("the mappings are correct", {
        
        expect_identical(deparse(soln_gg_bar$mapping$x), deparse(gg_bar$mapping$x),
            info = 'The `x` aesthetic is incorrect. Did you map it to `Gender`?')
        expect_identical(deparse(soln_gg_bar$mapping$y), deparse(gg_bar$mapping$y),
            info = 'The `y` aesthetic is incorrect. Did you map it to `prop`?')
        expect_identical(deparse(soln_gg_bar$mapping$fill), deparse(gg_bar$mapping$fill),
            info = 'The `fill` aesthetic is incorrect. Did you map it to `Gender`?')
        
    })
    
    test_that("the parameters of geom_text() are correct", {
        
        expect_equal(soln_gg_bar$labels$label, gg_bar$labels$label,
                     info = "The `label` for `geom_text()` is incorrect. Did you set it to percent(prop)?")
        
        expect_identical(soln_gg_bar$layers[[2]]$aes_params$vjust, gg_bar$layers[[2]]$aes_params$vjust,
            info = 'The value of `vjust` is incorrect. Did you set it to `-1`?')
        
    })
    
    test_that("the parameters of scale_y_continuous() are correct", {
        
        expect_identical(ggplot_build(soln_gg_bar)$layout$panel_params[[1]][[9]], ggplot_build(gg_bar)$layout$panel_params[[1]][[9]],
                        info = "The labels for the y axis are incorrect. Did you set them to `percent` in `scale_y_continuous()`?")
        
        expect_identical(ggplot_build(soln_gg_bar)$layout$panel_params[[1]][[8]], ggplot_build(gg_bar)$layout$panel_params[[1]][[8]],
                        info = "The `limits` for `scale_y_continuous()` are incorrect. Did you set them to c(0, 0.5)?")
        
    })
})

# Calculate acceptance/rejection rate

ucb_by_dept <- ucb_tidy %>% 
    group_by(Gender, Dept) %>% 
    mutate(prop = n/sum(n)) %>% 
    filter(Admit == 'Admitted')

# Print the dataset
print(ucb_by_dept)

# Prepare the bar plot for each department
gg_bar_faceted <- ucb_by_dept %>% 
  ggplot(aes(Gender, prop, fill = Gender)) +
  geom_col() +
  geom_text(aes(label = percent(prop)), vjust = -1) +
  labs(title = "Acceptance rate of male and female applicants",
       subtitle = "University of California, Berkeley (1973)",
       y = "Acceptance rate") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  facet_wrap(~Dept) +
  guides(fill=F)

# Print the bar plot for each department
print(gg_bar_faceted)

soln_ucb_by_dept <- soln_ucb_tidy %>% 
    group_by(Gender, Dept) %>% 
    mutate(prop = n / sum(n)) %>% 
    filter(Admit == "Admitted")

soln_gg_bar_faceted <- soln_ucb_by_dept %>% 
  ggplot(aes(Gender, prop, fill = Gender)) +
  geom_col() +
  geom_text(aes(label = percent(prop)), vjust = -1) +
  labs(title = "Acceptance rate of male and female applicants",
       subtitle = "University of California, Berkeley (1973)",
       y = "Acceptance rate") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  facet_wrap(~Dept) +
  guides(fill = FALSE)

run_tests({
    
    test_that("data is correctly prepared", {
        
        expect_equal(group_vars(soln_ucb_by_dept), group_vars(ucb_by_dept), info = "Did you group by `Gender` and `Dept`?")
        expect_equal(soln_ucb_by_dept$prop, ucb_by_dept$prop, info = "Did you calculate `prop` as `n / sum(n)`?")
        expect_false("Rejected" %in% ucb_by_dept$Admit, info = "Did you filter for `Admitted` students only?")
        
    })
    
    test_that("plot is correctly prepared", {
        
        expect_identical(soln_gg_bar_faceted$facet$params$facets, gg_bar_faceted$facet$params$facets,
                        info = "Did you facet by department? Remember to use a one-sided formula: `~Dept`.")
        expect_identical(soln_gg_bar_faceted$guides, gg_bar_faceted$guides,
                         info = "Did you remove the legend? Remember to set `fill = FALSE`.")
        
    })
    
})

# Define function that repeats each row in each column n times
multiply_rows <- function(column, n) {
  rep(column, n)
}

# Create new de-aggregated data frame using the multiply_rows function
ucb_full <- data.frame(Admit = multiply_rows(ucb_tidy$Admit, ucb_tidy$n),
                      Gender = multiply_rows(ucb_tidy$Gender, ucb_tidy$n),
                      Dept = multiply_rows(ucb_tidy$Dept, ucb_tidy$n))

# Check the number of rows equals the number of students
nrow(ucb_full) == 4526

sln_multiply_rows <- function(column, n) {
  rep(column, n)
}

sln_ucb_full <- data.frame(Admit = sln_multiply_rows(soln_ucb_tidy$Admit, soln_ucb_tidy$n),
                      Gender = sln_multiply_rows(soln_ucb_tidy$Gender, soln_ucb_tidy$n),
                      Dept = sln_multiply_rows(soln_ucb_tidy$Dept, soln_ucb_tidy$n))

run_tests({
    
    test_that("function is correct", {
        
        expect_identical(args(sln_multiply_rows), args(multiply_rows),
                         info = "Did you define the function correctly? It should have two arguments: `column` and `n`.")
        
        expect_equal(body(sln_multiply_rows), body(multiply_rows),
                        info = "The function body is incorrect. `rep()` should have two arguments: `column` and `n`.")
        
    })
    
    test_that("ucb_full is correct", {
        
        expect_true("Admit" %in% colnames(ucb_full),
                   info = "Did you apply `multiply_rows` to `ucb_tidy$Admit` when creating `ucb_full`?")
        
        expect_true("Gender" %in% colnames(ucb_full),
                   info = "Did you apply `multiply_rows` to `ucb_tidy$Gender` when creating `ucb_full`?")
        
        expect_true("Dept" %in% colnames(ucb_full),
                   info = "Did you apply `multiply_rows` to `ucb_tidy$Dept` when creating `ucb_full`?")
        
    })
    
    test_that("number of rows is correct", {
        expect_equal(nrow(sln_ucb_full), nrow(ucb_full),
                    info = "The number of rows is incorrect. Did you apply `multiply_rows` to `Admit`, `Gender` and `Dept` with `n` as the second argument?")
    })
    
})

# Load the forcats library
library(forcats)

# Reverse the coding of the Admit variable
ucb_full$Admit <- fct_relevel(ucb_full$Admit, 'Rejected', 'Admitted')

# Run the regression
glm_gender <- glm(Admit ~ Gender, data = ucb_full, family = "binomial")

# Summarize the results
summary(glm_gender)

sln_ucb_full$Admit <- fct_relevel(sln_ucb_full$Admit, "Rejected", "Admitted")

sln_glm_gender <- glm(Admit ~ Gender, data = sln_ucb_full, family = "binomial")

run_tests({
    
    test_that("packages are loaded", {
        expect_true("forcats" %in% .packages(), info = "Did you load the `forcats` package?")
        
    })
    
    test_that("factor levels are reversed", {
        
        expect_equal(levels(as.factor(sln_ucb_full$Admit)), levels(as.factor(ucb_full$Admit)),
                     info = "Did you reverse the coding of the `Admit` variable using `fct_rev()`?")
        
    })
    
    test_that("the regression is prepared correctly", {
        
        expect_equal(sln_glm_gender$family[[1]], glm_gender$family[[1]],
                    info = "Did you set `family` equal to `binomial`?")
        expect_equal(sln_glm_gender$xlevels[[1]], glm_gender$xlevels[[1]],
                    info = "Did you set `Gender` as the only predictor variable?")
        
    })
})

# Run the regression, including Dept as an explanatory variable
glm_genderdept <- glm(Admit ~ Gender + Dept, data = ucb_full, family = "binomial")

# Summarize the results
summary(glm_genderdept)

sln_glm_genderdept <- glm(Admit ~ Gender + Dept, data = sln_ucb_full, family = "binomial")

run_tests({
    
    test_that("the regression is prepared correctly", {
        expect_true(glm_genderdept$family[[1]] == "binomial", info = "Did you set the `family` argument equal to `binomial`?")
        expect_equal(sln_glm_genderdept$xlevels, glm_genderdept$xlevels, info = "Did you set `Gender` and `Dept` as the predictor variables?")
    })
})

# Filter for Department A
head(ucb_full)
dept_a <- ucb_full %>% filter(Dept=='A')

# Run the regression
glm_gender_depta <- glm_dept_a <- glm(Admit ~ Gender, data = dept_a, family = "binomial")


# Summarize the results
summary(glm_gender_depta)

sln_dept_a <- sln_ucb_full[sln_ucb_full$Dept == "A",]
sln_glm_gender_depta <- glm(Admit ~ Gender, data = sln_dept_a, family = "binomial")

run_tests({
    
    test_that("the data is properly filtered", {
        expect_true(exists("dept_a"), info = "Did you create a new data frame called `dept_a`?")
        expect_equal(sln_dept_a$Dept, dept_a$Dept, info = "Did you filter for Department A only?")
    })
    
    test_that("the regression is prepared correctly", {
        expect_true(glm_gender_depta$family[[1]] == "binomial", info = "Did you set the `family` argument equal to `binomial`?")
        expect_equal(sln_glm_gender_depta$xlevels, glm_gender_depta$xlevels, info = "Did you set `Gender` as the only predictor variable?")
        expect_equal(sln_glm_gender_depta$data, glm_gender_depta$data, info = "Did you remember to use `dept_a` instead of `ucb_full`?")
    })
    
})

# Define bias
bias <- "a pattern of association between a particular decision and a particular sex of applicant, of sufficient strength to make us confident that it is unlikely to be the result of chance alone"

# Define discrimination
discrimination <- "the exercise of decision influenced by the sex of the applicant when that is immaterial to the qualifications for entry"

# Does bias equal discrimination?
bias == discrimination


sln_bias <- "a pattern of association between a particular decision and a particular sex of applicant, of sufficient strength to make us confident that it is unlikely to be the result of chance alone"
sln_discrimination <- "the exercise of decision influenced by the sex of the applicant when that is immaterial to the qualifications for entry"

run_tests({

    test_that("bias and discrimination are defined correctly", {
        
        expect_true(agrep(bias, sln_bias) == 1,
                     info = "Did you find the correct definition of bias? It should begin as 'a pattern of association...'.")
        
        expect_true(agrep(discrimination, sln_discrimination) == 1,
                     info = "Did you find the correct definition of discrimination? It should begin as 'the exercise of decision...'.")
        
        expect_true(bias != discrimination,
                    info = "Did you define bias and discrimination differently?")
        
    })
    
})
