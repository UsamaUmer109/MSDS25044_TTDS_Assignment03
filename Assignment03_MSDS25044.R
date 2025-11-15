# TTDS Assignment 3: Git & R Solutions
# Using Sakila Database 
# Load required libraries
library(data.table)
library(DBI)
library(RMySQL)
library(ggplot2)
library(scales)  # For better formatting
# Source your connection function
source("con.R")
# Function to execute query and return data.table
query_to_dt <- function(sql, con) {
  tryCatch({
    result <- dbGetQuery(con, sql)
    return(as.data.table(result))
  }, error = function(e) {
    cat("Query failed:", e$message, "\n")
    cat("SQL:", sql, "\n")
    return(NULL)
  })
}
# Test the connection first
cat("Testing database connection...\n")
con <- connect_sakila()
if (!is.null(con)) {
    # Load all tables
    cat("\nLoading Sakila tables...\n")
    film <- query_to_dt("SELECT * FROM film", con)
    actor <- query_to_dt("SELECT * FROM actor", con)
    film_actor <- query_to_dt("SELECT * FROM film_actor", con)
    category <- query_to_dt("SELECT * FROM category", con)
    film_category <- query_to_dt("SELECT * FROM film_category", con)
    language <- query_to_dt("SELECT * FROM language", con)
    customer <- query_to_dt("SELECT * FROM customer", con)
    rental <- query_to_dt("SELECT * FROM rental", con)
    payment <- query_to_dt("SELECT * FROM payment", con)
    staff <- query_to_dt("SELECT * FROM staff", con)
    inventory <- query_to_dt("SELECT * FROM inventory", con)
    store <- query_to_dt("SELECT * FROM store", con)
    cat("✓ All tables loaded successfully!\n")
}
# ==========================================================================
# 1. BASICS
# ==========================================================================

cat("\n", strrep("=", 50), "\n")
cat("1. BASICS\n")
cat(strrep("=", 50), "\n")

# Q1: Films with PG rating and rental duration > 5 days
cat("\nQ1: Films with PG rating and rental duration > 5 days\n")
q1_result <- film[rating == 'PG' & rental_duration > 5, 
                  .(film_id, title, rating, rental_duration, rental_rate)]
print(q1_result)
cat("Number of films:", nrow(q1_result), "\n")







# Q2: Average rental rate by rating
cat("\nQ2: Average rental rate by rating\n")

q2_result <- film[, .(avg_rental_rate = round(mean(rental_rate), 2)), 
                  by = rating][order(-avg_rental_rate)]
print(q2_result)










# Q3: Film count by language
cat("\nQ3: Film count by language\n")
q3_result <- film[language, on = "language_id"][
  , .(film_count = .N), by = .(language_name = name)][order(-film_count)]
print(q3_result)







# Q4: Customers and their stores
cat("\nQ4: Customers and their stores (first 10)\n")
q4_result <- customer[store, on = "store_id"][
  1:10, .(customer_id, first_name, last_name, store_id)]
print(q4_result)























# Q5: Payment details with staff
cat("\nQ5: Payment details with staff (first 10)\n")
q5_result <- payment[staff, on = "staff_id"][
  1:10, .(payment_id, amount, payment_date, 
          staff_name = paste(first_name, last_name))]
print(q5_result)










# Q6: Films not rented
cat("\nQ6: Films not rented\n")
rented_films <- unique(rental[inventory, on = "inventory_id"]$film_id)
q6_result <- film[!film_id %in% rented_films, .(film_id, title)]
cat("Number of films not rented:", nrow(q6_result), "\n")
print(head(q6_result, 10))















# ==========================================================================
# 4. VISUALIZATIONS - ENHANCED GRAPHS
# ==========================================================================

cat("\n", strrep("=", 50), "\n")
cat("4. VISUALIZATIONS - ENHANCED GRAPHS\n")
cat(strrep("=", 50), "\n")

# Plot 1: Rental rate by rating (Enhanced)
cat("\nCreating visualization 1: Rental Rate by Rating (Boxplot)\n")
p1 <- ggplot(film, aes(x = rating, y = rental_rate, fill = rating)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "Rental Rate Distribution by Film Rating",
       subtitle = "Sakila Movie Database Analysis",
       x = "Film Rating", 
       y = "Rental Rate ($)",
       caption = "Source: Sakila Database") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40"))
print(p1)










# Plot 2: Films by category (Enhanced)
cat("Creating visualization 2: Films by Category (Bar Chart)\n")
category_films <- film_category[, .(film_count = .N), by = category_id][
  category, on = "category_id"]

p2 <- ggplot(category_films, aes(x = reorder(name, film_count), y = film_count, fill = film_count)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = film_count), hjust = -0.2, size = 3) +
  labs(title = "Number of Films by Category",
       subtitle = "Distribution across different film categories",
       x = "Category", 
       y = "Number of Films") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))
print(p2)









# Plot 3: Top actors by film count (New)
cat("Creating visualization 3: Top Actors by Film Count\n")
actor_films <- film_actor[, .(film_count = .N), by = actor_id][
  actor, on = "actor_id"][order(-film_count)][1:15]
actor_films[, actor_name := paste(first_name, last_name)]

p3 <- ggplot(actor_films, aes(x = reorder(actor_name, film_count), y = film_count)) +
  geom_bar(stat = "identity", fill = "orange", alpha = 0.8) +
  geom_text(aes(label = film_count), hjust = -0.2, size = 3) +
  labs(title = "Top 15 Actors by Number of Films",
       subtitle = "Most prolific actors in the database",
       x = "Actor Name", 
       y = "Number of Films") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 8))
print(p3)
















# Plot 4: Rental duration analysis (New)
cat("Creating visualization 4: Rental Duration Analysis\n")
rental_duration_summary <- film[, .(count = .N, avg_rental_rate = mean(rental_rate)), 
                                by = rental_duration][order(rental_duration)]

p4 <- ggplot(rental_duration_summary, aes(x = factor(rental_duration), y = count, 
                                          fill = avg_rental_rate)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Avg Rental Rate") +
  labs(title = "Films by Rental Duration",
       subtitle = "Bar height shows count, color shows average rental rate",
       x = "Rental Duration (days)", 
       y = "Number of Films") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
print(p4)

















# Save results
cat("\nSaving results to CSV files...\n")
fwrite(q1_result, "q1_pg_films.csv")
fwrite(q2_result, "q2_avg_rental.csv")
fwrite(q3_result, "q3_films_by_language.csv")
fwrite(q4_result, "q4_customers_stores.csv")
fwrite(q5_result, "q5_payments_staff.csv")
fwrite(q6_result, "q6_films_not_rented.csv")
cat("✓ All CSV files saved!\n")

















# Save plots as high-quality PNG files
cat("Saving plots as high-quality PNG files...\n")
ggsave("plot1_rental_rate_boxplot.png", p1, width = 10, height = 6, dpi = 300)
ggsave("plot2_films_by_category.png", p2, width = 10, height = 6, dpi = 300)
ggsave("plot3_top_actors.png", p3, width = 10, height = 6, dpi = 300)
ggsave("plot4_rental_duration_analysis.png", p4, width = 10, height = 6, dpi = 300)
cat("✓ All plots saved as high-quality PNG files!\n")



