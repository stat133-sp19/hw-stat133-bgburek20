# Title: make-shot-charts-script.R
# Descriptions: Makes shot charts and image files for each of the 5 major
# GSW players.
# Inputs: ./images/nba-court.jpg,
# thompson, curry, durant, green, iguodala, and gsw
# Outputs: Creates variables for shot charts:
# thompson_shot_chart,
# curry_shot_chart, 
# durant_shot_chart, 
# iguodala_shot_chart, 
# green_shot_chart, 
# gsw_shot_chart,
# ./images/stephen-curry-shot-chart.pdf, 
# ./images/klay-thompson-shot-chart.pdf,
# ./images/kevin-durant-shot-chart.pdf,
# ./images/andre-iguodala-shot-chart.pdf,
# ./images/draymond-green-shot-chart.pdf,
# ./images/gsw-shot-charts.pdf,
# ./images/gsw-shot-charts.png,
# ./images/curry-shot-chart.png, 
# ./images/thompson-shot-chart.png,
# ./images/durant-shot-chart.png,
# ./images/iguodala-shot-chart.png,
# ./images/green-shot-chart.png,


# Ensure working directory is set to workout01
library("jpeg")
library("grid")
library("ggplot2")


court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file), 
  width = unit(1, "npc"),
  height = unit(1, "npc"))

thompson_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()

curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()

durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()

iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()

green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()

# Export shot charts as pdfs
pdf(file = "../images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)
iguodala_shot_chart
dev.off()
pdf(file = "../images/draymond-green-shot-chart.pdf", width = 6.5, height = 5)
green_shot_chart
dev.off()
pdf(file = "../images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)
durant_shot_chart
dev.off()
pdf(file = "../images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)
thompson_shot_chart
dev.off()
pdf(file = "../images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()

# Facet Wrap GS Shooting Charts
gsw_shot_chart <- ggplot(data = gsw) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal() +
  facet_wrap(~name)

# Save as PDF and PNG
pdf(file = "../images/gsw-shot-charts.pdf", width = 8, height = 7)
gsw_shot_chart
dev.off()

png(file = "../images/gsw-shot-charts.png", width = 8, height = 7, units = 'in',
    res = 1200)
gsw_shot_chart
dev.off()

# Create png of shot chart for each player
png(filename = "../images/curry-shot-chart.png", width = 8, height = 7, units = 'in', 
    res = 1200)
curry_shot_chart
dev.off()

png(file = "../images/thompson-shot-chart.png", width = 8, height = 7, unit = 'in',
    res = 1200)
thompson_shot_chart
dev.off()

png(file = "../images/durant-shot-chart.png", width = 8, height = 7, unit = 'in',
    res = 1200)
durant_shot_chart
dev.off()

png(file = "../images/iguodala-shot-chart.png", width = 8, height = 7, unit = 'in',
    res = 1200)
iguodala_shot_chart
dev.off()

png(file = "../images/green-shot-chart.png", width = 8, height = 7, unit = 'in',
    res = 1200)
green_shot_chart
dev.off()

