source("C:/Users/tjohn/Desktop/wd/scripts/clearall.R")
clearall()

library(ggplot2)
library(hms) 
library(gridExtra)
# ggg

data <- read.csv("C:/Users/tjohn/Desktop/household_power_consumption.txt", 
                 sep=";", na.strings="?", 
                 colClasses=c("character", "character", rep("numeric", 7)))

# Remove rows with missing data
data <- na.omit(data)

# Convert the Date column to Date type
data$Date <- as.Date(data$Date, format="%d/%m/%Y")

# Select only data from the dates 2007-02-01 and 2007-02-02
data <- subset(data, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")

# Save the filtered data to a new text file
write.table(data, file="filtered_data.txt", sep=";", row.names=FALSE, quote=FALSE)

# Check the head of the filtered data
head(data)
View(data)

# Create the histogram
plot1 <- ggplot(data, aes(x=Global_active_power)) +
  geom_histogram(fill="red", color="black", binwidth=0.5,boundary=0) +  # Adjusted binwidth to 0.5 for 4 bars per 2 units on x-axis
  labs(title="Global Active Power", x="Global Active Power (kW)", y="Frequency") +
  scale_x_continuous(breaks=seq(0, max(data$Global_active_power, na.rm=TRUE), by=2)) +  # X-axis in intervals of 2
  scale_y_continuous(breaks=seq(0, max(table(cut(data$Global_active_power, breaks=seq(0, max(data$Global_active_power, na.rm=TRUE), by=0.5)))), by=200)) +  # Y-axis in intervals of 200
  theme_minimal(base_family = "sans") +  # White background with minimal styling
  theme(
    plot.title = element_text(color="black", size=14, face="bold"),
    axis.title = element_text(color="black"),
    axis.text = element_text(color="black"),
    panel.background = element_rect(fill="white"),  # Ensure the background is white
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color="black")  # Black ticks on both axes
  )

print(plot1)
ggsave("plot1.png", plot=plot1, width=8, height=6, dpi=300)


# Extract the day of the week to use for labeling
data$Day <- weekdays(data$DateTime)

# Subset the data to ensure it only includes the desired days
data_filtered <- data[data$Day %in% c("Thursday", "Friday", "Saturday"), ]

# Create the plot
plot2 <- ggplot(data_filtered, aes(x=DateTime, y=Global_active_power)) +
  geom_line(color="black") +  # Line plot for Global Active Power over time
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +  # Label x-axis by day (Thu, Fri, Sat)
  labs(title="Global Active Power Over Time", x="Time", y="Global Active Power (kW)") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold"),
    axis.title = element_text(color="black"),
    axis.text = element_text(color="black"),
    panel.background = element_rect(fill="white"),  # Ensure the background is white
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color="black")  # Black ticks on both axes
  )

# Print the plot
print(plot2)

# Export the plot as a PNG file
ggsave("plot2.png", plot=plot2, width=8, height=6, dpi=300)

plot3 <- ggplot(data_filtered, aes(x=DateTime)) +
  geom_line(aes(y=Sub_metering_1, color="Sub_metering_1"), linewidth=1) +  # Use `linewidth` instead of `size`
  geom_line(aes(y=Sub_metering_2, color="Sub_metering_2"), linewidth=1) +  # Updated to `linewidth`
  geom_line(aes(y=Sub_metering_3, color="Sub_metering_3"), linewidth=1) +  # Updated to `linewidth`
  scale_color_manual(values=c("Sub_metering_1"="black", "Sub_metering_2"="red", "Sub_metering_3"="blue")) +  # Custom colors
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +  # Label x-axis by day (Thu, Fri, Sat)
  labs(title="Energy Sub Metering Over Time", x="Time", y="Energy Sub Metering") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold"),
    axis.title = element_text(color="black"),
    axis.text = element_text(color="black"),
    panel.background = element_rect(fill="white"),  # Ensure the background is white
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color="black"),  # Black ticks on both axes
    legend.position = "top",  # Position legend at the top right
    legend.title = element_blank(),  # Remove the legend title
    legend.background = element_rect(fill="white")  # White background for the legend
  ) +
  guides(color = guide_legend(override.aes = list(linewidth=2)))  # Increase legend key size
# Print the plot
print(plot3)

# Export the plot as a PNG file
ggsave("plot3.png", plot=plot3, width=8, height=6, dpi=300)

plot4a <- ggplot(data_filtered, aes(x=DateTime, y=Voltage)) +
  geom_line(color="black", linewidth=1) +  # Line plot for Voltage over time
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +  # Label x-axis by day (Thu, Fri, Sat)
  labs(x="Time", y="Voltage") +
  theme_minimal(base_family = "sans") +
  theme(
    axis.title = element_text(color="black"),
    axis.text = element_text(color="black"),
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color="black")
  )
print(plot4a)

plot4b <- ggplot(data_filtered, aes(x=DateTime, y=Global_reactive_power)) +
  geom_line(color="black", linewidth=1) +  # Line plot for Global Reactive Power over time
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +  # Label x-axis by day (Thu, Fri, Sat)
  labs(x="Time", y="Global Reactive Power (kW)") +
  theme_minimal(base_family = "sans") +
  theme(
    axis.title = element_text(color="black"),
    axis.text = element_text(color="black"),
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color="black")
  )
print(plot4b)

plot4 <- grid.arrange(plot2, plot4a, plot3, plot4b, ncol=2)

# Save the combined plot as a PNG file
ggsave("plot4.png", plot=plot4, width=12, height=10, dpi=300)