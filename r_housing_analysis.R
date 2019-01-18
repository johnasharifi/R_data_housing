# https://www.zillow.com/research/data/
# home values -> Median home value / sqft

setwd("C:/Users/John/Desktop/Projects/R_data_processing/")
csv = read.csv("Zip_MedianValuePerSqft_AllHomes.csv", header = TRUE, stringsAsFactors = FALSE)
cal_csv = csv[csv["State"] == "CA", ]

manifest_metros = unlist(unique(cal_csv["Metro"]))
# note: the unlisting and unique-ifying of this list shuffles elements. for labels need to note names of cities as they are encountered
manifest_cities = unlist(unique(cal_csv["City"]))
manifest_cities_label_order = c()

time_start = "X2010.01"
time_end = "X2018.11"

xyzdata = matrix(data = NA, nrow = length(manifest_cities), ncol = 3)
k = 0
for (i in 1:length(manifest_metros)) {
	subsection = cal_csv[cal_csv["Metro"] == manifest_metros[i], ]

	data_metro = subsection[, time_end] / subsection[, time_start]
	data_metro = data_metro[!is.na(data_metro)]
	data_metro = mean(data_metro)

	cities_within_metro = unlist(unique(subsection["City"]))
	
	for (j in 1:length(cities_within_metro)) {
		k = k + 1
		
		subsection_city = cal_csv[cal_csv["City"] == cities_within_metro[j], ]

		data_city = subsection_city[, time_end] / subsection_city[, time_start]
		data_city = data_city[!is.na(data_city)]
		data_city = mean(data_city)
	
		xyzdata[k,] = c(i, j, data_city / data_metro)
		manifest_cities_label_order = c(manifest_cities_label_order, cities_within_metro[j])
		}
	}

pdf("image.pdf", width = 16, height = 40)
plot(x = xyzdata[,3], y = xyzdata[,1], type = "p", col = "black")
text(x = xyzdata[,3], y = xyzdata[,1], labels = manifest_cities_label_order, pos = 3, cex = 0.7, srt = 45)
dev.off()