library(tidyverse)
library(httr)

source(paste0(getwd(),"/scripts/functions.R"))

bt_data = get_kobo_data(202136)

beans = 1
cassava = 2
sunflower = 3

land_prep = 1
yield_assessment = 2
marketing = 3

beans_data_landprep = 
  get_data(data = bt_data,
                    before = "2020-10-30T18:09:26.233+03",
                    after = "2019-10-01T13:07:12.798+03",
                    crop = beans,
                    activity = land_prep)

#boxplot( fert_labor_acre~`survey_info/district`, data = beans_data_landprep)
fixed_acreage_data = 
  fix_outliers(beans_data_landprep,beans_data_landprep$acreage)

fixed_seed_acre_data = 
  fix_outliers(beans_data_landprep,beans_data_landprep$seed_per_acre)

fixed_fert_acre_data = 
  fix_outliers(beans_data_landprep,beans_data_landprep$fertilizer_acre)

fixed_pesticide_acre_data = 
  fix_outliers(beans_data_landprep,beans_data_landprep$pesticide_acre)

fixed_cult_acre_data = 
  fix_outliers(beans_data_landprep,beans_data_landprep$cultivation_acre)

fixed_plant_acre_data = 
  fix_outliers(beans_data_landprep,beans_data_landprep$planting_acre)

fixed_labor_acre_data = 
  fix_outliers(beans_data_landprep,beans_data_landprep$fert_labor_acre)


average_acre = round(mean(fixed_acreage_data$acreage),2)
seed_acre = median(fixed_seed_acre_data$seed_per_acre)
fert_acre = mean(fixed_fert_acre_data$fertilizer_acre)
pesticide_acre = mean(beans_data_landprep$pesticide_acre)
cult_acre = mean(fixed_cult_acre_data$cultivation_acre)
plant_acre = mean(fixed_plant_acre_data$planting_acre)
fert_labor_acre = mean(fixed_labor_acre_data$fert_labor_acre)

plant_fert_application_acre = fert_labor_acre+plant_acre

#################################################################

beans_data_marketing = get_data(data = bt_data,
                                before = "2020-10-30T18:09:26.233+03",
                                after = "2019-10-01T13:07:12.798+03",
                                crop = beans,
                                activity = marketing)

fixed_harvest_data = fix_outliers(beans_data_marketing,
                            beans_data_marketing$harvesting_cost)

fixed_price_data = fix_outliers(beans_data_marketing,
                                beans_data_marketing$price_per_kg) %>% 
  mutate(
    weight = sold_kg/sum(sold_kg),
    weighted_price = price_per_kg*weight
    )

fixed_weed_data = fix_outliers(beans_data_marketing,
                               beans_data_marketing$weeding_cost)

average_harvest_cost_acre = mean(fixed_harvest_data$harvesting_cost)/average_acre
average_weed_cost_per_acre = mean(fixed_weed_data$weeding_cost)/average_acre

beans_total_cost_acre = seed_acre+
  fert_acre+
  pesticide_acre+
  cult_acre+
  plant_fert_application_acre+
  average_weed_cost_per_acre+
  average_harvest_cost_acre

beans_total_cost_hectare = beans_total_cost_acre*2.47105381

# from 2020 IPTT yield and price value for beans
bean_yield_hectare_kg = 536
bean_price_kg = 1827

beans_revenue = bean_yield_hectare_kg*bean_price_kg

GMA_beans = beans_revenue - beans_total_cost_hectare # 454,528TZS
