library(tidyverse)
library(httr)

kobo_server_url = "https://kc.humanitarianresponse.info/"
form_id = "202136" #Sunflower Yield Tracking
url = paste0(kobo_server_url,"api/v1/data/",form_id,".csv")
rawdata = GET(url,authenticate(Sys.getenv("sakirp_user"),Sys.getenv("sakirp_pw")))
content = content(rawdata,"raw",encoding="UTF-8")
bt_data = read_csv(content)

bt_data2 = xlsx::read.xlsx(
  "C:\\Users\\tnkil\\OneDrive\\Enabel - SAKiRP\\M&E\\Data Collection\\S09 - Beneficiary Tracking\\Data\\Beneficiary_Tracking_-_all_versions_-_English_-_2021-03-03-19-50-52.xlsx",1)

beans = 1
cassava = 2
sunflower = 3

land_prep = 1
yield_assessment = 2
marketing = 3

beans_data_landprep = bt_data %>% 
  filter(endtime >= as.POSIXct("2019-10-01T13:07:12.798+03"),
         endtime <= as.POSIXct("2020-10-30T18:09:26.233+03"),
         `survey/activity`== land_prep,
          `survey/land_preparation/crop_grown`== beans) %>% 
  rename(beans_price = `survey/land_preparation/beans_price`,
         area_geoC = `survey/land_preparation/area_geoC`,
         fertilizer_price =`survey/land_preparation/fertilizer_price`,
         pesticide_price = `survey/land_preparation/herbicide`,
         cultivation_price = `survey/land_preparation/land_cultivation/price_hired_cult`,
         planting_price = `survey/land_preparation/planting/price_hired_pl`,
         fert_labor_price =`survey/land_preparation/fertilizer_application/price_hired_fa`) %>% 
  mutate(beans_price = as.double(beans_price),
         acreage = as.double(area_geoC)*0.000247105,
         fertilizer_price = if_else(fertilizer_price == "n/a",0,
                                    as.double(fertilizer_price)),
         pesticide_price = as.double(pesticide_price),
         cultivation_price = as.double(cultivation_price),
         planting_price = as.double(planting_price),
         fert_labor_price = as.double(fert_labor_price)
         
         ) %>% 
  filter(acreage != 0,pesticide_price!=999) %>% 
  mutate(seed_per_acre = beans_price/acreage,
         fertilizer_acre = fertilizer_price/acreage,
         pesticide_acre = pesticide_price/acreage,
         cultivation_acre = cultivation_price/acreage,
         planting_acre = planting_price/acreage,
         fert_labor_acre = fert_labor_price/acreage)

boxplot( fert_labor_acre~`survey_info/district`, data = beans_data_landprep)

q_acreage = quantile(beans_data_landprep$acreage,na.rm = T,probs = c(.25,.75))
q_fert_labor = quantile(beans_data_landprep$fert_labor_acre,na.rm = T,probs = .75)
q_plant = quantile(beans_data_landprep$planting_acre,na.rm = T,probs = .75)
q_beans = quantile(beans_data_landprep$seed_per_acre,na.rm = T,probs = .75)
q_fert = quantile(beans_data_landprep$fertilizer_acre,na.rm = T,probs = .75)
q_cult = quantile(beans_data_landprep$cultivation_acre,na.rm = T,probs = .75)
# Threshold beyond which to consider the data point an outlier
upper_acreage = 1.5*(IQR(beans_data_landprep$acreage))
upper_fert_labor = 1.5*(IQR(beans_data_landprep$fert_labor_acre))
upper_beans = 1.5*(IQR(beans_data_landprep$seed_per_acre))
upper_fertilizer = 1.5*(IQR(beans_data_landprep$fertilizer_acre))
upper_cult = 1.5*(IQR(beans_data_landprep$cultivation_acre))
upper_plant = 1.5*(IQR(beans_data_landprep$planting_acre))

# Remove upper outliers
acreage_data = beans_data_landprep %>% 
  filter(acreage < q_acreage[2] + upper_acreage,
         acreage > q_acreage[1] - upper_acreage)

fert_labor_acre_data = beans_data_landprep %>% 
  filter(fert_labor_acre < q_plant[1] + upper_fert_labor)

plant_acre_data = beans_data_landprep %>% 
  filter(planting_acre < q_plant[1] + upper_plant)

cult_acre_data = beans_data_landprep %>% 
  filter(cultivation_acre < q_cult[1] + upper_cult)

seed_acre_data = beans_data_landprep %>% 
  filter(seed_per_acre < q_beans[1] + upper_beans)

fert_acre_data = beans_data_landprep %>% 
  filter(fertilizer_acre < q_fert[1] + upper_fertilizer)

average_acre = round(mean(acreage_data$acreage),2)
seed_acre = median(seed_acre_data$seed_per_acre)
fert_acre = mean(fert_acre_data$fertilizer_acre)
pesticide_acre = mean(beans_data_landprep$pesticide_acre)
cult_acre = median(cult_acre_data$cultivation_acre)
plant_acre = median(plant_acre_data$planting_acre)
fert_labor_acre = mean(fert_labor_acre_data$fert_labor_acre)
plant_fert_application_acre = fert_labor_acre+plant_acre
#################################################################
beans_data_marketing = bt_data %>% 
  filter(endtime >= as.POSIXct("2019-10-01T13:07:12.798+03"),
         endtime <= as.POSIXct("2020-10-30T18:09:26.233+03"),
         `survey/activity`==marketing,
         `survey/marketing/crop_grown-m` == beans) %>% 
  rename(weeding_cost = `survey/marketing/price_weeding`,
         harvesting_cost = `survey/marketing/price_harvesting`) %>% 
  mutate(weeding_cost = as.double(weeding_cost),
         harvesting_cost = as.double(harvesting_cost))
  
q_harvest = quantile(beans_data_marketing$harvesting_cost,na.rm = T,probs = .75)
q_weeding = quantile(beans_data_marketing$weeding_cost,na.rm = T,probs = .75)

iqr_range_weed = 1.5*(IQR(beans_data_marketing$weeding_cost))
iqr_range_harvest = 1.5*(IQR(beans_data_marketing$harvesting_cost))

weeding_data = beans_data_marketing %>% 
  filter(weeding_cost < q_weeding[1] + iqr_range_weed)

harvest_data = beans_data_marketing %>% 
  filter(harvesting_cost < q_harvest[1] + iqr_range_harvest)

average_harvest_cost_acre = mean(harvest_data$harvesting_cost)/average_acre
average_weed_cost_per_acre = mean(weeding_data$weeding_cost)/average_acre

beans_total_cost_acre = seed_acre+
  fert_acre+
  pesticide_acre+
  cult_acre+
  plant_fert_application_acre+
  average_weed_cost_per_acre+
  average_harvest_cost_acre

beans_total_cost_hectare = beans_total_cost_acre*2.47105381

#from 2020 IPTT yield value for beans
bean_yield_hectare_kg = 536
bean_price_kg = 1827

beans_revenue = bean_yield_hectare_kg*bean_price_kg

GMA_beans = beans_revenue - beans_total_cost_hectare # 454,528TZS



beans = beans_data %>% 
  #filter( as.double(area_geoC)>=IQR(as.double(beans$area_geoC)))
  mutate(acreage = if_else(as.double(Survey.Land.Preparation.and.Inputs.area_geoC) > 0,
                           as.double(Survey.Land.Preparation.and.Inputs.area_geoC)/4046.86,0),
         seed_cost_per_acre = as.double(Survey.Land.Preparation.and.Inputs.Total.price.paid.for.beans.purchased.for.planting...transport..TZS.)/
           acreage,
         fertilizer_cost_per_acre = 
           as.double(Survey.Land.Preparation.and.Inputs.What.was.the.total.price.paid.for.the.chemical.fertilizer.applied....transport..TZS.)/
           acreage,
         pesticide_cost_per_acre = if_else(as.double(Survey.Land.Preparation.and.Inputs.Total.price.paid.of.herbicide.applied.before.planting...transport...TZS.) !=0 | 999 | NA,0,
                                           as.double(Survey.Land.Preparation.and.Inputs.Total.price.paid.of.herbicide.applied.before.planting...transport...TZS.)/
                                                       acreage),
         land_cultivation_cost_per_acre = as.double(Survey.Land.Preparation.and.Inputs.Land.Cultivation.How.much.were.they.paid.in.total.)/acreage) %>%
  filter(acreage > 0)

paid_for_planting = as.double(Survey.Land.Preparation.and.Inputs.Planting.How.much.were.they.paid.in.total.)
paid_for_fert_application = as.double(Survey.Land.Preparation.and.Inputs.Fertilizer.Application.How.much.were.they.paid.in.total.)

plant_fert_application = beans %>% 
  # filter(paid_for_planting & 
  #          paid_for_fert_application > 0) %>% 
  mutate(planting = as.double(Survey.Land.Preparation.and.Inputs.Planting.How.much.were.they.paid.in.total.)/acreage,
         fertilizer = paid_for_fert_application/acreage)

plant_fert_application_cost_per_acre = median(plant_fert_application$planting) + median(plant_fert_application$fertilizer)/median(beans$acreage)

land_cultivation_cost_per_acre = beans %>% 
  filter(land_cultivation_cost_per_acre > 0)

land_cultivation_cost_per_acre = median(beans$land_cultivation_cost_per_acre, na.rm = T)

seed_cost_per_acre = median(as.double(beans$seed_cost_per_acre),na.rm = T)
fertilizer_cost_per_acre = median(as.double(beans$fertilizer_cost_per_acre),na.rm = T)

pesticide_cost_per_acre_df = beans %>% 
  filter(pesticide_cost_per_acre > 0) 

pesticide_cost_per_acre = median(pesticide_cost_per_acre_df$pesticide_cost_per_acre)

beans_data_weeding = beans_data_marketing %>% 
  filter(as.double(Survey.Harvesting...Marketing.What.was.the.total.amount.of.money.spent.on.weeding...all.weeding.times.together.) > 0) %>% 
  mutate(weeding_cost_acre = 
           as.double(Survey.Harvesting...Marketing.What.was.the.total.amount.of.money.spent.on.weeding...all.weeding.times.together.)/
           median(beans$acreage,na.rm = T))

weeding_cost_per_acre = median(beans_data_weeding$weeding_cost_acre)

beans_data_harvesting = beans_data_marketing %>% 
  filter(as.double(Survey.Harvesting...Marketing.Harvesting.cost) > 0 &
           as.double(Survey.Harvesting...Marketing.Harvesting.cost)!= 999) %>% 
  mutate(harvest_cost_acre = as.double(Survey.Harvesting...Marketing.Harvesting.cost)/
           median(beans$acreage))

harvest_cost_acre = median(beans_data_harvesting$harvest_cost_acre)
  
bean_costs = seed_cost_per_acre +
  harvest_cost_acre +
  fertilizer_cost_per_acre + 
  land_cultivation_cost_per_acre +
  pesticide_cost_per_acre +
  plant_fert_application_cost_per_acre +
  weeding_cost_per_acre

# yield from IPTT (Anthonys figure), price from Whatsapp Anthonys graph
# revenue = yield * price
# revenue = (yield per hectare/2.47105)*price
bean_revenue = (536/2.47105)*1827
GMA_beans_acre = bean_revenue - bean_costs
GMA_beans_hectare = GMA_beans_acre*2.47105

