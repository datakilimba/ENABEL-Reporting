library(tidyverse)


bt_data2 = xlsx::read.xlsx(
  "C:\\Users\\tnkil\\OneDrive\\Enabel - SAKiRP\\M&E\\Data Collection\\S09 - Beneficiary Tracking\\Data\\Beneficiary_Tracking_-_all_versions_-_English_-_2021-03-03-19-50-52.xlsx",1)

beans_data = bt_data2 %>% 
  filter(endtime >= as.POSIXct("2019-10-01T13:07:12.798+03"),
         endtime <= as.POSIXct("2020-10-30T18:09:26.233+03"),
         `Survey.Select.the.planned.activity`=="Land Preparation and Inputs",
          Survey.Land.Preparation.and.Inputs.What.crop.is.was.grown.on.this.field. == "Beans") 

beans_data_marketing = bt_data2 %>% 
  filter(endtime >= as.POSIXct("2019-10-01T13:07:12.798+03"),
         endtime <= as.POSIXct("2020-10-30T18:09:26.233+03"),
         `Survey.Select.the.planned.activity`=="Marketing",
         Survey.Harvesting...Marketing.What.crop.is.was.grown.on.this.field. == "Beans") 

beans = beans_data %>% 
  #filter( as.double(area_geoC)>=IQR(as.double(beans$area_geoC)))
  mutate(acreage = if_else(as.double(Survey.Land.Preparation.and.Inputs.area_geoC) > 0,
                           as.double(Survey.Land.Preparation.and.Inputs.area_geoC)/4046.86,0),
         seed_cost_per_acre = as.double(Survey.Land.Preparation.and.Inputs.Total.price.paid.for.beans.purchased.for.planting...transport..TZS.)/
           acreage,
         fertilizer_cost_per_acre = 
           as.double(Survey.Land.Preparation.and.Inputs.What.was.the.total.price.paid.for.the.chemical.fertilizer.applied....transport..TZS.)/
           acreage,
         pesticide_cost_per_acre = if_else(as.double(Survey.Land.Preparation.and.Inputs.Total.price.paid.of.herbicide.applied.before.planting...transport...TZS.) !=0 | 999 &
                                             !(is.na(Survey.Land.Preparation.and.Inputs.Total.price.paid.of.herbicide.applied.before.planting...transport...TZS.)),
                                           as.double(Survey.Land.Preparation.and.Inputs.Total.price.paid.of.herbicide.applied.before.planting...transport...TZS.)/
                                                       acreage,0),
         land_cultivation_cost_per_acre = as.double(Survey.Land.Preparation.and.Inputs.Land.Cultivation.How.much.were.they.paid.in.total.)/acreage) %>%
  filter(acreage > 0)


plant_fert_application = beans %>% 
  filter(as.double(Survey.Land.Preparation.and.Inputs.Planting.How.much.were.they.paid.in.total.) & 
           as.double(Survey.Land.Preparation.and.Inputs.Fertilizer.Application.How.much.were.they.paid.in.total.) > 0) %>% 
  mutate(planting = as.double(Survey.Land.Preparation.and.Inputs.Planting.How.much.were.they.paid.in.total.)/acreage,
         fertilizer = as.double(Survey.Land.Preparation.and.Inputs.Fertilizer.Application.How.much.were.they.paid.in.total.)/acreage)

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

