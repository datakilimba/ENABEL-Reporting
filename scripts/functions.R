get_kobo_data = function(form_id){
  kobo_server_url = "https://kc.humanitarianresponse.info/"
  url = paste0(kobo_server_url,"api/v1/data/",form_id,".csv")
  rawdata = GET(url,authenticate(Sys.getenv("sakirp_user"),Sys.getenv("sakirp_pw")))
  content = content(rawdata,"raw",encoding="UTF-8")
  read_csv(content)
}

get_data = function(data,before,after,activity,crop){
  browser()
  data_ = data %>% 
    filter(endtime >= as.POSIXct(after),
           endtime <= as.POSIXct(before),
           `survey/activity`== activity
           )
  
  if(crop == 1 & activity == 1){
    tidy_bean_landprep(data_ 
                       %>% filter(`survey/land_preparation/crop_grown`== crop))
  }else if(crop == 1 & activity == 2){
    # bean yield assessment
  }else if(crop == 1 & activity == 3){
    tidy_bean_marketing(data_ %>% 
                          filter(`survey/marketing/crop_grown-m` == crop))
  }
}

tidy_bean_marketing = function(bean_data_marketing){
  
  #total_sold = sum(as.double(bean_data_marketing$`survey/marketing/sold`))
  
  bean_data_marketing %>% 
    rename(weeding_cost = `survey/marketing/price_weeding`,
           harvesting_cost = `survey/marketing/price_harvesting`,
           sold_kg = `survey/marketing/sold`,
           sale_price = `survey/marketing/price`) %>% 
    mutate(weeding_cost = as.double(weeding_cost),
           harvesting_cost = as.double(harvesting_cost),
           sold_kg = as.double(sold_kg),
           sale_price = as.double(sale_price),
           price_per_kg = sale_price/sold_kg)
}

tidy_bean_landprep = function(bean_data_landprep){
    bean_data_landprep %>% 
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
}

fix_outliers = function(data,variable){
  browser()
  q = quantile(variable,na.rm = T,probs = c(.25,.75))
  iqr = IQR(variable,na.rm = T)
  upper_range = q[2]+1.5*iqr
  lower_range = q[1]-1.5*iqr
  
  # return only those rows where the variable is not considered an outlier.
  filter(data, variable > (q[1] - 1.5*iqr),
         variable < (q[1] + 1.5*iqr))
}
