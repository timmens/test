Table_6<-function(data) {

child_controls = c("control_fu_child_age", "control_fu_female", "missing_fu_child_age", "missing_fu_female")

    
hh_controls = c("control_fu_adults", "control_fu_hh_head_edu","control_fu_hh_head_occ_farmer" ,"control_fu_total_land", 
             "control_fu_household_size" ,"missing_fu_adults", "missing_fu_hh_head_edu" ,"missing_fu_hh_head_occ_farmer",  
             "missing_fu_total_land", "missing_fu_household_size")

independent_vars = c("treatment_1", "treat_1_female" ,"treatment_2","treat_2_female")
district_control =c("factor(bl_district)")


child_data_subset= data %>% filter(fu_child_level == 1 & fu_young_child == 1)

x_vars <- c(independent_vars,child_controls,hh_controls,district_control)


full.formula <- as.formula(paste('fu_child_enrolled', paste(x_vars,collapse = ' + '),sep='~'))

lm_enroll<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)

full.formula <- as.formula(paste('total_score_dev', paste(x_vars,collapse = ' + '),sep='~'))

lm_score<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)

full.formula <- as.formula(paste('fu_child_highest_grade', paste(x_vars,collapse = ' + '),sep='~'))

lm_grades<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)

rob_se <- list(sqrt(diag(vcovHC(lm_enroll, type = "HC1"))),
               sqrt(diag(vcovHC(lm_score, type = "HC1"))),
               sqrt(diag(vcovHC(lm_grades, type = "HC1"))))


display_vars<- c("treatment_1","treat_1_female","treatment_2","treat_2_female")
    
outreg <- capture.output( 
    
    
    stargazer(lm_enroll,lm_grades,lm_score, 
          type = "html", 
          se = rob_se,
          keep=display_vars,
          column.sep.width = "3pt",
          covariate.labels = c("Treatment gender uniform","Treatment gender uniform x Female ",
                               "Treatment gender differentiated","Treatment gender differentiated x Female"
                                          ),
          title = "Gender differential impacts by the subsidy treatment",
          dep.var.labels   = c("Reported Enrollment","Highest Grade attained","Test Scores"),
          model.numbers = FALSE)
    
)
    
    
display_html(toString(outreg))

}