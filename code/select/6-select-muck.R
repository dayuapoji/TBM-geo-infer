# SELECT FEATURES - MUCK REMOVAL ###############################################

# Screw Conveyor ---------------------------------------------------------------

var_screw_rot <- data %>%
  select(contains('screw')) %>% 
  .[ , which(.[1,] == 'r/min')] %>%
  set_df(.) %>%
  set_colnames(., 'ScrewRotSpeed') #create_var_names('ScrewRotSpeed',2)) %>%
  # mutate(ScrewRotSpeed = rowMeans(., na.rm = T))

var_screw_torque <- data %>%
  select(contains('screw')) %>%
  select(contains('torque')) %>%
  # .[ , which(.[1,] == 'kN-m')] %>%
  set_df(.) %>%
  set_colnames(., 'ScrewTorque')

var_screw_pres <- data %>%
  select(contains('screw')) %>%
  select(contains('pres')) %>%
  select(contains('front') | contains('back')) %>%
  # select(!contains('casings')) %>%
  # select(!contains('polymer')) %>%
  .[ , which(.[1,] == 'kPa')] %>%
  set_df(.) %>%
  set_colnames(., c('ScrewPresFront', 'ScrewPresBack'))# c(create_var_names('ScrewPres1', 2), 
  #                   create_var_names('ScrewPres2', 2))) %>%
  # mutate(ScrewPres = rowMeans(.))

# var_screwcas_pres <- data %>%
#   select(contains('screw')) %>%
#   select(contains('pres')) %>%
#   select(contains('casings')) %>%
#   .[ , which(.[1,] == 'bar')] %>%
#   set_df(.) %>%
#   set_colnames(., c(create_var_names('ScrewCasingPres1', 5), 
#                     create_var_names('ScrewCasingPres2', 5))) %>%
#   mutate(ScrewCasingPres = rowMeans(., na.rm = T))
# 
# var_screw_muck_flow <- data %>%
#   select(contains('screw')) %>%
#   select(contains('muck flow')) %>%
#   .[ , which(.[1,] == 'm3/min')] %>%
#   set_df(.) %>%
#   set_colnames(., 'ScrewMuckFlow')

# var_screw_muck_vol <- data %>%
#   select(contains('screw')) %>%
#   select(contains('muck vol')) %>%
#   .[ , which(.[1,] == 'm3')] %>%
#   set_df(.) %>%
#   set_colnames(., 'ScrewMuckVol')

var_screw_gate_open <- data %>%
  select(contains('screw')) %>%
  select(contains('gate')) %>%
  select(contains('open')) %>%
  set_df(.) %>%
  set_colnames(., c('GateOpenRateRot', 'GateOpenRateHor')) #%>%
  # mutate(ScrewGateStroke = rowMeans(., na.rm = T))

var_screw_gate_pres <- data %>%
  select(contains('screw')) %>%
  select(contains('gate')) %>%
  select(contains('pres')) %>%
  set_df(.) %>%
  set_colnames(., 'GatePres') #%>%


# Screw Polymer ----------------------------------------------------------------

# var_screw_polymer <- data %>%
#   select(contains('screw')) %>%
#   select(contains('polymer')) %>%
#   select(!contains('chamber')) %>%
#   set_df(.) %>%
#   set_colnames(., c(create_var_names('ScrewPolymerFlow', 2),
#                     create_var_names('ScrewPolymerPres', 2),
#                     create_var_names('ScrewPolymerVol', 2))) %>%
#   mutate(ScrewPolymerFlow = rowSums(.[,1:2], na.rm = T),
#          ScrewPolymerPres = rowSums(.[,3:4], na.rm = T),
#          ScrewPolymerVol = rowSums(.[,5:6], na.rm = T))

# Belt Conveyor ---------------------------------------------------------------

# var_belt_muck_vol <- data %>%
#   select(contains('belt')) #%>%
#   select(contains('vol')) %>%
#   .[ , which(.[1,] == 'm3')] %>%
#   .[ , 1:2] %>% # 3rd cols zeros
#   set_df(.) %>%
#   set_colnames(., create_var_names('BeltMuckVol', 2)) %>%
#   mutate(BeltMuckVol = rowSums(., na.rm = T))
#   
# 
# var_belt_muck_weight <- data %>%
#   select(contains('belt')) %>%
#   select(contains('weight')) %>%
#   .[ , which(.[1,] == 'ton')] %>%
#   .[, 1:2] %>%
#   set_df(.) %>%
#   set_colnames(., create_var_names('BeltMuckWeight', 2)) %>%
#   mutate(BeltMuckWeight = rowSums(., na.rm = T))

# Bind -------------------------------------------------------------------------

group_muck <- cbind(var_screw_rot, 
                    var_screw_torque, 
                    var_screw_pres,
                    var_screw_gate_open,
                    var_screw_gate_pres)#, 
                    # var_screwcas_pres, 
                    # var_screw_muck_flow, 
                    # var_screw_muck_vol,
                    # var_screw_gate,
                    # var_screw_polymer,
                    # var_belt_muck_weight, 
                    # var_belt_muck_vol)
