# SELECT FEATURES - THRUST #####################################################

var_thrust_speed <- data %>%
  select(contains('thrust') | contains('jack')) %>%
  select(contains('speed')) %>%
  select(contains('average')) %>%
  .[ , which(.[1,] == 'mm/min')] %>%
  set_df(.) %>%
  set_colnames(., c('AdvanceSpeed'))#, create_var_names('ThrustSpeed', 8)))

var_thrust_pres <- data %>%
  select(contains('thrust') | contains('jack')) %>%
  select(contains('pres')) %>%
  select(!contains('artic') &
           !contains('erector') &
           !contains('traction') &
           !contains('conveyor') &
           !contains('holding') &
           !contains('ground') &
           !contains('(rod)')) %>%
  .[ , which(.[1,] == 'MPa')] %>%
  set_df(.) %>%
  mutate(ThrustPres = rowSums(., na.rm = T)) %>%
  .[,'ThrustPres']

var_thrust_force <- data %>%
  select(contains('thrust')) %>%# | contains('jack')) #%>%
  # select(contains('force')) %>%
  select(contains('cutter')) %>%
  select(!contains('artic')) %>%
  .[ , which(.[1,] == 'kN')] %>%
  set_df(.) %>%
  set_colnames(., 'ThrustForce')

var_thrust_stroke <- data %>%
  # select(contains('thrust') | contains('jack') | contains('net stroke')) %>%
  select(contains('net stroke')) %>%
  # select(!contains('erector')) %>%
  select(!contains('survey')) %>%
  select(!contains('artic')) %>%
  select(!contains('dif')) %>%
  .[ , which(.[1,] == 'mm')] %>%
  set_df(.) %>%
  set_colnames(., 'ThrustStrokeNet')

# Bind -------------------------------------------------------------------------

group_advance <- cbind(var_thrust_speed, 
                       var_thrust_pres,
                       var_thrust_force, 
                       var_thrust_stroke)
