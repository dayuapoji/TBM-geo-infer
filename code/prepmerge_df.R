prep_df <- function(data_orig) {
  
  # SELECT FEATURES ##############################################################
  
  # Remove Unused Columns and Rows -----------------------------------------------
  
  data <- data_orig %>% 
    
    # remove columns with 'NA' unit
    .[ , which(!is.na(.[1, ]))] %>% # 45 features
    # remove columns with categorical data
    .[, which(.[1,] != 'on/off')] %>% # 2436 features
    .[, which(.[1,] != 'off/on')] %>%
    select(!contains('status')) %>% # 1 features
    # remove unused columns, i.e. contains (Not Used) & [DEL]
    select(!contains('(Not Used)')) %>% # 8 features
    select(!contains('[DEL]')) %>% # 249 features
    # remove calculated values
    select(!contains('calc')) %>% # 5 features
    # remove columns contain information on cumulative
    select(!contains('cumulative')) %>% # 208 features
    select(!contains('accumulated')) %>%
    select(!contains('count')) %>% # 110 features
    select(!contains('counter')) %>%
    select(!contains('day')) %>%
    select(!contains('shift')) %>%
    # remove columns contain information on target
    select(!contains('setting')) %>%
    select(!contains('target')) %>%
    select(!contains('tgt')) %>%
    # remove columns contain information on capacity
    select(!contains('capacity')) %>%
    select(!contains('range')) %>%
    select(!contains('level')) %>%
    select(!contains('max')) %>%
    select(!contains('min')) %>%
    select(!contains('limit'))
  
  
  # Position ---------------------------------------------------------------------
  
  ring <- data %>%
    select(contains('ring no')) %>%
    set_df(.) %>%
    set_colnames(., 'Ring')
    
  
  chainage <- data %>%
    select(contains('chainage')) %>% 
    select(!contains('gyro')) %>%
    # .[ , which(.[1,] == 'ft')] %>% 
    set_df(.) %>%
    set_colnames(., c('ChainageHead',
                      'ChainageArt',
                      'ChainageTail'))
  
  # coord <- data %>%
  #   select(contains('crd')) %>%
  #   .[,c(1:9)] %>%
  #   set_df(.) %>%
  #   # select(contains('head')) %>%
  #   set_colnames(c('NHead', 'NArt', 'NTail',
  #                  'EHead', 'EArt', 'ETail',
  #                  'ZHead', 'ZArt', 'ZTail'))
  
  
  
  # SELECT FEATURES - CUTTER #####################################################
  
  # Cutter ----
  
  var_cutter_rot <- data %>%
    select(contains('cutter')) %>%
    select(contains('speed')) %>%
    # select(!contains('copy')) %>% 
    .[ , which(.[1,] == 'r/min')] %>%
    set_df(.) %>%
    set_colnames(., 'CutterRotSpeed')
  
  var_cutter_torque <- data %>%
    select(contains('cutter')) %>%
    select(contains('torque')) %>% 
    .[ , which(.[1,] != '%')] %>% 
    set_df(.) %>%
    mutate(CutterTorque = rowSums(., na.rm = T)) %>%
    select(contains('CutterTorque'))
  
  # var_cutter_force <- data %>%
  #   select(contains('cutter')) %>%
  #   select(contains('force')) %>%
  #   select(contains('total')) %>%
  #   select(!contains('articulation')) %>%
  #   .[ , which(.[1,] == 'kN')] %>% 
  #   set_df(.) %>%
  #   set_colnames(., c('CutterForce'))
  
  # Copy Cutter ----
  
  # var_copy_cutter_pres <- data %>%
  #   select(contains('cutter')) %>% 
  #   select(contains('copy')) %>%
  #   select(contains('pres')) %>%
  #   .[ , which(.[1,] == 'bar')] %>% 
  #   set_df(.) %>%
  #   set_colnames(., c('CopyCutterPres1', 'CopyCutterPres2')) %>%
  #   mutate(CopyCutterPres = rowSums(., na.rm = T))
  
  # var_copy_cutter_stroke <- data %>%
  #   select(contains('cutter')) %>%
  #   select(contains('copy')) %>%
  #   select(contains('use')) %>%
  #   .[ , which(.[1,] == 'mm')] %>%
  #   set_df(.) %>%
  #   set_colnames(., c('CopyCutterStroke1', 'CopyCutterStroke2')) %>%
  #   mutate(CopyCutterStroke = rowSums(., na.rm = T))
  # 
  # var_copy_cutter_pos <- data %>%
  #   select(contains('cutter')) %>% 
  #   select(contains('copy')) %>%
  #   select(contains('position')) %>%
  #   .[ , which(.[1,] == 'deg')] %>% 
  #   set_df(.) %>%
  #   set_colnames(., c('CopyCutterPos1', 'CopyCutterPos2')) %>%
  #   mutate(CopyCutterPos = rowSums(., na.rm = T))
  
  # Bind -------------------------------------------------------------------------
  
  group_excav <- cbind(var_cutter_rot,
                       var_cutter_torque) #,
  # var_cutter_force,
  # var_copy_cutter_pres,
  # var_copy_cutter_stroke,
  # var_copy_cutter_pos)
  
  
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
  
  
  # SELECT FEATIRES - STEERING ###################################################
  
  # Thrust Difference ---------------------------------------------------------         
  
  # var_diff_force <- data %>%
  #   select(contains('cutter')) %>%
  #   select(contains('force')) %>%
  #   select(contains('diff')) %>%
  #   select(!contains('articulation')) %>%
  #   .[ , which(.[1,] == 'kN')] %>% 
  #   set_df(.) %>%
  #   set_colnames(., c('CutterForceDifLR', 'CutterForceDifTB'))
  # 
  # var_diff_stroke <- data %>%
  #   select(contains('thrust')) %>%
  #   select(contains('dif')) %>%
  #   select(!contains('start') & !contains('finish')) %>%
  #   .[ , which(.[1,] == 'mm')] %>%
  #   set_df(.) %>%
  #   set_colnames(., c('ThrustStrokeDifLR', 'ThrustStrokeDifTB'))
  
  # Articulation --------------------------------------------------------------         
  
  # var_artic_pres <- data %>%
  #   select(contains('thrust') | contains('jack')) %>%
  #   select(contains('pres')) %>%
  #   select(contains('artic')) #%>%
  #   select(!contains('erector') & !contains('traction') & !contains('conveyor') & !contains('(rod)')) %>%
  #   .[ , which(.[1,] == 'bar')] %>%
  #   set_df(.) %>%
  #   set_colnames(., c('ArticThrustPres',
  #                     create_var_names('ArticThrustPres', 2)))
  # 
  # var_artic_force <- data %>%
  #   select(contains('thrust') | contains('jack')) %>%
  #   select(contains('force')) %>%
  #   select(contains('artic')) %>%
  #   .[ , which(.[1,] == 'kN')] %>%
  #   set_df(.) %>%
  #   set_colnames(., 'ArticThrustForce')
  # 
  # var_artic_stroke <- data %>%
  #   select(contains('thrust') | contains('jack') | contains('net stroke')) %>%
  #   select(contains('stroke')) %>%
  #   select(contains('artic')) %>%
  #   select(!contains('erector')) %>%
  #   select(!contains('robotec')) %>%
  #   .[ , which(.[1,] == 'mm')] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('ArticThrustStroke', 4)) %>%
  #   mutate(ArticThrustStrokeAve = rowMeans(., na.rm = T))
  # 
  # var_artic_angle <- data %>% 
  #   select(contains('articulation')) %>%
  #   .[ , which(.[1,] == 'deg')] %>%
  #   set_df(.) %>%
  #   set_colnames(., c('ArticAngleLR', 'ArticAngleTB'))
  
  # Shield Attitude --------------------------------------------------------------         
  
  var_shield_attitude <- data %>%
    select(contains('pitch') | contains('roll') | contains('bear')) %>%
    select(contains('front') | contains('back')) %>%
    select(!contains('input') & !contains('signal') & !contains('gyro')) %>%
    .[ , which(.[1,] == 'deg')] %>% 
    set_df(.) %>%
    set_colnames(., c('PitchFront', 'RollFront', 
                      'PitchRear', 'RollRear'))
  
  # plot_grid(
  #   ggplot(cbind(ring, 
  #                var_shield_attitude %>%
  #                  select(contains('pitch'))) %>% 
  #            melt(., id = "Ring")) +
  #     geom_line(aes(x = Ring, y = value, color = variable)) +
  #     ylab('Pitch (deg)') +
  #     ylim(-2, 2) +
  #     theme_bw(),
  #   
  #   ggplot(cbind(ring, 
  #                var_shield_attitude %>%
  #                  select(contains('roll'))) %>% 
  #            melt(., id = "Ring")) +
  #     geom_line(aes(x = Ring, y = value, color = variable)) +
  #     ylab('Roll (deg)') +
  #     ylim(-2, 2) +
  #     theme_bw(),
  #   
  #   ncol = 1)
  
  
  
  # Deviations -------------------------------------------------------------------         
  
  var_shield_deviation <- data %>%
    select(contains('deviation')) %>% 
    select(!contains('gyro')) %>%
    select(!contains('azimuth')) %>%
    .[ , which(.[1,] == 'mm')] %>% 
    set_df(.) %>%
    set_colnames(., c(#'ArticDevLR', 'ArticDevTB', 
      'DevHeadHor', 'DevArtHor', 'DevTailHor',
      'DevHeadVer', 'DevArtVer', 'DevTailVer'))
  
  # plot_grid(
  #   
  #   ggplot(cbind(ring,
  #                var_shield_deviation %>%
  #                  select(contains('ver'))) %>%
  #            melt(., id = "Ring")) +
  #     geom_line(aes(x = Ring, y = value, color = variable)) +
  #     ylab('Vertical Deviation (mm)') +
  #     scale_y_continuous(n.breaks = 8) +
  #     # ylim(-100, 100) +
  #     theme_bw(),
  #   
  #   ggplot(cbind(ring,
  #                var_shield_deviation %>%
  #                  select(contains('hor'))) %>%
  #            melt(., id = "Ring")) +
  #     geom_line(aes(x = Ring, y = value, color = variable)) +
  #     ylab('Horizontal Deviation (mm)') +
  #     scale_y_continuous(n.breaks = 16) +
  #     # ylim(-100, 100) +
  #     theme_bw(),
  # 
  #   ncol = 1)
  
  # Bind -------------------------------------------------------------------------
  
  group_steer <- cbind(#var_diff_force, var_diff_stroke,
    #var_artic_pres, var_artic_force, 
    #var_artic_stroke, var_artic_angle,
    var_shield_attitude, 
    var_shield_deviation)
  
  
  # SELECT FEATURES - GROUND CONDITIONING ########################################
  
  # Foam -------------------------------------------------------------------------
  
  var_foam_pres <- data %>%
    select(contains('foam')) %>%
    select(contains('pres')) %>%
    select(contains('ave')) %>%
    select(!contains('air')) %>%
    #   .[ , which(.[1,] == 'bar')] %>%
    set_df(.) %>%
    set_colnames(., 'FoamPres')
  #   set_colnames(., create_var_names('FoamPres', 32)) %>%
  #   mutate(FoamPres = rowMeans(., na.rm = T))
  # 
  var_foam_flow <- data %>%
    select(contains('foam')) %>%
    select(contains('flow')) %>%
    select(!contains('air')) %>%
    #   select(!contains('agent')) %>%
    select(contains('total')) %>%
    .[ , which(.[1,] == 'L/min')] %>%
    set_df(.) %>%
    set_colnames(., 'FoamFlow')
  #   set_colnames(., create_var_names('FoamFlow', 32)) %>%
  #   mutate(FoamFlow = rowSums(., na.rm = T))
  
  var_foam_vol <- data %>%
    select(contains('foam')) %>%
    select(contains('accum')) %>%
    # select(!contains('water')) %>%
    # select(!contains('liquid')) %>%
    # select(!contains('agent')) %>%
    # select(!contains('foam volume')) %>%
    .[ , which(.[1,] == 'm3')] %>%
    set_df(.) %>%
    .[,9] %>% # select total vol only
    set_colnames(., 'FoamVol') #create_var_names('FoamVol', 32)) %>%
  # mutate(FoamVol = rowSums(., na.rm = T))
  
  # var_foam_fill_rate <- data %>%
  #   select(contains('foam')) %>%
  #   select(contains('fill rate')) %>%
  #   .[ , which(.[1,] == '%')] %>%
  #   set_df(.) %>%
  #   set_colnames(., 'FoamFillRate')
  
  # Air --------------------------------------------------------------------------
  
  var_air_pres <- data %>%
    select(contains('air')) %>%
    select(contains('pres')) %>%
    select(contains('avg')) %>%
    #   .[ , which(.[1,] == 'bar')] %>%
    set_df(.) %>%
    set_colnames(., 'AirPres')
  #   set_colnames(., create_var_names('AirPres', 8)) %>%
  #   mutate(AirPres = rowMeans(., na.rm = T))
  # 
  var_air_flow <- data %>%
    select(contains('air')) %>%
    select(contains('flow')) %>%
    select(!contains('integrating')) %>%
    #   select(!contains('total')) %>%
    #   .[ , which(.[1,] == 'L/min')] %>%
    set_df(.) %>%
    # set_colnames(., create_var_names('AirFlow', 8)) %>%
    mutate(AirFlow = rowSums(., na.rm = T)) %>%
    .[,9] # select total only
  
  var_air_vol <- data %>%
    select(contains('air')) %>%
    select(contains('accum')) %>%
    select(contains('total')) %>%
    .[ , which(.[1,] == 'm3')] %>%
    set_df(.) %>%
    set_colnames(., 'AirVol') #create_var_names('AirVol', 32)) %>%
  # mutate(AirVol = rowSums(., na.rm = T))
  
  # Foam Liquid (Solution) -------------------------------------------------------
  
  # var_foam_liquid_pres <- data %>%
  #   select(contains('foam liquid')) %>% 
  #   select(contains('pres')) %>%
  #   .[ , which(.[1,] == 'bar')] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('FoamLiquidPres', 32)) %>%
  #   mutate(FoamLiquidPres = rowMeans(., na.rm = T))
  # 
  # var_foam_liquid_flow <- data %>%
  #   select(contains('foam liquid')) %>% 
  #   select(contains('flow')) %>%
  #   select(!contains('water')) %>%
  #   select(!contains('bent')) %>%
  #   .[ , which(.[1,] == 'L/min')] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('FoamLiquidFlow', 32)) %>%
  #   mutate(FoamLiquidFlow = rowSums(., na.rm = T))
  # 
  # var_foam_liquid_vol <- data %>%
  #   select(contains('foam liquid')) %>%
  #   select(contains('vol')) %>%
  #   select(!contains('water')) %>%
  #   select(!contains('bent')) %>%
  #   select(!contains('batch')) %>%
  #   select(!contains('total')) %>%
  #   .[ , which(.[1,] == 'm3')] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('FoamLiquidVol', 32)) %>%
  #   mutate(FoamLiquidVol = rowSums(., na.rm = T))
  
  # Water ------------------------------------------------------------------------
  
  # var_water_vol <- data %>% 
  #   select(contains('foam water')) %>%
  #   .[ , which(.[1,] == 'm3')] %>%
  #   .[ , 1:2] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('WaterVol', 2)) %>%
  #   mutate(WaterVol = rowSums(., na.rm = T))
  
  # Foam Agent -------------------------------------------------------------------
  
  # var_foam_agent_flow <- data %>% 
  #   select(contains('foam agent')) %>% 
  #   select(contains('flow')) %>%
  #   select(!contains('volume')) %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('FoamAgentFlow', 6)) %>%
  #   mutate(FoamAgentFlow = rowSums(., na.rm = T))
  # 
  # var_foam_agent_vol <- data %>% 
  #   select(contains('foam agent')) %>% 
  #   select(contains('volume')) %>%
  #   select(!contains('flow')) %>%
  #   select(!contains('batch')) %>%
  #   .[ , c(13:14)] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('FoamAgentVol', 2)) %>%
  #   mutate(FoamAgentVol = rowSums(., na.rm = T))
  
  
  # Polymer Agent  ---------------------------------------------------------------
  
  # var_polymer_agent_flow <- data %>%
  #   select(contains('polymer agent')) %>%
  #   select(!contains('volume')) %>%
  #   select(contains('flow')) %>%
  #   .[ , which(.[1,] == 'L/min')] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('PolymerAgentFlow', 6)) %>%
  #   mutate(PolymerAgentFlow = rowSums(., na.rm = T))
  # 
  # var_polymer_agent_vol <- data %>%
  #   select(contains('polymer agent')) %>%
  #   select(contains('volume')) %>%
  #   select(!contains('flow')) %>%
  #   select(!contains('batch')) %>%
  #   .[ , c(13:18)] %>%
  #   .[ , which(.[1,] == 'm3')] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('PolymerAgentVol', 6)) %>%
  #   mutate(PolymerAgentVol = rowSums(., na.rm = T))
  
  
  # Polymer ----------------------------------------------------------------------
  
  # var_polymer_pres <- data %>%
  #   select(contains('polymer')) %>%
  #   select(contains('pres')) %>%
  #   select(contains('port')) %>%
  #   .[ , which(.[1,] == 'bar')] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('PolymerPres', 15)) %>%
  #   mutate(PolymerPres = rowMeans(., na.rm = T))
  # 
  var_polymer_flow <- data %>%
    select(contains('polymer')) %>%
    select(contains('flow')) %>%
    select(contains('integrating')) %>%
    # .[ , which(.[1,] == 'L/min')] %>%
    set_df(.) %>%
    #   set_colnames(., create_var_names('PolymerFlow', 15)) %>%
    mutate(PolymerFlow = rowSums(., na.rm = T)) %>%
    .[,4] # select total only
  
  # 
  var_polymer_vol <- data %>%
    select(contains('polymer')) %>%
    #   select(contains('vol')) %>%
    select(contains('total')) %>%
    #   .[ , which(.[1,] == 'm3')] %>%
    .[,4] %>% # select total only
    set_df(.) %>%
    set_colnames(., 'PolymerVol')
  #   set_colnames(., create_var_names('PolymerVol', 15)) %>%
  #   mutate(PolymerVol = rowSums(., na.rm = T))
  
  
  # Slurry ---------------------------------------------------------------------
  
  # var_slurry_vol <- data %>%
  #   select(contains('slurry')) %>%
  #   select(!contains('bent')) %>%
  #   .[ , which(.[1,] == 'm3')] %>%
  #   set_df(.) %>%
  #   set_colnames(., c(create_var_names('SlurryVol', 26), 'SlurryVol'))
  # 
  var_slurry_flow <- data %>%
    select(contains('shock')) %>%
    #   select(contains('flow')) %>%
    select(!contains('liquid')) %>%
    .[ , which(.[1,] == 'L/min')] %>%
    set_df(.) %>%
    mutate(SlurryFlow = rowSums(., na.rm = T)) %>%
    .[, 3]
  #   set_colnames(., c('SlurryFlow', create_var_names('SlurryFlow', 26)))
  
  var_slurry_vol <- data %>%
    select(contains('shock')) %>%
    .[ , which(.[1,] == 'm3')] %>%
    .[, 7] %>% # select total only
    set_df(.) %>%
    set_colnames(., 'SlurryVol')
  
  
  var_slurry_pres <- data %>%
    select(contains('shock')) %>%
    select(contains('pres')) %>%
    .[ , which(.[1,] == 'MPa')] %>%
    .[,1] %>%
    set_df(.) %>%
    set_colnames(., 'SlurryPres')
  #   set_colnames(., create_var_names('SlurryPres', 26)) %>%
  #   mutate(SlurryPres = rowMeans(., na.rm = T))
  
  # Additive ---------------------------------------------------------------------
  
  var_additive_vol <- data %>%
    select(contains('additive')) %>%
    # select(contains('vol')) %>%
    #   select(!contains('total')) %>%
    .[ , which(.[1,] == 'L')] %>%
    set_df(.) %>%
    #   set_colnames(., create_var_names('AdditiveVol', 15)) %>%
    mutate(AdditiveVol = rowSums(., na.rm = T)) %>%
    select(contains('additivevol'))
  
  var_additive_flow <- data %>%
    select(contains('additive')) %>%
    # select(contains('vol')) %>%
    #   select(!contains('total')) %>%
    .[ , which(.[1,] == 'L/min')] %>%
    set_df(.) %>%
    #   set_colnames(., create_var_names('AdditiveVol', 15)) %>%
    mutate(AdditiveFlow = rowSums(., na.rm = T)) %>%
    select(contains('additiveflow'))
  
  var_additive_pres <- data %>%
    select(contains('additive')) %>%
    select(contains('pres')) %>%
    #   select(!contains('total')) %>%
    # .[ , which(.[1,] == 'L/min')] %>%
    set_df(.) %>%
    #   set_colnames(., create_var_names('AdditiveVol', 15)) %>%
    mutate(AdditivePres = rowMeans(., na.rm = T)) %>%
    select(contains('additivepres'))
  # 
  # var_additive_fill_rate <- data %>%
  #   select(contains('additive')) %>%
  #   select(contains('fill rate')) %>%
  #   .[ , which(.[1,] == '%')] %>%
  #   set_df(.) %>%
  #   set_colnames(., 'AdditiveFillRate')
  
  # Bind -------------------------------------------------------------------------
  
  group_gcs <- cbind(var_foam_flow, var_foam_vol, #var_foam_pres,#var_foam_fill_rate,
                     var_air_flow, var_air_vol, #var_air_pres,
                     # var_foam_liquid_pres, var_foam_liquid_flow, var_foam_liquid_vol,
                     # var_water_vol,
                     # var_foam_agent_flow, var_foam_agent_vol,
                     var_polymer_flow, var_polymer_vol, #var_polymer_pres,
                     # var_polymer_agent_flow, var_polymer_agent_vol,
                     var_slurry_flow, var_slurry_vol, #var_slurry_pres,
                     var_additive_vol, var_additive_flow) #var_additive_pres)
  
  
  
  # SELECT FEATURES - EPB ########################################################
  
  var_chamber_pres <- data %>%
    select(contains('ep')) %>%
    select(contains('average')) %>%
    # select(contains('No.')) %>%
    .[ , which(.[1,] == 'kPa')] %>%
    set_df(.) %>%
    set_colnames(., 'ChamberPres')# create_var_names('ChamberPres', 12)) %>%
  # mutate(ChamberPres = rowMeans(., na.rm = T))
  
  # var_circum_pres <- data %>%
  #   select(contains('soil')) %>%
  #   select(contains('outer')) %>%
  #   .[ , which(.[1,] == 'bar')] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('CircumPres', 6)) %>%
  #   mutate(CircumPres = rowMeans(., na.rm = T))
  # 
  # var_backfill_pres <- data %>%
  #   select(contains('soil')) %>%
  #   select(contains('back-fill')) %>%
  #   .[ , which(.[1,] == 'bar')] %>%
  #   set_df(.) %>%
  #   set_colnames(., create_var_names('BackFillPres', 6)) %>%
  #   mutate(BackFillPres = rowMeans(., na.rm = T))
  
  # Bind -------------------------------------------------------------------------
  
  group_epb <-cbind(var_chamber_pres) #, 
  # var_circum_pres,
  # var_backfill_pres)
  
  
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
  
  # SELECT FEATURES - BACKFILL GROUTING ##########################################
  
  # Grout --------------------------------------------------------------------------
  
  var_grout_pres <- data %>%
    select(contains('grout')) %>%
    select(contains('pres')) %>%
    # .[ , which(.[1,] == 'MPa')] %>%
    set_df(.) %>%
    mutate(TailGroutPres = rowMeans(., na.rm = T)) %>%
    select(contains('tailgroutpres'))
  
  # var_grout_flow <- data %>%
  #   select(contains('grout')) %>% 
  #   select(contains('flow')) %>%
  #   select(!contains('A+B')) %>%
  #   # .[ , which(.[1,] == 'L/min')] %>%
  #   set_df(.) %>%
  #   set_colnames(., c(create_var_names('GroutFlowA', 2),
  #                     create_var_names('GroutFlowB', 2),
  #                     'GroutFlow'))
  # 
  # var_grout_vol <- data %>%
  #   select(contains('grout')) %>%
  #   select(contains('vol')) %>%
  #   .[ , which(.[1,] == 'm3')] %>%
  #   .[, 1:22] %>%
  #   set_df(.) %>%
  #   set_colnames(., c(create_var_names('GroutVolA', 11),
  #                     create_var_names('GroutVolB', 11))) %>%
  #   mutate(GroutVol = rowSums(., na.rm = T))
  
  # var_grout_fill_rate <- data %>%
  #   select(contains('grout')) %>%
  #   select(contains('fill rate')) %>%
  #   .[ , which(.[1,] == '%')] %>%
  #   set_df(.) %>%
  #   set_colnames(., 'GroutFillRate')
  
  # Bind -------------------------------------------------------------------------
  
  group_grout <- cbind(var_grout_pres) #, 
  # var_grout_flow, 
  # var_grout_vol) #, 
  # var_grout_fill_rate)
  
  
  chainage_head <- chainage[, 'ChainageHead'] %>%
    set_colnames('Chainage')
  
  
  df <- cbind(ring, chainage_head, group_excav, group_advance, group_steer,
              group_gcs, group_epb, group_muck, group_grout) %>% # 
    filter(Chainage > 0) %>% 
    filter(CutterTorque > 0) %>% 
    distinct(., Chainage, .keep_all = TRUE) %>%
    drop_na()
  
  return(df)
  
  }






